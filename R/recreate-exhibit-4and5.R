# SETUP -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(snakecase)

g <- glimpse

# LOAD DATA ---------------------------------------------------------------

tract_data_filepath <- "data/source-files/2011thru2015-140-csv.zip"

county_data_filepath <- "data/source-files/2011thru2015-050-csv.zip"


# Unzip

unzip(tract_data_filepath, exdir = "data/source-files")
unzip(county_data_filepath, exdir = "data/source-files")


data_dictionary <- 
  readxl::read_excel("data/source-files/140/CHAS data dictionary 11-15.xlsx", 
                     sheet = "Table 8") %>% 
  janitor::clean_names("screaming_snake") %>% 
  mutate_all(to_screaming_snake_case) %>% 
  mutate(COLUMN_NAME = snakecase::to_screaming_snake_case(COLUMN_NAME)) %>% 
  mutate(COLUMN_NAME = str_replace(COLUMN_NAME,"T_8","T8"),
         COLUMN_NAME = str_replace(COLUMN_NAME,"EST_","EST"))

tr <- 
  read_csv("data/source-files/140/Table8.csv") %>% 
  janitor::clean_names("screaming_snake") %>%
  dplyr::mutate_at(dplyr::vars(-dplyr::matches("T8")),as.character)

cnty <- 
  read_csv("data/source-files/050/Table8.csv") %>% 
  janitor::clean_names("screaming_snake") %>%
  dplyr::mutate_at(dplyr::vars(-dplyr::matches("T8")),as.character)



# recode the columns to make it easier to work with them

# dd_join <-
data_dictionary %>%  
  mutate(TENURE = case_when(
    TENURE == "OWNER_OCCUPIED" ~ "OWN",
    TENURE == "RENTER_OCCUPIED" ~ "RENT",
    TENURE == "TOTAL_OCCUPIED_HOUSING_UNITS" ~ "TOTAL",
    TRUE ~ NA_character_
  )) %>% 
  mutate(HOUSEHOLD_INCOME = case_when(
    HOUSEHOLD_INCOME == "ALL" ~ "ALL",
    HOUSEHOLD_INCOME == "GREATER_THAN_100_OF_HAMFI" ~ "GT100",
    HOUSEHOLD_INCOME == "GREATER_THAN_30_BUT_LESS_THAN_OR_EQUAL_TO_50_OF_HAMFI" ~ "BTWN_30_50",
    HOUSEHOLD_INCOME == "GREATER_THAN_50_BUT_LESS_THAN_OR_EQUAL_TO_80_OF_HAMFI" ~ "BTWN_50_80",
    HOUSEHOLD_INCOME == "GREATER_THAN_80_BUT_LESS_THAN_OR_EQUAL_TO_100_OF_HAMFI" ~ "BTWN_80_100",
    HOUSEHOLD_INCOME == "LESS_THAN_OR_EQUAL_TO_30_OF_HAMFI" ~ "LT_30",
    TRUE ~ NA_character_ 
  )) %>%  
  mutate(COST_BURDEN = case_when(
    COST_BURDEN == "ALL" ~ "ALL",
    COST_BURDEN == "GREATER_THAN_30_BUT_LESS_THAN_OR_EQUAL_TO_50" ~ "GT30_LTEQ50",
    COST_BURDEN == "GREATER_THAN_50" ~ "GT50",
    COST_BURDEN == "LESS_THAN_OR_EQUAL_TO_30" ~ "LTEQ30",
    COST_BURDEN == "NOT_COMPUTED_NO_NEGATIVE_INCOME" ~ "NC",
    TRUE ~ NA_character_
  )) %>% 
  mutate(AFFORD_70AMI = case_when(
    HOUSEHOLD_INCOME %in% c("LT_30", "BTWN_30_50") ~ "can not afford",
    HOUSEHOLD_INCOME %in% c("BTWN_50_80") ~ "can afford (maybe)",
    TRUE ~ "can afford"
  )) %>%  
  mutate(COLNAME_DESC = str_c(LINE_TYPE,TENURE, HOUSEHOLD_INCOME, sep = "_")) 

skyway_tracts <- c("026100", "026001")

tr_skyway <-
  tr %>% 
  filter(TRACT %in% skyway_tracts)

tr_long <-
  tr_skyway %>% 
  gather(COLUMN_NAME, VALUE, starts_with("T8")) %>% 
  left_join(dd_join, by = "COLUMN_NAME") %>% 
  select(NAME,
         TRACT,
         COLUMN_NAME:COLNAME_DESC)


# PREPARE DATA FOR EXHIBIT 4 ----------------------------------------------

# create the tenure by income level data

ex4_own_rent <-
  tr_long %>%  
  filter(LINE_TYPE %in% c("SUBTOTAL")) %>% 
  filter(COST_BURDEN %in% "ALL") %>%   
  group_by(TENURE, HOUSEHOLD_INCOME) %>% 
  summarise(COUNT = sum(VALUE))

ex4_own_rent_all <-
  ex4_own_rent %>%  
  group_by(HOUSEHOLD_INCOME) %>% 
  summarise(COUNT = sum(COUNT)) %>% 
  mutate(TENURE = "ALL") %>% 
  bind_rows(ex4_own_rent)

# calculate the percentage of each subtotal (for labeling the plot)

mode <- function(x) {
  ux <- unique(x[which(!is.na(x))])
  ux[which.max(tabulate(match(x, ux)))]
}

ex4_pct <- 
  ex4_own_rent_all %>% 
  mutate(ROLE = case_when(
    HOUSEHOLD_INCOME == "ALL" ~ str_c(TENURE,"_TOTAL"),
    TRUE ~ "COUNT"
  )) %>% 
  spread(ROLE, COUNT) %>% 
  mutate_at(vars(ends_with("TOTAL")), mode) %>% 
mutate(PERCENT = case_when(
  TENURE == "ALL" ~ COUNT/ALL_TOTAL,
  TENURE == "RENT" ~ COUNT/RENT_TOTAL,
  TENURE == "OWN" ~ COUNT/OWN_TOTAL
)) %>% 
  mutate(PERCENT_LABEL = scales::percent(PERCENT,accuracy = 1)) %>% 
  filter(! HOUSEHOLD_INCOME == "ALL")  

# set the factor levels (important for intuitive ordering of plot elements)

lvls <- list(
  tenure = c("All Households", "Renter",  "Owner"),
  income = c("Extremely Low-Income (<=30% AMFI)",
             "Very Low-Income (<30-50% AMFI)",
             "Low-Income (50-80% AMFI)",
             "Moderate-Income (80-100% AMFI)",
             "Above Median Income (>=100% AMFI)")
)

ex4_ready <- 
  ex4_pct %>% 
  mutate(TENURE = case_when(
    TENURE == "ALL" ~ "All Households",
    TENURE == "RENT" ~ "Renter", 
    TENURE == "OWN" ~ "Owner",
    TRUE ~ NA_character_
  )) %>% 
  mutate(
    HOUSEHOLD_INCOME = case_when(
      HOUSEHOLD_INCOME == "LT_30" ~ "Extremely Low-Income (<=30% AMFI)",
      HOUSEHOLD_INCOME == "BTWN_30_50" ~ "Very Low-Income (<30-50% AMFI)",
      HOUSEHOLD_INCOME == "BTWN_50_80" ~ "Low-Income (50-80% AMFI)",
      HOUSEHOLD_INCOME == "BTWN_80_100" ~ "Moderate-Income (80-100% AMFI)",
      HOUSEHOLD_INCOME == "GT100" ~ "Above Median Income (>=100% AMFI)",
      TRUE ~ NA_character_
    ) 
  ) %>% 
  mutate(TENURE = factor(TENURE, levels = lvls[["tenure"]], ordered = TRUE),
         HOUSEHOLD_INCOME = factor(HOUSEHOLD_INCOME, levels = lvls[["income"]], ordered = TRUE)
  )

# CREATE EXHIBIT 4 PLOT ---------------------------------------------------

# color palette

pal <- RColorBrewer::brewer.pal(11,"RdYlBu")[c(3,4,8,9,10)] %>% 
  set_names(lvls[["income"]])

gg <- ggplot(data = ex4_ready,
             aes(x = fct_rev(TENURE), y = COUNT, fill = fct_rev(HOUSEHOLD_INCOME)))

gg <- gg + geom_bar(stat = "identity", position = "fill",width = .5)

gg <- gg + geom_text(aes(x = fct_rev(TENURE), 
                         y = PERCENT,  
                         label = PERCENT_LABEL,
                         hjust = .5),  
                     position = position_fill(vjust = 0.5),
                     size = rel(3))

gg <- gg + geom_hline(yintercept = -.005, size = .5, color="gray")

gg <- gg + scale_fill_manual(values = pal)

gg <- gg + scale_y_continuous(breaks = seq(0,20000, by = 2500),labels = scales::comma) 

gg <- gg + coord_flip() 

gg <- gg + guides(fill = guide_legend(reverse = T))

gg <- gg + theme_minimal() +
  theme( 
    legend.position = "right",
    legend.box = "horizontal",
    legend.title = element_blank(), 
    legend.text = element_text(size = rel(0.75)),
    legend.key.size = unit(10, "points"),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(), 
    axis.line.y = element_blank(), 
    panel.grid = element_blank(),  
    plot.margin = margin(4,4,4,4,unit = "mm"),
    plot.caption = element_text(hjust = 0) 
  )
gg <- gg +labs(x = "", y = "",
               title = "Housing Tenure by Household Income Level (AMFI%)",
               subtitle = "Occupied housing units in Skyway and Bryn Mawr census tracts\n(Tracts 261 and 260.01)",
               caption = "Source: HUD CHAS (based on ACS 2011-2015 5-year estimates); Futurewise, 2019")

gg

ggsave("outputs/exhibit-4.png", width = 8, height = 4, units = "in", device = "png")
