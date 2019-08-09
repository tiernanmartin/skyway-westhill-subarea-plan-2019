
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

# PREPARE DATA ------------------------------------------------------------

dd_join <-
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
  mutate(AFFORD_70AMI = case_when(
    HOUSEHOLD_INCOME %in% c("LT_30", "BTWN_30_50") ~ "can not afford",
    HOUSEHOLD_INCOME %in% c("BTWN_50_80") ~ "can afford (maybe)",
    TRUE ~ "can afford",
  )) %>% 
  mutate(COLNAME_DESC = str_c(LINE_TYPE,TENURE, HOUSEHOLD_INCOME, sep = "_")) 

skyway_tracts <- c("026100", "026001")

tr_skyway <-
  tr %>% 
  filter(TRACT %in% skyway_tracts)

tr_long <-
  tr_skyway %>% 
  gather(COLUMN_NAME, VALUE, starts_with("T8")) %>% 
  left_join(dd_join, by = "COLUMN_NAME") 

tr_ready <- 
tr_long %>% 
  filter(TENURE %in% "RENT") %>% 
  filter(LINE_TYPE %in% c("SUBTOTAL")) %>% 
  filter(COST_BURDEN %in% "ALL") %>% 
  group_by(COLNAME_DESC) %>% 
  summarise(AFFORD_70AMI = first(AFFORD_70AMI),
            COUNT = sum(VALUE)) %>% 
  mutate(COL_TYPE = if_else(str_detect(COLNAME_DESC,"RENT_ALL"),"TOTAL","SUBTOTAL")) %>% 
  spread(COL_TYPE, COUNT) %>% 
  fill(TOTAL) %>%  
  filter(! COLNAME_DESC %in% "SUBTOTAL_RENT_ALL") %>% 
  transmute(COLNAME_DESC,
            COLNAME_DESC_LABEL = case_when(
              str_detect(COLNAME_DESC,"GT100") ~ ">100",
              str_detect(COLNAME_DESC,"BTWN_30_50") ~ ">=30 but <50",
              str_detect(COLNAME_DESC,"BTWN_50_80") ~ ">=50 but <80",
              str_detect(COLNAME_DESC,"BTWN_80_100") ~ ">=80 but <100",
              str_detect(COLNAME_DESC,"LT_30") ~ "<30"
            ),
            COLNAME_DESC_LABEL = factor(COLNAME_DESC_LABEL,
                                        levels = c("<30",
                                                   ">=30 but <50",
                                                   ">=50 but <80",
                                                   ">=80 but <100",
                                                   ">100"),
                                        ordered = TRUE),
            AFFORD_70AMI = factor(AFFORD_70AMI,
                                  levels = c("can afford",
                                             "can afford (maybe)",
                                             "can not afford"),
                                  ordered = TRUE),
            COUNT = SUBTOTAL, 
            TOTAL,
            PERCENT = COUNT/TOTAL) %>% 
  mutate(SUM = sum(PERCENT),
         NONE = 3)




tr_by_afford70ami <-
tr_ready %>% 
  group_by(AFFORD_70AMI) %>% 
  summarize(COUNT = sum(COUNT),
            TOTAL = first(TOTAL),
            PERCENT = sum(PERCENT),
            NONE = first(NONE))

pal <- RColorBrewer::brewer.pal(11,"Spectral")[c(11,10,3)] %>% 
  set_names(c("can afford","can afford (maybe)","can not afford"))

y_can <- tr_by_afford70ami %>% filter(AFFORD_70AMI == "can afford") %>% pull("COUNT")

y_maybe <- tr_by_afford70ami %>% filter(AFFORD_70AMI == "can afford (maybe)") %>% pull("COUNT")

y_cant <- tr_by_afford70ami %>% filter(AFFORD_70AMI == "can not afford") %>% pull("COUNT")


# CREATE PLOT -------------------------------------------------------------

ggplot(data = tr_by_afford70ami,
       aes(x = NONE, y = COUNT, fill = AFFORD_70AMI)) +
  scale_x_continuous(limits = c(1,5)) +
  scale_y_continuous(breaks = seq(0,20000, by = 2500),labels = scales::comma) +
  scale_fill_manual(values = pal) + 
  geom_bar(stat = "identity", position = "stack",width = .25) +
  geom_hline(yintercept = sum(y_maybe, y_cant), size = .5, linetype="dashed") +
  geom_hline(yintercept = y_cant, size = .5, linetype="dashed") +
  geom_hline(yintercept = 0, size = .5, color="gray") +
  annotate("text",x = 2, y = sum(y_cant,y_maybe, y_can/2),
           label = 'atop(atop("", bold("income is 80% AMI or more")), atop("2,999 households", "(22.8% of renters)"))',
           parse = TRUE, colour = pal["can afford"]) +
  annotate("text",x = 2, y = sum(y_cant,y_maybe/2),
           label = 'atop(atop("", bold("income is between 50-80% AMI")), atop("2,405 households", "(18.3% of renters)"))', 
           parse = TRUE, colour = pal["can afford (maybe)"]) +
  annotate("text",x = 2, y = y_cant/2,
           label = 'atop(atop("", bold("income is between 0-50% AMI")), atop("7,765 households", "(59% of renters)"))', 
           parse = TRUE, colour = pal["can not afford"]) +
  annotate("text", x = 4, y = sum(y_cant,y_maybe, y_can/2), 
           label = "can afford\nthe 70% AMI rent threshold") + 
  annotate("text", x = 4, y = sum(y_cant,y_maybe/2),
           label = "probably can\'t afford\nthe 70% AMI rent threshold *") + 
  annotate("text", x = 4, y = y_cant/2,  
           label = "can\'t afford\nthe 70% AMI rent threshold") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.line.y = element_line(colour = "gray",size = .5), 
        axis.ticks.y = element_line(colour = "gray",size = .25),
        axis.text.y = element_text(color = "darkgray"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(4,4,4,4,unit = "mm"),
        plot.caption = element_text(hjust = 0)
          
        ) +
  labs(x = "", y = "",
       title = "Most Renters Can't Afford a 70% AMI Rent",
       subtitle = "Data: Renter households in the Skyway and Bryn Mawr census tracts",
       caption = "Data Source: 2011-2015 CHAS Data; includes Tracts 261 and 260.01 only\n \n* The CHAS data groups households earning 50-80% AMI, so while some members of this\ngroup may be able to afford 70% AMI rent, we assume that most renters in this income\ngroup can not.")


# SAVE PLOT TO FILE -------------------------------------------------------

ggsave("outputs/renters-by-AMI.png", width = 6.35, height = 5.55, units = "in", device = "png")

