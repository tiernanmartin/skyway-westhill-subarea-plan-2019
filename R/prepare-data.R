
# SETUP -------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)

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



# WRITE DATA TO FILE ------------------------------------------------------


