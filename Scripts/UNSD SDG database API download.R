library(tidyverse)
library(httr)
library(jsonlite)

# Get ODW Master sheet of codes
odw_master_codes <- read_csv("https://raw.githubusercontent.com/lnoe10/odw_covid19_gender_indicators/master/Input/2021%20ODW%20Country%20and%20Region%20Codes.csv") %>%
  # Clean all variable names by converting to snake case
  janitor::clean_names() %>% 
  # Clear out extra lines at the bottom that just contain notes
  filter(!is.na(country_name)) %>%
  # Clear out duplicate Faroe Islands
  distinct(iso_3_code, .keep_all = TRUE) %>%
  # Keep only relevant indicators and rename for clarity 
  select(iso3c = iso_3_code, country = country_name, odw_region_name, un_code = un_m49_code,
         incgroup = world_bank_income_group_name, lending_cat = world_bank_lending_code_july_2020,
         wbregion = world_bank_all_income_region_names) %>%
  mutate(un_code = as.numeric(un_code), 
         incgroup = fct_relevel(incgroup, "Low income", "Lower middle income", "Upper middle income", "High income"))

# Get number of goals and goal codes
goals <- fromJSON("https://unstats.un.org/SDGAPI/v1/sdg/Goal/List")
goal_code <- goals %>%
  pull(code)

# Create dataframe that contains count of how many countries have data for each goal
df <- data.frame()
for (i in goal_code){
  df <- df %>%
    bind_rows(
      fromJSON(str_c("https://unstats.un.org/SDGAPI/v1/sdg/Goal/", i, "/GeoAreas")) %>%
        mutate(un_code = as.numeric(geoAreaCode)) %>%
        inner_join(odw_master_codes) %>%
        count() %>%
        mutate(goal = i) %>%
        select(goal, n))
}

# Get list of all indicators and indicator codes
indicators <- fromJSON("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/List")
indicator_code <- indicators %>%
  # Manually drop indicator 13.2.2 because it doesn't actually have any data, in this case no series
  # And will screw up loop below if left in
  # FIND WAY TO MAKE THIS PROGRAMMATIC
  filter(code != "13.2.2") %>%
  pull(code)

# Create dataframe that contains count of how many countries have data for each indicator
indicator_df <- data.frame()
for (i in indicator_code){
  indicator_df <- indicator_df %>%
    bind_rows(
      fromJSON(str_c("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/", i, "/GeoAreas")) %>%
        mutate(un_code = as.numeric(geoAreaCode)) %>%
        inner_join(odw_master_codes) %>%
        count() %>%
        mutate(indicator = i) %>%
        select(indicator, n))
}