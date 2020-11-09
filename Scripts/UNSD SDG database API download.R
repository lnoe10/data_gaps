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

# Add list of UN member states and manually include Palestine
un_member_states <- read_csv("Input/un_members.csv") %>%
  janitor::clean_names() %>%
  add_row(member_state = "Palestine") %>%
  mutate(un_code = countrycode::countrycode(member_state, "country.name", "un")) %>%
  select(un_code)

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
  # Filter out indicators with no series (i.e. no data) by counting rows in dataframes
  # that are nested in the series column.
  group_by(code) %>%
  mutate(rows = nrow(series[[1]])) %>%
  ungroup() %>%
  # Drop codes without any series attached. This is the same as having no actual data
  filter(rows != 0) %>%
  # Create vector
  pull(code)

# Create dataframe that contains count of how many countries have data for each indicator
indicator_df <- data.frame()
for (i in indicator_code){
  indicator_df <- indicator_df %>%
    bind_rows(
      fromJSON(str_c("https://unstats.un.org/SDGAPI/v1/sdg/Indicator/", i, "/GeoAreas")) %>%
        mutate(un_code = as.numeric(geoAreaCode)) %>%
        inner_join(un_member_states) %>%
        count() %>%
        mutate(indicator = i) %>%
        select(indicator, n))
}
