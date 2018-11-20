# data formatting

library(tidyverse)

read_csv("osce_hate_incidents_2016.csv") %>% 
    mutate( iso2c= countrycode(country, 'country.name', destination = "iso2c"),
            iso3c= countrycode(country, 'country.name', destination = "iso3c")) %>% 
    select(country, iso2c, iso3c, data_source, number_of_incidents, year ) %>% 
    spread(key=data_source, value =  number_of_incidents, fill = NA) %>%
    select(country, iso2c, iso3c, year, figures_civil_intern_orgs=`Civil Society, International Organisations and the Holy See`, figures_official_records=`Official Records`) %>% 
    write_csv("osce_hate_incidents_2016_wide.csv")

osce_data <- read_csv('osce_hate_incidents_2016_wide.csv')

osce_data
