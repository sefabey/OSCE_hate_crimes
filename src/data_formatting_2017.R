library(tidyverse)
library(countrycode)

read_csv("data/2017/osce_hate_incidents_2017.csv") %>% 
    mutate( iso2c= countrycode(country, 'country.name', destination = "iso2c"),
            iso3c= countrycode(country, 'country.name', destination = "iso3c")) %>% 
    select(country, iso2c, iso3c, data_source, number_of_incidents, year ) %>% 
    spread(key=data_source, value =  number_of_incidents, fill = NA) %>%
    select(country, iso2c, iso3c, year, 
           official_records=`Official Records`,
           other_sources=`Other Sources`) %>% 
    write_csv("data/2017/osce_hate_incidents_2017_wide.csv")

osce_data_wide <- read_csv("data/2017/osce_hate_incidents_2017_wide.csv")
osce_data_wide


# see the countries with from 2016
read_csv("data/2017/osce_hate_incidents_2017.csv") %>% 
    filter(data_source %in% "Official Records", last_year==1)
