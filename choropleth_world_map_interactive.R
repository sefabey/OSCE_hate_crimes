# interactive maps

# install.packages("plotly")
# install.packages("ggpubr")
# devtools::install_github('ropensci/plotly')

library(tidyverse)
library(plotly)
library(ggpubr)
library(RColorBrewer)

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
df %>% colnames() #"COUNTRY", "GDP..BILLIONS.", "CODE"
df <- read_csv('osce_hate_incidents_2016_wide.csv')
df %>% colnames() %>% cat() #country iso2c iso3c year figures_civil_intern_orgs figures_official_records

# black country boundaries
l <- list(color = toRGB("black"), width = 0.3)

# specify map projection/options
g <- list(
    showframe = TRUE,
    showcoastlines = TRUE,
    projection = list(type = 'Gnomic')
)
g <- NULL

p <- plot_geo(df) %>%
    add_trace(
        z = ~figures_civil_intern_orgs %>% log2, 
        color = ~figures_civil_intern_orgs %>% log2,
        colors = rev( brewer.pal(8,"Spectral")),
        text = ~paste(country,'<br>Reported Incidents: ', figures_civil_intern_orgs), 
        type = 'scatter',
        hoverinfo = "text",
        hoverlabel=bgcolor('red'),
            locations = ~iso3c, 
        marker = list(line = l)
    ) %>%
    colorbar(title = 'Incident Counts',
             tickmode="array",#set this to array to edit coloursclae vlaues manually
             tickvals= c(0,1,2,3,4,5,6,7,8,9,10,11), #set log scales for further maual editing
             ticktext= c(1,2,5, 10, 20, 30, 50, 100, 250, 500, 1000, 2000), #replace log scales with these values while displaying
             len=0.5,
             y = 0.6,
             x=1
    ) %>% 
    layout(
        title = 'Map of Racist and Xenophobic Incidents\nReported by International and Civil Society Organisations in 2016<br>Source:<a href="http://hatecrime.osce.org/what-hate-crime/racism-and-xenophobia">Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)</a>',        geo = g
    )
p

# need to add other countrieswhich might be NA #done
# colours log scale is loking good but manully edit keep the numbers ## done

htmlwidgets::saveWidget(as_widget(p), "/home/rstudio/OSCE_hate_crimes/viz/choropleth_interactive.html")


p2 <- plot_geo(df) %>%
    add_trace(
        z = ~figures_official_records %>% log2, 
        color = ~figures_official_records %>% log2,
        colors = rev( brewer.pal(8,"Spectral")),
        text = ~paste(country,'<br>Recorded Incidents: ', figures_official_records), 
        type = 'scatter',
        hoverinfo = "text",
        hoverlabel=bgcolor('red'),
        locations = ~iso3c, 
        marker = list(line = l)
    ) %>%
    colorbar(title = 'Incident Counts',
             tickmode="array",#set this to array to edit coloursclae values manually
             tickvals= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), #set log scales for further maual editing
             ticktext= c(2, 5, 10, 20, 50, 100,200,400,800,1500, 2500,5000, 10000, 20000,40000,80000), #replace log scales with these values while displaying
             len=0.5,
             y = 0.6,
             x=1
    ) %>% 
    layout(
        title = 'Map of Racist and Xenophobic Incidents\nRecorded by the Police in Participating States in 2016<br>Source:<a href="http://hatecrime.osce.org/what-hate-crime/racism-and-xenophobia">Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)</a>',        geo = g
    )
p2

# need to add other countrieswhich might be NA #done
# colours log scale is loking good but manully edit keep the numbers ## done

htmlwidgets::saveWidget(as_widget(p2), "/home/rstudio/OSCE_hate_crimes/viz/choropleth_interactive_official.html")

