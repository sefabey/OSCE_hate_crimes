library(Rcartogram)
library(getcartr)
library(ggplot2)
library(tidyverse)


world <- readShapePoly('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')

## Using Racism and Xenophobia Stats from OSCE World Bank
osce_data <- read_csv('osce_hate_incidents_2016_wide.csv')

## Join the two datasets using their common field

joined_world <- world %>%
    as.tibble() %>% 
    mutate(iso3c=as.character(ISO3)) %>% 
    left_join(osce_data)
joined_world

world@data <- joined_world #weird notation but seems to work, need to learn this @ thing
world_carto <- quick.carto(world, world@data$figures_civil_intern_orgs, blur = 0.5)


## Convert the object into data frame
world.f <- fortify(world.carto, region = "Country.Code")
## Merge the cartogram transformation with the world map shapefile
world.f <- merge(world.f, world@data, by.x = "id", by.y = "Country.Code")
## Make a plot of the transformed polygons, where each country is
## further shaded by their population size (lighter means bigger)
my_map <- ggplot(world.f, aes(long, lat, group = group, fill = world.f$Population)) + geom_polygon()
## Display the plot and give it a title
(my_map <- my_map + ggtitle("Cartogram of Racist and Xenophobic Incidents Recorded in 2016"))


my_map+
    scale_fill_gradientn (name="Incident Numbers",
                          colours=rev(brewer.pal(9,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar")+
    # coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Cartogram of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 10))+
    theme(legend.position = c(0.9,0.25))

ggsave("Cartogram_001.png", width = 16, height = 9, units = "in" , dpi = 300, scale = 1.5)

