Things to try:
    
# not too bad    
# https://stackoverflow.com/questions/9319597/cartogram-choropleth-map-in-r

# looking neat
# http://trucvietle.me/r/tutorial/2016/12/18/cartogram-plotting-using-r.html


# going with the latter

library(Rcartogram)
library(getcartr)
library(ggplot2)
library(tidyverse)

world <- readShapePoly('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')


## We are using the world's population data from World Bank
world.pop <- read.csv(file = 'osce_hate_incidents_2016_wide.csv', stringsAsFactors = FALSE)
world.pop
## Create a smaller dataset by retaining the world's population in 2013 and the ISO3
## country code, which will be used for matching and merging with the input shapefile
smaller.data <- data.frame(Country.Code = world.pop$iso3c, Population = world.pop$figures_civil_intern_orgs)
smaller.data
smaller.data <- na.omit(smaller.data)


## Join the two datasets using their common field
matched.indices <- match(world@data[, "ISO3"], smaller.data[, "Country.Code"])
world@data <- data.frame(world@data, smaller.data[matched.indices, ])
## Compute the cartogram transformation of each country using its population
## with the degree of Gaussian blur = 0.5 (otherwise, it may not work)
world.carto <- quick.carto(world, world@data$Population, blur = 0.5)


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

