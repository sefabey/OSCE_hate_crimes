# Things to try:

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


# Using hate incident stats by OSce's ODIHR
osce_data <- read.csv(file = 'osce_hate_incidents_2016_wide.csv', stringsAsFactors = FALSE)

smaller.data <- data.frame(Country.Code = osce_data$iso3c, hate_fig_civil = osce_data$figures_civil_intern_orgs)
smaller.data
smaller.data <- na.omit(smaller.data)
smaller.data

## Join the two datasets using their common field
matched.indices <- match(world@data[, "ISO3"], smaller.data[, "Country.Code"])
world@data <- data.frame(world@data, smaller.data[matched.indices, ])
## Compute the cartogram transformation of each country using its population
## with the degree of Gaussian blur = 0.5 (otherwise, it may not work)
world.carto_civil <- quick.carto(world, world@data$hate_fig_civil, blur = 0.5)
plot(world.carto_civil)


## Convert the object into data frame
world.f_civil <- fortify(world.carto_civil, region = "Country.Code")
## Merge the cartogram transformation with the world map shapefile
world.f_civil <- merge(world.f_civil, world@data, by.x = "id", by.y = "Country.Code")

world.f_civil %>% head()

world.f_civil %>% 
    group_by(ISO3) %>% 
    summarise(longit=mean (long),latid=mean(lat)) %>% 
    right_join(world.f_civil) %>% 
    mutate(label_text_civil= paste(.$ISO3,.$hate_fig_civil,sep = ":"))  ->
    world.f2

carto1 <- ggplot(world.f2, aes(long, lat, group = group, fill = world.f2$hate_fig_civil %>% log2))+
    geom_polygon()+
    geom_text(data=world.f2, aes(x=longit, y=latid, label = label_text_civil),size=4)+ #cant ge this to work
    # geom_text(data=world.f2, aes(x=longit-3, y=latid, label = ISO3),size=4)+ #cant ge this to work
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:11),
                          labels=c(2,5, 10, 20, 30, 50, 100, 250, 500, 1000, 2000)
                          )+
    # coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Distorted Longitude", 
         y='Distorted Latitude', 
         title="Cartogram of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 12))+
    theme(legend.position = c(0.1,0.25))+
    theme(legend.key.size = unit(0.4, "in"))

carto1


ggsave("viz/Cartogram_002_civilorg.png", width = 16*0.8, height = 9*0.8, units = "in" , dpi = 300, scale = 1.5)
beepr::beep()



# do the same for hate fig official


hate_fig_offic= osce_data$figures_official_records

world.carto_offic <- quick.carto(world, world@data$hate_fig_offic, blur = 0.5, prec=0.5)



world.carto_offic$Country.Code

## Convert the object into data frame
world.f_offic <- fortify(world.carto_offic, region = "Country.Code")
## Merge the cartogram transformation with the world map shapefile
world.f_offic <- merge(world.f_civil, world@data, by.x = "id", by.y = "Country.Code")



carto2 <- ggplot(world.f2, aes(long, lat, group = group, fill = world.f2$hate_fig_offic %>% log2))+
    geom_polygon()+
    geom_text(data=world.f2, aes(x=longit, y=latid, label = label_text_offic),size=4)+ #cant ge this to work
    # geom_text(data=world.f2, aes(x=longit-3, y=latid, label = ISO3),size=4)+ #cant ge this to work
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:12),
                          labels=c(5, 10, 20, 50, 100, 200, 1000, 5000, 10000, 40000,80000, 100000)
    )+
    # coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Distorted Longitude", 
         y='Distorted Latitude', 
         title="Cartogram of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 12))+
    theme(legend.position = c(0.1,0.25))+
    theme(legend.key.size = unit(0.4, "in"))

carto2


ggsave("viz/Cartogram_002_official.png", width = 16*0.8, height = 9*0.8, units = "in" , dpi = 300, scale = 1.5)
beepr::beep()




