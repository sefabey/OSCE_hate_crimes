ptm <- proc.time()
library(tweenr)
library(gganimate)
library(cartogram)    # for the cartogram
library(broom)        # from geospatial format to data frame
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(tidyverse)


# choropleth=====
world <- rgdal::readOGR('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')
osce_data <- read_csv("osce_hate_incidents_2016_wide.csv") #reading in OSCE hate incidents

smaller.data_choro <- osce_data %>% dplyr::select(Country.Code = iso3c, hate_fig_civil = figures_civil_intern_orgs) %>% 
    na.omit() %>%
    data.frame()

## Join the two datasets using their common field
matched.indices <- match(world@data[, "ISO3"], smaller.data_choro[, "Country.Code"])
world@data <- data.frame(world@data, smaller.data_choro[matched.indices, ])


world_choro_civil <- tidy(world, region = "Country.Code") %>% left_join(world@data, by=c('id'= "Country.Code"))
# world_choro_civil <- merge(world_choro_civil, world@data, by.x = "id", by.y = "Country.Code")

choro_civil <- ggplot(world_choro_civil, aes(long, lat, group = group, fill = world_choro_civil$hate_fig_civil %>% log2))+ 
    # theme(legend.position="bottom")+ 
    geom_polygon()+
    # geom_text(data=centroids_joined, aes(x=long, y=lat, label = figures_civil_intern_orgs), size=2.5)+
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(RColorBrewer::brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:11),
                          labels=c(2,5, 10, 20, 30, 50, 100, 250, 500, 1000, 2000)
    )+
    coord_map(xlim=c(-180,180), ylim = c(0, 150))+
    NULL

choro_civil

# carto=======

# world <- rgdal::readOGR('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')
# osce_data <- read_csv("osce_hate_incidents_2016_wide.csv") #reading in OSCE hate incidents
# 
# smaller.data_carto <- osce_data %>% dplyr::select(Country.Code = iso3c, hate_fig_civil = figures_civil_intern_orgs) %>% 
#     na.omit() %>%
#     data.frame()
# 
# ## Join the two datasets using their common field
# matched.indices <- match(world@data[, "ISO3"], smaller.data[, "Country.Code"])
# world@data <- data.frame(world@data, smaller.data[matched.indices, ])

world_carto_civil <- cartogram (world, 'hate_fig_civil', itermax=7)
plot(world_carto_civil)


## Convert the object into data frame
world_carto_civil_tidy <- tidy(world_carto_civil, region = "Country.Code") %>% left_join(world@data, by=c('id'= "Country.Code"))
## Merge the cartogram transformation with the world map shapefile
# world.carto_civil_tidy <- merge(world.carto_civil, world@data, by.x = "id", by.y = "Country.Code")

carto_civil <- ggplot(world_carto_civil_tidy, aes(long, lat, group = group, fill = world_carto_civil_tidy$hate_fig_civil %>% log2))+
    geom_polygon()+
    # geom_text(data=world.f2, aes(x=longit, y=latid, label = label_text_civil),size=4)+ #cant ge this to work
    # geom_text(data=world.f2, aes(x=longit-3, y=latid, label = ISO3),size=4)+ #cant ge this to work
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(RColorBrewer::brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:11),
                          labels=c(2,5, 10, 20, 30, 50, 100, 250, 500, 1000, 2000)
    )+
    coord_map(xlim=c(-180,180), ylim = c(0, 150))+
    NULL

carto_civil


# enter tweenr======

# Give an id to every single point that compose the boundaries

world_carto_civil_tidy$id=seq(1,nrow(world_carto_civil_tidy))
world_choro_civil$id=seq(1,nrow(world_choro_civil))

world_carto_civil_tidy %>% colnames()
world_choro_civil %>% colnames()

# Bind both map info in a data frame. 3 states: map --> cartogram --> map
data=rbind(world_choro_civil, world_carto_civil_tidy, world_choro_civil)
world_choro_civil %>% nrow
world_carto_civil_tidy %>% nrow
# Set transformation type + time
data$ease="cubic-in-out"
data$time=1
152869+world_carto_civil_tidy %>% nrow
data$time[152870:305729] <- 2
data$time[305730:nrow(data)] <- 3

# Calculate the transition between these 2 objects?
data_tween <- tween_elements(data, time='time', group='id', ease='ease', nframes = 60)

# check a few frame
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==0) %>% arrange(order), aes(fill = hate_fig_civil, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==5) %>% arrange(order), aes(fill = hate_fig_civil, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==10) %>% arrange(order), aes(fill = hate_fig_civil, x = long, y = lat, group = group) , size=0, alpha=0.9)

# Plot
p= ggplot() + 
    geom_polygon(data = data_tween  %>% arrange(order) , 
                 aes(fill = hate_fig_civil %>% log2, x = long, y = lat, group = group, frame=.frame) 
                 , size=1, alpha=1) +
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(RColorBrewer::brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:11),
                          labels=c(2,5, 10, 20, 30, 50, 100, 250, 500, 1000, 2000))+
    hrbrthemes::theme_ipsum_rc()+
    labs( title = "World Hateful and Xenophobic Incidents Reported in 2016", 
          subtitle="Source: OSCE",
          caption="Social Data Science Lab, Cardiff University",
          x="Longitude", 
          y='Latitude') +
    coord_map(xlim=c(-180,180), ylim = c(20, 150))+
    theme(legend.position="right",legend.direction="vertical")+
    theme(plot.caption = element_text( size=15))+
    theme(legend.key.height = unit(3, "cm"))+
    NULL


# Make the animation
animation::ani.options(ani.width=1920, ani.height=1080, ani.res=300, interval=1/9)
gganimate(p, "viz/choro_to_carto_animation_hd.gif", title_frame = F)
proc.time() - ptm