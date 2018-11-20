# choropleth_to_cartogram.

# This script has one aim: To convert static choropleths into cartograms and back in an animation.

# @drob has an gganimate package which helps but I really want to check out @thomasp85's version as
# the latter will possibly replace the former.

# Therefore, starting by replicating Pedersen's amazing gapminder animation. Might need to install his dev version
# of the gganimate

# install.packages("gapminder")
# devtools::install_github('thomasp85/gganimate')
# install.packages("tweenr")


# animated gapminder=====
# replication of https://gist.github.com/thomasp85/05169ad44ddcc8ed56da6ff7bf7fbe36
library(gapminder)
library(tweenr)
library(ggplot2)
library(gganimate)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
    geom_point(alpha = 0.7) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    facet_wrap(~continent) +
    theme(legend.position = 'none') +
    labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
    transition_time(year) +
    ease_aes('linear')

animate(p, 300, 50)

# Looking good. I can replicate the above example without issues, 
# which means my dependencies are setup correctly.

# Now off to a more relevant example

# Transition Between Chloropleth and Cartogram ======
# Replication of https://www.r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram/

# devtools::install_github('dgrtwo/gganimate') #requires drobs version, otherwise code does not work.

library(cartogram)    # for the cartogram

# install.packages("cartogram")
library(ggplot2)      # to realize the plots
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis)      # for a nice color palette
library(tidyverse)

# Get the shape file of Africa
data(wrld_simpl)
wrld_simpl@data
afr <- wrld_simpl[wrld_simpl$REGION==2,]

# A basic representation
plot(afr)

afr_cartogram <- cartogram(afr, "POP2005", itermax=7)
plot(afr_cartogram)



# Transform these 2 objects in dataframe, plotable with ggplot2
afr_cartogram_df <- tidy(afr_cartogram) %>% left_join(. , afr_cartogram@data, by=c("id"="ISO3")) 
afr_df <- tidy(afr) %>% left_join(. , afr@data, by=c("id"="ISO3")) 

# And using the advices of chart #331 we can custom it to get a better result:
ggplot() +
    geom_polygon(data = afr_df, aes(fill = POP2005/1000000, x = long, y = lat, group=group) , size=0, alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Population (M)", breaks=c(1,50,100, 140),
                       guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                             keywidth=unit(12, units = "mm"), 
                                             label.position = "bottom", 
                                             title.position = 'top', nrow=1)) +
    labs( title = "Africa", subtitle="Population per country in 2005" ) +
    ylim(-35,35) +
    theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f4", color = NA), 
        panel.background = element_rect(fill = "#f5f5f4", color = NA), 
        legend.background = element_rect(fill = "#f5f5f4", color = NA),
        plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.position = c(0.2, 0.26)) +
    coord_map()+
    NULL

# You can do the same for afr_cartogram_df


ggplot() +
    geom_polygon(data = afr_cartogram_df, aes(fill = POP2005/1000000, x = long, y = lat, group=group) , size=0, alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Population (M)", breaks=c(1,50,100, 140),
                       guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                             keywidth=unit(12, units = "mm"), 
                                             label.position = "bottom", 
                                             title.position = 'top', nrow=1)) +
    labs( title = "Africa", subtitle="Population per country in 2005" ) +
    ylim(-35,35) +
    theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f4", color = NA), 
        panel.background = element_rect(fill = "#f5f5f4", color = NA), 
        legend.background = element_rect(fill = "#f5f5f4", color = NA),
        plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.position = c(0.2, 0.26)) +
    coord_map()+
    NULL

# Give an id to every single point that compose the boundaries
afr_cartogram_df$id=seq(1,nrow(afr_cartogram_df))
afr_df$id=seq(1,nrow(afr_df))

# Bind both map info in a data frame. 3 states: map --> cartogram --> map
data=rbind(afr_df, afr_cartogram_df, afr_df)

# Set transformation type + time
data$ease="cubic-in-out"
data$time=rep(c(1:3), each=nrow(afr_df))

# Calculate the transition between these 2 objects?
dt <- tween_elements(data, time='time', group='id', ease='ease', nframes = 30)

# check a few frame
ggplot() + geom_polygon(data = data %>% filter(.frame==0) %>% arrange(order), aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = dt %>% filter(.frame==5) %>% arrange(order), aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = dt %>% filter(.frame==10) %>% arrange(order), aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9)

# Plot
p=ggplot() + 
    geom_polygon(data = dt  %>% arrange(order) , aes(fill = POP2005/1000000, x = long, y = lat, group = group, frame=.frame) , size=0, alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Population (M)", breaks=c(1,50,100, 140), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) +
    labs( title = "Africa", subtitle="Population per country in 2005" ) +
    ylim(-35,35) +
    theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f4", color = NA), 
        panel.background = element_rect(fill = "#f5f5f4", color = NA), 
        legend.background = element_rect(fill = "#f5f5f4", color = NA),
        plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.position = c(0.2, 0.26)
    ) +
    coord_map()

# Make the animation
animation::ani.options(interval = 1/9)
gganimate(p, "Animated_Africa.gif",  title_frame = F) # had to install imagemagick https://www.imagemagick.org/script/install-source.php

# it runs! replicated fine using @drob's version.
# Wgat I want to do is to use the above instructions to animate choropleth to carto and 
# then redo the animation using Pedersen's version of the gganimage


# Now off to my own work after the yummy breakfast!

# WIP choro====> carto ============
world_shape <- rgdal::readOGR('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')
world_shape %>% plot #quick peak
osce_data <- read_csv('osce_hate_incidents_2016_wide.csv')
world_shape <-  world_shape[world_shape$POP2005!=0,] #remove countries with no population

#merge 
world_shape@data <- left_join(world_shape@data,osce_data, by= c('ISO3'='iso3c'))
world_shape@data %>% View
world_shape <- merge(world_shape, world_shape@data) #merge shapefile with data
world_shape$figures_official_records

world_shape$incidents_official <- world_shape$figures_official_records %>% log2 
world_cartogram <- cartogram_cont (world_shape, 'incidents_official', itermax = 7)
world_cartogram %>% plot()

cartogram_df <- tidy(world_cartogram)

cartogram_df$id %>% unique()

cartogram_df <- tidy(world_cartogram) %>% left_join( world_cartogram@data, by=c("id"="ISO3")) 
cartogram_df
afr_df <- tidy(afr) %>% left_join(. , afr@data, by=c("id"="ISO3")) 

# And using the advices of chart #331 we can custom it to get a better result:
ggplot() +
    geom_polygon(data = afr_df, aes(fill = POP2005/1000000, x = long, y = lat, group = group) , size=0, alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Population (M)", breaks=c(1,50,100, 140), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) +
    labs( title = "Africa", subtitle="Population per country in 2005" ) +
    ylim(-35,35) +
    theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f4", color = NA), 
        panel.background = element_rect(fill = "#f5f5f4", color = NA), 
        legend.background = element_rect(fill = "#f5f5f4", color = NA),
        plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        legend.position = c(0.2, 0.26)
    ) +
    coord_map()

# choropleth=====
world <- rgdal::readOGR('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')
map.world <- map_data(world) #getting world map in dataframe format to plot in ggplot2
osce_data <- read_csv("osce_hate_incidents_2016_wide.csv") #reading in OSCE hate incidents
map.world$iso3c <- countrycode::countrycode(map.world$region, origin = 'country.name', destination = 'iso3c' ) #getting country codes in iso3c format
osce_map_joined <- left_join(map.world, osce_data, by = 'iso3c')
osce_map_joined


smaller.data_choro <- osce_data %>% dplyr::select(Country.Code = iso3c, hate_fig_civil = figures_civil_intern_orgs) %>% 
    na.omit() %>%
    data.frame()

## Join the two datasets using their common field
matched.indices <- match(world@data[, "ISO3"], smaller.data_choro[, "Country.Code"])
world@data <- data.frame(world@data, smaller.data_choro[matched.indices, ])

world_choro_civil <- fortify(world, region = "Country.Code")
world_choro_civil <- merge(world_choro_civil, world@data, by.x = "id", by.y = "Country.Code")
world_choro_civil$hate_fig_civil

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
     # labs(x="Longitude", 
     #     y='Latitude', 
     #     title="Choropleth of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
     #     subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
     #     caption="Social Data Science Lab, Cardiff University")+
    # hrbrthemes::theme_ipsum_rc()+
    # theme(plot.caption = element_text(size = 10))+
    # theme(legend.position = c(0.1,0.25))+# for positioning the legend inside the plot
    # theme(legend.key.size = unit(0.4, "in"))+
    NULL

choro_civil

# carto=======

world <- rgdal::readOGR('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')
osce_data <- read_csv("osce_hate_incidents_2016_wide.csv") #reading in OSCE hate incidents

smaller.data <- osce_data %>% dplyr::select(Country.Code = iso3c, hate_fig_civil = figures_civil_intern_orgs) %>% 
    na.omit() %>%
    data.frame()

## Join the two datasets using their common field
matched.indices <- match(world@data[, "ISO3"], smaller.data[, "Country.Code"])
world@data <- data.frame(world@data, smaller.data[matched.indices, ])


## Compute the cartogram transformation of each country using its population
## with the degree of Gaussian blur = 0.5 (otherwise, it may not work)
world.carto_civil <- cartogram (world, 'hate_fig_civil', itermax=7)
plot(world.carto_civil)

## Convert the object into data frame
world.carto_civil <- fortify(world.carto_civil, region = "Country.Code")
## Merge the cartogram transformation with the world map shapefile
world.carto_civil <- merge(world.carto_civil, world@data, by.x = "id", by.y = "Country.Code")

# world.f_civil %>% 
#     group_by(ISO3) %>% 
#     summarise(longit=mean (long),latid=mean(lat)) %>% # calculate middle point of the distorted country polygons
#     right_join(world.f_civil) %>% 
#     mutate(label_text_civil= paste(.$ISO3,.$hate_fig_civil,sep = ":"))  -># create labels by pasting country iso3 and hate figures 
#     world.f2

# manual geo adjust country labels

# world.f2 %>% 
#     mutate(longit= ifelse(ISO3=="USA", longit+7,longit),
#            longit=ifelse(ISO3=="CAN", longit-2,longit),
#            longit=ifelse(ISO3=="TUR", longit+1,longit),
#            longit=ifelse(ISO3=="SWE", longit-0.8,longit),
#            longit=ifelse(ISO3=="GBR", longit+1,longit),
#            latid=ifelse(ISO3=="USA", latid-2.5,latid),
#            latid=ifelse(ISO3=="MKD", latid+1,latid),
#            latid=ifelse(ISO3=="GRC", latid-1,latid),
#            latid=ifelse(ISO3=="ROU", latid-0.5,latid),
#            latid=ifelse(ISO3=="CAN", latid-0.5,latid),
#            latid=ifelse(ISO3=="HRV", latid-0.5,latid)
#            # latid=ifelse(ISO3=="ROU", latid,latid)
#            # ISO3=ifelse(ISO3=="BIH", NA,ISO3),#get rid of overlapping NA countries
#            # ISO3=ifelse(ISO3=="SVN", NA,ISO3),#get rid of overlapping NA countries
#            # label_text_official=ifelse(ISO3=="SVN", NA,label_text_official),
#            # label_text_official=ifelse(ISO3=="BIH", NA,label_text_official)
#     )->world.f2






carto_civil <- ggplot(world.carto_civil, aes(long, lat, group = group, fill = world.carto_civil$hate_fig_civil %>% log2))+
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
    # coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    # labs(x="Distorted Longitude", 
    #      y='Distorted Latitude', 
    #      title="Cartogram of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
    #      subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
    #      caption="Social Data Science Lab, Cardiff University")+
    # hrbrthemes::theme_ipsum_rc()+
    # theme(plot.caption = element_text(size = 12))+
    # theme(legend.position = c(0.1,0.25))+
    # theme(legend.key.size = unit(0.4, "in"))
    NULL

carto_civil


# enter tweenr======

# Give an id to every single point that compose the boundaries

world.carto_civil$id=seq(1,nrow(world.carto_civil))
world_choro_civil$id=seq(1,nrow(world_choro_civil))

world.carto_civil %>% colnames()
world_choro_civil %>% colnames()

# Bind both map info in a data frame. 3 states: map --> cartogram --> map
data=rbind(world_choro_civil, world.carto_civil, world_choro_civil)
world_choro_civil %>% nrow
world.carto_civil %>% nrow
# Set transformation type + time
data$ease="cubic-in-out"
data$time=1
152869+world.carto_civil %>% nrow
data$time[152870:305729] <- 2
data$time[305730:nrow(data)] <- 3

# Calculate the transition between these 2 objects?
dt <- tween_elements(data, time='time', group='id', ease='ease', nframes = 60)

# check a few frame
ggplot() + geom_polygon(data = dt %>% filter(.frame==0) %>% arrange(order), aes(fill = hate_fig_civil, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = dt %>% filter(.frame==5) %>% arrange(order), aes(fill = hate_fig_civil, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = dt %>% filter(.frame==10) %>% arrange(order), aes(fill = hate_fig_civil, x = long, y = lat, group = group) , size=0, alpha=0.9)

# Plot
p= ggplot() + 
    geom_polygon(data = dt  %>% arrange(order) , 
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
    theme(plot.caption = element_text( face="bold", size=15))+
    theme(legend.key.height = unit(3, "cm"))+
    NULL
    

# Make the animation
animation::ani.options(ani.width=1600*0.75, ani.height=900*0.75, ani.res=900)
gganimate(p, "viz/choro_to_carto_animation.gif", title_frame = F)



