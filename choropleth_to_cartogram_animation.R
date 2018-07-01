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

devtools::install_github('dgrtwo/gganimate') #requires drobs version, otherwise code does not work.

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

# WIP choro====>carto============