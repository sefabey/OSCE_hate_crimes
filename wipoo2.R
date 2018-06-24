# Load libraries
library(cartogram)    # for the cartogram
library(ggplot2)      # to realize the plots
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis)      # for a nice color palette


# Get the shape file of Africa
data(wrld_simpl)
plot(wrld_simpl)
osce_data <- read_csv("osce_hate_incidents_2016_wide.csv")

wrld_simpl@data <- left_join( wrld_simpl@data, osce_data, by=c("ISO3"="iso3c") )
wrld_simpl@data %>% View()
# construct a cartogram using the population in 2005
world_cartogram <- cartogram(wrld_simpl, "figures_civil_intern_orgs", itermax=7)

# A basic representation
plot(world_cartogram)




# Transform these 2 objects in dataframe, plotable with ggplot2
cartogram_df <- tidy(world_cartogram) %>% left_join(. , world_cartogram@data, by=c("id"="ISO3")) 
world_df <- tidy(wrld_simpl) %>% left_join(. , wrld_simpl@data, by=c("id"="ISO3")) 

# And using the advices of chart #331 we can custom it to get a better result:
ggplot() +
    geom_polygon(data = world_df, aes(fill = figures_civil_intern_orgs, x = long, y = lat, group = group) , size=0, alpha=0.9) +
    theme_void() +
    scale_fill_viridis(name="Hate Incidents Recorded", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) +
    labs( title = "Racist and Xenophobic Events in 2016", subtitle="Source:Civil Society and Int Org" ) +
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

# You can do the same for afr_cartogram_df


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
ggplot() + geom_polygon(data = dt %>% filter(.frame==0) %>% arrange(order), aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9)
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
gganimate(p, "Animated_Africa.gif",  title_frame = F)


