# devtools::install_github('thomasp85/gganimate')
# devtools::install_github('thomasp85/tweenr', force=T)
# devtools::install_github('thomasp85/transformr', force=T)
# devtools::install_github("tidyverse/ggplot2", force=T)

library(tweenr)
library(transformr)
library(tidyverse)
library(ggplot2)
library(gganimate) # thomasp85/gganimate 
library(cartogram)
# devtools::install_github('jbaileyh/geogrid')
library(geogrid) # Need github version jbaileyh/geogrid
library(rnaturalearth)
library(sf)
# install.packages("scico")
library(scico)


us <- ne_states('united states of america', returnclass = 'sf')
us <- us[!us$woe_name %in% c('Alaska', 'Hawaii'), ]
us <- st_transform(us, '+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs')
# Population data from https://www.census.gov/data/tables/2017/demo/popest/state-total.html
pop <- read.csv('https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/national/totals/nst-est2017-popchg2010_2017.csv',
                header = TRUE, skip = 0, check.names = F) #my berst bet is this data
us$pop <- pop[match(us$name, pop$NAME),'POPESTIMATE2017' ] # selected 2017 population as weights 

us_ca <- cartogram_cont(us, 'pop')
us_hex <- calculate_grid(shape = us, grid_type = "hexagonal", seed = 1)
us_hex <- assign_polygons(us, us_hex)
us_sq <- calculate_grid(shape = us, grid_type = "regular", seed = 13)
us_sq <- assign_polygons(us, us_sq)

types <- c(
    'Original',
    'Cartogram Weigted by Population',
    'Hexagonal Tiling',
    'Square Tiling'
)
us$type <- types[1]
us_ca$type <- types[2]
us_hex$type <- types[3]
us_sq$type <- types[4]
us_all <- rbind(us, us_hex[, names(us)], us_ca[, names(us)], us_sq[, names(us)])
us_all$type <- factor(us_all$type, levels = types)

var <-  ggplot(us_all) + 
    geom_sf(aes(fill = pop, group = name)) + 
    scale_fill_scico(palette = 'lapaz') + 
    coord_sf(datum = NA) +
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.text = element_text(angle = 30, hjust = 1)) + 
    labs(title = 'Showing {closest_state}', 
         fill = 'Population') +
    transition_states(type, 2, 1)

var

animate(var,nframes = 100, fps = 10,
        renderer = file_renderer(dir = "reprex_gganimate/file_renderer/", prefix = "gganim_plot", overwrite = FALSE))

animate(var, nframes = 30, 'reprex_using_animate.gif',
        renderer = file_renderer(dir = "~", prefix = "gganim_plot", overwrite = FALSE))


