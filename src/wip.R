library(tidyverse)
library(ggplot2)
library(gganimate) # thomasp85/gganimate 
library(cartogram)
library(geogrid) 
library(rnaturalearth)
library(sf)
library(scico)
library(getcartr)

# data pre-processing
# read in world shapefile and OSCE data, merge, filter
world_sp <- maptools::readShapePoly("TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp") %>% na.omit() #find shp here https://github.com/sefabey/OSCE_hate_crimes/blob/master/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp
osce_data <- read_csv('https://raw.githubusercontent.com/sefabey/OSCE_hate_crimes/master/osce_hate_incidents_2016_wide.csv')
world_sp@data <- world_sp@data %>% left_join(osce_data, by=c('ISO3'='iso3c'))
world_sp2 <- world_sp[world_sp$POP2005>0,] #filter countries with 0 population
world_sp3 <- world_sp2[!is.na(world_sp2$figures_civil_intern_orgs),] #filter countries without any data

world_sp2 %>% plot
world_sp3 %>% plot

# create cartograms for both dataset using two diffrent cartogram algorithms
world_carto_civil <- quick.carto(world_sp3, world_sp3$figures_civil_intern_orgs,blur = 0.5)
# world_carto_civil_2 <- cartogram::cartogram_cont(world_sp3,"figures_civil_intern_orgs", itermax=5)

# visually check both states
world_sp3 %>% plot() 
world_carto_civil %>% plot()
# world_carto_civil_2 %>% plot()

# check whether these polygons are valid or not (https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec)
sum(gIsValid(world_sp3, byid=TRUE)==FALSE)
sum(gIsValid(world_carto_civil, byid=TRUE)==FALSE)
# sum(gIsValid(world_carto_civil_2, byid=TRUE)==FALSE)

#apparently not, all polygons have self intersections 
#hacking this following the stackoverflow advice above using gBuffer()


world_sp3_tidy <- gBuffer(world_sp3, byid=TRUE, id=world_sp3$ISO3, width=0) %>%
    broom::tidy(region="ISO3") %>% 
    left_join(world_sp3@data, by=c('id'='ISO3'))

world_carto_civil_tidy <-  gBuffer(world_carto_civil, byid=TRUE, id=world_carto_civil$ISO3, width=0) %>% 
    broom::tidy(region="ISO3") %>% 
    left_join(world_carto_civil@data, by=c('id'='ISO3'))

# name states
types <- c('Original','Cartogram Weigted by OSCE data')
world_sp3_tidy$type <- types[1]
world_carto_civil_tidy$type <- types[2]

# row bind original and cartogram
world_all <- rbind(world_sp3_tidy, world_carto_civil_tidy[, names(world_sp3_tidy)])

# create factors for both states
world_all$type <- factor(world_all$type, levels = types)
world_all %>% str()
# animate
var <- ggplot(world_all, aes(long, lat, group=id, fill = figures_civil_intern_orgs ))+
    geom_polygon()+
    scale_fill_scico(palette = 'roma')+
    theme_void() + 
    theme(legend.position = 'bottom', 
          legend.text = element_text(angle = 30, hjust = 1)) + 
    labs(title = 'Showing {closest_state}', 
          fill = 'figures_civil_intern_orgs') +
    transition_states(type, 2, 2, wrap = T)
var


#getting Error: $ operator is invalid for atomic vectors

animate(var, nframes = 100, '../local_folder/reprex_using_animate.gif',
        renderer = file_renderer(dir = "../local_folder/reprex_gganimate/filerenderer/", prefix = "gganim_plot", overwrite = FALSE))
