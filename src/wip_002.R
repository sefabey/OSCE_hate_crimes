# wip_002======

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
world_sp <- maptools::readShapePoly('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp') %>% na.omit()
osce_data <- read_csv('https://raw.githubusercontent.com/sefabey/OSCE_hate_crimes/master/osce_hate_incidents_2016_wide.csv')
world_sp@data <- world_sp@data %>% left_join(osce_data, by=c('ISO3'='iso3c'))
world_sp2 <- world_sp[world_sp$POP2005>0,] #filter countries with 0 population
world_sp3 <- world_sp2[!is.na(world_sp2$figures_civil_intern_orgs),] #filter countries without any data

world_sp2 %>% plot
world_sp3 %>% plot
world_sp3@data -> world_data # all hail right assign (careful though), export world@data

# create cartograms for both dataset using two diffrent cartogram algorithms
# world_carto_civil <- quick.carto(world_sp3, world_sp3$figures_civil_intern_orgs,blur = 0.5)
world_carto_civil <- cartogram::cartogram_cont(world_sp3,"figures_civil_intern_orgs", itermax=5)

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

world_sp3_tidy <- world_sp3 %>% 
    gSimplify(tol = 0.0001, topologyPreserve = TRUE) %>% 
    gBuffer(byid=TRUE, id=world_sp3$ISO3, width=0) %>%
    broom::tidy(region="ISO3") %>% 
    left_join(world_sp3@data, by=c('id'='ISO3'))

world_carto_civil_tidy <- world_carto_civil %>% 
    gSimplify(tol = 0.0001, topologyPreserve = TRUE) %>% 
    gBuffer(byid=TRUE, id=world_carto_civil$ISO3, width=0) %>%
    broom::tidy(region="ISO3") %>% 
    left_join(world_carto_civil@data, by=c('id'='ISO3'))

data_tween <- world_sp3_tidy %>%
    tweenr::tween_state( world_carto_civil_tidy, 'cubic-in-out', 60, id = 'group') %>%
    tweenr::tween_state( world_sp3_tidy, 'cubic-in-out', 60, id = 'group')


data_tween %>% colnames()
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==1) %>% arrange(order), aes(fill = figures_civil_intern_orgs, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==5) %>% arrange(order), aes(fill = figures_civil_intern_orgs, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==30) %>% arrange(order), aes(fill = figures_civil_intern_orgs, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==45) %>% arrange(order), aes(fill = figures_civil_intern_orgs, x = long, y = lat, group = group) , size=0, alpha=0.9)


p <-  ggplot() + 
    geom_polygon(data = data_tween  %>% arrange(order) , 
                 aes(fill = figures_civil_intern_orgs %>% log2, x = long, y = lat, group = group, frame=.frame) 
                 , size=1, alpha=1) +
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(RColorBrewer::brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:11),
                          labels=c(2,5, 10, 20, 30, 50, 100, 250, 500, 1000, 2000))+
    # coord_sf(datum = NA) +
    hrbrthemes::theme_ipsum_rc()+
    labs( title = "World Hateful and Xenophobic Incidents Reported in 2016",
          subtitle="Source: OSCE",
          caption="Social Data Science Lab, Cardiff University",
          x="Longitude",
          y='Latitude') +
    coord_map(xlim=c(-180,180), ylim = c(20, 150))+
    theme(text =element_text( size=100) )+
    theme(title =element_text( size=100)  )
    theme(legend.position = 'bottom', 
          legend.text = element_text(angle = 30, hjust = 1))+
    theme(plot.caption = element_text( size=100))+
    theme(plot.title = element_text(size=150))+
    theme(plot.subtitle = element_text(size=100))+
    theme(axis.title.x =element_text(size=100,face="bold"),
          axis.title.y =element_text(size=100,face="bold"),
          axis.text.x = element_text(size=100),
          axis.text.y = element_text(size=100))+
    theme(legend.title=element_text(size=80))+ 
    theme(legend.text=element_text(size=80))+
    theme(legend.key.size = 100)+
    theme(legend.key.height = 50)+
    NULL

animation::ani.options(ani.width=1920, ani.height=1080, ani.res=300, interval=1/18)
gganimate:: gganimate(p, "viz/choro_to_carto_animation_hd_temp.gif", title_frame = F)


# name states
types <- c('Original','Cartogram Weigted by OSCE data')
world_sp3_tidy$type <- types[1]
world_carto_civil_tidy$type <- types[2]

world_sp3_tidy %>% colnames()
world_carto_civil_tidy %>% colnames()

# row bind original and cartogram
world_all <- rbind(world_sp3_tidy, world_carto_civil_tidy[, names(world_sp3_tidy)])

# create factors for both states
world_all$type <- factor(world_all$type, levels = types)
world_all %>% str()
# animate
var <- ggplot(world_all, aes(long, lat, group = group, fill = figures_civil_intern_orgs ))+
    geom_polygon()+
    # geom_text(data=world.f2_official, aes(x=longit, y=latid, label = label_text_official),size=3.5)+ #cant ge this to work
    scale_fill_scico(palette = 'turku')+
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
