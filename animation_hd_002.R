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
    labs( title = "Hateful and Xenophobic Incidents Reported by International\nand Civil Society Organisations in 2016",
          subtitle="Source: ODIHR of the OSCE",
          caption="Social Data Science Lab\nCardiff University",
          x="Longitude",
          y='Latitude') +
    # coord_map(xlim=c(-180,180), ylim = c(20, 150))++
    theme(legend.position = 'bottom', 
          legend.direction="horizontal",
          legend.title = element_text( size = 20),
          legend.text = element_text(angle = 30, size = 20))+
    theme(plot.caption = element_text( size=20))+
    theme(legend.key.width = unit(4, "cm"))+
    theme(legend.key.height = unit(0.8, "cm"))+
    theme(plot.title = element_text( size=30, face = 'bold'))+
    theme(plot.subtitle = element_text( size=20))+
    theme(aspect.ratio=0.5)+
    NULL

animation::ani.options(ani.width=1920*0.6, ani.height=1200*0.6, ani.res=300, interval=1/18)
gganimate:: gganimate(p, "viz/choro_to_carto_animation_hd_civil.gif", title_frame = F)

# looking good. do the same for official data.


world_sp <- maptools::readShapePoly('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp') %>% na.omit()
osce_data <- read_csv('https://raw.githubusercontent.com/sefabey/OSCE_hate_crimes/master/osce_hate_incidents_2016_wide.csv')
world_sp@data <- world_sp@data %>% left_join(osce_data, by=c('ISO3'='iso3c'))
world_sp2 <- world_sp[world_sp$POP2005>0,] #filter countries with 0 population
world_sp4 <- world_sp2[!is.na(world_sp2$figures_official_records),] #filter countries without any data

world_sp2 %>% plot
world_sp4 %>% plot
world_sp4@data -> world_data # all hail right assign (careful though), export world@data

# create cartograms for both dataset using two diffrent cartogram algorithms
# world_carto_civil <- quick.carto(world_sp3, world_sp3$figures_civil_intern_orgs,blur = 0.5)
world_carto_official <- cartogram::cartogram_cont(world_sp4,"figures_official_records", itermax=5)

# visually check both states
world_sp3 %>% plot() 
world_carto_official %>% plot()
# world_carto_civil_2 %>% plot()

# check whether these polygons are valid or not (https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec)
sum(gIsValid(world_sp3, byid=TRUE)==FALSE)
sum(gIsValid(world_carto_official, byid=TRUE)==FALSE)
# sum(gIsValid(world_carto_civil_2, byid=TRUE)==FALSE)

#apparently not, all polygons have self intersections 
#hacking this following the stackoverflow advice above using gBuffer()

world_sp4_tidy <- world_sp4 %>% 
    gSimplify(tol = 0.0001, topologyPreserve = TRUE) %>% 
    gBuffer(byid=TRUE, id=world_sp4$ISO3, width=0) %>%
    broom::tidy(region="ISO3") %>% 
    left_join(world_sp4@data, by=c('id'='ISO3'))

world_carto_official_tidy <- world_carto_official %>% 
    gSimplify(tol = 0.0001, topologyPreserve = TRUE) %>% 
    gBuffer(byid=TRUE, id=world_carto_official$ISO3, width=0) %>%
    broom::tidy(region="ISO3") %>% 
    left_join(world_carto_official@data, by=c('id'='ISO3'))

data_tween_official <- world_sp4_tidy %>%
    tweenr::tween_state( world_carto_official_tidy, 'cubic-in-out', 60, id = 'group') %>%
    tweenr::tween_state( world_sp4_tidy, 'cubic-in-out', 60, id = 'group')


data_tween %>% colnames()
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==1) %>% arrange(order), aes(fill = figures_official_records, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==5) %>% arrange(order), aes(fill = figures_official_records, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==30) %>% arrange(order), aes(fill = figures_official_records, x = long, y = lat, group = group) , size=0, alpha=0.9)
ggplot() + geom_polygon(data = data_tween %>% filter(.frame==45) %>% arrange(order), aes(fill = figures_official_records, x = long, y = lat, group = group) , size=0, alpha=0.9)


p_official <-  ggplot() + 
    geom_polygon(data = data_tween_official  %>% arrange(order) , 
                 aes(fill = figures_official_records %>% log, x = long, y = lat, group = group, frame=.frame) 
                 , size=1, alpha=1) +
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(RColorBrewer::brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:12),
                          labels=c(5, 10, 20, 50, 100, 200, 1000, 5000, 10000, 40000,80000, 100000))+
    # coord_sf(datum = NA) +
    hrbrthemes::theme_ipsum_rc()+
    labs( title = "Hate Crimes Recorded by Police Forces in 2016",
          subtitle="Source: ODIHR of the OSCE",
          caption="Social Data Science Lab\nCardiff University",
          x="Longitude",
          y='Latitude') +
    # coord_map(xlim=c(-180,180), ylim = c(20, 150))++
    theme(legend.position = 'bottom', 
          legend.direction="horizontal",
          legend.title = element_text( size = 20),
          legend.text = element_text(angle = 30, size = 20))+
    theme(plot.caption = element_text( size=20))+
    theme(legend.key.width = unit(4, "cm"))+
    theme(legend.key.height = unit(0.8, "cm"))+
    theme(plot.title = element_text( size=30, face = 'bold'))+
    theme(plot.subtitle = element_text( size=20))+
    theme(aspect.ratio=0.5)+
    NULL

animation::ani.options(ani.width=1920*0.6, ani.height=1200*0.6, ani.res=300, interval=1/18)
gganimate:: gganimate(p_official, "viz/choro_to_carto_animation_hd_official.gif", title_frame = F)
