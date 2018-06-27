# Things to try:

# not too bad    
# https://stackoverflow.com/questions/9319597/cartogram-choropleth-map-in-r

# looking neat
# http://trucvietle.me/r/tutorial/2016/12/18/cartogram-plotting-using-r.html


# going with the latter
        #Install external dependencies for Docker

        # wget http://www.fftw.org/fftw-3.3.3.tar.gz
        # tar -xzf fftw-3.3.3.tar.gz
        # cd fftw-3.3.3
        # ./configure --enable-shared
        # make
        # sudo make install

        # wget http://cran.r-project.org/src/contrib/fftw_1.0-4.tar.gz
        # sudo R CMD INSTALL fftw_1.0-4.tar.gz

# 
# library(devtools)
# install_github('omegahat/Rcartogram')
# ## Wait for installation, then:
# install_github('chrisbrunsdon/getcartr', subdir='getcartr')

# install.packages("hrbrthemes")

library(Rcartogram)
library(getcartr)
library(ggplot2)
library(tidyverse)

# read in shape file
world <- readShapePoly('TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')

# Using hate incident stats by OSCE's ODIHR
osce_data <- read_csv(file = 'osce_hate_incidents_2016_wide.csv')


# plot1=======
smaller.data <- osce_data %>% dplyr::select(Country.Code = iso3c, hate_fig_civil = figures_civil_intern_orgs) %>% 
    na.omit() %>% data.frame()

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

world.f_civil %>% 
    group_by(ISO3) %>% 
    summarise(longit=mean (long),latid=mean(lat)) %>% # calculate middle point of the distorted country polygons
    right_join(world.f_civil) %>% 
    mutate(label_text_civil= paste(.$ISO3,.$hate_fig_civil,sep = ":"))  -># create labels by pasting country iso3 and hate figures 
    world.f2

carto_civil <- ggplot(world.f2, aes(long, lat, group = group, fill = world.f2$hate_fig_civil %>% log2))+
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

carto_civil

ggsave("viz/Cartogram_002_civilorg.png", width = 16*0.8, height = 9*0.8, units = "in" , dpi = 300, scale = 1.5)

# do the same for hate fig official

# plot2======

smaller.data_2 <- data.frame(Country.Code = osce_data$iso3c, hate_fig_official = osce_data$figures_official_records)
smaller.data_2 <- na.omit(smaller.data_2)

## Join the two datasets using their common field
matched.indices_2 <- match(world@data[, "ISO3"], smaller.data_2[, "Country.Code"])
world@data <- data.frame(world@data, smaller.data_2[matched.indices_2, ])
## Compute the cartogram transformation of each country using its population
## with the degree of Gaussian blur = 0.5 (otherwise, it may not work)
world.carto_official <- quick.carto(world, world@data$hate_fig_official, blur = 0.7)
plot(world.carto_official)

## Convert the object into data frame
world.f_official <- fortify(world.carto_official, region = "Country.Code") %>% 
    merge(world@data, by.x = "id", by.y = "Country.Code")


world.f_official %>% 
    group_by(ISO3) %>% 
    summarise(longit=mean (long),latid=mean(lat)) %>% 
    right_join(world.f_official) %>% 
    mutate(label_text_official= paste(.$ISO3,.$hate_fig_official,sep = ":"))  ->
    world.f2_official


# temp===========
world.f2_official %>%
    dplyr::select(ISO3,hate_fig_official)%>%
    distinct %>% 
    dplyr::arrange(hate_fig_official) %>% 
    print(n=Inf)

world.f2_official %>% 
    mutate( .$ISO3%in%'USA', longit=longit+5, latid=latid-5)

world.f2_official$ISO3=='USA' 
# temp=======

carto_official <- ggplot(world.f2_official, aes(long, lat, group = group, fill = world.f2_official$hate_fig_official %>% log))+
    geom_polygon()+
    geom_text(data=world.f2_official, aes(x=longit, y=latid, label = label_text_official),size=3.5)+ #cant ge this to work
    # geom_text(data=world.f2, aes(x=longit-3, y=latid, label = ISO3),size=4)+ #cant ge this to work
    scale_fill_gradientn (name="Incident Counts",
                          colours=rev(brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:12),
                          labels=c(5, 10, 20, 50, 100, 200, 1000, 5000, 10000, 40000,80000, 100000))+
    # coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Distorted Longitude", 
         y='Distorted Latitude', 
         title="Cartogram of Racist and Xenophobic Incidents Recorded by the Police in Participating States in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 12))+
    theme(legend.position = c(0.1,0.25))+
    theme(legend.key.size = unit(0.4, "in"))

carto_official

ggsave("viz/Cartogram_002_official.png", width = 16*0.8, height = 9*0.8, units = "in" , dpi = 300, scale = 1.5)
