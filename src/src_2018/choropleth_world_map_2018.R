# Choropleth world map
# Choropleth world map
# install.packages("rworldmap")
# install.packages("countrycode")
# install.packages("hrbrthemes")
# install.packages("units") # requires sudo apt-get update -y and sudo apt-get install -y libudunits2-dev in the system
# install.packages("sf") #requires sudo apt-get install libgdal-dev
# install.packages("rnaturalearth")
# install.packages("CoordinateCleaner")
# install.packages("viridis")
# install.packages("mapproj")

library(rworldmap)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(viridis)

# data preperation====
map.world <- map_data(map="world") #getting world map in dataframe format to plot in ggplot2
osce_data <- read_csv("data/2018/osce_hate_incidents_2018_wide.csv") #reading in OSCE hate incidents
map.world$iso3c <- countrycode::countrycode(map.world$region, origin = 'country.name', destination = 'iso3c' ) #getting country codes in iso3c format

osce_map_joined <- left_join(map.world, osce_data, by = 'iso3c')#joining map dataframe and hate incidents


# first plot in grey scale or blue shades====
plot1 <- ggplot()+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=other_sources), colour="gray50")+
    scale_fill_gradient (low ="gray80" , high = "black",
                         na.value = "white",
                         name="Incident Numbers",
                         guide = "colourbar")+
    
    # scale_fill_grey(name="Incident Numbers", aesthetics = "fill")+
    # scale_fill_viridis_c(name="Incident Numbers")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Racist and Xenophobic Incidents Reported by Other Sources in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size=12))+
    theme(legend.position = c(0.1,0.25))+
    NULL
plot1



ggsave(filename = "viz/2018/Choropleth_002_other_org_2018_greyscale.pdf",
       plot = plot1,
       width = 16*0.8, height = 9*0.8, units = "in" ,
       dpi = 500, scale = 1.3, 
       device = cairo_pdf) 
# second plot using viridis or RcolorBrewer====
# mind that legend has been moved inside the map below. can move it to outside if needed (right or bottom)
plot2 <- ggplot()+ 
    # theme(legend.position="bottom")+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=other_sources))+
    # scale_fill_gradientn (name="Incident Numbers",
    #                       colours=rev(brewer.pal(9,"Spectral")),
    #                       # na.value="white",
    #                       na.value = "grey90",
    #                       guide = "colourbar")+
    scale_fill_viridis_c(name="Incident Numbers")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Racist and Xenophobic Incidents Reported by Other Sources in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+  
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))# for positioning the legend inside the plot

plot2


# third plot, printing incident counts for each country with a value====

#need to calculate the centroids (centre points) of the countries before adding a text layer
hate_incidents_centroids <- aggregate(cbind(long, lat) ~ other_sources, data=osce_map_joined, FUN=mean)
hate_incidents_centroids # 35 controids

osce_map_joined %>% 
    filter(!is.na(other_sources)) 
    # distinct(country)

osce_data %>% nrow() #57 countries
osce_data %>% drop_na(other_sources) %>% nrow() # 47 countries with other sources
osce_data %>% drop_na(other_sources) %>% arrange(other_sources)

osce_map_joined



plot3 <- ggplot()+ 
    # theme(legend.position="bottom")+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=other_sources))+
    geom_text(data=hate_incidents_centroids, aes(long, lat, label = other_sources), size=2)+
    scale_fill_gradientn (name="Incident Numbers",
                          colours=rev(brewer.pal(9,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Racist and Xenophobic Incidents Reported by Other Source in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+   
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))# for positioning the legend inside the plot

plot3
# seems to be working fine but placement of figures are still not great because some centroids seem to be off a little. Reason is centroid calculation is based on polygons for mapping and some countries, such as the us has external territories (e.g. Alaska) which results in weird centroid placement. 

# attempting to calculate centroids externally
centroids <- rworldmap::getMap(resolution = 'high') %>% 
    rgeos::gCentroid( byid = T) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as.tibble() %>% 
    mutate(iso3c= countrycode::countrycode(rowname, origin = 'country.name', destination = 'iso3c' )) %>% 
    left_join(osce_data) %>% 
    distinct(.keep_all = T)
centroids

plot4 <- ggplot()+ 
    # theme(legend.position="bottom")+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=osce_map_joined$other_sources))+
    geom_text(data=centroids, aes(x, y, label = other_sources), size=2)+
    scale_fill_gradientn (name="Incident Numbers",
                          colours=rev(brewer.pal(9,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Racist and Xenophobic Incidents Reported by Other Sources in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+  
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))# for positioning the legend inside the plot

plot4

# still centroids of some countries seems to be off (us looks fine but France seems to have issues atm.)

#trying to add text based on capital geolocations using CoordinateCleaner package
capitals_joined <- CoordinateCleaner::countryref %>% 
    distinct(iso3,.keep_all = T) %>% 
    mutate(iso3c=iso3, long=capital.lon, lat=capital.lat) %>% 
    left_join(osce_data)

plot5 <- ggplot()+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=osce_map_joined$other_sources))+
    geom_text(data=capitals_joined, aes(x=long, y=lat, label = other_sources), size=2)+
    scale_fill_gradientn (name="Incident Numbers",
                          colours=rev(brewer.pal(9,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Racist and Xenophobic Incidents Reported by Other Sources in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+   
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))# for positioning the legend inside the plot
plot5

# still not looking as it should. Trying centroids dataset from the CoordinateCleaner package.

centroids_joined <- CoordinateCleaner::countryref %>% 
    distinct(iso3,.keep_all = T) %>% 
    mutate(iso3c=iso3, long=centroid.lon, lat=centroid.lat) %>% 
    left_join(osce_data)

# final plots ======

plot6 <- ggplot()+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=other_sources %>% log))+
    geom_text(data=centroids_joined, aes(x=long, y=lat, label = other_sources), size=2.5)+
    
    scale_fill_gradientn (name="Incident Counts\n(Log Scale)",
                          colours=rev(brewer.pal(9,"Spectral")),
                          breaks= rep(1:10),
                          labels=c( 5,10, 25, 100, 250, 500, 1000, 1500, 2000, 5000),
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Racist and Xenophobic Incidents Reported by Other Sources in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+    
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))+# for positioning the legend inside the plot
    theme(legend.key.size = unit(0.4, "in"))

plot6

ggsave(filename = "viz/2018/Choropleth_002_other_org_2018.pdf",
       plot = plot6,
       width = 16*0.8, height = 9*0.8, units = "in" ,
       dpi = 500, scale = 1.3, 
       device = cairo_pdf)
# much better! as they say, 6th time is the charm!


plot7 <- ggplot()+
geom_map(data=osce_map_joined, map=map.world, 
         aes(map_id=region, x=long, y=lat, fill=police_records %>% log))+
    geom_text(data=centroids_joined %>% filter(police_records>0), 
              aes(x=long, y=lat, label = police_records), size=2.5)+
    scale_fill_gradientn (name="Incident Counts\n(Log Scale)",
                          colours=rev(brewer.pal(9,"Spectral")),
                          breaks= rep(1:12),
                          labels=c( 5,10, 25, 100, 250, 500, 1000, 2000, 5000, 10000,20000, 100000),
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Hate Crimes Recorded by the Police in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+   
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))+# for positioning the legend inside the plot
    theme(legend.key.size = unit(0.4, "in"))

plot7

ggsave(plot = plot7, filename = "viz/2018/Choropleth_002_police_2018.pdf",
       device = cairo_pdf,
       width = 16*0.8, height = 9*0.8, units = "in" , dpi = 500, scale = 1.5)
ggsave(plot = plot7, filename = "viz/2018/Choropleth_002_police_2018.eps",
       device = cairo_ps,
       width = 16*0.8, height = 9*0.8, units = "in" , dpi = 500, scale = 1.5)

plot7_greyscale <- ggplot()+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=police_records
                 ), 
             colour="gray50")+
    scale_fill_gradient (low ="gray80" , high = "black",
                         na.value = "white",
                         name="Incident Counts",
                         guide = "colourbar")+
    # geom_text(data=centroids_joined %>% filter(police_records>0), 
    #           aes(x=long, y=lat, label = police_records), size=3.5)+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Hate Crimes Recorded by the Police in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+   
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))+# for positioning the legend inside the plot
    theme(legend.key.size = unit(0.4, "in"))
plot7_greyscale

ggsave(plot = plot7_greyscale, filename = "viz/2018/Choropleth_002_police_2018_greyscale.pdf",
       device = cairo_pdf,
       width = 16*0.8, height = 9*0.8, units = "in" , dpi = 500, scale = 1.5)





plot6 <- ggplot()+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=other_sources %>% log))+
    geom_text(data=centroids_joined, aes(x=long, y=lat, label = other_sources), size=2.5)+
    
    scale_fill_gradientn (name="Incident Counts\n(Log Scale)",
                          colours=rev(brewer.pal(9,"Spectral")),
                          breaks= rep(1:10),
                          labels=c( 5,10, 25, 100, 250, 500, 1000, 1500, 2000, 5000),
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Racist and Xenophobic Incidents Reported by Other Sources in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
    theme(plot.caption = element_text(size = 14))+   
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))+# for positioning the legend inside the plot
    theme(legend.key.size = unit(0.4, "in"))


plot7_greyscale_log <- ggplot()+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=police_records %>% log
             ), 
             colour="gray50")+
    
    scale_fill_gradient (low ="gray80" , high = "black",
                         na.value = "white",
                         name="Incident Counts\n(Log Scale)",
                         guide = "colourbar")+
    # geom_text(data=centroids_joined %>% filter(police_records>0), 
    #           aes(x=long, y=lat, label = police_records), size=3.5)+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Hate Crimes Recorded by the Police in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)",
         caption = "HateLab, 2019, by @SefaOzalp")+
    hrbrthemes::theme_ipsum()+
   
    theme(plot.caption = element_text(size = 14))+
    theme(plot.subtitle = element_text(size = 14))+
    theme(legend.position = c(0.1,0.25))+# for positioning the legend inside the plot
    theme(legend.key.size = unit(0.4, "in"))
    

plot7_greyscale_log

ggsave(plot = plot7_greyscale_log, filename = "viz/2018/Choropleth_002_police_2018_greyscale_log.pdf",
       device = cairo_pdf,
       width = 16*0.8, height = 9*0.8, units = "in" , dpi = 500, scale = 1.5)
