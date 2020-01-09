library(Rcartogram)
library(getcartr)
library(ggplot2)
library(tidyverse)


calc_carto <-  function (dataset_path, value_col, country_code_col, shape_file_path, blur=2){
    # read the shape file
    world <- readShapePoly(shape_file_path)
    # read the dataset
    dataset <- read.csv(file = dataset_path, stringsAsFactors = FALSE)
    # subset the dataset with iso3 and the value to use for cartogram distortion
    smaller_data <- data.frame(country_code = dataset[,country_code_col], value = dataset[,value_col])
    smaller_data <- na.omit(smaller_data)
    ## Join the two datasets using their common field
    matched_indices <- match(world@data[, "ISO3"], smaller_data[, "country_code"])
    world@data <- data.frame(world@data, smaller_data[matched_indices, ])
    ## Compute the cartogram transformation of each country using value
    ## with the degree of Gaussian blur = 0.5 (otherwise, it may not work)
    world_carto <- quick.carto(world, world@data$value, blur = blur)
    ## Convert the object into data frame
    carto_fortified <- fortify(world_carto, region = "country_code")
    ## Merge the cartogram transformation with the world map shapefile
    world_carto_merge <- merge(carto_fortified, world@data, by.x = "id", by.y = "country_code")
    
}




cartogram_data <- calc_carto(dataset_path = 'data/2018/osce_hate_incidents_2018_wide.csv',
               shape_file_path= 'TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp', 
               value_col = "other_sources",
               country_code_col="iso3c"
               )


my_map <- cartogram_data %>% 
    ggplot( aes(long, lat, group = group, fill =value)) + 
    geom_polygon()

my_map


cartogram_data %>% 
    group_by(ISO3) %>% 
    summarise(longit=mean (long),latid=mean(lat)) %>% # calculate middle point of the distorted country polygons
    right_join(cartogram_data) %>% 
    mutate(label_text_civil= paste(.$ISO3,.$value,sep = ":"))  -> # create labels by pasting country iso3 and hate figures 
    cartogram_data_1


cartogram_data_1 %>% 
    mutate(longit= ifelse(ISO3=="USA", longit+7,longit),
           longit=ifelse(ISO3=="CAN", longit-2,longit),
           longit=ifelse(ISO3=="TUR", longit+1,longit),
           longit=ifelse(ISO3=="SWE", longit-0.8,longit),
           longit=ifelse(ISO3=="GBR", longit+1,longit),
           latid=ifelse(ISO3=="USA", latid-2.5,latid),
           latid=ifelse(ISO3=="MKD", latid+1,latid),
           latid=ifelse(ISO3=="GRC", latid-1,latid),
           latid=ifelse(ISO3=="ROU", latid-0.5,latid),
           latid=ifelse(ISO3=="CAN", latid-0.5,latid),
           latid=ifelse(ISO3=="HRV", latid-0.5,latid)
           # latid=ifelse(ISO3=="ROU", latid,latid)
           # ISO3=ifelse(ISO3=="BIH", NA,ISO3),#get rid of overlapping NA countries
           # ISO3=ifelse(ISO3=="SVN", NA,ISO3),#get rid of overlapping NA countries
           # label_text_official=ifelse(ISO3=="SVN", NA,label_text_official),
           # label_text_official=ifelse(ISO3=="BIH", NA,label_text_official)
    )->cartogram_data_1





carto_other_sources <- ggplot(cartogram_data_1, aes(long, lat, group = group, fill = cartogram_data_1$value %>% log2))+
    geom_polygon()+
    geom_text(data=cartogram_data_1, aes(x=longit, y=latid, label = label_text_civil),size=4)+ #cant ge this to work
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
         title="Racist and Xenophobic Incidents Reported by Other Sources in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="HateLab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 12))+
    theme(legend.position = c(0.1,0.25))+
    theme(legend.key.size = unit(0.4, "in"))

carto_other_sources
