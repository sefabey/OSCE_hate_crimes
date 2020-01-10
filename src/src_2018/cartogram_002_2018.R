library(Rcartogram)
library(getcartr)
library(ggplot2)
library(tidyverse)

# Function to calculate the cartogram----------

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


# OSCE other org. data--------

# call the carto_calc function for other organisation data

cartogram_data <- calc_carto(dataset_path = 'data/2018/osce_hate_incidents_2018_wide.csv',
               shape_file_path= 'TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp', 
               value_col = "other_sources",
               country_code_col="iso3c"
               )

# quick visual inspection of the cartogram calculation
my_map <- cartogram_data %>% 
    ggplot( aes(long, lat, group = group, fill =value)) + 
    geom_polygon()

my_map


# add centroids and country labels
cartogram_data %>% 
    group_by(ISO3) %>% 
    summarise(longit=mean (long),latid=mean(lat)) %>% # calculate middle point (centroid) of the distorted country polygons
    right_join(cartogram_data) %>% 
    mutate(label_text_civil= paste(.$ISO3,.$value,sep = ":"))  -> # create labels by pasting country iso3 and hate figures 
    cartogram_data_1

# manual adjustments to centroids after visual inspection (expect to do this iteratively)
cartogram_data_1 <- cartogram_data_1 %>% 
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
)


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
         caption="HateLab, 2020, by @SefaOzalp")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 12))+
    theme(legend.position = c(0.1,0.25))+
    theme(legend.key.size = unit(0.4, "in"))

carto_other_sources

ggsave(plot=carto_other_sources, filename = "viz/2018/cartogram_002_other_org_labels.pdf",
       device = cairo_pdf,
       width = 16*0.8, height = 9*0.8, units = "in" , dpi = 500, scale = 1.5)

# OSCE police recorded data--------

# call the carto_calc() function
cartogram_data_police <- calc_carto(dataset_path = 'data/2018/osce_hate_incidents_2018_wide.csv',
                             shape_file_path= 'TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp', 
                             value_col = "police_records",
                             country_code_col="iso3c"
)

# quick visual inspection of the cartogram calculation
my_map_police <- cartogram_data_police %>% 
    ggplot( aes(long, lat, group = group, fill =value)) + 
    geom_polygon()

my_map_police


# add centroids and country labels
cartogram_data_police %>% 
    group_by(ISO3) %>% 
    summarise(longit=mean (long),latid=mean(lat)) %>% # calculate middle point (centroid) of the distorted country polygons
    right_join(cartogram_data_police) %>% 
    mutate(label_text_police= paste(.$ISO3,.$value,sep = ":"))  -> # create labels by pasting country iso3 and hate figures 
    cartogram_data_police_1


# manual adjustments to centroids after visual inspection (expect to do this iteratively)
cartogram_data_police_1 <- cartogram_data_police_1 %>% 
    mutate(longit= ifelse(ISO3=="USA", longit+7,longit),
           longit=ifelse(ISO3=="GBR", longit+2,longit),
           longit=ifelse(ISO3=="TUR", longit+1,longit),
           longit=ifelse(ISO3=="MKD", longit+0.5,longit),
           longit=ifelse(ISO3=="SRB", longit-0.5,longit),
           longit=ifelse(ISO3=="SVK", longit-0.5,longit),
           longit=ifelse(ISO3=="ROU", longit+0.5,longit),
           longit=ifelse(ISO3=="CZE", longit-1,longit),
           longit=ifelse(ISO3=="HUN", longit+1,longit),
           latid=ifelse(ISO3=="USA", latid-2.5,latid),
           latid=ifelse(ISO3=="SWE", latid+1,latid),
           latid=ifelse(ISO3=="UKR", latid+0.7,latid),
           latid=ifelse(ISO3=="GRC", latid-0.5,latid),
           latid=ifelse(ISO3=="SRB", latid-0.5,latid),
           latid=ifelse(ISO3=="MKD", latid-0.1,latid),
           latid=ifelse(ISO3=="HUN", latid-0.8,latid),
           latid=ifelse(ISO3=="SVK", latid+0.5,latid),
           latid=ifelse(ISO3=="ROU", latid-0.5,latid),
           latid=ifelse(ISO3=="HRV", latid-1,latid),
           latid=ifelse(ISO3=="POL", latid+0.3,latid),
           # ISO3=ifelse(ISO3=="BIH", NA,ISO3),#get rid of overlapping NA countries
           # ISO3=ifelse(ISO3=="SVN", NA,ISO3),#get rid of overlapping NA countries
           # label_text_official=ifelse(ISO3=="SVN", NA,label_text_official),
           # label_text_official=ifelse(ISO3=="BIH", NA,label_text_official)
           )




carto_police_records <- cartogram_data_police_1 %>% 
    ggplot(aes(long, lat, group = group, fill =value %>% log))+
    geom_polygon()+
    geom_text(data=cartogram_data_police_1, aes(x=longit, y=latid, label = label_text_police),size=3.5)+ #cant ge this to work
    # geom_text(data=world.f2, aes(x=longit-3, y=latid, label = ISO3),size=4)+ #cant ge this to work
    scale_fill_gradientn (name="Police Records",
                          colours=rev(brewer.pal(8,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar",
                          breaks= rep(1:12),
                          labels=c(5, 10, 20, 50, 100, 200, 1000, 5000, 10000, 40000,80000, 100000))+
    # coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Distorted Longitude", 
         y='Distorted Latitude', 
         title="Hate Crimes Recorded by the Police in Participating States in 2018",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Hate Lab, 2020, by @SefaOzalp")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 12))+
    theme(legend.position = c(0.1,0.25))+
    theme(legend.key.size = unit(0.4, "in"))

carto_police_records

ggsave(plot=carto_police_records, filename = "viz/2018/cartogram_002_police_labels.pdf",
       device = cairo_pdf,
       width = 16*0.8, height = 9*0.8, units = "in" , dpi = 500, scale = 1.5)