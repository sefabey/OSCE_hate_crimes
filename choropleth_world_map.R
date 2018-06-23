# Choropleth world map

library(rworldmap)
library(ggplot2)
library(RColorBrewer)

# data preperation====
map.world <- map_data(map="world") #getting world map in dataframe format to plot in ggplot2
osce_data <- read_csv("osce_hate_incidents_2016_wide.csv") #reading in OSCE hate incidents
map.world$iso3c <- countrycode::countrycode(map.world$region, origin = 'country.name', destination = 'iso3c' ) #getting country codes in iso3c format
osce_map_joined <- left_join(map.world, osce_data, by = 'iso3c') #joining map dataframe and hate incidents

# first plot in blue shades====
plot1 <- ggplot()+ 
    theme(legend.position="bottom")+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=osce_map_joined$figures_civil_intern_orgs))+
    scale_fill_gradient (low ="#56B1F7" , high = "#132B43", 
                         na.value = "grey90", 
                         name="Incident Numbers",
                         guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Choropleth of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size=10))
plot1

# second plot using RcolorBrewer====
# mind that legend has been moved inside the map below. can move it to outside if needed (right or bottom)
plot2 <- ggplot()+ 
    # theme(legend.position="bottom")+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=osce_map_joined$figures_civil_intern_orgs))+
    scale_fill_gradientn (name="Incident Numbers",
                          colours=rev(brewer.pal(9,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Choropleth of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 10))+
    theme(legend.position = c(0.1,0.25))# for positioning the legend inside the plot

plot2

# third plot, printing incident counts for each country with a value====

#need to calculate the centroids (centre points) of the countries before adding a text layer
hate_incidents_centroids <- aggregate(cbind(long, lat) ~ figures_civil_intern_orgs, data=osce_map_joined, FUN=mean)

plot3 <- ggplot()+ 
    # theme(legend.position="bottom")+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=osce_map_joined$figures_civil_intern_orgs))+
    geom_text(data=hate_incidents_centroids, aes(long, lat, label = figures_civil_intern_orgs), size=2)+
    scale_fill_gradientn (name="Incident Numbers",
                          colours=rev(brewer.pal(9,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Choropleth of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 10))+
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
    left_join(osce_data)
centroids


plot4 <- ggplot()+ 
    # theme(legend.position="bottom")+ 
    geom_map(data=osce_map_joined, map=map.world, 
             aes(map_id=region, x=long, y=lat, fill=osce_map_joined$figures_civil_intern_orgs))+
    geom_text(data=centroids, aes(x, y, label = figures_civil_intern_orgs), size=2)+
    scale_fill_gradientn (name="Incident Numbers",
                          colours=rev(brewer.pal(9,"Spectral")),
                          # na.value="white",
                          na.value = "grey90",
                          guide = "colourbar")+
    coord_map(xlim=c(-180,180), ylim = c(-60, 150))+
    labs(x="Longitude", 
         y='Latitude', 
         title="Choropleth of Racist and Xenophobic Incidents Reported by International and Civil Society Organisations in 2016",
         subtitle="Source: Office for Democratic Institutions and Human Rights (ODIHR) of the\nOrganization for Security and Co-operation in Europe (OSCE)  ",
         caption="Social Data Science Lab, Cardiff University")+
    hrbrthemes::theme_ipsum_rc()+
    theme(plot.caption = element_text(size = 10))+
    theme(legend.position = c(0.1,0.25))# for positioning the legend inside the plot

plot4

# still centroids of some countries seems to be off (us looks fine but France seems to have issues atm.)


txtVal <- doBy::summaryBy(long + lat + Value ~ id, data=osce_map_joined, FUN=mean, keep.names=T)
m3 <- m2 + geom_text(aes(x=long, y=lat, label=Value), data=txtVal, col="yellow", cex=3)