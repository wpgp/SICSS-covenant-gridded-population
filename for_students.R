library(tidyverse)
library(tmap)

tmap_mode('view')
tmap_options(check.and.fix = TRUE) 

# Defining the study area -------------------------------------------------
library(sf)

lga <- st_read('data/GRID3_Nigeria_-_Local_Government_Area_Boundaries/GRID3_Nigeria_-_Local_Government_Area_Boundaries.shp')

lga %>% 
  st_drop_geometry() %>% 
  View()

tm_shape(lga)+
  tm_polygons()

lga_Ado <- lga %>% 
  filter(lga_name_x=='Ado Odo/Ota')

# EXERCISE: can you build an interactive map of the LGAs in Nigeria using Leaflet?







# Discovering the health facilities dataset -------------------------------


health_facilities <- st_read('data/GRID3_Nigeria_-_Health_Care_Facilities_/GRID3_Nigeria_-_Health_Care_Facilities_.shp')

# exploration

health_facilities %>% 
  st_drop_geometry() %>% 
  View()

health_facilities_Ado <- health_facilities %>% 
  filter(lga_name=='Ado Odo/Ota')

# EXERCISE: How many health facilities in Ado Odo/Ota?

tm_shape(health_facilities_Ado)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(lga_Ado)+
  tm_borders(lwd=4)

# EXERCISE: How many health facilities are offering tertiary services in Ado Odo/Ota?






# Discovering the gridded population dataset ------------------------------

library(raster)

pop <- raster('data/NGA_population_v2_0_gridded/NGA_population_v2_0_gridded.tif')


pop_Ado <- crop(pop, lga_Ado)
plot(pop_Ado)

pop_Ado <- mask(pop_Ado, lga_Ado)
plot(pop_Ado)

tm_shape(health_facilities_Ado)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(pop_Ado)+
  tm_raster()+
  tm_shape(lga_Ado)+
  tm_borders(lwd=4)



# Buffering points --------------------------------------------------------


library(units)

health_facilities_Ado_buffered <- st_buffer(health_facilities_Ado, dist=set_units(1, km))

health_facilities_Ado[1,]


tm_shape(pop_Ado)+
  tm_raster()+
  tm_shape(health_facilities_Ado_buffered[1,])+
  tm_borders()+
  tm_shape(health_facilities_Ado[1,])+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')



# Computing the population ------------------------------------------------


health_facilities_Ado_pop <- raster::extract(pop_Ado, health_facilities_Ado_buffered, fun=sum, na.rm=T,df=T)

health_facilities_Ado_buffered$pop <- health_facilities_Ado_pop$NGA_population_v2_0_gridded

#EXERCISE: How many people are living in 1km of Ado Odo Ii Health Center?





summary(health_facilities_Ado_buffered$pop)
hist(health_facilities_Ado_buffered$pop, breaks=20)

tm_shape(health_facilities_Ado_buffered)+
  tm_fill('pop', style='pretty', id='pop')+
  tm_shape(health_facilities_Ado)+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

tm_shape(health_facilities_Ado_buffered %>% 
           filter(pop<1000))+
  tm_fill( id='pop', alpha=0.5, col='grey20')+
  tm_shape(pop_Ado)+
  tm_raster()+
  tm_basemap('OpenStreetMap')

#  How many people are not covered by health facilities? ------------------

health_facilities_Ado_buffered_rasterized <- rasterize(health_facilities_Ado_buffered, pop_Ado, field=1)
plot(health_facilities_Ado_buffered_rasterized)

pop_Ado_masked <- mask(pop_Ado, health_facilities_Ado_buffered_rasterized)
plot(pop_Ado_masked)

sum(pop_Ado_masked[], na.rm=T)

#EXERCISE: How many people are not living in a 1km of an health facility?







# How many are not covered by a maternity home? ---------------------------


tm_shape(pop_Ado)+
  tm_raster()+
  tm_shape(health_facilities_Ado)+
  tm_dots( size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_shape(health_facilities_Ado %>% 
             filter(category=='Maternity Home'))+
  tm_dots(col='darkgreen', size=0.08, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')

# EXERCISE: How many maternity homes are listed in the LGA?










# EXERCISE: How many people are not living in a 1km distance of a maternity center?











# What is the furthest a woman has to travel to reach a maternity? --------
health_facilities_Ado_maternity <- health_facilities_Ado %>% 
  filter(category=='Maternity Home')

health_facilities_Ado_maternity_distance <- distanceFromPoints(pop_Ado, health_facilities_Ado_maternity)
plot(health_facilities_Ado_maternity_distance)


health_facilities_Ado_maternity_distance_pop <- mask(health_facilities_Ado_maternity_distance, pop_Ado)
plot(health_facilities_Ado_maternity_distance_pop)

summary(health_facilities_Ado_maternity_distance_pop[])


#EXERCISE: How many people are living at more than 8km from a maternity?







# And how many woman of childbearing age?... -----------------------------------------------------

women <- raster('data/NGA_population_v2_0_agesex/NGA_population_v2_0_agesex_f15_49.tif')

#EXERCISE: How many women of childbearing age are living at more than 8km from a maternity?







# EXERCISE: What about the number of women of childbearing age living at more than 8km from a maternity in the country?





