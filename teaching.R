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


leaflet(lga) %>% 
  addPolygons() %>% 
  addTiles()

tm_shape(lga)+
  tm_polygons()

lga_Ado <- lga %>% 
  filter(lga_name_x=='Ado Odo/Ota')


tm_shape(lga_Ado)+
  tm_borders(col='orange', lwd=5)+
tm_shape(lga)+
  tm_borders()+
  tm_basemap('OpenStreetMap')

# Discovering the health facilities dataset -------------------------------


health_facilities <- st_read('data/GRID3_Nigeria_-_Health_Care_Facilities_/GRID3_Nigeria_-_Health_Care_Facilities_.shp')

# exploration

health_facilities %>% 
  st_drop_geometry() %>% 
  View()

health_facilities_Ado <- health_facilities %>% 
  filter(lga_name=='Ado Odo/Ota')

# EXERCISE: How many health facilities in Ado Odo/Ota?
dim(health_facilities_Ado)


tm_shape(health_facilities_Ado)+
  tm_dots(col='type', size=0.07, id='primary_na', popup.vars=c('category','functional','source'))+
  tm_basemap('OpenStreetMap')+
  tm_shape(lga_Ado)+
  tm_borders(lwd=4)


# EXERCISE: How many health facilities are offering tertiary services in Ado Odo/Ota?
table(health_facilities_Ado$type)

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
health_facilities_Ado_buffered[1,]


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
# first method
sum(pop_Ado[], na.rm=T) - sum(pop_Ado_masked[], na.rm=T)

# second method
lga_Ado_pop <- raster::extract(pop, lga_Ado, fun=sum, na.rm=T,df=T)

# third method
lga_Ado$mean


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
health_facilities_Ado_buffered_maternity <- health_facilities_Ado_buffered %>% 
  filter(category=='Maternity Home')
nrow(health_facilities_Ado_buffered_maternity)

# EXERCISE: How many people are not living in a 1km distance of a maternity center?
health_facilities_Ado_buffered_maternity_rasterized <- rasterize(health_facilities_Ado_buffered_maternity, pop_Ado, field=1)
plot(health_facilities_Ado_buffered_maternity_rasterized)

pop_Ado_masked_maternity <- mask(pop_Ado, health_facilities_Ado_buffered_maternity_rasterized)
plot(pop_Ado_masked_maternity)

sum(pop_Ado[], na.rm=T) - sum(pop_Ado_masked_maternity[], na.rm=T)



# What is the furthest a woman has to travel to reach a maternity? --------
health_facilities_Ado_maternity <- health_facilities_Ado %>% 
  filter(category=='Maternity Home')

health_facilities_Ado_maternity_distance <- distanceFromPoints(pop_Ado, health_facilities_Ado_maternity)
plot(health_facilities_Ado_maternity_distance)


health_facilities_Ado_maternity_distance_pop <- mask(health_facilities_Ado_maternity_distance, pop_Ado)
plot(health_facilities_Ado_maternity_distance_pop)

summary(health_facilities_Ado_maternity_distance_pop[])


#EXERCISE: How many people are living at more than 8km from a maternity?
health_facilities_Ado_maternity_buffered8km <- st_buffer(health_facilities_Ado_maternity, dist=set_units(8, km))

health_facilities_Ado_maternity_buffered8km_rasterized <- rasterize(health_facilities_Ado_maternity_buffered8km, pop_Ado, field=1)
plot(health_facilities_Ado_maternity_buffered_rasterized)

pop_Ado_masked_maternity_8km <- mask(pop_Ado, health_facilities_Ado_maternity_buffered8km_rasterized, inverse=T)
plot(pop_Ado_masked_maternity_8km)

sum(pop_Ado_masked_maternity_8km[], na.rm=T)



# And how many woman of childbearing age?... -----------------------------------------------------

women <- raster('data/NGA_population_v2_0_agesex/NGA_population_v2_0_agesex_f15_49.tif')

#EXERCISE: How many women of childbearing age are living at more than 8km from a maternity?
women_Ado <- crop(women, lga_Ado)
women_Ado <- mask(women_Ado, lga_Ado)

women_Ado_masked_maternity_8km <- mask(women_Ado, health_facilities_Ado_maternity_buffered8km_rasterized, inverse=T)
sum(women_Ado_masked_maternity_8km[], na.rm=T)


# What about the number of women of childbearing age living at more than 8km from a maternity in the country?
health_facilities_maternity <- health_facilities %>% 
  filter(category=='Maternity Home')
health_facilities_maternity_buffered8km <- st_buffer(health_facilities_maternity, dist=set_units(8, km))
health_facilities_maternity_buffered8km_rasterized <- rasterize(health_facilities_maternity_buffered8km, pop, field=1)
pop_masked_maternity_8km <- mask(pop, health_facilities_maternity_buffered8km_rasterized, inverse=T)



tm_shape(pop_masked_maternity_8km)+
  tm_raster()
