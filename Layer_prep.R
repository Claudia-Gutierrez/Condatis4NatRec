library (sf)
library(raster)

#Read original Priority Habitat Inventory England (phi) North
N_phi<-st_read("spatial_data/Priority_Habitat_Inventory_England_1.shp")


#subset heathland habitats
table(N_phi$mainhabs) #habitat names
N_heath_shp<- N_phi[N_phi$mainhabs=='Upland heathland'|N_phi$mainhabs=='Lowland heathland'|N_phi$mainhabs=='Fragmented heath'|N_phi$mainhabs=='Lowland dry acid grassland'|N_phi$mainhabs=='Lowland fens'|N_phi$mainhabs=='Upland flushes fens and swamps',]

#Convert to raster with the characteristics of the England boundaries raster
Eng_bound<-raster("spatial_data/engboundary1kmBNG.tif")#extent reference w/British National Grid 

# Set up a raster "template" to rasterize PHI shapefile
ext <- extent(Eng_bound) #extent of the England boundaries raster
#Select raster resolution
gridsize <- 50
r <- raster(ext, res=gridsize)

###Test region
test_area<-st_read("spatial_data/test_area.shp")

test_N_heath_shp<- st_intersection(N_heath_shp, test_area) #clip shapefile with test region

test_N_heath_50m<-rasterize (test_N_heath_shp, r,1) #convert to 50m-resolution raster 
crs(test_N_heath_50m)<-"EPSG:27700" #assign spatial reference British National Grid
writeRaster(test_N_heath_50m,"spatial_data/test_N_heath_50m.tif") #save raster for local scale analyses

test_N_heath_int<- aggregate(test_N_heath_50m, fact = 20, fun= sum, na.rm=TRUE) #rescale raster to 1km-resolution where raster value equals to the sum of 50m-pixels within 1k pixels

test_N_heath_1km<-(test_N_heath_int*2500)/1000000 #calculate the proportion of habitat per km^2
crs(test_N_heath_1km)<-"EPSG:27700" #assign spatial reference British National Grid
writeRaster(test_N_heath_1km,"spatial_data/test_N_heath_1km.tif") #save raster for national scale analyses



### Clip rasters to England boundaries 

#option 1: clip shapefiles (should be first step)
#bound<-st_read("spatial_data/boundaries2.shp")
#N_heath_1k_bound<- st_intersection(N_heath_shp, bound)

#option 2: reassign raster values (after rescale)
#N_heath_1k_bound<- ifelse(Eng_bound>0,NA,N_heath_1km)









