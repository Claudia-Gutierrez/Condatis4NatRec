library(raster)
library (sf)

#Convert to raster with the characteristics of the 100m- England boundaries raster
Eng_bound<-raster("spatial_data/derived/england.anything.100.tif")#extent reference w/British National Grid 
Nat_heath<-st_read("spatial_data/derived/Nat_heath.shp")

# Set up a raster "template" to rasterize PHI shapefile
ext <- extent(Eng_bound) #extent of the England boundaries raster
#Select raster resolution
gridsize <- 250
r <- raster(ext, res=gridsize)

nat_heath_250m<-rasterize(Nat_heath, r,getCover=TRUE) #convert to 250m-resolution raster 
NAvalue(nat_heath_250m) <- 0

crs(nat_heath_250m)<-"EPSG:27700" #assign spatial reference British National Grid
writeRaster(nat_heath_250m,"spatial_data/derived/Nat_250m_heathland.tif", na.rm=TRUE,overwrite=TRUE) #save raster 

nat_heath_1k<- aggregate(nat_heath_250m, fact = 4, fun= sum, na.rm=TRUE)/16 #rescale raster to 1km-resolution where raster value equals to the sum of the proportion of habitat in 250m-pixels within 1k pixels

crs(nat_heath_1k)<-"EPSG:27700" #assign spatial reference British National Grid
writeRaster(nat_heath_1k,"spatial_data/derived/Nat_1km_heathland.tif",overwrite=TRUE) #save raster for national scale analyses
