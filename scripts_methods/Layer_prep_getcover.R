library (sf)
library(raster)

#Read original Priority Habitat Inventory England (phi) North
N_phi<-st_read("spatial_data/derived/Priority_Habitat_Inventory_England_1.shp")
S_phi<-st_read("spatial_data/derived/Priority_Habitat_Inventory_England_2.shp")

#subset heathland habitats
N_heath<- N_phi[grep('*HEAT',N_phi$habcodes),]
st_write(N_heath, "spatial_data/derived", "N_heath", driver= "ESRI Shapefile", update = TRUE)

S_heath<- S_phi[grep('*HEAT',S_phi$habcodes),]
st_write(S_heath, "spatial_data/derived", "S_heath", driver= "ESRI Shapefile", update = TRUE)

rm(phi_full)
#Join north and south
Nat_heath<- rbind(N_heath, S_heath)
st_write(Nat_heath,"spatial_data/derived", "Nat_heath", driver= "ESRI Shapefile", update = TRUE)

#Convert to raster with the characteristics of the England boundaries raster
Eng_bound<-raster("spatial_data/derived/england.anything.100.tif")#extent reference w/British National Grid 

# Set up a raster "template" to rasterize PHI shapefile
ext <- extent(Eng_bound) #extent of the England boundaries raster
#Select raster resolution
gridsize <- 250
r <- raster(ext, res=gridsize)

###Test region
test_area<-st_read("spatial_data/derived/test_area.shp")

test_heath<- st_intersection(Nat_heath, test_area) #clip shapefile with test region

test_heath_250m<-rasterize (test_heath, r, getCover=TRUE) #convert to 250m-resolution raster with the extent of the boundaries. The value is the fraction of the 250m cell that is covered by the polygons.The fraction is estimated by dividing each cell into 100 sub-cells (25m) and determining presence/absence of the polygon in the center of each sub-cell
NAvalue(test_heath_250m) <- 0 #All zeros converted to NA

writeRaster(test_heath_250m,"spatial_data/derived/test_heath_250m_int.tif", overwrite=TRUE) #save raster as spatial reference is not assigned to 'Formal class Raster Layer' objects

# assign spatial reference British National Grid ("EPSG:27700") and rewrite
test_heath_250m<-raster("spatial_data/derived/test_heath_250m_int.tif")
NAvalue(test_heath_250m) <- 0 #All zeros converted to NA
crs(test_heath_250m)<-"EPSG:27700"
writeRaster(test_heath_250m,"spatial_data/derived/test_heath_250m.tif", overwrite= TRUE )# save raster for local scale analyses

#re-sample at 1km-resolution
test_heath_1km<- aggregate(test_heath_250m, fact = 4, fun= sum, na.rm=TRUE)/16 #re-scale raster to 1km-resolution where raster value equals to the sum of 250m-pixels divided by 16 (no of pixels within 1km cell)
NAvalue(test_heath_1km) <- 0 
writeRaster(test_heath_1km,"spatial_data/derived/test_heath_1km_int.tif", overwrite= TRUE)#save raster as spatial reference is not assigned to 'Formal class Raster Layer' objects

# assign spatial reference British National Grid ("EPSG:27700") and rewrite
test_heath_1km<-raster("spatial_data/derived/test_heath_1km_int.tif")
crs(test_heath_1km)<-"EPSG:27700"
writeRaster(test_heath_1km,"spatial_data/derived/test_heath_1km.tif", overwrite=TRUE)











