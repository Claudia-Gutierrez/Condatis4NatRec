library (sf)
library(raster)

#Read original Priority Habitat Inventory England (phi) North
N_phi<-st_read("spatial_data/derived/Priority_Habitat_Inventory_England_1.shp")
S_phi<-st_read("spatial_data/derived/Priority_Habitat_Inventory_England_2.shp")

#subset heathland habitats
N_heath<- N_phi[grep('*HEAT',N_phi$habcodes),]
S_heath<- S_phi[grep('*HEAT',S_phi$habcodes),]

#Join north and south
phi_full<- st_union(N_heath, S_heath)

st_write(phi_full)

#Convert to raster with the characteristics of the England boundaries raster
Eng_bound<-raster("spatial_data/derived/england.anything.100.tif")#extent reference w/British National Grid 

# Set up a raster "template" to rasterize PHI shapefile
ext <- extent(Eng_bound) #extent of the England boundaries raster
#Select raster resolution
gridsize <- 250
r <- raster(ext, res=gridsize)

###Test region
test_area<-st_read("spatial_data/derived/test_area.shp")

test_heath<- st_intersection(phi_full, test_area) #clip shapefile with test region

test_heath_250m<-rasterize (test_heath, r,getCover=TRUE,na.rm=TRUE) #convert to 250m-resolution raster with the extent of the boundaries. The value is the fraction of the 250m cell that is covered by the polygons.The fraction is estimated by dividing each cell into 100 subcells (25m) and determining presence/absence of the polygon in the center of each subcell

crs(test_heath_250m)<-"EPSG:27700" #assign spatial reference British National Grid

writeRaster(test_heath_250m,"spatial_data/test_heath_250m.tif") #save raster for local scale analyses

test_heath_1km<- aggregate(test_heath_250m, fact = 4, fun= sum, na.rm=TRUE)/16 #rescale raster to 1km-resolution where raster value equals to the sum of 250m-pixels divided by 16 (no of pixes within 

crs(test_heath_1km)<-"EPSG:27700" #assign spatial reference British National Grid
writeRaster(test_heath_1km,"spatial_data/test_heath_1km.tif") 









