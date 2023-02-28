#This script divides the boundaries raster in four directions to run in Condatis (north-south, east-west, northeast-southwest, northwest-southeast) 
#source=1
#target=2

library (raster)

boundaries<-raster("spatial_data/derived/borders/Loc_lwetl8sects250km.tif")
crs(boundaries)
plot(boundaries, col= c("red","black", "pink", "orange", "green", "purple", "grey","cyan"))

#North to south
st_N_S<-boundaries
st_N_S[st_N_S %in% 1:2|st_N_S %in% 4:6|st_N_S %in% 8] <- NA
st_N_S[st_N_S%in% 7]<-1
st_N_S[st_N_S%in% 3]<-2
plot(st_N_S, col=c("red","black"))

writeRaster(st_N_S,"spatial_data/derived/st_loc_lwet_N_S.tif", overwrite=TRUE)

#East to West
st_E_W<-boundaries
st_E_W[st_E_W %in% 2:4|st_E_W %in% 6:8]<- NA
st_E_W[st_E_W %in% 5]<-2
st_E_W[st_E_W %in% 1]<-1
plot(st_E_W, col= c("red","black"))

writeRaster(st_E_W,"spatial_data/derived/st_loc_lwet_E_W.tif",overwrite=TRUE)


#Northwest to southeast
st_NW_SE<-boundaries
st_NW_SE[st_NW_SE %in% 1:3|st_NW_SE %in% 5:7]<-NA
st_NW_SE[st_NW_SE %in% 8]<-1
st_NW_SE[st_NW_SE %in% 4]<-2
plot(st_NW_SE, col= c("red","black"))

writeRaster(st_NW_SE,"spatial_data/derived/st_loc_lwet_NW_SE.tif",overwrite=TRUE)


#Northeast to southwest
st_NE_SW<-boundaries
st_NE_SW[st_NE_SW %in% 1|st_NE_SW %in% 3:5|st_NE_SW %in% 7:8]<-NA
st_NE_SW[st_NE_SW %in% 6]<-1
st_NE_SW[st_NE_SW %in% 2]<-2
plot(st_NE_SW, col= c("red","black"))

writeRaster(st_NE_SW,"spatial_data/derived/st_loc_lwet_NE_SW.tif",overwrite=TRUE)


