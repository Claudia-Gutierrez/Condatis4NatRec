library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)

#Condatis inputs (hab, R, disp, st)

hab<-raster("spatial_data/derived/Nat_1km_heathland.tif")
R<- 1000
disp<- 3.4

dsn<-'spatial_data/derived/' #path to output files location

filename<-'Nat_1k_heathland_3k_NESW_'
st<- raster("spatial_data/derived/st_nat_NE_SW.tif")

Nat_heathland_ConNESW<- Condatis_bottlenecks(hab=hab, st=stNESW,R=R,disp=disp, filename =filenameNESW, dsn=dsn, maxdisp=500, threshold=0.99, maxlink=10000, minlink=1000)

#--------

hab<-raster("spatial_data/derived/Nat_1km_wetland.tif")
R<- 1000
disp<- 3.4

dsn<-'spatial_data/derived/' #path to output files location

filename<-'Nat_1k_wetland_3k_NS_'
st<- raster("spatial_data/derived/st_nat_N_S.tif")

Nat_wetland_ConNESW<- Condatis_bottlenecks(hab=hab, st=stNESW,R=R,disp=disp, filename =filenameNESW, dsn=dsn, maxdisp=500, threshold=0.99, maxlink=10000, minlink=1000)



#------------

hab<-raster("spatial_data/derived/Nat_1km_grassland.tif")
R<- 1000
disp<- 1

dsn<-'spatial_data/derived/' #path to output files location

filenameNESW<-'Nat_1k_grassland_1k_N_S_'
stNESW<- raster("spatial_data/derived/st_nat_N_S.tif")

Nat_grassland_ConNESW<- Condatis_bottlenecks(hab=hab, st=stNESW,R=R,disp=disp, filename =filenameNESW, dsn=dsn, maxdisp=500, threshold=0.99, maxlink=10000, minlink=1000)