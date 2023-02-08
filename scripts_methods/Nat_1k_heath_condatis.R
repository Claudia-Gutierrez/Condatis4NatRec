################################################
#                                              #
#           Condatis heathland                 #
#                                              #
################################################

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


#Run Condatis_bottlenecks North-South
filenameNS<-'Nat_1k_heathland_3k_NS_' #scale_resolution_habitat-type_dispersal-distance_direction
stNS<- raster("spatial_data/derived/st_nat_N_S.tif")

Nat_heathland_ConNS<- Condatis_bottlenecks_outputs(hab=hab, st=stNS,R=R,disp=disp, filename =filenameNS, dsn=dsn )


#Run Condatis_bottlenecks East-West
filenameEW<-'Nat_1k_heathland_3k_EW_'
stEW<- raster("spatial_data/derived/st_nat_E_W.tif")

Nat_heathland_ConEW<- Condatis_bottlenecks_outputs(hab=hab, st=stEW,R=R,disp=disp, filename =filenameEW, dsn=dsn)


#Run Condatis_bottlenecks Northeast-Southwest
filenameNESW<-'Nat_1k_heathland_3k_NESW_'
stNESW<- raster("spatial_data/derived/st_nat_NE_SW.tif")

Nat_heathland_ConNESW<- Condatis_bottlenecks_outputs(hab=hab, st=stNESW,R=R,disp=disp, filename =filenameNESW, dsn=dsn)

#Run Condatis_bottlenecks Northwest-Southeast
filenameNWSE<-'Nat_1k_heathland_3k_NWSE_'
stNWSE<- raster("spatial_data/derived/st_nat_NW_SE.tif")

Nat_heathland_ConNWSE<- Condatis_bottlenecks_outputs(hab=hab, st=stNWSE,R=R,disp=disp, filename =filenameNWSE, dsn=dsn)






