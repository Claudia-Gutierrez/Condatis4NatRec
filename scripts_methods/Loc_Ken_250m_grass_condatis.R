################################################
#                                              #
#         Condatis heathland Surrey            #
#                                              #
################################################

library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)


#clip national grassland (Nat_250m_grassland)to Surrey-Kent project adjusted extent
kent<-raster('spatial_data/derived/local/SussexKent/st_loc_ken_N_S.tif')
Nat_grassland<- raster('spatial_data/derived/national/Nat_250m_grassland.tif')
kent_pol<- st_read('spatial_data/original/SussexKent/NRP_SussexKent/NRP_SussexKent.shp')
kent_grassland<-crop(Nat_grassland,kent)
kent_grass_proj<-mask(kent_grassland,kent_pol)
plot(kent_grass_proj)


#Condatis inputs (hab, R, disp, st)

hab<-kent_grass_proj
R<- 1000
disp<- 3.4

dsn<-'spatial_data/derived/local/SussexKent/' #path to output files location


#Run Condatis_bottlenecks North-South
filenameNS<-'Loc_250m_grassland_3k_NS_' #scale_resolution_habitat-type_dispersal-distance_direction
stNS<- raster("spatial_data/derived/local/SussexKent/st_loc_ken_N_S.tif")
Loc_heathland_ConNS<- Condatis_bottlenecks(hab=hab, st=stNS,R=R,disp=disp, filename =filenameNS, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)


#Run Condatis_bottlenecks East-West
filenameEW<-'Loc_250m_grassland_3k_EW_'
stEW<- raster("spatial_data/derived/local/SussexKent/st_loc_ken_E_W.tif")

Loc_heathland_ConEW<- Condatis_bottlenecks(hab=hab, st=stEW,R=R,disp=disp, filename =filenameEW, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)


#Run Condatis_bottlenecks Northeast-Southwest
filenameNESW<-'Loc_250m_grassland_3k_NESW_'
stNESW<- raster("spatial_data/derived/local/SussexKent/st_loc_ken_NE_SW.tif")

Loc_heathland_ConNESW<- Condatis_bottlenecks(hab=hab, st=stNESW,R=R,disp=disp, filename =filenameNESW, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)

#Run Condatis_bottlenecks Northwest-Southeast
filenameNWSE<-'Loc_250m_grassland_3k_NWSE_'
stNWSE<- raster("spatial_data/derived/local/SussexKent/st_loc_ken_NW_SE.tif")

Loc_heathland_ConNWSE<- Condatis_bottlenecks(hab=hab, st=stNWSE,R=R,disp=disp, filename =filenameNWSE, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)

