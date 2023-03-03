library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)


#crop national wetlands (Nat_250m_wetland)to Lost Wetlands project extent
lwetl<-raster("spatial_data/derived/local/LostWetlands/st_loc_lwet_N_S.tif")
Nat_wetland<- raster("spatial_data/derived/national/Nat_250m_wetland.tif")
lwetl_pol<- st_read('spatial_data/original/Lost_Wetlands_Boundary/Lost_Wetlands_Boundary_proj.shp')
lwetl_wetland<-crop(Nat_wetland,lwetl)
lwetl_wetland_proj<-mask(lwetl_wetland,lwetl_pol)
plot(lwetl_wetland_proj)
#Condatis inputs (hab, R, disp, st)

hab<-lwetl_wetland_proj
R<- 1000
disp<- 3.4

dsn<-'spatial_data/derived/local/LostWetlands/' #path to output files location

#Run Condatis_bottlenecks North-South
filenameNS<-'Loc_250m_wetland_3k_NS_' #scale_resolution_habitat-type_dispersal-distance_direction
stNS<- raster("spatial_data/derived/local/LostWetlands/st_loc_lwet_N_S.tif")

Loc_wetland_ConNS<- Condatis_bottlenecks(hab=hab, st=stNS,R=R,disp=disp, filename =filenameNS, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)

#Run Condatis_bottlenecks East-West
filenameEW<-'Loc_250m_wetland_3k_EW_' #scale_resolution_habitat-type_dispersal-distance_direction
stEW<- raster("spatial_data/derived/local/LostWetlands/st_loc_lwet_E_W.tif")

Loc_wetland_ConEW<- Condatis_bottlenecks(hab=hab, st=stEW,R=R,disp=disp, filename =filenameEW, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)

#Run Condatis_bottlenecks Northeast-Southwest
filenameNESW<-'Loc_250m_wetland_3k_NESW_' #scale_resolution_habitat-type_dispersal-distance_direction
stNESW<- raster("spatial_data/derived/local/LostWetlands/st_loc_lwet_NE_SW.tif")

Loc_wetland_ConNESW<- Condatis_bottlenecks(hab=hab, st=stNESW,R=R,disp=disp, filename =filenameNESW, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)

#Run Condatis_bottlenecks Northwest-Southeast
filenameNWSE<-'Loc_250m_wetland_3k_NWSE_' #scale_resolution_habitat-type_dispersal-distance_direction
stNWSE<- raster("spatial_data/derived/local/LostWetlands/st_loc_lwet_NW_SE.tif")

Loc_wetland_ConNWSE<- Condatis_bottlenecks(hab=hab, st=stNWSE,R=R,disp=disp, filename =filenameNWSE, dsn=dsn, threshold=0.99, maxlink=10000, minlink=1000)



