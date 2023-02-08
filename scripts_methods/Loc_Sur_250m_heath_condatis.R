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


#clip national heathland (Nat_250m_heathland)to Surrey project
surrey<-st_read('spatial_data/original/SurreyHeathlands/Heathlands Connections with Buffer_1.shp')
Nat_heathland<- raster('spatial_data/derived/Nat_250m_heathland.tif')
surrey_heathland<-crop(Nat_heathland,surrey)
surrey_heath_proj<-mask(surrey_heathland,surrey)
plot(surrey_heath_proj)


#Condatis inputs (hab, R, disp, st)

hab<-surrey_heath_proj
R<- 1000
disp<- 3.4

dsn<-'spatial_data/derived/' #path to output files location


#Run Condatis_bottlenecks North-South
filenameNS<-'Loc_Sur_250m_heathland_3k_NS_' #scale_resolution_habitat-type_dispersal-distance_direction
stNS<- raster("spatial_data/derived/st_loc_sur_N_S.tif")
Loc_heathland_ConNS<- Condatis_bottlenecks_outputs_local(hab=hab, st=stNS,R=R,disp=disp, filename =filenameNS, dsn=dsn )


#Run Condatis_bottlenecks East-West
filenameEW<-'Loc_Sur_250m_heathland_3k_EW_'
stEW<- raster("spatial_data/derived/st_loc_sur_E_W.tif")

Loc_heathland_ConEW<- Condatis_bottlenecks_outputs_local(hab=hab, st=stEW,R=R,disp=disp, filename =filenameEW, dsn=dsn)


#Run Condatis_bottlenecks Northeast-Southwest
filenameNESW<-'Loc_Sur_250m_heathland_3k_NESW_'
stNESW<- raster("spatial_data/derived/st_loc_sur_NE_SW.tif")

Loc_heathland_ConNESW<- Condatis_bottlenecks_outputs_local(hab=hab, st=stNESW,R=R,disp=disp, filename =filenameNESW, dsn=dsn)

#Run Condatis_bottlenecks Northwest-Southeast
filenameNWSE<-'Loc_Sur_250m_heathland_3k_NWSE_'
stNWSE<- raster("spatial_data/derived/st_loc_sur_NW_SE.tif")


Loc_heathland_ConNWSE<- Condatis_bottlenecks_outputs_local(hab=hab, st=stNWSE,R=R,disp=disp, filename =filenameNWSE, dsn=dsn)

