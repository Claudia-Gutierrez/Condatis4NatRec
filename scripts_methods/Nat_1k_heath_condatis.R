################################################
#                                              #
#           Condatis heathland test            #
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
 


#Run Condatis_bottlenecks North-South
stNS<- raster("spatial_data/derived/st_N_S.tif")

Nat_heathland_ConNS<- Condatis_bottlenecks(hab=hab, st=stNS,R=R,disp=disp)

#save results 

######################
#Output files' nomenclature (with underscores instead of spaces):
#Scale (Nat or Loc)
#habitat resolution (1k or 250m #TBC)
#habitat type (heathland...#TBC)
#dispersal distance (1k or 3k [3.4k] #TBC)
#Direction (NS or EW or NWSE or NESW)
######################
filename<-'Nat_1k_heathland_3k_NS_'

write.csv(Nat_heathland_ConNS$flow, paste0('spatial_data/derived/',filename,'flow.csv'))
write.csv(Nat_heathland_ConNS$power, paste0('spatial_data/derived/',filename,'power.csv'))
writeRaster(Nat_heathland_ConNS$flow_raster,paste0('spatial_data/derived/',filename,'flow_raster.tif'),overwrite=TRUE)
writeRaster(Nat_heathland_ConNS$progress_raster,paste0('spatial_data/derived/',filename,'progress_raster.tif'),overwrite=TRUE)
st_write(Nat_heathland_ConNS$flow_shp, paste0('spatial_data/derived/',filename,'flow.shp'),append = FALSE)
writeOGR(Nat_heathland_ConNS$bottlenecks, dsn="spatial_data/derived" ,layer=paste0(filename,'bottlenecks'), driver="ESRI Shapefile",overwrite_layer=TRUE)


#Run Condatis_bottlenecks East-West
stEW<- raster("spatial_data/derived/st_E_W.tif")

Nat_heathland_ConEW<- Condatis_bottlenecks(hab=hab, st=stEW,R=R,disp=disp)

#save results 
filename<-'Nat_1k_heathland_3k_EW_'

write.csv(Nat_heathland_ConEW$flow, paste0('spatial_data/derived/',filename,'flow.csv'))
write.csv(Nat_heathland_ConEW$power, paste0('spatial_data/derived/',filename,'power.csv'))
writeRaster(Nat_heathland_ConEW$flow_raster,paste0('spatial_data/derived/',filename,'flow_raster.tif'),overwrite=TRUE)
writeRaster(Nat_heathland_ConEW$progress_raster,paste0('spatial_data/derived/',filename,'progress_raster.tif'),overwrite=TRUE)
st_write(Nat_heathland_ConEW$flow_shp, paste0('spatial_data/derived/',filename,'flow.shp'), append = FALSE)
writeOGR(Nat_heathland_ConEW$bottlenecks, dsn="spatial_data/derived" ,layer=paste0(filename,'bottlenecks'), driver="ESRI Shapefile",overwrite_layer=TRUE)


#Run Condatis_bottlenecks Northeast-Southwest
stNESW<- raster("spatial_data/derived/st_NE_SW.tif")



Nat_heathland_ConNESW<- Condatis_bottlenecks(hab=hab, st=stNESW,R=R,disp=disp)

#save results 
filename<-'Nat_1k_heathland_3k_NESW_'

write.csv(Nat_heathland_ConNESW$flow, paste0('spatial_data/derived/',filename,'flow.csv'))
write.csv(Nat_heathland_ConNESW$power, paste0('spatial_data/derived/',filename,'power.csv'))
writeRaster(Nat_heathland_ConNESW$flow_raster,paste0('spatial_data/derived/',filename,'flow_raster.tif'),overwrite=TRUE)
writeRaster(Nat_heathland_ConNESW$progress_raster,paste0('spatial_data/derived/',filename,'progress_raster.tif'),overwrite=TRUE)
st_write(Nat_heathland_ConNESW$flow_shp, paste0('spatial_data/derived/',filename,'flow.shp'), append = FALSE)
writeOGR(Nat_heathland_ConNESW$bottlenecks, dsn="spatial_data/derived" ,layer=paste0(filename,'bottlenecks'), driver="ESRI Shapefile",overwrite_layer=TRUE)


#Run Condatis_bottlenecks Northwest-Southeast
stNWSE<- raster("spatial_data/derived/st_NW_SE.tif")

Nat_heathland_ConNWSE<- Condatis_bottlenecks(hab=hab, st=stNWSE,R=R,disp=disp)

#save results 
filename<-'Nat_1k_heathland_3k_NWSE_'

write.csv(Nat_heathland_ConNWSE$flow, paste0('spatial_data/derived/',filename,'flow.csv'))
write.csv(Nat_heathland_ConNWSE$power, paste0('spatial_data/derived/',filename,'power.csv'))
writeRaster(Nat_heathland_ConNWSE$flow_raster,paste0('spatial_data/derived/',filename,'flow_raster.tif'),overwrite=TRUE)
writeRaster(Nat_heathland_ConNWSE$progress_raster,paste0('spatial_data/derived/',filename,'progress_raster.tif'),overwrite=TRUE)
st_write(Nat_heathland_ConNWSE$flow_shp, paste0('spatial_data/derived/',filename,'flow.shp'), append = FALSE)
writeOGR(Nat_heathland_ConNWSE$bottlenecks, dsn="spatial_data/derived" ,layer=paste0(filename,'bottlenecks'), driver="ESRI Shapefile",overwrite_layer=TRUE)




