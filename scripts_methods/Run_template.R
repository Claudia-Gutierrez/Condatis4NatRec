################################################
#                                              #
#           Condatis run template              #
#                                              #
################################################

library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(maptools)

#Condatis inputs (hab, R, disp, st, filename)

hab<-raster("spatial_data/derived/Nat_1km_heathland.tif")
R<- 1000
disp<- 3.4

#Direction North-South
stNS<- raster("spatial_data/derived/st_N_S.tif")

######################
#Output file nomenclature (with underscores instead of spaces):
# 1.Scale (Nat or Loc)
# 2.habitat resolution (1k or 250m #TBC)
# 3.habitat type (heathland...#TBC)
# 4.dispersal distance (1k or 3k [3.4k] #TBC)
# 5.Direction (NS or EW or NWSE or NESW)
#Condatis output type will be appended at the end
######################
filename<-'Nat_1k_heathland_3k_NS_t_'

#assign output directory
dsn<-'spatial_data/derived'

#Run Condatis with outputs
Nat_heathland_ConNS<- Condatis_bottlenecks_output(hab=hab, st=stNS,R=R,disp=disp, filename = filename, dsn=dsn)



