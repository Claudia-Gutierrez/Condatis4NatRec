###############
#
# Calculate habitat proportion
#
##############

habitat_propotion<-function(total, habitat){
  library(raster)
  library(sf)
  area_total_km2<-st_area(total)/1000000
  area_hab_km2<-sum(st_area(habitat)/1000000)
  habitat_prop<-(area_hab_km2/area_total_km2)*100
  print(habitat_prop)
  return(habitat_prop)
}


# National ----------------------------------------------------------------

library(raster)
library(sf)

total<- st_read('spatial_data/derived/borders/Nat_border_BGN.shp')
habitat<- st_read('spatial_data/derived/PHI_Nat_heath.shp')

heathland<-habitat_propotion(total, habitat)
#4.488404 [1]

habitat<- st_read('spatial_data/derived/PHI_Nat_grass.shp')
grassland<-habitat_propotion(total, habitat)
#0.9283041 [1]

habitat<- st_read('spatial_data/derived/PHI_Nat_wetl.shp')
wetland<-habitat_propotion(total, habitat)
#2.260031 [1]


# Local ----------------------------------------------------------------

#Surrey

habitat<-st_read('spatial_data/derived/PHI_Nat_heath.shp')
local<-st_read('spatial_data/original/SurreyHeathlands/Heathlands Connections with Buffer_1.shp')
habitat_loc<- st_intersection(habitat, local)

heathland_loc<-habitat_propotion(total=local, habitat=habitat_loc)
#7.520501 [1]


#Sussex_Kent

habitat<-st_read('spatial_data/derived/PHI_Nat_grass.shp')
local<-st_read('spatial_data/original/SussexKent/NRP_LandOnly/NRP_LandOnly.shp')
habitat_loc<- st_intersection(habitat, local)

grassland_loc<-habitat_propotion(total=local, habitat=habitat_loc)
#14.82099 [1]

#Lost wetlands
habitat<-st_read('spatial_data/derived/PHI_Nat_wetl.shp')
local<-st_read('spatial_data/original/Lost_Wetlands_Boundary/Lost_Wetlands_Boundary.shp')
habitat_loc<- st_intersection(habitat, local)

wetland_loc<-habitat_propotion(total=local, habitat=habitat_loc)
#4.336189 [1]
