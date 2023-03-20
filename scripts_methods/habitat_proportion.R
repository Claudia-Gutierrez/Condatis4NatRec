###############
#
# Calculate habitat proportion
#
##############
area_proportion<-function(total, area){
  library(sf)
  area_perc<-data.frame(total_km2=numeric(1),area_km2=numeric(1),percentage=numeric(1))
  area_perc$total_km2<-as.numeric(st_area(total)/1000000)
  area_perc$area_km2<-as.numeric(sum(st_area(area)/1000000))
  area_perc$percentage<-(area_perc$area_km2/area_perc$total_km2)*100
  return(area_perc)
}


# National ----------------------------------------------------------------
library(sf)

total<- st_read('spatial_data/derived/borders/Nat_border_BGN.shp')

habitat<- st_read('spatial_data/derived/national/PHI_Nat_heath.shp')
heathland<-area_proportion(total, area=habitat)
#4.488404 [1]

habitat<- st_read('spatial_data/derived/national/PHI_Nat_grass.shp')
grassland<-area_proportion(total, area=habitat)
#0.9283041 [1]

habitat<- st_read('spatial_data/derived/national/PHI_Nat_wetl.shp')
wetland<-area_proportion(total, area= habitat)
#2.260031 [1]

nat_hab<-rbind(grassland, heathland, wetland)
nat_hab$habitat<-c('grassland', 'heathland', 'wetland')

write.csv(nat_hab, 'spatial_data/derived/tables/nat_hab_perc.csv')

# Local ----------------------------------------------------------------

#Surrey

habitat<-st_read('spatial_data/derived/national/PHI_Nat_heath.shp')
local<-st_read('spatial_data/original/SurreyHeathlands/Heathlands Connections with Buffer_1.shp')
habitat_loc<- st_intersection(habitat, local)

heathland_loc<-area_proportion(total=local, area = habitat_loc)
#7.520501 [1]

#Sussex_Kent

habitat<-st_read('spatial_data/derived/national/PHI_Nat_grass.shp')
local<-st_read('spatial_data/original/SussexKent/NRP_LandOnly/NRP_LandOnly.shp')
habitat_loc<- st_intersection(habitat, local)

grassland_loc<-area_proportion(total=local, area = habitat_loc)
#14.82099 [1]

#Lost wetlands
habitat<-st_read('spatial_data/derived/national/PHI_Nat_wetl.shp')
local<-st_read('spatial_data/original/Lost_Wetlands_Boundary/Lost_Wetlands_Boundary_proj.shp')
habitat_loc<- st_intersection(habitat, local)

wetland_loc<-area_proportion(total=local, area= habitat_loc)
#0.4294269 [1]

loc_hab<-rbind(grassland_loc, heathland_loc, wetland_loc)
loc_hab$project<-c('Sussex-Kent','Surrey', 'Lost wetlands')

write.csv(loc_hab, 'spatial_data/derived/tables/loc_hab_perc.csv')
