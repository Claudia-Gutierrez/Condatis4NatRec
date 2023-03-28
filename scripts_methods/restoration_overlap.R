###############
#
# Overlap restoration opportunities
#
##############

library(sf)
library(tydiverse)

#Surrey heathlands local bottlenecks-------------------

project<-st_read('spatial_data/original/SurreyHeathlands/Heathlands Connections with Buffer_1.shp')

major_bn<-st_read('spatial_data/derived/local/Surrey/1k/Loc_250m_heathland_1k__bottleneck_major_area.shp')%>%
  st_intersection(project) 
plot(major_bn)

nh_heath<-st_read('spatial_data/derived/national/NH_heath.shp')%>%
  st_intersection(project)%>%
  subset(Class== 'Network Expansion Zone'|Class=='Network Enhancement Zone 1'|Class=='Fragmentation Action Zone')%>%
  st_union()
plot(nh_heath)

nh_heath_area<-as.numeric(sum(st_area(nh_heath)/1000000))
#185.6033

nh_bn_overlap<-st_intersection(nh_heath, major_bn)
plot(nh_bn_overlap)

nh_bn_area<-as.numeric(sum(st_area(nh_bn_overlap)/1000000))
# 16.00606 km^2


#Surrey heathlands national EW bottlenecks-------------------

severe_bn<-st_read('spatial_data/derived/national/heathland/3k/Nat_1k_heathland_3k__bottleneck_severe_area.shp')%>%
  subset(line_count==7)

major_bn<-st_read('spatial_data/derived/national/heathland/3k/Nat_1k_heathland_3k__bottleneck_major_area.shp')%>%
  subset(line_count==33)
  
plot(major_bn$geometry)
plot(severe_bn$geometry, add=TRUE)

nh_heath<-st_read('spatial_data/derived/national/NH_heath.shp')%>%
  st_intersection(major_bn)%>%
  subset(Class== 'Network Expansion Zone'|Class=='Network Enhancement Zone 1'|Class=='Fragmentation Action Zone')%>%
  st_union()
plot(nh_heath, col='gray', add = TRUE)

nh_major_area<-as.numeric(sum(st_area(nh_heath)/1000000))
# 116.0854 km^2

nh_heath<-st_read('spatial_data/derived/national/NH_heath.shp')%>%
  st_intersection(severe_bn)%>%
  subset(Class== 'Network Expansion Zone'|Class=='Network Enhancement Zone 1'|Class=='Fragmentation Action Zone')%>%
  st_union()
plot(nh_heath, col='blue', add = TRUE)

nh_severe_area<-as.numeric(sum(st_area(nh_heath)/1000000))
# 35.14893 km^2

#Lost Wetlands---------------------------
project<-st_read('spatial_data/original/Lost_Wetlands_Boundary/Lost_Wetlands_Boundary_proj.shp')

major_bn<-st_read('spatial_data/derived/local/LostWetlands/1k/Loc_250m_wetland_1k__bottleneck_major_area.shp')%>%
  st_intersection(project) 
plot(major_bn)

wet_creation<-st_read('spatial_data/original/Lost_Wetlands_Wetland_Creation_Zones/Lost_Wetlands_Wetland_Creation_Zones.shp')%>%
  st_intersection(project)%>%
  st_union()
plot(wet_creation)

wet_creat_bn_overlap<-st_intersection(wet_creation, major_bn)
plot(nh_bn_overlap)

wet_creat_bn_area<-as.numeric(sum(st_area(wet_creat_bn_overlap)/1000000))
# 4.946264 km^2


