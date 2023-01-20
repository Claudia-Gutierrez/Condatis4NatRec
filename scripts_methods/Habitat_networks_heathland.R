library (sf)

#Read original Priority Habitat Inventory England (phi) North
HN_L_heath<-st_read("spatial_data/original/Habitat_Networks_Lowland_Heathland/Habitat_Networks_(England)_-_Lowland_Heathland.shp")
HN_U_heath<-st_read("spatial_data/original/Habitat_Networks_Upland_Heathland/Habitat_Networks_(England)_-_Upland_Heathland.shp")

HN_heath<- rbind(HN_L_heath, HN_U_heath)
st_write(HN_heath,"spatial_data/derived", "NH_heath", driver= "ESRI Shapefile")
