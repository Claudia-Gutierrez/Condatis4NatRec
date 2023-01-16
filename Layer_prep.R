library (sf)

#Read original Priority Habitat Inventory England (phi) North
N_phi<-st_read("spatial_data/Priority_Habitat_Inventory_England_1.shp")


#subset heathland habitats
table(N_phi$mainhabs) #habitat names
N_heath_shp<- N_phi[N_phi$mainhabs=='Upland heathland'|N_phi$mainhabs=='Lowland heathland'|N_phi$mainhabs=='Fragmented heath'|N_phi$mainhabs=='Lowland dry acid grassland'|N_phi$mainhabs=='Lowland fens'|N_phi$mainhabs=='Upland flushes fens and swamps',]

#Convert to raster with the characteristics of the England boundaries raster

plot(N_heath_shp$mainhabs)


  