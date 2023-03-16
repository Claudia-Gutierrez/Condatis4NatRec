###############
#
# Calculate percentage of areas 
#
##############


#Function to calculate proportion of areas

area_proportion<-function(total, area){
  library(sf)
  area_perc<-data.frame(total_km2=numeric(1),area_km2=numeric(1),percentage=numeric(1))
  area_perc$total_km2<-as.numeric(st_area(total)/1000000)
  area_perc$area_km2<-as.numeric(sum(st_area(area)/1000000))
  area_perc$percentage<-(area_perc$area_km2/area_perc$total_km2)*100
  return(area_perc)
}


# National scale ----------------------------------------------------------
# land cover percentage of bottleneck areas (major and sever)

folder<- 'spatial_data/derived'
scale<-'national'
dispt<-'3k'
habitat<-'grassland'
type<-'severe'

area_prop<-data.frame()
for(habitat in c("grassland","heathland","wetland")){
  for(type in c("major","severe")){
    
    filename<- paste('Nat_1k', habitat, dispt, sep='_')
    #read bottlenecks
    area<- st_read(file.path(folder,scale,habitat,dispt,paste0(filename,'__bottleneck_',type,'_area.shp')))
    
    #national border
    total<-st_read(paste0(folder,'/borders/Nat_border_BGN.shp'))
    
    area_clip<-st_intersection(area, total) #remove non-land area from bottleneck layers
    
    #calculate area percentage
    area<-area_proportion(total=total, area=area_clip)
    #layer ID
    area$layer<-paste0(filename,'__bottleneck_',type,'_area.shp')
    area_prop<-rbind(area_prop,area)
    
   }
}
  
write.csv(area_prop, paste0(file.path(folder,scale),'/tables/bottleneck_area_nat_perc.csv'))

#Proportion of combined networks
#severe
grass<-st_read('spatial_data/derived/national/grassland/3k/Nat_1k_grassland_3k__bottleneck_severe_area.shp')
heath<-st_read('spatial_data/derived/national/heathland/3k/Nat_1k_heathland_3k__bottleneck_severe_area.shp')
wetl<-st_read('spatial_data/derived/national/wetland/3k/Nat_1k_wetland_3k__bottleneck_severe_area.shp')

b_severe<- rbind(grass,heath,wetl)
b_severe<-st_union(b_severe)

total<-st_read('spatial_data/derived/borders/Nat_border_BGN.shp')

severe<-st_intersection(b_severe, total)#remove non-land area from combined bottleneck layers

combined_area_prop<- area_proportion(area=severe, total=total)
#  total_km2 area_km2 percentage
#  130834.1 9589.937   7.329844

#major
grass<-st_read('spatial_data/derived/national/grassland/3k/Nat_1k_grassland_3k__bottleneck_major_area.shp')
heath<-st_read('spatial_data/derived/national/heathland/3k/Nat_1k_heathland_3k__bottleneck_major_area.shp')
wetl<-st_read('spatial_data/derived/national/wetland/3k/Nat_1k_wetland_3k__bottleneck_major_area.shp')

b_major<- rbind(grass,heath,wetl)
b_major<-st_union(b_major)

total<-st_read('spatial_data/derived/borders/Nat_border_BGN.shp')

major<-st_intersection(b_major, total)#remove non-land area from combined bottleneck layers

combined_area_prop<- area_proportion(area=major, total=total)
#  total_km2 area_km2 percentage
#  130834.1 39894.53   30.49246

plot(major, col='blue')
plot(severe, col='red', add=TRUE)
plot(total$geometry, add=TRUE)


#Network expansion zones



