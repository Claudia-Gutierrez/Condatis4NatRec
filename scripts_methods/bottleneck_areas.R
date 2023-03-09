#Generate ranked (score sum) buffers of major and severe bottlenecks for individual directions and all bottlenecks (combined)in the landscape 

library(sf)
library(tidyverse)

# National scale ----------------------------------------------------------

folder<- 'spatial_data/derived'
scale<-'national'
dispt<-'3k'

for(habitat in c("heathland","grassland","wetland")){
  for(direction in c("NWSE","NESW","NS","EW")){
    
    filename<- paste('Nat_1k', habitat, dispt, direction,sep='_')
    #read bottlenecks
    bottlenecks<- st_read(file.path(folder,scale,habitat,dispt,paste0(filename,'_bottlenecks.shp')))
    
    score_major= c(5,50) #interval of score of bottlenecks considered major
    score_severe= 50 # >score considered severe bottlenecks
    path= file.path(folder,scale,habitat,dispt) #

    bottleneck_area(bottlenecks, score_major, score_severe, path, filename)
    
  }
}

# Local scale ----------------------------------------------------------

folder<- 'spatial_data/derived'
scale<-'local'

for(k in 1:3){ 
  for(j in 1:4){
    for(i in 1:2){
      habitat <- c("heathland","grassland","wetland")[k]
      area <- c("Surrey","SussexKent","LostWetlands")[k]
      direction <- c("NWSE","NESW","NS","EW") [j]
      dispt<-c('1k','3k')[i]
      
      filename<- paste('Loc_250m', habitat, dispt, direction,sep='_')
      
      err<-try({
        #read bottlenecks
        bottlenecks<- st_read(file.path(folder,scale,area,dispt,paste0(filename,'_bottlenecks.shp')))
      })
      
      if(!inherits(err,"try-error")){
      
      score_major= c(5,50) #interval of score of bottlenecks considered major
      score_severe= 50 # >score considered severe bottlenecks
      path= file.path(folder,scale, area,dispt) #
      
      try({bottleneck_area(bottlenecks, score_major, score_severe, path, filename)})
      }
   }
  }
}
    

# Combined national scale ----------------------------------------------------------

#Combine all bottlenecks into a single shapefile
folder<- 'spatial_data/derived'
scale<-'national'
dispt<-'3k'

for(habitat in c("heathland",'grassland', 'wetland')){
  filename<- paste('Nat_1k', habitat, dispt, sep='_')
  path= file.path(folder,scale,habitat,dispt)
  
  bnEW<-st_read(file.path(folder, scale, habitat, dispt, paste0(filename,'_EW_bottlenecks.shp')))
  bnNS<-st_read(file.path(folder, scale, habitat, dispt, paste0(filename,'_NS_bottlenecks.shp')))
  bnNESW<-st_read(file.path(folder, scale, habitat, dispt, paste0(filename,'_NESW_bottlenecks.shp')))
  bnNWSE<-st_read(file.path(folder, scale, habitat, dispt, paste0(filename,'_NWSE_bottlenecks.shp')))
  
  bnEW$direction<-'EW'
  bnNS$direction<-'NS'
  bnNESW$direction<-'NESW'
  bnNWSE$direction<-'NWSE'
  
  all_bn<- rbind(bnEW,bnNS,bnNESW, bnNWSE)
  
  st_write(all_bn, file.path(folder, scale, habitat, dispt, paste0(filename,'_all_bottlenecks.shp')), append = FALSE)
}


# Create bottleneck areas

score_major= c(5,50) #interval of score of bottlenecks considered major
score_severe= 50 # >score considered severe bottlenecks

## wetland 
habitat<-'wetland'
filename<-'Nat_1k_wetland_3k_'
path= file.path(folder,scale,habitat,dispt)

bottlenecks<-st_read(file.path(folder, scale, habitat, dispt, paste0(filename,'all_bottlenecks.shp')))

bottleneck_area(bottlenecks, score_major, score_severe, path, filename)


## grassland
habitat<-'grassland'
filename<-'Nat_1k_grassland_3k_'
path= file.path(folder,scale,habitat,dispt)

bottlenecks<-st_read(file.path(folder, scale, habitat, dispt, paste0(filename,'all_bottlenecks.shp')))

bottleneck_area(bottlenecks, score_major, score_severe, path, filename)

## heathland
habitat<-'heathland'
filename<-'Nat_1k_heathland_3k_'
path= file.path(folder,scale,habitat,dispt)

bottlenecks<-st_read(file.path(folder, scale, habitat, dispt, paste0(filename,'all_bottlenecks.shp')))

bottleneck_area(bottlenecks, score_major, score_severe, path, filename)



# Combined local scale --------------

#Combine all bottlenecks into a single shapefile
folder<- 'spatial_data/derived'
scale<-'local'
dispt<-'1k'

for(habitat in c("heathland",'grassland', 'wetland')){
  for(area in c("Surrey","SussexKent","LostWetlands")){
    
    filename<- paste('Loc_250m', habitat, dispt, sep='_')
  
  err<-try({
    #read bottlenecks
    bnEW<-st_read(file.path(folder, scale, area, dispt, paste0(filename,'_EW_bottlenecks.shp')))
    bnNS<-st_read(file.path(folder, scale, area, dispt, paste0(filename,'_NS_bottlenecks.shp')))
    bnNESW<-st_read(file.path(folder, scale, area, dispt, paste0(filename,'_NESW_bottlenecks.shp')))
    bnNWSE<-st_read(file.path(folder, scale, area, dispt, paste0(filename,'_NWSE_bottlenecks.shp')))
  
  })
  
  if(!inherits(err,"try-error")){
    
    bnEW$direction<-'EW'
    bnNS$direction<-'NS'
    bnNESW$direction<-'NESW'
    bnNWSE$direction<-'NWSE'
    
    all_bn<- rbind(bnEW,bnNS,bnNESW, bnNWSE)
    
    st_write(all_bn, file.path(folder, scale, area, dispt, paste0(filename,'_all_bottlenecks.shp')), append = FALSE)
  }
 }
}

## Surrey
habitat<-'heathland'
area <- "Surrey"
filename<-'Loc_250m_heathland_1k_'
path= file.path(folder,scale, area,dispt)

bottlenecks<- st_read(file.path(folder, scale, area, dispt, paste0(filename,'all_bottlenecks.shp')))

bottleneck_area(bottlenecks, score_major, score_severe, path, filename)


## Lost wetlands
habitat<-'wetland'
area <- "LostWetlands"
filename<-'Loc_250m_wetland_1k_'
path= file.path(folder,scale, area,dispt)

bottlenecks<- st_read(file.path(folder, scale, area, dispt, paste0(filename,'all_bottlenecks.shp')))

bottleneck_area(bottlenecks, score_major, score_severe, path, filename)


## Sussex Kent
habitat<-'grassland'
area <- "SussexKent"
filename<-'Loc_250m_grassland_1k_'
path= file.path(folder,scale, area,dispt)


bottlenecks<- st_read(file.path(folder, scale, area, dispt, paste0(filename,'all_bottlenecks.shp')))

bottleneck_area(bottlenecks, score_major, score_severe, path, filename)
