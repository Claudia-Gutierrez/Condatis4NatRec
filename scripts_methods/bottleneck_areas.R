#Generate ranked (score sum) buffers of major and severe bottlenecks

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

#Sussex example, discuss thresholds

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
      
      score_major= c(1.6,2) #interval of score of bottlenecks considered major
      score_severe= 2 # >score considered severe bottlenecks
      path= file.path(folder,scale, area,dispt) #
      
      try({bottleneck_area(bottlenecks, score_major, score_severe, path, filename)})
      }
   }
  }
}
    






