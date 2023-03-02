
#Calculate bottleneck scores

library(sf)

# National scale ----------------------------------------------------------

folder<- 'spatial_data/derived'
scale<-'national'
dispt<-'3k'

for(habitat in c("heathland","grassland","wetland")){
  for(direction in c("NWSE","NESW","NS","EW")){
    
    filename<- paste('Nat_1k', habitat, dispt, direction,sep='_')
    #read bottlenecks
    bottlenecks<- st_read(file.path(folder,scale,habitat,dispt,paste0(filename,'_bottlenecks.shp')))
    
    #assign British National Grid spatial reference
    bottlenecks<-st_transform(bottlenecks, st_crs('EPSG:27700'))
    
    #read Condatis flow output file to obtain its number of patches (cells) , i.e. dimension
    flow<-read.csv(file.path(folder,scale,habitat,dispt, paste0(filename,'_flow.csv')),row.names=1)
    
    patchcount<-dim(flow)[1]
    #calculate score and add to shapefile and rewrite
    bottlenecks$score<-NA
    bottlenecks$score<-(bottlenecks$perc*patchcount/100)
    
    st_write(bottlenecks,file.path(folder,scale,habitat,dispt,paste0(filename,'_bottlenecks.shp')), append = FALSE)
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
      #assign British National Grid spatial reference
      bottlenecks<-st_transform(bottlenecks, st_crs('EPSG:27700'))
        
      #read Condatis flow output file to obtain its number of patches (cells) , i.e. dimension
      flow<-read.csv(file.path(folder,scale,area,dispt, paste0(filename,'_flow.csv')),row.names=1)
        
      patchcount<-dim(flow)[1]
      #calculate score and add to shapefile and rewrite
      bottlenecks$score<-NA
      bottlenecks$score<-(bottlenecks$perc*patchcount/100)
        
      st_write(bottlenecks,file.path(folder,scale,area,dispt,paste0(filename,'_bottlenecks.shp')), append = FALSE) }
   }
  }
}
  
    

