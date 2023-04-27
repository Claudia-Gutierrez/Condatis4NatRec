###############
#
# Score stats
#
##############

library(sf)

folder<-"spatial_data/derived" 

# National ------------------------
scale<-"national"
dispt<-"3k"

scores<-data.frame()
for(habitat in c("grassland","heathland","wetland")){
 
    filename<-paste("Nat_1k",habitat,dispt,sep="_")
    
      bottlenecks<-st_read(file.path(folder,scale,habitat,dispt,paste0(filename,"_all_bottlenecks.shp")))
      
      bottlenecks<-bottlenecks[bottlenecks$score>=1,]
      hist(bottlenecks$score)
      
      bottlenecks$habitat<- paste0(habitat)
      
      scores<-rbind(scores, bottlenecks)
     }


total<-nrow(scores [scores$habitat =='wetland',])
total
# 8507,787, 4504
minor<-nrow(scores[scores$score > 1 & scores$score <=5 & scores$habitat =='wetland',])
minor
# 6678, 411, 3083
major<-nrow(scores[scores$score > 5 & scores$score <=50 & scores$habitat =='wetland',])
major
# 1735, 264, 1231
severe<-nrow(scores[scores$score > 50& scores$habitat =='wetland',])
severe
# 94, 112, 190

minor_perc<- (minor/total)*100
minor_perc
#78.5, 52.2, 68.4
major_perc<- (major/total)*100
major_perc
#20.4, 33.6, 27.3
severe_perc<-(severe/total)*100
severe_perc
#1.1, 14.2, 4.3

minor_perc+major_perc+severe_perc




# Local ------------------------
scale<-'local'

scores_loc<-data.frame()
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
        
        try({
        bottlenecks<-bottlenecks[bottlenecks$score>=1,]
        bottlenecks$habitat<- paste0(habitat)
        bottlenecks$dispt<- paste0(dispt)
        
        scores_loc<-rbind(scores_loc, bottlenecks)})
      }
    }
  }
}


total<-nrow(scores_loc [scores_loc$habitat =='grassland' & scores_loc$dispt=='1k',])
total
# ,737 ,105
minor<-nrow(scores_loc[scores_loc$score > 1 & scores_loc$score <=5 & scores_loc$habitat =='grassland'& scores_loc$dispt=='1k',])
minor
# ,695 ,82
major<-nrow(scores_loc[scores_loc$score > 5 & scores_loc$score <=50 & scores_loc$habitat =='grassland'& scores_loc$dispt=='1k',])
major
# ,42 ,23 
severe<-nrow(scores_loc[scores_loc$score > 50& scores_loc$habitat =='grassland'& scores_loc$dispt=='1k',])
severe
# ,0 ,94 

minor_perc<- (minor/total)*100
minor_perc
#100, 94.3, 78.1
major_perc<- (major/total)*100
major_perc
#0, 5.7, 21.9
severe_perc<-(severe/total)*100
severe_perc
#0, 0, 0

minor_perc+major_perc+severe_perc


