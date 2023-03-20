###############
#
# Power statistics
#
##############

library(sf)
library(ggplot2)

folder<-"spatial_data/derived" 

# National ------------------------
scale<-"national"
dispt<-"3k"

power_nat<-data.frame()
for(habitat in c("grassland","heathland","wetland")){
 
    filename<-paste("Nat_1k",habitat,dispt,sep="_")
    
      bottlenecks<-st_read(file.path(folder,scale,habitat,dispt,paste0(filename,"_all_bottlenecks.shp")))
      
      bottlenecks<-bottlenecks[bottlenecks$score>=1,]
      hist(log10(bottlenecks$power))
      
      area<-st_read(paste0(folder,'/borders/Nat_border_BGN.shp'))
      area_km2<-as.numeric(st_area(area)/1000000)
      
      powerstats<-data.frame(scale=character(1),habitat=character(1),area_km2=numeric(1), min=numeric(1),max=numeric(1),mean=numeric(1),sum=numeric(1),pow_km2=numeric(1))
     
      powerstats$scale<-'national'
      powerstats$habitat<-paste0(habitat)
      powerstats$area_km2<- area_km2
      powerstats$min<-min(bottlenecks$power)
      powerstats$mean<-mean(bottlenecks$power)
      powerstats$max<- max(bottlenecks$power)
      powerstats$sum<-sum(bottlenecks$power)
      powerstats$pow_km2<-(sum(bottlenecks$power)/area_km2)
      
      power_nat<-rbind(power_nat, powerstats)
 
    }


#Local --------------
scale<-'local'
dispt<-'1k'

power_loc<-data.frame()
for(habitat in c('grassland',"heathland", 'wetland')){
  for(site in c("Surrey","SussexKent","LostWetlands")){
    for(area in c(
      'spatial_data/original/SussexKent/NRP_LandOnly/NRP_LandOnly.shp',
      'spatial_data/original/SurreyHeathlands/Heathlands Connections with Buffer_1.shp',
      'spatial_data/original/Lost_Wetlands_Boundary/Lost_Wetlands_Boundary_proj.shp')){
      
    filename<- paste('Loc_250m', habitat, dispt, sep='_')
    
    err<-try({
      #read bottlenecks
      bottlenecks<-st_read(file.path(folder, scale, site, dispt, paste0(filename,'_all_bottlenecks.shp')))
      
    })
    
    if(!inherits(err,"try-error")){
      
      bottlenecks<-bottlenecks[bottlenecks$score>=1,]
      hist(log10(bottlenecks$power))
      
      area<-st_read(area)
      area_km2<-as.numeric(st_area(area)/1000000)
      
      powerstats<-data.frame(scale=character(1),habitat=character(1),area_km2=numeric(1), min=numeric(1),max=numeric(1),mean=numeric(1),sum=numeric(1),pow_km2=numeric(1))
      
      powerstats$scale<-'local'
      powerstats$habitat<-paste0(habitat)
      powerstats$area_km2<- area_km2
      powerstats$min<-min(bottlenecks$power)
      powerstats$mean<-mean(bottlenecks$power)
      powerstats$max<- max(bottlenecks$power)
      powerstats$sum<-sum(bottlenecks$power)
      powerstats$pow_km2<-(sum(bottlenecks$power)/area_km2)
      
      power_loc<-rbind(power_loc, powerstats)
      
      }
    }
  }
}
    
power_loc<-power_loc[c(1,5,9),]

power_stats<-rbind(power_nat,power_loc)

write.csv(power_stats, 'spatial_data/derived/tables/power_stats.csv')



# Boxplots ------------------------

#National
scale<-"national"
dispt<-"3k"

all_power_nat<-data.frame()
for(habitat in c("grassland","heathland","wetland")){
  
  filename<-paste("Nat_1k",habitat,dispt,sep="_")
  
  bottlenecks<-st_read(file.path(folder,scale,habitat,dispt,paste0(filename,"_all_bottlenecks.shp")))
  
  bottlenecks<-bottlenecks[bottlenecks$score>=1,]
  
  powerstats<-data.frame(scale=character(length = dim(bottlenecks)[1]),habitat=character(length = dim(bottlenecks)[1]),power=numeric(length = dim(bottlenecks)[1]))
  
  powerstats$scale<-'national'
  powerstats$habitat<-paste0(habitat)
  powerstats$power<- bottlenecks$power
  
  all_power_nat<-rbind(all_power_nat, powerstats)
}

#Local 
scale<-'local'
dispt<-'1k'

all_power_loc<-data.frame()
for(habitat in c('grassland',"heathland", 'wetland')){
  for(site in c("Surrey","SussexKent","LostWetlands")){

      filename<- paste('Loc_250m', habitat, dispt, sep='_')
      
      err<-try({
        #read bottlenecks
        bottlenecks<-st_read(file.path(folder, scale, site, dispt, paste0(filename,'_all_bottlenecks.shp')))
        
      })
      
      if(!inherits(err,"try-error")){
        
        bottlenecks<-bottlenecks[bottlenecks$score>=1,]

        powerstats<-data.frame(scale=character(length = dim(bottlenecks)[1]),habitat=character(length = dim(bottlenecks)[1]),power=numeric(length = dim(bottlenecks)[1]))
        
        powerstats$scale<-'local'
        powerstats$habitat<-paste0(habitat)
        powerstats$power<- bottlenecks$power
        
        all_power_loc<-rbind(all_power_loc, powerstats)
        
      }
    }
  }

all_power<-rbind(all_power_nat,all_power_loc)


ggplot(aes(y = log10(power), x = habitat, fill = scale), data = all_power) + 
  geom_boxplot()



ggplot(aes(y = log10(pow_km2), x = habitat), data = power) + 
  geom_point(aes(colour = factor(scale)), size = 4)+
  ylab('log10(power/km^2)')

