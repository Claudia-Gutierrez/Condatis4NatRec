##############################
#                            #
# Bottleneck area function  #
#                            #
##############################

#This function creates a buffer (distance = half of the length of bottleneck) around the middle point of a) major and b)severe bottlenecks, merges the overlapping features (buffers) and calculates the sum of their scores.     

#inputs
#bottlenecks = shapefile of bottleneck lines 
#score_major= c(5,50) #interval of score of bottlenecks considered major
#score_severe= 50 # >score considered severe bottlenecks
#path= output's folder path
#filename= output's name


bottleneck_area<-function(bottlenecks, score_major, score_severe, path,filename){
  
  library(sf)
  library(tidyverse)
  
  #assign British National Grid spetial reference
  bottlenecks<-st_transform(bottlenecks, st_crs('EPSG:27700'))
  
  b_major<- filter(bottlenecks, score>score_major[1] & score<score_major[2])%>%
    st_line_sample(sample=0.5)%>%
    st_buffer(bottlenecks$length/2)%>%
    st_sf %>%
    mutate(m= filter(bottlenecks, score>score_major[1] & score<score_major[2]))
  
  b_major_units<-b_major%>%
    st_union()%>%
    st_cast('POLYGON')%>%
    st_sf %>%
    mutate(
    unit = row_number())
  
  b_m_score_sum<- st_join(b_major_units, b_major)%>%
    group_by(unit)%>%
    mutate(
    sumscore=sum(m$score))%>%
    st_sf()
  
  st_write(b_m_score_sum, paste0(path,'/', filename,'_bottleneck_major_area.shp'), append=FALSE)
  
  b_severe<- filter(bottlenecks, score>score_severe)%>%
    st_line_sample(sample=0.5)%>%
    st_buffer(bottlenecks$length/2)%>%
    st_sf %>%
    mutate(s= filter(bottlenecks, score>score_severe))
  
  b_severe_units<-b_severe%>%
    st_union()%>%
    st_cast('POLYGON')%>%
    st_sf %>%
    mutate(
      unit = row_number())
  
  b_s_score_sum<- st_join(b_severe_units, b_severe)%>%
    group_by(unit)%>%
    mutate(
      sumscore=sum(s$score))%>%
    st_sf()
  
  st_write(b_s_score_sum, paste0(path, '/', filename,'_bottleneck_severe_area.shp'), append=FALSE)
}

 






