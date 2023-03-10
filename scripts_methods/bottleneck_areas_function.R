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
  
  #assign British National Grid spatial reference
  bottlenecks<-st_transform(bottlenecks, st_crs('EPSG:27700'))
  
  #select major bottlenecks
  b_major<- filter(bottlenecks, score>score_major[1] & score<score_major[2])
  
  #create a point at the middle of bottleneck
  b_major_point<- st_line_sample(b_major, sample=0.5)
  
  #create a buffer
  b_major_buffer<- b_major_point%>%
    st_sf()%>%#allows to add columns
    mutate(m= filter(bottlenecks, score>score_major[1] & score<score_major[2]))%>%#add bottlenecks information
    mutate(buf_length=m$length/2)%>%#calculate  bottleneck mid-length 
    st_sf()
  
  b_major_buffer<-st_buffer(b_major_buffer, b_major_buffer$buf_length) #use mid-length as buffer distance
  
  #dissolve overlapping buffers into separate polygons (units)
  b_major_units<-b_major_buffer%>%
    st_union()%>%
    st_cast('POLYGON')%>%
    st_sf %>%
    mutate(
      unit = row_number()) #assigns each polygon an ID
 
  #add information of unit to  individual buffers
  b_m_score_sum<- st_join(b_major_units, b_major_buffer)%>%
    group_by(unit)%>%
    mutate(
      sumscore=sum(m$score),#calculate the sum of score in each unit
      line_count=NA) #create a field to record the number of buffers that form each unit
  
  #calculate the number of buffers that form each unit
  b_m_score_sum$line_count<-ave(as.numeric(b_m_score_sum[[1]]), b_m_score_sum[["unit"]], FUN=length)
  
  #remove all duplicated records, keeps only units, score sum and number of buffers per unit
  b_m_score_sum<- b_m_score_sum[!duplicated(b_m_score_sum$unit),]
  
  #remove unnecessary information
  b_m_score_sum<-subset(b_m_score_sum, select = -c(unit,buf_length))
  
  #save major bottleneck area shapefile
  st_write(b_m_score_sum, paste0(path,'/', filename,'_bottleneck_major_area.shp'), append=FALSE)
  
  
  #select severe bottlenecks
  b_severe<- filter(bottlenecks, score>score_severe)
  
  #create a point at the middle of bottleneck
  b_severe_point<- st_line_sample(b_severe, sample=0.5)
  
  #create a buffer
  b_severe_buffer<-b_severe_point%>%
    st_sf %>%#allows to add columns
    mutate(s= filter(bottlenecks, score>score_severe))%>%#add bottlenecks information
    mutate(buf_length=s$length/2)%>%
    st_sf()
  
  #calculate  bottleneck mid-length 
  b_severe_buffer<-st_buffer(b_severe_buffer, b_severe_buffer$buf_length) #use mid-length as buffer distance
  
  
  #identify overlapping buffers
  b_severe_units<-b_severe_buffer%>%
    st_union()%>% #merges all buffer in single feature
    st_cast('POLYGON')%>% #disaggregate into multiple polygons (i.e. overlapping buffers)
    st_sf %>%
    mutate(
      unit = row_number()) #assigns each polygon an ID
  
  #add information of unit to  individual buffers
  b_s_score_sum<- st_join(b_severe_units, b_severe_buffer)%>%
    group_by(unit)%>%
    mutate(
      sumscore=sum(s$score),#calculate the sum of score in each unit
      line_count=NA) #create a field to record the number of buffers that form each unit
  
  #calculate the number of buffers that form each unit
  b_s_score_sum$line_count<-ave(as.numeric(b_s_score_sum[[1]]), b_s_score_sum[["unit"]], FUN=length)
  
  #remove all duplicated records, keeps only units, score sum and number of buffers per unit
  b_s_score_sum<- b_s_score_sum[!duplicated(b_s_score_sum$unit),]
  
  #remove unnecessary information
  b_s_score_sum<-subset(b_s_score_sum, select = -c(unit,buf_length))
  
  #save shapefile
  st_write(b_s_score_sum, paste0(path,'/', filename,'_bottleneck_severe_area.shp'), append=FALSE)
}

 






