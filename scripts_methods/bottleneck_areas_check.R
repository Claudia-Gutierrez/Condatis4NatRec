#Step by step test of the bottleneck_areas function

library(sf)
library(tidyverse)

#E.g. Lost Wetlands area 1k East-West bottlenecks-------

folder<- 'spatial_data/derived'
scale<-'local'
dispt<-'1k'
habitat<-'wetland'
area <- "LostWetlands"
direction <- "EW"

filename<- paste('Loc_250m', habitat, dispt, direction,sep='_')

bottlenecks<- st_read(file.path(folder,scale,area,dispt,paste0(filename,'_bottlenecks.shp')))
st_crs(bottlenecks)

score_major= c(5,50) #interval of score of bottlenecks considered major

path= file.path(folder) #test saved outside, in derived folder 

#select major bottlenecks
b_major<- filter(bottlenecks, score>score_major[1] & score<score_major[2])

#create a point at the middle of bottleneck
b_major_point<- st_line_sample(b_major, sample=0.5)

st_write(b_major_point, paste0(path,'/', filename,'_bottleneck_major_points.shp'), append=FALSE)

#create a buffer
b_major_buffer<- b_major_point%>%
  st_sf()%>%#allows to add columns
  mutate(m= filter(bottlenecks, score>score_major[1] & score<score_major[2]))%>%#add bottlenecks information
  mutate(buf_length=m$length/2)%>%#calculate  bottleneck mid-lengthcalculate  bottleneck mid-length 
  st_sf()
b_major_buffer<-st_buffer(b_major_buffer, b_major_buffer$buf_length) #use mid-length as buffer distance

st_write(b_major_buffer, paste0(path,'/', filename,'_bottleneck_major_buffer.shp'))

#dissolve overlapping buffers into separate polygons (units)
b_major_units<-b_major_buffer%>%
  st_union()%>%
  st_cast('POLYGON')%>%
  st_sf %>%
  mutate(
    unit = row_number()) #assigns each polygon an ID
st_write(b_major_units, paste0(path,'/', filename,'_bottleneck_major_units.shp'), append=FALSE)

#add information of unit to  individual buffers
b_m_score_sum<- st_join(b_major_units, b_major_buffer)%>%
  group_by(unit)%>%
  mutate(
    sumscore=sum(m$score),#calculate the sum of score in each unit
    line_count=NA)#create a field to record the number of buffers that form each unit

#calculate the number of buffers that form each unit    
b_m_score_sum$line_count<-ave(as.numeric(b_m_score_sum[[1]]), b_m_score_sum[["unit"]], FUN=length)
st_write(b_m_score_sum, paste0(path,'/', filename,'_bottleneck_major_units_sum.shp'), append=FALSE)
#remove all duplicated records, keeps only units, score sum and number of buffers per unit
b_m_score_sum<- b_m_score_sum[!duplicated(b_m_score_sum$unit),]

#remove unnecessary information
b_m_score_sum<-subset(b_m_score_sum, select = -c(unit,buf_length))

#save major bottleneck area shapefile
st_write(b_m_score_sum, paste0(path,'/', filename,'_bottleneck_major_area.shp'), append=FALSE)


#E.g. National wetlands 3k East-West bottlenecks-------

#select severe bottlenecks
bottlenecks<-st_read('spatial_data/derived/national/wetland/3k/Nat_1k_wetland_3k_EW_bottlenecks.shp')
score_severe= 50
filename<-'Nat_1k_wetland_3k_EW'
path<-'spatial_data/derived'

b_severe<- filter(bottlenecks, score>score_severe)

#create a point at the middle of bottleneck
b_severe_point<- st_line_sample(b_severe, sample=0.5)
st_write(b_severe_point, paste0(path,'/', filename,'_bottleneck_severe_points.shp'), append=FALSE)

#create a buffer
b_severe_buffer<-b_severe_point%>%
  st_sf %>%#allows to add columns
  mutate(s= filter(bottlenecks, score>score_severe))%>%#add bottlenecks information
  mutate(buf_length=s$length/2)%>%
  st_sf()

#calculate  bottleneck mid-length 
b_severe_buffer<-st_buffer(b_severe_buffer, b_severe_buffer$buf_length) #use mid-length as buffer distance

st_write(b_severe_buffer, paste0(path,'/', filename,'_bottleneck_severe_buffer.shp'), append=FALSE)

#identify overlapping buffers
b_severe_units<-b_severe_buffer%>%
  st_union()%>% #merges all buffer in single feature
  st_cast('POLYGON')%>% #disaggregate into multiple polygons (i.e. overlapping buffers)
  st_sf %>%
  mutate(
    unit = row_number()) #assigns each polygon an ID
st_write(b_severe_units, paste0(path,'/', filename,'_bottleneck_major_units.shp'), append=FALSE)

#add information of unit to  individual buffers
b_s_score_sum<- st_join(b_severe_units, b_severe_buffer)%>%
  group_by(unit)%>%
  mutate(
    sumscore=sum(s$score),#calculate the sum of score in each unit
    line_count=NA) #create a field to record the number of buffers that form each unit

#calculate the number of buffers that form each unit
b_s_score_sum$line_count<-ave(as.numeric(b_s_score_sum[[1]]), b_s_score_sum[["unit"]], FUN=length)
st_write(b_s_score_sum, paste0(path,'/', filename,'_bottleneck_severe_units_sum.shp'), append=FALSE)

#remove all duplicated records, keeps only units, score sum and number of buffers per unit
b_s_score_sum<- b_s_score_sum[!duplicated(b_s_score_sum$unit),]

#save shapefile
st_write(b_s_score_sum, paste0(path,'/', filename,'_bottleneck_severe_area.shp'), append=FALSE)




# Test combine all layers ------------


bnEW<-st_read('spatial_data/derived/national/wetland/3k/Nat_1k_wetland_3k_EW_bottlenecks.shp')
bnNS<-st_read('spatial_data/derived/national/wetland/3k/Nat_1k_wetland_3k_NS_bottlenecks.shp')
bnNESW<-st_read('spatial_data/derived/national/wetland/3k/Nat_1k_wetland_3k_NESW_bottlenecks.shp')
bnNWSE<-st_read('spatial_data/derived/national/wetland/3k/Nat_1k_wetland_3k_NWSE_bottlenecks.shp')

bnEW$direction<-'EW'
bnNS$direction<-'NS'
bnNESW$direction<-'NESW'
bnNWSE$direction<-'NWSE'

all_bn<- rbind(bnEW,bnNS,bnNESW, bnNWSE)

filename<-'Nat_1k_wetland_3k'

st_write(all_bn, paste0(folder,'/',filename, '_all_bottlenecks.shp'), append = FALSE)

bottlenecks<-all_bn



