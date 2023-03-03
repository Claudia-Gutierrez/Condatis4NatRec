
library(sf)

#inputs
bottlenecks<-st_read('spatial_data/derived/national/heathland/3k/Nat_1k_heathland_3k_EW_bottlenecks.shp')
score_major<-c(5,50) #score of bottlenecks considered as a priority
score_severe<-50


#assign British National Grid spetial reference
bottlenecks<-st_transform(bottlenecks, st_crs('EPSG:27700'))

#subset major or severe bottlenecks 
b_major<-bottlenecks[bottlenecks$score>score_major[1] & bottlenecks$score<score_major[2],]
b_severe<-bottlenecks[bottlenecks$score>score_severe,]

b_m_point<-st_line_sample(b_major, sample=0.5)
b_m_point<-st_transform(b_m_point, st_crs('EPSG:27700'))
b_m_point<-cbind(b_m_point,b_major)



st_write(mid_point,'delete.shp', append = FALSE)