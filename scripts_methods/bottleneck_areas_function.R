
library(sf)

#read bottlenecks
bottlenecks<- st_read('spatial_data/derived/local/Surrey/3k/Loc_250m_heathland_3k_EW_bottlenecks.shp')

#assign British National Grid spetial reference
bottlenecks<-st_transform(bottlenecks, st_crs('EPSG:27700'))
st_crs(bottlenecks)
plot(bottlenecks)

mid_point<-st_line_sample(bottlenecks, sample=0.5)
mid_point<-st_transform(mid_point, st_crs('EPSG:27700'))

st_write(mid_point,'delete.shp', append = FALSE)