##########################
#                        #
#     Local boundary     #
#                        #
##########################
#Author: Jenny Hodgson (modified by Claudia Gutierrez from 'england-boundary' script)
#February 2023

#This script divides a polygon in eight sections based on the extreme extent points (most northern, southern, eastern and western points) and tangents to obtain the N, NE, NW, E, W, SE, SW sections of the polygon
#Requires functions: 'localextent' and 'condatis_boundaries' in 'local_extent_functions.R'

library(raster)
library(igraph)
library(sf)
library(rgeos)

# Surrey ------------------------------------------------------------------

#Polygon of local projects, e.g. Surrey
local<-st_read('spatial_data/original/SurreyHeathlands/Heathlands Connections with Buffer_1.shp')
local_bound<- st_boundary(local) #polygon boundary

local_bound_buf<-st_buffer(local_bound, 250, singleSide = TRUE) #create a buffer outside the polygon, size of the desired resolution 

plot(local_bound_buf)
st_write(local_bound_buf, 'delete2.shp', append=FALSE)

#template to rasterize the boundary
r<-raster(extent(local_bound_buf), res=250) #resolution equal to habitat layer
crs(r)<-crs(local_bound_buf)

region<-rasterize(local_bound_buf, r, 1)

plot(region, col='red')

extent(region)
# class      : Extent 
# xmin       : 477970.8 
# xmax       : 507970.8 
# ymin       : 125875.3 
# ymax       : 154375.3 

#Adjust the extent of the a given region to the next round km of the polygon

lcm<- raster("spatial_data/derived/Nat_250m_heathland.tif")# reference land cover map

local_ext<-localextent(lcm, region) # raters of reference land cover map and region of interest
region<-rasterize(local_bound_buf, local_ext, 1) #re-rasterize with new extent 


extent(region)
# class      : Extent 
# xmin       : 470000 
# xmax       : 510000 
# ymin       : 120000 
# ymax       : 160000 
 
freq(region)
# value count
# [1,]     1   716
# [2,]    NA 12964


folder<-'spatial_data/derived/borders/'
filename<-'Loc_surrey8sects250km.tif'

local_bound<-condatis_boundaries(region, folder,filename)

# SKent ------------------------------------------------------------------

#Polygon of local projects, e.g. SurreyKent
local<-st_read('spatial_data/original/SussexKent/NRP_LandOnly/NRP_LandOnly.shp')
local_bound<- st_boundary(local) #polygon boundary

local_bound_buf<-st_buffer(local_bound, 250, singleSide = TRUE) #create a buffer outside the polygon, size of the desired resolution 

#template to rasterize the boundary
r<-raster(extent(local_bound_buf), res=250) #resolution equal to habitat layer
crs(r)<-crs(local_bound_buf)

region<-rasterize(local_bound_buf, r, 1)

plot(region, col='red')

extent(region)
# class      : Extent 
# xmin       : 543023 
# xmax       : 561523 
# ymin       : 94862.33 
# ymax       : 108362.3 

#Adjust the extent of the a given region to the next round km of the polygon

lcm<- raster("spatial_data/derived/Nat_250m_grassland.tif")# reference land cover map

local_ext<-localextent(lcm, region) # raters of reference land cover map and region of interest
region<-rasterize(local_bound_buf, local_ext, 1) #re-rasterize with new extent 

extent(region)
# class      : Extent 
# xmin       : 540000 
# xmax       : 570000 
# ymin       : 90000 
# ymax       : 110000 

freq(region)
# value count
# [1,]     1   418
# [2,]    NA  9182


folder<-'spatial_data/derived/borders/'
filename<-'Loc_skentsects250km.tif'

local_bound<-condatis_boundaries(region, folder,filename)

# LostWetlands ----------------------------------------------------------------
#Polygon of local projects, e.g. Surrey
local<-st_read('spatial_data/original/Lost_Wetlands_Boundary/Lost_Wetlands_Boundary.shp')
local<-st_transform(local, st_crs('EPSG:27700'))#assign British National Grid spatial reference

local_bound<- st_boundary(local) #polygon boundary

local_bound_buf<-st_buffer(local_bound, 250, singleSide = TRUE) #create a buffer outside the polygon, size double of the desired resolution e.g. 500m to have a 250 m outer buffer

#template to rasterize the boundary
r<-raster(extent(local_bound_buf), res=250) #resolution equal to habitat layer
crs(r)<-crs(local_bound_buf)

region<-rasterize(local_bound_buf, r, 1)

plot(region, col='red')

extent(region)

#Adjust the extent of the a given region to the next round km of the polygon

lcm<- raster("spatial_data/derived/Nat_250m_grassland.tif")# reference land cover map

local_ext<-localextent(lcm, region) # raters of reference land cover map and region of interest
region<-rasterize(local_bound_buf, local_ext, 1) #re-rasterize with new extent 

extent(region)

freq(region)
# value count
# [1,]     1   418
# [2,]    NA  9182


folder<-'spatial_data/derived/borders/'
filename<-'Loc_lwetlsects250km.tif'

local_bound<-condatis_boundaries(region, folder,filename)

