##########################
#                        #
#     Local boundary     #
#                        #
##########################
#Author: Jenny Hodgson (modified by Claudia Gutierrez from 'england-boundary' script)
#February 2023

#This script divided a polygon in eight sections based on the extreme extent points (most northern, southern, eastern and western points) and tangents to obtain the N, NE, NW, E, W, SE, SW sections of the polygon

library(raster)
library(igraph)
library(sf)
library(rgeos)


# Surrey ------------------------------------------------------------------


#Polygon of local projects, e.g. Surrey
local<-st_read('spatial_data/original/SurreyHeathlands/Heathlands Connections with Buffer_1.shp')
local_bound<- st_boundary(local) #polygon boundary

#template to rasterize the boundary
r<-raster(extent(local), res=250) #resolution equal to habitat layer
crs(r)<-crs(local)

region<-rasterize(local_bound, r, 1)

plot(region, col='red')

extent(region)
# class      : Extent 
# xmin       : 478220.8 
# xmax       : 507670.8 
# ymin       : 126150.4 
# ymax       : 154125.4  
crs(region)

freq(region)
# value count
# [1,]     1   456
# [2,]   NaN 12760

bpt<-rasterToPoints(region, spatial=F)

#select the most southerly point on the boundary
distN<- rasterize(as.data.frame(bpt[order(bpt[,"y"]),1:2])[1,],region,field=2,update=T)

plot(distN)#south-east tip chosen
points(as.data.frame(bpt[order(bpt[,"y"]),1:2])[1,],pch=15)

# #include the region layer in the area to calculate grid distances
 distN2<-cover(distN,region)
# 
 freq(distN2)
 # value count
 # [1,]     1   457
 # [2,]     2     1
 # [3,]    NA 12758

distN<-gridDistance(x=distN2, origin=2, omit=c(0,NA,NaN))

distN<- distN/cellStats(distN, stat='max', na.rm=TRUE)

plot(distN)

bpt<-rasterToPoints(distN,spatial=F)

#select points closest to halfway round in either direction
ew<- bpt[order(abs(bpt[,"layer"]-0.5)),][1:16,]

#of those, select the most westerly for new start
distE<- rasterize(as.data.frame(ew[order(ew[,"x"]),1:2])[1,],region,field=2,update=T)

plot(distE)
points(as.data.frame(ew[order(ew[,"x"]),1:2])[1,],pch=15)#chosen point near Morecambe

# #include the region in the area to calculate grid distances
distE2<-cover(distE,region)

distE<-gridDistance(x=distE2, origin=2, omit=c(0,NA,NaN))

distE<- distE/cellStats(distE, stat='max', na.rm=TRUE)

plot(distE)

##reduce data to just boundary again
distE2<- distE*region

#######combine 2 axes in a data frame#########
bpt<-as.data.frame(bpt)
names(bpt)[3]<-"distN"
bpt<-merge(bpt,as.data.frame(rasterToPoints(distE2,spatial=F)))
names(bpt)[4]<-"distE"

#categorise the boundary based on relative distances N and E
bpt$sector<- paste( c("S","N")[1+(bpt$distN>0.5)],c("W","E")[1+(bpt$distE>0.5)])

points(bpt$x,bpt$y,pch=19,col=as.numeric(as.factor(bpt$sector)))

bpt$sector[(bpt$distN>0.875)]<-"N" 
bpt$sector[(bpt$distN<0.125)]<-"S"
bpt$sector[(bpt$distE>0.875)]<-"E"
bpt$sector[(bpt$distE<0.125)]<-"W"

##calculating angles for the other directions

bpt$angle<-atan2(bpt$distN-0.5,bpt$distE-0.5)

mypal<-rainbow(20)[c(1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16)]

points(bpt$x,bpt$y,pch=19,col=mypal[as.numeric(cut(bpt$angle,breaks=16))])#looks reasonable

legend("topleft",legend=1:16,pch=15,col=mypal)

bpt$cutangle<-as.numeric(cut(bpt$angle,breaks=16))

bpt$sector2<-factor(bpt$cutangle,labels=c(
  "W","S W","S W","S","S","S E","S E","E" ,"E"  ,"N E","N E", "N", "N","N W", "N W","W"))

table(bpt$sector2)

points(bpt$x,bpt$y,pch=19,col=mypal[as.numeric(bpt$sector2)])#discontinuities have gone

legend("topleft",legend=levels(bpt$sector2),pch=15,col=mypal[1:8])

#########save data####

bpt$sector<- bpt$sector2
write.csv(bpt[,1:6],"spatial_data/derived/borders/Loc_surrey.4directions.csv")
sectors<- rasterize(bpt[,c(1,2)],region,field=as.numeric(bpt[,5]))
#numbered 1-8, would not accept factor so labels lost

#sectors<- rasterFromXYZ(bpt[,c(1,2,5)],crs=crs(loregionb),res=res(loregionb))#also does not work
crs(sectors)
#name layer accordingly
writeRaster(sectors,"spatial_data/derived/borders/Loc_surrey8sects250km.tif", overwrite=TRUE)


# SussexKent --------------------------------------------------------------

#Polygon of local projects, e.g. SussexKent
local<-st_read('spatial_data/original/SussexKent/NRP_SussexKent/NRP_SussexKent.shp')
local_bound<- st_boundary(local) #polygon boundary

#template to rasterize the boundary
r<-raster(extent(local), res=250) #resolution equal to habitat layer
crs(r)<-crs(local)

region<-rasterize(local_bound, r, 1)

plot(region, col='red')

extent(region)
# class      : Extent 
# xmin       : 543272.9 
# xmax       : 561272.9 
# ymin       : 94112.38 
# ymax       : 108112.4

crs(region)

freq(region)
# value count
# [1,]     1   252
# [2,]    NA  3780

bpt<-rasterToPoints(region, spatial=F)

#select the most southerly point on the boundary
distN<- rasterize(as.data.frame(bpt[order(bpt[,"y"]),1:2])[1,],region,field=2,update=T)

plot(distN)#south-east tip chosen
points(as.data.frame(bpt[order(bpt[,"y"]),1:2])[1,],pch=15)

# #include the region layer in the area to calculate grid distances
distN2<-cover(distN,region)
# 
freq(distN2)
#       value count
# [1,]     1   251
# [2,]     2     1
# [3,]    NA  3780

distN<-gridDistance(x=distN2, origin=2, omit=c(0,NA,NaN))

distN<- distN/cellStats(distN, stat='max', na.rm=TRUE)

plot(distN)

bpt<-rasterToPoints(distN,spatial=F)

#select points closest to halfway round in either direction
ew<- bpt[order(abs(bpt[,"layer"]-0.5)),][1:16,]

#of those, select the most westerly for new start
distE<- rasterize(as.data.frame(ew[order(ew[,"x"]),1:2])[1,],region,field=2,update=T)

plot(distE)
points(as.data.frame(ew[order(ew[,"x"]),1:2])[1,],pch=15)#chosen point near Morecambe

# #include the region in the area to calculate grid distances
distE2<-cover(distE,region)

distE<-gridDistance(x=distE2, origin=2, omit=c(0,NA,NaN))

distE<- distE/cellStats(distE, stat='max', na.rm=TRUE)

plot(distE)

##reduce data to just boundary again
distE2<- distE*region

#######combine 2 axes in a data frame#########
bpt<-as.data.frame(bpt)
names(bpt)[3]<-"distN"
bpt<-merge(bpt,as.data.frame(rasterToPoints(distE2,spatial=F)))
names(bpt)[4]<-"distE"

#categorise the boundary based on relative distances N and E
bpt$sector<- paste( c("S","N")[1+(bpt$distN>0.5)],c("W","E")[1+(bpt$distE>0.5)])

points(bpt$x,bpt$y,pch=19,col=as.numeric(as.factor(bpt$sector)))

bpt$sector[(bpt$distN>0.875)]<-"N" 
bpt$sector[(bpt$distN<0.125)]<-"S"
bpt$sector[(bpt$distE>0.875)]<-"E"
bpt$sector[(bpt$distE<0.125)]<-"W"

##calculating angles for the other directions

bpt$angle<-atan2(bpt$distN-0.5,bpt$distE-0.5)

mypal<-rainbow(20)[c(1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16)]

points(bpt$x,bpt$y,pch=19,col=mypal[as.numeric(cut(bpt$angle,breaks=16))])#looks reasonable

legend("topleft",legend=1:16,pch=15,col=mypal)

bpt$cutangle<-as.numeric(cut(bpt$angle,breaks=16))

bpt$sector2<-factor(bpt$cutangle,labels=c(
  "W","S W","S W","S","S","S E","S E","E" ,"E"  ,"N E","N E", "N", "N","N W", "N W","W"))

table(bpt$sector2)

points(bpt$x,bpt$y,pch=19,col=mypal[as.numeric(bpt$sector2)])#discontinuities have gone

legend("topleft",legend=levels(bpt$sector2),pch=15,col=mypal[1:8])

#########save data####

bpt$sector<- bpt$sector2
write.csv(bpt[,1:6],"spatial_data/derived/borders/Loc_skent.4directions.csv")
sectors<- rasterize(bpt[,c(1,2)],region,field=as.numeric(bpt[,5]))
#numbered 1-8, would not accept factor so labels lost

#sectors<- rasterFromXYZ(bpt[,c(1,2,5)],crs=crs(loregionb),res=res(loregionb))#also does not work
crs(sectors)
#name layer accordingly
writeRaster(sectors,"spatial_data/derived/borders/Loc_skent8sects250km.tif", overwrite=TRUE)
plot(sectors)






