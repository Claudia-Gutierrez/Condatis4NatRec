###########making boundary sections for England - customised to get around island problems################

library(raster)
library(igraph)
library(rgdal)
library(rgeos)

region<-raster("spatial_data/derived/england.anything.100.tif")

plot(region)

extent(region)
#class       : Extent 
#xmin        : 80000 
#xmax        : 660000 
#ymin        : 0 
#ymax        : 660000 
crs(region)

#resolution england.anything
resn<- res(region)[1] 

#resolution for condatis 
bigresn<- 1000

aggfactor<- round (bigresn/resn)



loreg1<- aggregate(region, fact=aggfactor, fun=max,na.rm=T)

freq(loreg1)
# 
# value  count
# [1,]     0    290
# [2,]     1 133051
# [3,]   NaN 249459


loregionb<- boundaries(loreg1>= 0, type="outer") #detects boundaries with NA region

loreg1<- subs(loreg1, data.frame(by=c(0,NaN),which=c(1,0)),subsWithNA=FALSE) #needed for subtraction

plot(loreg1)

plot(loregionb)#as well as islands, there are a couple gaps at the edges where boundary could not extend out

freq(loregionb)
#      value  count
# [1,]     0 133341
# [2,]     1   4010
# [3,]   NaN 245449

bufferb<-buffer(loregionb, width=20000,  doEdge=TRUE)#20 km just enough to join scilly isles

plot(bufferb)#original land and buffer become =1

bufferb<-bufferb-loreg1


bpt<-rasterToPoints(loregionb,fun=function(x){x>0},spatial=F)

#select the most southerly point on the boundary
distN<- rasterize(as.data.frame(bpt[order(bpt[,"y"]),1:2])[1,],loregionb,field=2,update=T)

plot(distN)#south-east tip chosen
points(as.data.frame(bpt[order(bpt[,"y"]),1:2])[1,],pch=15)

#include the buffer in the area to calculate grid distances
distN2<-cover(distN,bufferb)

freq(distN2)

# value  count
# [1,]     0 133341
# [2,]     1  51697
# [3,]     2      1
# [4,]    NA 197761


distN<-gridDistance(x=distN2, origin=2, omit=c(0,NA,NaN))

distN<- distN/cellStats(distN, stat='max', na.rm=TRUE)

plot(distN)

##reduce data to just boundary again
distN2<- distN*loregionb

bpt<-rasterToPoints(distN2,spatial=F)

#select points closest to halfway round in either direction
ew<- bpt[order(abs(bpt[,"layer"]-0.5)),][1:16,]

#of those, select the most westerly for new start
distE<- rasterize(as.data.frame(ew[order(ew[,"x"]),1:2])[1,],loregionb,field=2,update=T)

plot(distE)
points(as.data.frame(ew[order(ew[,"x"]),1:2])[1,],pch=15)#chosen point near Morecambe

#include the buffer in the area to calculate grid distances
distE2<-cover(distE,bufferb)

distE<-gridDistance(x=distE2, origin=2, omit=c(0,NA,NaN))

distE<- distE/cellStats(distE, stat='max', na.rm=TRUE)

plot(distE)

##reduce data to just boundary again
distE2<- distE*loregionb

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

#bpt$sector<-factor(bpt$sector)

#points(bpt$x,bpt$y,pch=19,col=as.numeric(bpt$sector))#not bad, but there are discontinuities around Wash and Isle of Wight

##try another approach calculating angles

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
   write.csv(bpt[,1:6],"spatial_data/derived/england.4directions.csv")
   sectors<- rasterize(bpt[,c(1,2)],loregionb,field=as.numeric(bpt[,5]))
   #numbered 1-8, would not accept factor so labels lost
   
   #sectors<- rasterFromXYZ(bpt[,c(1,2,5)],crs=crs(loregionb),res=res(loregionb))#also does not work
   crs(sectors)
   writeRaster(sectors,"spatial_data/derived/engboundary8sects1km.tif")
   