##########################
#                        #
# Aling local areas      #
#                        #
##########################
#Author: Jenny Hodgson (modified by Claudia Gutierrez from 'GIS_functions' script)
#February 2023

#This script adjust the extent of the a given region to the next round km of the polygon

#Inputs
# - lcm: reference land cover map in raster format (e.g.,national heathland map ) 
# - region: raster of the region of interest

localextent<-function(lcm, region=NA,align=10000,workresn=NA){
  
  resn<-mean(res(lcm))
  if(identical(workresn,NA)){
    workresn<- resn
  }else{if(workresn<resn){
    warning("resolution cannot be increased, setting workresn to the resolution of landcover raster")
    workresn<- resn
  }}
  
  aggfactor<- round(workresn/resn)
  workresn<- resn*aggfactor
  
  if(align<workresn){
    warning("align should be >= workresn, setting it to next power of 10")
    align<- 10^ ceiling(log10(workresn))
  }
  
  if( (align %% workresn) > 0){
    warning("align should be a multiple of workresn, setting it to next multiple")
    align<- ceiling(align/workresn)*workresn 
  }
  
  if(identical(region,NA)){
      algextent<-extent(lcm)#if no polygon or extent supplied, assume use whole lcm as study area
      lcmx<-lcm
    }else{
      algextent<-extent(region)
      lcmx<-crop(lcm,algextent)#hope this will do nothing (without detriment) if no cropping needed
    } #we use align wherever extent comes from
  
  #we add one workresn cell on all sides and then round out to whole units of align 
  #align should be bigger than (and a multiple of) the coarsest resolution you might need to use,
  #to make sure you can easily overlay all the resulting maps (if you care about that)
  algextent@xmin<- floor((algextent@xmin-workresn)/align)*align
  algextent@ymin<- floor((algextent@ymin-workresn)/align)*align
  algextent@xmax<- ceiling((algextent@xmax+workresn)/align)*align
  algextent@ymax<- ceiling((algextent@ymax+workresn)/align)*align
  
  lcmx<-extend(lcmx, algextent, value=NA)#hope this will do nothing (without detriment) if no extension needed
  
  region_bound<- lcmx>=0 
  }

##########################
#                        #
# Condatis boundaries    #
#                        #
##########################
#Author: Jenny Hodgson (modified by Claudia Gutierrez from 'england-boundary' script)
#February 2023

#This function divides a polygon in eight sections based on the extreme extent points (most northern, southern, eastern and western points) and tangents to obtain the N, NE, NW, E, W, SE, SW sections of the polygon

#Input: raster file of the boundary of the area of interest, folder path and name of the raster file

condatis_boundaries<-function(region, folder, filename) {
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
  # [1,]     1   715
  # [2,]     2     1
  # [3,]    NA 12964
  
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
  writeRaster(sectors,paste0(folder, filename), overwrite=TRUE)
}

