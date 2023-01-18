####GIS functions to support connectivity indicator analysis###
#Jenny Hodgson, January 2021

prepregion<-function(regionfname=NA,rastername,align=10000,workresn=NA,studyrect=NA,savefolder=getwd(),datfolder=getwd()){
  
  lcm<-raster(file.path(datfolder,rastername))
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
  
  if( identical(regionfname,NA)){
    if(identical(studyrect,NA)){
      algextent<-extent(lcm)#if no polygon or extent supplied, assume use whole lcm as study area
      lcmx<-lcm
    }else{
      algextent<-extent(studyrect)
      lcmx<-crop(lcm,algextent)#hope this will do nothing (without detriment) if no cropping needed
    }
  }else{#when regionfname is supplied
    
    bigpoly<- spTransform(readOGR(dsn= datfolder, layer=regionfname), crs(lcm))
    
    
    algextent<- extent(bigpoly)
    #lcmextent<- extent(lcm)
    
  }#we use align wherever extent comes from

    
  #we add one workresn cell on all sides and then round out to whole units of align 
  #align should be bigger than (and a multiple of) the coarsest resolution you might need to use,
  #to make sure you can easily overlay all the resulting maps (if you care about that)
  algextent@xmin<- floor((algextent@xmin-workresn)/align)*align
  algextent@ymin<- floor((algextent@ymin-workresn)/align)*align
  algextent@xmax<- ceiling((algextent@xmax+workresn)/align)*align
  algextent@ymax<- ceiling((algextent@ymax+workresn)/align)*align
  
  
  if(exists("bigpoly")){
    lcmx<-crop(lcm,algextent)#hope this will do nothing (without detriment) if no cropping needed
    lcmx<-extend(lcmx, algextent, value=NA)#hope this will do nothing (without detriment) if no extension needed
    lcmx<-mask(lcmx,bigpoly)#mask makes everything outside the study area NA. There could still be missing data within the study area if it's missing from lcm
  }else{
    lcmx<-extend(lcmx, algextent, value=NA)#hope this will do nothing (without detriment) if no extension needed
  }
  
  algextent<-extent(lcmx)#may be needed if lcm cell centres weren't sensible
  
  
  #create study area mask assuming that a value of NA or negative is to be ignored (but zeros will contribute to the land area)
  
  aggfactor<- round(workresn/resn)
  workresn<- resn*aggfactor
  
  if(aggfactor>1){
    regionp<- aggregate(lcmx, fact=aggfactor, fun=function(x,...){ifelse(
      sum(!is.na(x))>0,sum(x >=0,na.rm=T)/aggfactor^2,NA) })
  }else{
    regionp<- lcmx>=0 
  }
  
  try(regionb<- boundaries(regionp, type="outer") )#slow
  #condatis will need boundaries even if rectangle, so need to insert NAs all around
  
  #save the rasters individually so independent of workspace
  writeRaster(lcmx, filename = file.path(savefolder, paste0(regionfname, "_lcmx_",resn, ".tif")),overwrite=T)
  writeRaster(regionp,  filename = file.path(savefolder,paste0( regionfname, "_regionp_",workresn, ".tif")),overwrite=T)
  writeRaster(regionb, filename = file.path(savefolder, paste0(regionfname, "_regionb_",workresn, ".tif")),overwrite=T)
  
  #return the labels to enable them to be read back in
  
  list(regionfname=regionfname,rastername=rastername,align=align,workresn=workresn,resn=resn,datfolder=datfolder,
       lcmx=file.path(savefolder, paste0(regionfname, "_lcmx_",resn, ".tif")),
       regionp=file.path(savefolder,paste0( regionfname, "_regionp_",workresn, ".tif")),
       regionb=file.path(savefolder, paste0(regionfname, "_regionb_",workresn, ".tif")),
       workingcells=ncell(regionp)-summary(regionp)["NA's",1],
       lcmcells=ncell(lcmx)-summary(lcmx)["NA's",1])
  
}


makehabitat<-function(regionfname="myregion",lcmx,regionp,
            hablabel="myhabitat",habcodes,savefolder=getwd()){
  
  resn<-mean(res(lcmx))
  workresn<-mean(res(regionp))
  
  aggfactor<- round(workresn/resn)
  
      fhabitatreg<- lcmx %in% habcodes
  if(aggfactor>1){
    fhabitatreg<- aggregate(fhabitatreg,fact=aggfactor,fun=sum)/aggfactor^2
  }
      fhabitatreg<- mask(fhabitatreg,regionp)
 
      totarea<- cellStats(fhabitatreg,sum)*workresn*workresn/1000/1000
      regarea<- cellStats(regionp,sum)*workresn*workresn/1000/1000
      proparea<- totarea/regarea
      hcells<- cellStats(fhabitatreg>0,sum)
      
      writeRaster(fhabitatreg,  filename = file.path(savefolder, paste0(regionfname,"_",hablabel, "_",workresn, ".tif")),overwrite=T)
  
        #return the properties and file path   
      list(regionfname=regionfname,hablabel=hablabel,workresn=workresn,resn=resn,
           fhabitatreg=file.path(savefolder, paste0(regionfname,"_",hablabel, "_",workresn, ".tif")),
           totarea=totarea,regarea=regarea,proparea=proparea,hcells=hcells)
}

makehabitatpoints<-function(habitatraster,inm=TRUE){
apt<- rasterToPoints(habitatraster,fun=function(x){!is.na(x)},spatial=F)
apt<- as.data.frame(apt)
names(apt)<- c("xm","ym","cover")
apt<-apt[apt$cover>0,]#remove cells with 0 cover
if(inm){
apt$x<-apt$xm/1000
apt$y<-apt$ym/1000
}else{
names(apt)<- c("x","y","cover")
}
apt
}


