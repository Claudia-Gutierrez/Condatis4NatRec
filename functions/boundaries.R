#'@title Boundaries

# Documentation --------------
#'@author Jenny Hodgson

#'@description
#'This function divides a polygon in eight sections based on the extreme extent points (most northern, southern, eastern and western points) and tangents to obtain the N, NE, NW, E, W, SE, SW sections of the polygon. The output can be used to generate source and targets ([source_target()]) and run Condatis analyses ([condatis()]).

#'@usage boundaries (region, res, filename, folder)

#'@param region string, path of vector (polygon) defining the boundaries of the landscape of interest
#'@param res numeric, resolution of the output raster in meters, default= 250
#'@param folder string, path of destination folder
#'@param filename string, output raster name
#'
#'@section Outputs:
#'\tabular{ll}{
#'\code{boundaries.tif} \tab raster map with values 1 to 8, corresponding to each direction of movement (see details) \cr

#'@details Raster values:
#'\tabular{ll}{
#' Raster value \tab Direction\cr
#' \code{1} \tab North 'N'
#' \code{2} \tab Northeast 'NE'
#' \code{3} \tab East 'E'
#' \code{4} \tab Sotheast 'SE'
#' \code{5} \tab South 'S'
#' \code{6} \tab Southwest 'W'
#' \code{7} \tab West 'W'
#' \code{8} \tab Northwest 'NW'
#' }
#' 
#'@references Hodgson, J., Allen, K., & Cole, L. (2022). Help Document for Condatis Version 1.2. \href{https://pgb.liv.ac.uk/~condatis_dev/help/help.htm}{https://pgb.liv.ac.uk/~condatis_dev/help/help.htm}
#'

#'@seealso [boundaries()]
#'@seealso [condatis()]
#'

region<-'spatial_data/original/SussexKent/NRP_LandOnly/NRP_LandOnly.shp'
res<-100
folder<-'delete'
filename<-'sussex'

boundaries(region=region,res=res,folder, filename)




# Function --------------
#'@export
boundaries<-function(region, res=250, folder, filename) {

  region<-sf::st_read(region)
  
  #check polygon projection is in meters
  region1<-terra::vect(region)
  regcrs<-terra::crs(region1, proj=TRUE, describe=TRUE, parse=TRUE)
  
  if(grepl('units=m', regcrs$proj)){
    
    region<-region
    
  } else {
    
    region<-sf::st_transform(region,'EPSG:3857')
    }
  
  region_bound<- sf::st_boundary(region) #polygon boundary

  region_bound_buf<-sf::st_buffer(region_bound, res, singleSide = TRUE) #create a buffer outside the polygon, size of the desired resolution

  region_bound_buf<-terra::vect(region_bound_buf)

#rasterize buffer  
  r<-terra::rast(region_bound_buf, resolution=res, crs=terra::crs(region_bound_buf), vals=1)
  reg_rast<-terra::rasterize(region_bound_buf,r)
 
#Identify extreme points of the polygon   
  bpt<- terra::as.points(reg_rast, values=TRUE, na.rm=TRUE)
  bptcoord<-terra::crds(bpt, df=TRUE)
  bpt<-as.data.frame(bpt)
  bpt<-cbind(bptcoord, bpt)
  
  #select most southerly point
  bpts<-bpt[order(bpt[,"y"]),1:2][1,]
  bpts<-sf::st_as_sf(bpts, coords = c("x","y"), remove = FALSE)
  bpts<-terra::vect(bpts)

  distN<-terra::rasterize(bpts,r,2)
  terra::plot(distN)
  
  #include the region layer in the area to calculate grid distances
  distN2<-terra::cover(distN,reg_rast)
  
  distN<-terra::gridDistance(x=distN2, origin=2, omit=c(0,NA,NaN))

  distN<- distN/as.numeric(terra::global(distN, fun='max', na.rm=TRUE))

  ##reduce data to just boundary again
  distN2<- distN*reg_rast
  terra::plot(distN2)
  
  bpt<- terra::as.points(distN2, values=TRUE, na.rm=TRUE)
  bptcoord<-terra::crds(bpt, df=TRUE)
  bpt<-as.data.frame(bpt)
  bpt<-cbind(bptcoord, bpt)
  
  #select points closest to halfway round in either direction
  ew<- bpt[order(abs(bpt[,"lyr.1"]-0.5)),][1:16,]
  
  #of those, select the most westerly for new start
  bpte<-ew[order(ew[,"x"]),1:2][1,]
  bpte<-sf::st_as_sf(bpte, coords = c("x","y"), remove = FALSE)
  bpte<-terra::vect(bpte)
 
  distE<- terra::rasterize(bpte,r,2)
  terra::plot(distE)

  # #include the region in the area to calculate grid distances
  distE2<-terra::cover(distE,reg_rast)
  distE<-terra::gridDistance(x=distE2, origin=2, omit=c(0,NA,NaN))
  distE<- distE/as.numeric(terra::global(distE, fun='max', na.rm=TRUE))  

  ##reduce data to just boundary again
  distE2<- distE*reg_rast
  
  
  #combine 2 axes in a data frame
  names(bpt)[3]<-"distN"
  
  bpte<- terra::as.points(distE2, values=TRUE, na.rm=TRUE)
  bpte<-as.data.frame(bpte)

  bpt<-cbind(bpt, bpte)
  names(bpt)[4]<-"distE"
  
  ##calculating angles for the other directions
  
  bpt$angle<-atan2(bpt$distN-0.5,bpt$distE-0.5)

  bpt$cutangle<-as.numeric(cut(bpt$angle,breaks=16))
  
  bpt$sector<-factor(bpt$cutangle,labels=c(
    "W","S W","S W","S","S","S E","S E","E" ,"E"  ,"N E","N E", "N", "N","N W", "N W","W"))
  
  #convert sectors to raster
  sectors<-sf::st_as_sf(bpt, coords = c("x","y"), remove = FALSE)
  
  sectors$rastvalue<-NA
  #assign raster value to directions
  sectors$rastvalue[sectors$sector=='N']<-1
  sectors$rastvalue[sectors$sector=='N E']<-2
  sectors$rastvalue[sectors$sector=='E']<-3
  sectors$rastvalue[sectors$sector=='S E']<-4
  sectors$rastvalue[sectors$sector=='S']<-5
  sectors$rastvalue[sectors$sector=='S W']<-6
  sectors$rastvalue[sectors$sector=='W']<-7
  sectors$rastvalue[sectors$sector=='N W']<-8
  
  sectors<-terra::vect(sectors)
 
  
  sectors_rast<- terra::rasterize(sectors, reg_rast, field='rastvalue')
  terra::plot(sectors_rast, type='classes')
  #write raster
  terra::writeRaster(sectors_rast,paste0(folder,'/', filename,'_boundaries.tif'), overwrite=TRUE)
}
