################################################
#                                              #
#          Condatis bottlenecks function       #
#                                              #
################################################
#Authors: Jenny Hodgson,  Claudia Gutierrez, Thomas Travers (March 2023)

#This function calculates: conductance (speed), flow, percent of total power and score for each link (potential bottlenecks) and subsets the links that account for a given percentage (threshold) of the power, or a minimum and a maximum of the highest ranked powers. Produces the following outputs:

#########
#Tabular outputs:
#
#'speed_power.csv'- overall speed and total power of the habitat network 
#'flow.csv'- flow value per pixel
#'power.csv'- power values per link

#########
#Spatial outputs:
#
#'flow_raster.tif'- raster map of flow values
#'progress_raster.tif'- raster map of flow progress
#'bottlenecks.shp'- shapefile map of links (lines) of potential bottlenecks, with associated power value and score (calculated as (power% × number of cells)⁄100)
#'bottleneck_major_area.shp'- shapefile map of major bottleneck areas (buffers of half the length of bottleneck, around the middle point of major, merges the overlapping features and calculates the sum of their scores). Note: produced only if there are links within the 'score_major' interval (see 'Default values')
#'bottleneck_sever_area.shp'- shapefile map of severe bottleneck areas (buffers of half the length of bottleneck, around the middle point of severe, merges the overlapping features and calculates the sum of their scores). Note: produced only if there are links with score above 'score_severe' (see 'Default values')

##########
#Default values:
#
#threshold = 0.99      # value 0-1
#maxlink = 10000       # maximum number of links drawn
#minlink = 10          # minimum number of links drawn
#maxdisp = inf         # maximum distance to calculate current between cells [km]
#score_major = c(5,50) # interval of score of bottlenecks considered major
#score_severe = 50     # >score considered severe bottlenecks

##########
# Inputs:
#
# hab - raster of the habitat you wish to measure connectivity over
# st - raster of location of sources and targets
# R - R value of the species moving (number of dispersers produced per km^2 of habitat)
# disp - Dispersal value of the species in km 
# path - path of destination folder (add '/' to end path)
# filename - analyses name, output type will be appended at the end (e.g. filename=landscape, output= landscape_bottlenecks.shp, landscape_flow.csv,etc.)



#'@export
condatis_bottlenecks<- function(hab, st, R, disp, filename, path, threshold=0.99, maxlink=10000, minlink=10, maxdisp=Inf, score_major= c(5,50), score_severe= 50){
  
  library(raster)
  library(sf)
  library(rgdal)
  library(dplyr)
  library(maptools)
  library(sfheaders)
  library(tidyverse)

# Data preparation --------------------------------------------------------

  smart.round <- function(x) { #this function rounds numeric to integer while preserving their sum 
    y <- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y
  }
  
  # Oporator that  is the oposite of %in%
  `%!in%` = Negate(`%in%`)
  
  # Check if the habitat is in meters, and if it is make sure the cellside etc is divided by 1000
  
  if (grepl('units=m', hab@crs@projargs)){
    
    scaler <- 1000
    
  } else {
    
    scaler <- 1
    
  }
  
  amap <- hab
  
  # Take the habitat raster, convert to a dataframe
  apt <- as.data.frame(raster::rasterToPoints(amap, fun = function(x){!is.na(x)}, spatial = F))
  names(apt) <- c('xm', 'ym', 'cover')
  apt <- apt[apt$cover>0,]
  apt$x <- apt$xm/scaler # Create new columns for coordinates in km #
  apt$y <- apt$ym/scaler
  cellside <- raster::xres(amap)/scaler
  
  #convert st raster to dataframe#
  st <- as.data.frame(raster::rasterToPoints(st,fun = function(x){!is.na(x)}, spatial = F))
  names(st) <- c('xm', 'ym', 'label')
  st$x <- st$xm/scaler
  st$y <- st$ym/scaler
  

  #Get the distances between each cell of habitat and every other cell
  len<-dim(apt)[1]
  dm <- dist(apt[, c('x','y')])

  #Get x and y coordinates for sources and targets
  origin <- st[st$label == 1, c('x','y')]
  target <- st[st$label == 2, c('x','y')]
  
  # Define alpha (mean dispersal) and normalisation so the area under the dispersal kernel integrates to 1
  alpha <- 2/disp
  norm <- R*alpha^2/2/pi*cellside^4 
  

# Core Condatis Calculations -----------------------------------------------------------  
    
  #Current between cells
  Cfree <- norm*outer(apt$cover, apt$cover, '*')*exp(-alpha*as.matrix(dm))
  diag(Cfree) <- 0
  
  if(maxdisp!=Inf){
    Cfree[as.matrix(dm)>=maxdisp] <- 0
  }
  
  #Current into a cell and out of a cell
  Cin <- norm*apt$cover*
    rowSums(exp(-alpha*sqrt(outer(apt[, 'x'], origin[, 'x'], '-')^2 +
                              outer(apt[, 'y'], origin[, 'y'], '-')^2)))
  
  Cout <- norm*apt$cover*
    rowSums(exp(-alpha*sqrt(outer(apt[, 'x'], target[, 'x'], '-')^2 +
                              outer(apt[, 'y'], target[, 'y'], '-')^2)))
  M0 <- diag(Cin + Cout + rowSums(Cfree)) - Cfree
  w <- Cin - Cout
  
  v0 <- solve(M0, w, tol = exp(-255)) # This produces the resistance values in the network and is where errors will most likely occur, see 'try' function for error handling
  
  
  I0 <- (v0 + 1) * Cout
  I1 <- (1 - v0) * Cin
  
  #Conductance value of whole network 
  cond <- (sum(I0) + sum(I1))/4 #4 to make overall voltage difference 1 rather than 2
  
  ### flow by cell ###
  flo <- apply(Cfree * outer(v0, v0, '-'), 1, function(x) {
    sum(abs(x))
  })
  
  flo <- ((flo)/2 + I0 + I1)/2
  
  # combine progress, flow, standardised flow, and conductance into a data.frame for saving later
  f <- cbind(apt, progress = (v0 + 1)/2, flow = flo, std_flow = flo/max(flo), conductance = cond)
  
  # Create shapefile of standardised flow values (can be skipped if not needed)
  f_shp <- SpatialPointsDataFrame(f[, c('xm', 'ym')], f[, 4:ncol(f)],
                                  proj4string = crs(amap)) %>%
    as('sf') %>%
    st_buffer(dist = (xres(amap)/2), endCapStyle = 'SQUARE')
  
  # Create a raster of standardised flow and the 'progress' statistic (can be skipped if not needed)
  r <- raster(extent(amap), res = xres(amap), crs = crs(amap))
  r_f <- rasterize(f_shp, r, field = 'flow')
  r_p <- rasterize(f_shp, r, field = 'progress')
  
  ## Power calculations - for bottlenecks ##
  
  powr <- Cfree * (outer(v0, v0, '-')/2)^2 #divide by 2 to get same units again 
  
  powlong <- data.frame(
    a = c(matrix(1:len, nrow = len, ncol = len)[upper.tri(powr)]),
    b = c(matrix(1:len, nrow = len, ncol = len, byrow = T)[upper.tri(powr)]),
    powr=c(powr[upper.tri(powr)])
  )
  
  powlong <- powlong[order(-powlong$powr), ]#sorting the data frame so highest power comes first
  sumpow <- sum(powlong$powr)#total power 
  powlong$thresh <- cumsum(powlong$powr)/sumpow 
  
  
  # subset the power scores that account for threshold of the flow with a minimum of minlink and a maximum of maxlink of the highest powers
  upto <- 1+nrow(subset(powlong, powlong$thresh <= threshold))
  
  if ( upto < minlink){
    powlong <- powlong[1:minlink,]
  } else {if (upto > maxlink) {
    powlong <- powlong[1:maxlink,] 

  } else{
    powlong<-powlong[1:upto,]
  }}
  
  
  powlong$label <- paste(powlong$a, powlong$b, sep = '_')
  
  powlong$perc<-powlong$powr/sumpow*100 #percentage of total
  
  powlong$cumsum<-cumsum(powlong$powr) #power cumulative sum
  powlong$cumsum_perc<-powlong$thresh*100#power cumulative sum percentage
  
  #create dataframes of power scores and location of ends of the bottleneck
  
  powpoints <- cbind(apt[powlong$a, c('xm', 'ym')], powlong[,c('label','powr','perc', 'cumsum_perc')], type = 'a')
  
  #powpoints is to convert to line geometries, 2 rows per bottleneck
  #power is one row per bottleneck - to continue analysis in R
  power<- powpoints[,c('xm', 'ym','label','powr','perc','cumsum_perc')] 
  names(power)<-c('xma', 'yma','label','powr','perc','cumsum_perc')
  power<- cbind(power,apt[powlong$b, c('xm', 'ym')])
  names(power)<-c('xma', 'yma','label','powr','perc','cumsum_perc','xmb','ymb')
  
                
  powpoints <- rbind(powpoints, cbind(apt[powlong$b, c('xm', 'ym')], powlong[, c('label','powr','perc','cumsum_perc')], type = 'b'))
  names(powpoints) <- c('xm', 'ym', 'label', 'power','perc','cumsum_perc','type')
  
  #clean up to save memory - if sure no longer needed
  rm(Cfree)
  rm(powr)
  gc()
  
  # Create shapefile of the location of the top bottlenecks ####

  powpoints<- powpoints[order(powpoints$label),] 
  
  lineobj<- sf_linestring(
    obj = powpoints,
    x = 'xm',
    y = 'ym',
    z = NULL,
    m = NULL,
    linestring_id = 'label',
    keep = TRUE)
  
  lineobj<-subset(lineobj, select = -c(type))
  lineobj$length<-st_length(lineobj)# calculate bottleneck length (units defined by coordinate reference system, here meters)
  
  #assign spatial reference to bottlenecks
  st_crs(lineobj)<-crs(amap)
  
  speed_power<- as.data.frame(cbind(cond, sumpow))
  names(speed_power)<-c('Speed', 'Total power')
  
  #calculate score
  patchcount<- as.numeric(dim(lineobj)[1])
  lineobj$score<-NA
  lineobj$score<-(lineobj$perc*patchcount/100)
  
  # Write outputs -----------------------------------------------------------    
  write.csv(speed_power, paste0(path,filename,'_speed_power.csv'))
  write.csv(f, paste0(path,filename,'_flow.csv'))
  write.csv(power, paste0(path,filename,'_power.csv'))
  writeRaster(r_f,paste0(path,filename,'_flow_raster.tif'),overwrite=TRUE)
  writeRaster(r_p,paste0(path,filename,'_progress_raster.tif'),overwrite=TRUE)
  st_write(lineobj, paste0(path,filename,'_bottlenecks.shp'), append = FALSE)
  

  # Bottleneck areas --------------------------------------------------------

  err<-try({
    #select major bottlenecks
    b_major<- filter(lineobj, score>score_major[1] & score<score_major[2])
    
    #create a point at the middle of bottleneck
    b_major_point<- st_line_sample(b_major, sample=0.5)
    
    #create a buffer
    b_major_buffer<- b_major_point%>%
      st_sf()%>%#allows to add columns
      mutate(m= filter(lineobj, score>score_major[1] & score<score_major[2]))%>%#add bottlenecks information
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
    st_write(b_m_score_sum, paste0(path, filename,'_bottleneck_major_area.shp'), append=FALSE)
    
    
    #select severe bottlenecks
    b_severe<- filter(lineobj, score>score_severe)
    
    #create a point at the middle of bottleneck
    b_severe_point<- st_line_sample(b_severe, sample=0.5)
    
    #create a buffer
    b_severe_buffer<-b_severe_point%>%
      st_sf %>%#allows to add columns
      mutate(s= filter(lineobj, score>score_severe))%>%#add bottlenecks information
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
    st_write(b_s_score_sum, paste0(path, filename,'_bottleneck_severe_area.shp'), append=FALSE)

  })
  
  if(!inherits(err,"try-error")){
    st_write(b_m_score_sum, paste0(path, filename,'_bottleneck_major_area.shp'), append=FALSE)
    st_write(b_s_score_sum, paste0(path, filename,'_bottleneck_severe_area.shp'), append=FALSE)
  }
  
  # return result list --------------------------------------------------------
  results <- list(cond, sumpow,f, r_f, f_shp, r_p, power, lineobj)
  names(results) <- c('conductance', 'powersum','flow', 'flow_raster', 'flow_shp', 'progress_raster', 'power', 'bottlenecks')
  
  return(results)
}

library(rstudioapi)
previewRd('functions/condatis_bottlenecks_documentation.Rd')
