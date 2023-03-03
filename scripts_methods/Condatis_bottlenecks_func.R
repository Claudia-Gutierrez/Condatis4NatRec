################################################
#                                              #
#          Condatis bottlenecks function       #
#                                              #
################################################
#Authors: Jenny Hodgson and Thomas Travers (February 2023)

#This function calculates conductance, flow, percent of total power for each bottleneck  and subsets the bottlenecks that account for a given percentage (threshold) of the power (or a minimum of 10 and a maximum of 10,000 of the highest ranked powers)

# The function needs the following inputs:

# Hab - raster of the habitat you wish to measure connectivity over
# st - raster of location of sources and targets
# R - R value of the species moving (number of movers produced per km^2 of habitat)
# disp - Dispersal value of the species in km 

Condatis_bottlenecks<- function(hab, st, R, disp,filename,dsn, threshold=0.99, maxlink=10000, minlink=10, maxdisp=Inf){
  
  library(raster)
  library(sf)
  library(rgdal)
  library(dplyr)
  library(maptools)
  library(sfheaders)
  
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
  apt <- as.data.frame(rasterToPoints(amap, fun = function(x){!is.na(x)}, spatial = F))
  names(apt) <- c('xm', 'ym', 'cover')
  apt <- apt[apt$cover>0,]
  apt$x <- apt$xm/scaler # Create new columns for coordinates in km #
  apt$y <- apt$ym/scaler
  cellside <- xres(amap)/scaler
  
  #convert st raster to dataframe#
  st <- as.data.frame(rasterToPoints(st,fun = function(x){!is.na(x)}, spatial = F))
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
  
  hist(dm)
  
  #### Core Condatis Calculations ####
  
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
  
  #### create shapefile of the location of the top bottlenecks ####

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
  
  write.csv(speed_power, paste0(dsn,filename,'speed_power.csv'))
  write.csv(f, paste0(dsn,filename,'flow.csv'))
  write.csv(power, paste0(dsn,filename,'power.csv'))
  writeRaster(r_f,paste0(dsn,filename,'flow_raster.tif'),overwrite=TRUE)
  writeRaster(r_p,paste0(dsn,filename,'progress_raster.tif'),overwrite=TRUE)
  st_write(lineobj, paste0(dsn,filename,'bottlenecks.shp'), append = FALSE)
  
  results <- list(cond, sumpow,f, r_f, f_shp, r_p, power, lineobj)
  names(results) <- c('conductance', 'powersum','flow', 'flow_raster', 'flow_shp', 'progress_raster', 'power', 'bottlenecks')
  
  return(results)
}

