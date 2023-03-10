rasttodf <- function(x, dam, climvar = climvar) {
  if(climvar == "ndvi"){
    month <- as.numeric(substr(basename(x), 59,60))
    year <- as.numeric(substr(basename(x), 55,58))
    week <- as.numeric(substr(basename(x), 61,62))
    dam <- st_make_valid(st_difference(st_buffer(dam, 4000), st_buffer(dam, 1000))) # ndvi around reservoir important, not ndvi over water body
  }
  if(climvar %in% c("prec","tmax", "tmin")) {
    month <- as.numeric(substr(basename(x), 22,23))
    year <- as.numeric(substr(basename(x), 17,20))
  }
  clim <- rast(x)
  clim <- terra::crop(clim,dam, snap = "out")
  if(climvar == "ndvi"){
    clim <- mask(clim,dam)
    clim <- data.frame(as.data.frame(clim, xy = T))
    clim <- cbind(week,month, year, clim)
    colnames(clim)[6] <- climvar
  }
  if(climvar != "ndvi"){
    clim <- data.frame(as.data.frame(clim, xy = T))
    clim <- cbind(month, year, clim)
    colnames(clim)[5] <- climvar
  }
  return(clim)
}
