rasttodf <- function(x, dam, climvar = climvar) {
  if(climvar == "ndvi"){
    month <- as.numeric(substr(x, 95,96))
    year <- as.numeric(substr(x, 91,94))
    week <- as.numeric(substr(x, 97,98))
    dam <- st_make_valid(st_difference(st_buffer(dam, 4000), st_buffer(dam, 1000))) # ndvi around reservoir important, not ndvi over water body
  }
  if(climvar %in% c("prec","tmax")) {
    month <- as.numeric(substr(x, 62,63))
    year <- as.numeric(substr(x, 57,60))
  }
  if(climvar == "tmin") {
    month <- as.numeric(substr(x, 63,64))
    year <- as.numeric(substr(x, 58,61))
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
