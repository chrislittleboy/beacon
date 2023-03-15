topowrapper <- function(layer, az, ele){
  rast(topocorr(x = as(raster(layer), "SpatialGridDataFrame"),
              slope = slopeasp$slope,
              aspect = slopeasp$aspect,
              sunelev = ele,
              sunazimuth = az,
              method = "ccorrection"))
}
