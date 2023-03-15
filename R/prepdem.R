prepdem <- function(r){
  dem <- get_elev_raster(r, src = "gl1", config = timeout(30))
  dem <- rast(dem)
  r <- rast(r)
  dem <- crop(dem, r)
  dem <- resample(dem, r)
  dem <- as(raster(dem), 'SpatialGridDataFrame')
}
