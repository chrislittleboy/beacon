getslopeasp <- function(img, maxext) {
  r <- rast(paste0(path,"/temp/", img$name[1]))
  r <- raster(extend(r, maxext, snap = "out"))
  r[] <- 1
  dem <- prepdem(r)
  slopeaspect <- landsat::slopeasp(dem)
  return(slopeaspect)
}
