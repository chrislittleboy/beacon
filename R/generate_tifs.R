generate_tifs <- function(img,g,path,slopeasp, maxext){

  img <- img %>% filter(group == g)
  imlist <- as.list(rep(NA, nrow(img)))
  corimlist <- as.list(rep(NA, nrow(img)))

  for(i in 1:nrow(img)){
    imls <- ifelse(img[i,]$ls %in% c("4","5","7"), "4-7", "8-9")
    im <- rast(paste0(path, "/temp/", img$name[i]))
    # removes zeros
    im <- subst(x = im, from = 0, to = NA)
    # removes clouds
    if(imls == "4-7"){
      cloud <- subst(im$QA_PIXEL, from = c(5440,5504), to = 1, others = NA)
    }
    if(imls == "8-9"){
      cloud <- subst(im$QA_PIXEL, from = c(21824, 21952), to = 1, others = NA)
    }
    im <- im * cloud
    # standardises band names
    if(imls == "4-7"){
    im$c <- im$SR_B1
    im$c[] <- NA
    im <- subset(im, c(7,1:6))
    }
    names(im) <- c("c","b","g","r", "nir", "swir","qa")
    # corrects topography for spectral indices
    im <- extend(im, maxext, snap = "out")
    az <- img[i,]$azimuth
    ele <- img[i,]$elevation
    c_swir <- topowrapper(im$swir, az = az, ele = ele)
    c_nir <- topowrapper(im$nir, az = az, ele = ele)
    c_r <- topowrapper(im$r, az = az, ele = ele)
    corsi <- rast(list(c_swir, c_nir, c_r))
    imlist[[i]] <- im
    corimlist[[i]] <- corsi
  }
  # computes true colour (uncorrected) mosaic
  collection <- sprc(imlist)
  mosaic <- mosaic(collection, fun = "median")
  # computes spectral indicies (corrected) mosaic
  corcollection <- sprc(corimlist)
  cmosaic <- mosaic(corcollection, fun = "median")
  tc <- subset(mosaic, 2:4)
  ndvi <- (cmosaic$nir-cmosaic$r) / (cmosaic$nir + cmosaic$r)
  ndbi <- (cmosaic$swir-cmosaic$nir) / (cmosaic$swir + cmosaic$nir)
  writeRaster(ndvi, filename = paste0(path, "/output/ndvi_", g, ".tif"))
  writeRaster(ndbi, filename = paste0(path, "/output/ndbi_", g, ".tif"))
  writeRaster(tc, filename = paste0(path, "/output/tc_", g, ".tif"))
  rm(collection, mosaic, corcollection, cmosaic, tc,ndvi,ndbi, imlist, corimlist)
  gc()
}
