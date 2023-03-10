doprocessing <- function(group,path,slope,aspect){
  i <- 1
  img <- all[all$group == group,]
  for(i in 1:nrow(img)){
    drive_download(img$id[i], path = paste0(path, img$name[i]), overwrite = T)
  }
  img <- img %>% mutate(monthdif = abs(month-mean(img$month))) %>% arrange(monthdif)
  maxext <- getmaxextent(path)
  imlist <- as.list(rep(NA, nrow(img)))
  corimlist <- as.list(rep(NA, nrow(img)))
  i <- 1
  for(i in 1:nrow(img)){
    im <- rast(paste0(path, img$name[i]))
    im <- subst(x = im, from = 0, to = NA) # removes zeros
    cloud <- subst(im$QA_PIXEL, from = c(5440,5504), to = 1, others = NA)
    im <- im * cloud # removes clouds and cloud shadows
    im <- extend(im, maxext, snap = "out")
    az <- img[i,]$azimuth
    ele <- img[i,]$elevation
    c_swir <- topocorr(x = as(raster(im$SR_B5), "SpatialGridDataFrame"),
                       slope = slope, aspect = aspect,
                       sunelev = ele,
                       sunazimuth = az,
                       method = "ccorrection")
    c_nir <- topocorr(x = as(raster(im$SR_B4), "SpatialGridDataFrame"),
                      slope = slope, aspect = aspect,sunelev = ele,sunazimuth = az,
                      method = "ccorrection")
    c_r <- topocorr(x = as(raster(im$SR_B3), "SpatialGridDataFrame"),
                    slope = slope, aspect = aspect,sunelev = ele,sunazimuth = az,
                    method = "ccorrection")
    c_swir <- rast(c_swir)
    c_nir <- rast(c_nir)
    c_r <- rast(c_r)
    corsi <- rast(list(c_swir, c_nir, c_r))
    imlist[[i]] <- im
    corimlist[[i]] <- corsi
  }
  collection <- sprc(imlist)
  mosaic <- mosaic(collection, fun = "median")
  tc <- subset(mosaic, 1:3)
  corcollection <- sprc(corimlist)
  cmosaic <- mosaic(corcollection, fun = "median")
  ndvi <- (cmosaic$SR_B4-cmosaic$SR_B3) / (cmosaic$SR_B4 + cmosaic$SR_B3)
  ndbi <- (cmosaic$SR_B5-cmosaic$SR_B4) / (cmosaic$SR_B5 + cmosaic$SR_B4)
  writeRaster(ndvi, filename = paste0(path, "ndvi_", group, ".tif"))
  writeRaster(ndbi, filename = paste0(path, "ndbi_", group, ".tif"))
  writeRaster(tc, filename = paste0(path, "tc_", group, ".tif"))
}
