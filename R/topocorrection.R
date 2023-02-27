library(terra)
library(elevatr)
library(raster)
library(landsat)

test <- rast("/home/chris/Downloads/LC08_L2SP_146039_20221130_20221206_02_T1_362_2023_02_10_13_10_54.tif")
r <- test$SR_B4
r[r == 0] <- NA
b <- test$SR_B3
b[b == 0] <- NA
g <- test$SR_B2
g[g == 0] <- NA
nir <- test$SR_B5
nir[nir == 0] <- NA
swir <- test$SR_B6
swir[swir == 0] <- NA
rgb <- rast(list(r,g,b))
plotRGB(rgb, stretch = "lin")
dem <- get_elev_raster(raster(r), src = "gl1")
dem <- rast(dem)
dem <- crop(dem, r)
dem <- resample(dem, r)
dem <- mask(dem,r)
d <- as(raster(dem), 'SpatialGridDataFrame')
metadata <- list.files("/home/chris/Documents/data/landsat/tehri/metadata/", full.names = T)
meta <- matrix(nrow = length(metadata), ncol = 4)
i <- 1
while(i <= length(metadata)){
meta[i, 1:4] <- read.table(metadata[[i]])[[1]]
i <- i + 1
}

colnames(meta) <- c("id", "date", "azimuth", "elevation")
meta <- data.frame(meta)
library(tidyverse)
metas <- meta %>% dplyr::select(date) %>% distinct() %>%
  mutate(my = format(as.Date(date), "%Y-%m"),
         m = format(as.Date(date), "%m")) %>%
           group_by(my) %>%
           add_tally() %>%
  rename(nmy = n) %>%
  group_by(m) %>%
  add_tally()

my <- format(dates, "%Y-%m") %>% add_tally()
spec <- meta[meta$id == "LC08_L2SP_146039_20221130_20221206_02_T1",]
az <- as.numeric(spec[[3]])
ele <- as.numeric(spec[[4]])

slopeasp <- landsat::slopeasp(d)
dev.off()
plot(rast(slopeasp$aspect))
method = "ccorrection"
topowrapper <- function(layer, az, ele, slopeasp, method = "ccorrection"){
  layer <- as(raster(layer), "SpatialGridDataFrame")
  corrected <- topocorr(x = layer,
                        slope = slopeasp$slope,
                        aspect = slopeasp$aspect,
                        sunelev = ele,
                        sunazimuth = az,
                        method = method)
  return(rast(corrected))
}
rcor <- topowrapper(r, az,ele,slopeasp)
gcor <- topowrapper(g, az,ele,slopeasp)
bcor <- topowrapper(b, az,ele,slopeasp)
nircor <- topowrapper(nir, az,ele,slopeasp)
swircor <- topowrapper(swir, az,ele,slopeasp)

ndwi <- (nircor - swircor) / (nircor + swircor)

ndvi <- (nircor - rcor) / (nircor + rcor)
ndvi[ndvi >= 0.8] <- NA
ndbi <- (swircor - nircor) / (swircor + nircor)
plot(ndbi, range = c(-0.2, 0.7))
?plot
rcorm <- topowrapper(r, az,ele,slopeasp,method = "minnaert")
gcorm <- topowrapper(g, az,ele,slopeasp,method = "minnaert")
bcorm <- topowrapper(b, az,ele,slopeasp,method = "minnaert")
rgbcor <- rast(list(rcor, bcor,gcor))
rgbcorm <- rast(list(rcorm, bcorm,gcorm))
plotRGB(rgb, stretch = "hist")
plotRGB(rgbcor, stretch = "lin")
plotRGB(rgbcorm, stretch = "hist")

terra::writeRaster(rgbcorm, "rgb_m.tif")
setwd("/home/chris/Documents/testing")
test <- nchar(meta$id)
meta$id
test

library(imager)

?topocorr
plot(rcor)
plot(r)
topcored <- topocorr()
read.table(metadata[[1]])
c(read.table(metadata[[1]]))
read.table(metadata[[1]])[[1]]
