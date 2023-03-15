library(tidyverse)
library(terra)
library(googledrive)
library(sf)
library(landsat)
library(devtools)
library(elevatr)
library(httr)

load_all()

imlist <- as.list(rep("~/tehri", 5))
imlist <- lapply(imlist, drive_ls)
i <- 1
v <- c(0,0,0,0,0)
while(i <= length(imlist)){
v[i] <- nrow(imlist[[i]])
i <- i+ 1
}

images <- drive_ls("~/tehri")
drivetolocal(images)
firstfullyear <- max(c(
  min(all$year[all$season == "Monsoon"]),
  min(all$year[all$season == "Hot"]),
  min(all$year[all$season == "Cool"])
))

drivetolocal()

groups
for(1:length(groups)) {
img <- filter(all, group == groups[i])
drivetolocal(img)
maxext <- getmaxextent(paste0(path, "/temp/"))
slopeasp <- getslopeasp(img, maxext)
doprocessing(img = img, path = path, slope = slopeasp$slope, aspect = slopeasp$aspect, maxext = maxext)
unlink(paste0(path, "/temp/"), recursive = TRUE)
}
