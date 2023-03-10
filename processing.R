library(tidyverse)
library(terra)
library(googledrive)
library(sf)
library(landsat)
library(devtools)
library(elevatr)
rm(list = ls())
load_all()

# should try this several times. The google drive API is unstable, and may miss files in the directory
imlist <- as.list(rep("~/tehri", 5))
imlist <- lapply(imlist, drive_ls)
i <- 1
v <- c(0,0,0,0,0)
while(i <= length(imlist)){
v[i] <- nrow(imlist[[i]])
i <- i+ 1
}

# load images from drive # do not trust this command and do it many times
images <- drive_ls("~/tehri")
images$date <- as.POSIXct(substr(images$name, 3,12))
images$month <- format(images$date, "%m")
images$month <- as.numeric(images$month)
images$year <- format(images$date, "%Y")
images$size <- sapply(X = 1:nrow(images),
       FUN = function(x){images$drive_resource[[x]]$size})
images$size <- as.numeric(images$size)/1000000
images$ls <- substr(images$name,0,1)

yearly_prec <- list.files("/home/chris/Documents/data/climate/prec/", full.names = T)
yearly_tmax <- list.files("/home/chris/Documents/data/climate/tmax/", full.names = T)
yearly_tmin <- list.files("/home/chris/Documents/data/climate/tmin/", full.names = T)
weekly_ndvi <- list.files("/home/chris/Documents/data/cci/ndvi/", full.names = T)
dam <- read_sf("/home/chris/Documents/data/reservoir_expanded.shp")
seasons <- getSeasons(dam = dam,
                   yearly_prec = yearly_prec,
                   yearly_tmax = yearly_tmax,
                   yearly_tmin = yearly_tmin,
                   weekly_ndvi = weekly_ndvi)
images <- merge(images,seasons, by = "month")

ffs <- getfirstfullseason(seasons)
ffm <- min(seasons$month[seasons$season == ffs])
firstyear <- as.numeric(min(images$year))
lastyear <- as.numeric(max(images$year))
firstmonth <- min(images$month[images$year == firstyear])
lastmonth <- max(images$month[images$year == lastyear])
year <- as.vector(sapply(firstyear:lastyear, FUN = function(year){years <- rep(year,12)}))
month <- rep(1:12, lastyear-firstyear+1)
seasonindex <- rep(seasons$season, lastyear-firstyear+1)
i <- c(rep(NA, ffm-1), year-firstyear+1)
i <- i[1:length(year)]
indices <- data.frame(year, month, seasonindex, i)
images <- merge(images, indices)

images$date <- substr(images$date, 1,10)
metadata <- clean_metadata(path = "/home/chris/Documents/data/landsat/metadata/") %>%
  mutate(azimuth = as.numeric(azimuth), elevation = as.numeric(elevation)) %>%
  group_by(date) %>%
  summarise(azimuth = mean(azimuth), elevation = mean(elevation))
images <- merge(images,metadata)

before <- images %>% filter(year <= 2005)
after <- images %>% filter(year >= 2006)
ib <- as.list(sort(as.numeric(unique(before$i)), decreasing = F))
ia <- as.list(sort(as.numeric(unique(after$i)), decreasing = F))

hot_before <- lapply(ib, getdribbles, ssn = "Hot", img = before, time = "before")
hot_after <- lapply(ia, getdribbles, ssn = "Hot", img = after, time = "after")
cool_before <- lapply(ib, getdribbles, ssn = "Cool", img = before, time = "before")
cool_after <- lapply(ia, getdribbles, ssn = "Cool", img = after, time = "after")
monsoon_before <- lapply(ib, getdribbles, ssn = "Monsoon", img = before, time = "before")
monsoon_after <- lapply(ia, getdribbles, ssn = "Monsoon", img = after, time = "after")
ha <- listtodf(hot_after)
hb <- listtodf(hot_before)
cb <- listtodf(cool_before)
ca <- listtodf(cool_after)
mb <- listtodf(monsoon_before)
ma <- listtodf(monsoon_after)
all <- rbind(ha,hb,cb,ca,mb,ma)

firstfullyear <- max(c(
  min(all$year[all$season == "Monsoon"]),
  min(all$year[all$season == "Hot"]),
  min(all$year[all$season == "Cool"])
))
r <- rast(paste0(path, img$name[1]))
r <- raster(extend(r, maxext, snap = "out"))
r[] <- 1
dem <- prepdem(r)
slopaspect <- landsat::slopeasp(dem)
slope <- slopaspect$slope
aspect <- slopaspect$aspect
path = "/home/chris/Documents/data/landsat/test/"
groups <- all %>% select(group) %>% distinct()
groups <- as.character(groups[,1])
sapply(X = groups, FUN = doprocessing, path = path, slope = slope, aspect = aspect)
gc()
