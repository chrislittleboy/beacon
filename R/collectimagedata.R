collectimagedata <- function(images){
  images$date <- as.POSIXct(substr(images$name, 3,12))
  images$month <- format(images$date, "%m")
  images$month <- as.numeric(images$month)
  images$year <- format(images$date, "%Y")
  images$size <- sapply(X = 1:nrow(images),
                        FUN = function(x){images$drive_resource[[x]]$size})
  images$size <- as.numeric(images$size)/1000000
  images$ls <- substr(images$name,0,1)
  images <- images %>% filter(size >= 0.3)
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
}
