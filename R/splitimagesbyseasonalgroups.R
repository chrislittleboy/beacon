splitimagesbyseasonalgroups <- function(images, construction_year, maxext){
  images$area <- st_area(st_as_sf(as.polygons(maxext)))/1000000
  images$kbkm2 <- (images$size * 1024)/ images$area
  before <- images %>% filter(year < construction_year)
  after <- images %>% filter(year >= construction_year)
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
  return(all)
}
