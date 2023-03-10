getlscode <- function(ls){
  if(ls == 4){lsc <- "LT04"}
  if(ls == 5){lsc <- "LT05"}
  if(ls == 7){lsc <- "LE07"}
  if(ls == 8){lsc <- "LC08"}
  if(ls == 9){lsc <- "LC09"}
  lscode <- paste0("LANDSAT/", lsc, "/C02/T1_L2")
  return(lscode)
}
