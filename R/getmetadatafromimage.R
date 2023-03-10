getMetadatafromImage <- function(index, collection_list){
  selectedImage = ee$Image(collection_list$get(index))
  selectedImage$get("LANDSAT_PRODUCT_ID")$getInfo() -> id
  selectedImage$get("DATE_ACQUIRED")$getInfo() -> date
  selectedImage$get("SUN_AZIMUTH")$getInfo() -> sun_azimuth
  selectedImage$get("SUN_ELEVATION")$getInfo() -> sun_elevation
  metadata <- c(id,date,sun_azimuth,sun_elevation)
  return(metadata)
}
