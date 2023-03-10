do_download <- function(index,ls,collection_list,bands,aoi,name){
  image <- ee$Image(collection_list$get(index))$select(bands)$clip(aoi)
  date <- image$get("DATE_ACQUIRED")$getInfo()
  task <- ee_image_to_drive(image = image,
                            fileNamePrefix = paste0(ls,"_",date),
                            fileFormat = "GEO_TIFF",
                            maxPixels = 10000000000000,
                            folder = name)
  task$start()
}
