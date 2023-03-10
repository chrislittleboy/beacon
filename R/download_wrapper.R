download_wrapper <- function(ls,aoi,cc,name,metadata_path){
  aoi <- sf_as_ee(bbox_expand(aoi))
  lscode <- getlscode(ls)
  bands <- getbands(ls)
  collection <- ee$ImageCollection(lscode)$
    filterBounds(aoi)$
    filterMetadata('CLOUD_COVER', 'less_than', cc)
  collection_list <- collection$toList(collection$size())
  indices <- 0:(collection$size()$getInfo()-1)
  sapply(X = indices, FUN = download_landsat,
         ls = ls,
         collection = collection,
         collection_list = collection_list,
         bands = bands,
         aoi = aoi,
         name = name)
  size <- collection$size()$getInfo()
  metadata <- getMetadatafromCollection(collection, size)
  write(metadata, file = paste0(metadata_path,"/LS0",ls,"_",name, ".csv"))
}
