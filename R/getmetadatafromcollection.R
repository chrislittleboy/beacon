getMetadatafromCollection <- function(collection, size){
  collection_list <- collection$toList(collection$size())
  sizes <- as.list(0:(size-1))
  metadata_list <- lapply(X = sizes, FUN = getMetadatafromImage, collection_list = collection_list)
  metadata <- unlist(metadata_list)
  return(metadata)
}
