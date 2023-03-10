download_landsat <- function(index,ls,collection,collection_list,bands,aoi,name,metadata_path){
  do_download(
    index = index,
    ls = ls,
    collection_list = collection_list,
    bands = bands,
    aoi = aoi,
    name = name
  )
}
