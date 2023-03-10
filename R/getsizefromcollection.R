getIndexListfromCollection <- function(lscode,aoi,cc){
  collection <- ee$ImageCollection(paste0("LANDSAT/", lsc, "/C02/T1_L2"))$
    filterBounds(aoi)$
    filterMetadata('CLOUD_COVER', 'less_than', cc)

}
