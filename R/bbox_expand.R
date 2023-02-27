bbox_expand <- function(dam, factor = 0.25) {
  bbox <- st_bbox(dam)
  xrange <- bbox[[3]] - bbox[[1]]
  yrange <- bbox[[4]] - bbox[[2]]
  bbox[1] <- bbox[1] - (xrange * factor)
  bbox[2] <- bbox[2] - (yrange * factor)
  bbox[3] <- bbox[3] + (xrange * factor)
  bbox[4] <- bbox[4] + (yrange * factor)
  bbox <- st_as_sfc(bbox)
  return(bbox)
}
