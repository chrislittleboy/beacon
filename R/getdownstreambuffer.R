#' @export
getdownstreambuffer <- function(line, reservoir, buffer_downstream_size = 2000) {
  bufferdownstream <- st_buffer(line, buffer_downstream_size)
  intersection <- st_intersection(reservoir, bufferdownstream, dimension = "polygon")
  downstream <- st_difference(bufferdownstream, intersection, dimension = "polygon") %>% st_cast("POLYGON")
  downstream$area <- downstream %>% st_make_valid() %>% st_area()
  downstream <- downstream[downstream$area == max(downstream$area),]
  return(downstream)
}