# gets the minimum value (i.e. index number for the closes point)
# for which BOTH: 
# the flow accumulation is greater (i.e. not a tributary)
# and the elevation is higher (i.e. not upstream)

getutm <- function(latitude, longitude) {
  ESPG <- 
    32700-round((45+latitude)/90,0)*100+round((183+longitude)/6,0)
}