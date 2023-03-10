listtodf <- function(x){
  df <- x[[1]]
  i <- 2
  while(i <= length(x)) {
    df <- rbind(df, x[[i]])
    i <- i + 1
  }
  return(df)
}
