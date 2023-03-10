clean_metadata <- function(path){
  list_metadata <- list.files(path, full.names = T)
  metadata <- data.frame(matrix(nrow = 0, ncol = 4))
  i <- 1
  j <- 1
  while(j <= length(list_metadata)){
    mdts <- read.csv(list_metadata[[j]], header = F)
    mat <- data.frame(matrix(nrow = nrow(mdts)/4, ncol = 4))
    i <- 1
    while(i <= (nrow(mdts)/4)){
      mat[i, 1:4] <- mdts$V1[(4*i-3):(4*i)]
      i <- i + 1
    }
    metadata <- rbind(metadata,mat)
    j <- j + 1
  }
  colnames(metadata) <- c("id", "date", "azimuth", "elevation")
  return(metadata)
}
