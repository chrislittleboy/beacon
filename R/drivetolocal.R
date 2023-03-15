drivetolocal <- function(img){
  for(i in 1:nrow(img)){
    drive_download(img$id[i], path = paste0(path, "/temp/", img$name[i]), overwrite = T)
  }
}
