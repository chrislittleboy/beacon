getgroupsfromimages <- function(images){
  groups <- images %>% select(group) %>% distinct()
  groups <- groups[,1]
  return(groups)
}
