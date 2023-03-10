getdribbles <- function(x, ssn = ssn, img = img, time){
  dribble <- img %>% filter(season == ssn, i == x) %>%
    mutate(quality_flag = 0, group =paste0(ssn,"_",x,"_",time))
  if(nrow(dribble) <= 3){
    dribble <- img %>% filter(season == ssn, i %in% c((x-1):(x+1))) %>%
      mutate(quality_flag = 1, group =paste0(ssn,"_",x,"_",time))
  }
  if(nrow(dribble) <= 3){
    dribble <- img %>% filter(season == ssn, i %in% c((x-2):(x+2))) %>%
      mutate(quality_flag = 2, group =paste0(ssn,"_",x,"_",time))
  }
  return(dribble)
}
