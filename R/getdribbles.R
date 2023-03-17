getdribbles <- function(x, ssn = ssn, img = img, time){
  dribble <- img %>% filter(season == ssn, i == x) %>%
    mutate(quality_flag = 0, group =paste0(ssn,"_",x,"_",time))
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-1):(x+1))) %>%
      mutate(quality_flag = 1, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-2):(x+2))) %>%
      mutate(quality_flag = 2, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-3):(x+3))) %>%
      mutate(quality_flag = 3, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-4):(x+4))) %>%
      mutate(quality_flag = 4, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-5):(x+5))) %>%
      mutate(quality_flag = 5, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-6):(x+6))) %>%
      mutate(quality_flag = 6, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-7):(x+7))) %>%
      mutate(quality_flag = 7, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-8):(x+8))) %>%
      mutate(quality_flag = 8, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-9):(x+9))) %>%
      mutate(quality_flag = 9, group =paste0(ssn,"_",x,"_",time))
  }
  if(sum(dribble$kbkm2) < 20){
    dribble <- img %>% filter(season == ssn, i %in% c((x-10):(x+10))) %>%
      mutate(quality_flag = 10, group =paste0(ssn,"_",x,"_",time))
  }
  lsats <- dribble %>% group_by(ls) %>% add_tally()
  n <- nrow(lsats)
  n7 <- nrow(lsats[lsats$ls == 7,])
  if((n - n7) >= 3){
    dribble <- dribble %>% filter(ls != 7)
  }
  return(dribble)
}
