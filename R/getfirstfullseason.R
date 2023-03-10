getfirstfullseason <- function(seasons){
  ffs <- seasons %>% group_by(season) %>% add_tally() %>%
    mutate(minmonth = min(month), maxmonth = max(month)) %>%
    mutate(range = maxmonth-minmonth + 1) %>%
    ungroup() %>% filter(range == n) %>%
    filter(minmonth == min(minmonth)) %>%
    select(season) %>% distinct() %>% as.character()
}
