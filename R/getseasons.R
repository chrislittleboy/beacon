#' @import dplyr
#' @import tidyr
#' @import terra
#' @import sf
#' @import cluster

getSeasons <- function(dam, yearly_prec, yearly_tmax, yearly_tmin, weekly_ndvi){
  prec <- lapply(X = yearly_prec, FUN = rasttodf, dam = dam, climvar = "prec")
  tmax <- lapply(X = yearly_tmax, FUN = rasttodf, dam = dam, climvar = "tmax")
  tmin <- lapply(X = yearly_tmin, FUN = rasttodf, dam = dam, climvar = "tmin")
  ndvi <- lapply(X = weekly_ndvi, FUN =rasttodf,dam = dam, climvar = "ndvi")
  ndvi <- lstodf(ndvi)
  prec <- lstodf(prec)
  tmin <- lstodf(tmin)
  tmax <- lstodf(tmax)
  ndvi <- ndvi %>% drop_na() %>% group_by(x,y) %>% mutate(ndvi_lag = ndvi - lag(ndvi, 1)) %>%
    mutate(ndvi_lag = ifelse(is.na(ndvi_lag), mean(ndvi_lag, na.rm = T), ndvi_lag))

  clim <- cbind(select(prec, -x, -y), tmin$tmin, tmax$tmax) %>% drop_na()
  colnames(clim)[4:5] <- c("tmin", "tmax")
  clim <- clim %>% mutate(tran = tmax - tmin,
                          tmid = tmin + (tran/2),
                          pixel = rep(1:(nrow(clim) / length(unique(clim$year)) / 12),
                                      length(unique(clim$year))*12)
  ) %>% group_by(pixel) %>%
    mutate(tmid_lag = tmid - lag(tmid, n = 1),
           prec_lag = prec - lag(prec, n = 1)) %>%
    ungroup() %>% select(-pixel)
  clim$tmid_lag[is.na(clim$tmid_lag)] <- mean(clim$tmid_lag[clim$month == 1], na.rm = T)
  clim$prec_lag[is.na(clim$prec_lag)] <- mean(clim$prec_lag[clim$month == 1], na.rm = T)
  ag_clim <- clim %>% group_by(month) %>% summarise_all(mean)
  ag_ndvi <- ndvi %>% group_by(month) %>% summarise_all(mean)
  ag <- cbind(ag_clim, ag_ndvi$ndvi, ag_ndvi$ndvi_lag) %>% select(-month,-year,-tmin,-tmax,-prec_lag)
  vars <- data.frame(scale(ag))
  x <- max(ag_clim$tmax) - min(ag_clim$tmax)
  f_lag <- getFactor(x) + 1
  f_prec <- 2-getFactor(x)
  vars$tmid_lag <- vars$tmid_lag * f_lag
  vars$ag_ndvi.ndvi_lag <- vars$ag_ndvi.ndvi_lag*f_lag
  vars$prec <- vars$prec * f_prec
  krange <- 2:4
  avg_sil <- sapply(krange, silhouette_score)
  k <- which(avg_sil == max(avg_sil)) + 1
  cs <- kmeans(vars, centers = k)$cluster
  ag_clim <- ag_clim %>% mutate(cluster = cs)
  ag_clim_summary <- ag_clim %>% group_by(cluster) %>%
    summarise(p = max(prec),
              t = mean(tmid),
              tlag = mean(tmid_lag)) %>%
    ungroup() %>%
    mutate(k = k,
           r_p = rank(p),
           r_t = rank(t),
           r_tlag = rank(tlag)) %>%
    mutate(season3 = ifelse(r_p == max(r_p), "Monsoon",
                            ifelse(r_t == min(r_t), "Cool", "Hot")),
           season4 = ifelse(r_tlag == max(r_tlag), "Spring",
                            ifelse(r_tlag == min(r_tlag), "Autumn",
                                   ifelse(r_t == min(r_t), "Winter","Summer"))),
           season2 = ifelse(r_p == max(r_p), "Monsoon", "Dry")) %>%
    mutate(season = ifelse(k == 2, season2,
                           ifelse(k == 3, season3, season4))) %>%
    select(cluster, season)
  monthseason <- merge(select(ag_clim, month, cluster), ag_clim_summary) %>% arrange(month)
  return(monthseason)
}
