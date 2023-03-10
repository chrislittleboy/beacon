silhouette_score <- function(k, vars = vars){
  km <- kmeans(x = vars, centers = k, nstart=25)
  ss <- cluster::silhouette(km$cluster, dist(vars))
  mean(ss[, 3])
}
