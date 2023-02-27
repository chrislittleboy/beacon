silhouette_score <- function(x){
  km <- kmeans(vars, centers = x, nstart=25)
  ss <- silhouette(km$cluster, dist(vars))
  mean(ss[, 3])
}
