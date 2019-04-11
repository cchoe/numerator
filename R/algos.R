#' K-means optimal cluster calculation using elbow method
#' 
#' Low-stats NbClust instability solved for data frames with less than 5 rows
#'
#' @param x input var
#' @param y 
#' @return A vector of clusters
#' @examples 
#' @export
kmeans_optimal_elbow <- function(x, y) {
  df1 = as.matrix(cbind(x, y))
  n_clusters = nrow(unique(df1))
  if (n_clusters > 1) {
    if (nrow(unique(df1)) < 5) {
      a1 = fviz_nbclust(df1, kmeans, 
                        k.max = min(nrow(unique(df1))-1, 10),
                        method = "wss",
                        print.summary = T,
                        verbose = T)
      plot(a1)
      q = a1$data$y/a1$data$y[1]
      n_clusters = ifelse(min(which(q < 0.15)) == Inf & 
                              nrow(unique(df1)) == 3, 2,
                          min(which(q < 0.15)))  
      clusters = kmeans(df1 ,n_clusters)
    } else {
      nb = NbClust(data = df1, 
                   diss = NULL, 
                   distance = "euclidean",
                   min.nc = 2, 
                   max.nc = min(nrow(unique(df1))-1, 10), 
                   method = "kmeans", 
                   alphaBeale = 0)
      n_clusters = max(unlist(nb[4]))
      print(n_clusters)
      clusters = kmeans(df1, n_clusters)
    } 
  }
  
  return(clusters)
}
