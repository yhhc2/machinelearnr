


#' Generate plots to help decide optimal number of clusters for Kmeans
#'
#' Multiple methods are included for assessing optimal number of clusters.
#'
#' Function generates plots that allow you to evaluate optimal number of clusters
#' using 3 different methods:
#' 1. Elbow method: https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/.
#' Plot intra-cluster variation. Pick the K (cluster) corresponding to a sharp decrease (the elbow).
#'
#' 2. Calinski-Harabasz Index (ch) method:
#' https://www.datasciencecentral.com/profiles/blogs/machine-learning-unsupervised-k-means-clustering-and and
#' https://medium.com/@haataa/how-to-measure-clustering-performances-when-there-are-no-ground-truth-db027e9a871c
#'
#' 3. Average Silhouette Width (asw) method: https://www.datasciencecentral.com/profiles/blogs/machine-learning-unsupervised-k-means-clustering-and
#' and https://medium.com/@haataa/how-to-measure-clustering-performances-when-there-are-no-ground-truth-db027e9a871c
#'
#' ch and asw both produce a score. Higher score = better defined cluster. Maximize between
#' cluster dispersion and minimize within cluster dispersion. Can plot the score versus the number of clusters.
#'
#' @param inputted.data A dataframe.
#' @param clustering.columns Name of columns with data to use for kmeans clustering.
#'
#' @return No object is outputted. Plots are displayed.
#' @export
#'
#' @examples
#'
#' CalcOptimalNumClustersForKMeans(Example.data.for.cluster, colnames(Example.data.for.cluster)[c(4:9)])
#'
CalcOptimalNumClustersForKMeans <- function(inputted.data, clustering.columns){

  dev.new()

  working.data <- inputted.data


  fviz_nbclust(working.data[,clustering.columns], kmeans, method = "wss") +
    labs(subtitle = "Elbow method")

  # Two other methods
  kclust.ch <- kmeansruns(working.data[,clustering.columns],krange=1:10,criterion='ch')
  kclust.ch$bestk
  kclust.asw <- kmeansruns(working.data[,clustering.columns],krange=1:10,criterion='asw')
  kclust.asw$bestk
  ##Looks like 2 is a good number to use.
  crit.df <- data.frame(k=1:10, ch=scale(kclust.ch$crit), asw=scale(kclust.asw$crit))
  crit.df <- melt(crit.df, id.vars=c('k'), variable.name = "measure", value.name="score")

  dev.new()
  ggplot(crit.df, aes(x=k, y=score, color=measure))+
    geom_point(aes(shape=measure))+
    geom_line(aes(linetype=measure))+
    scale_x_continuous(breaks = 1:10, labels=1:10)

}

#generate.plots.comparing.clusters()

#GenerateParcoordForClusters()

#generate.2D.clustering.with.labeled.subgroup()

#generate.3D.clustering.with.labeled.subgroup()

#Own separate file
#HierarchicalClustering()
