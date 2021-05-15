
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
#' @return A list is returned with two elements:
#' 1. ggplot for elbow plot.
#' 2. ggplot for ch and asw plot.
#'
#'
#' @export
#'
#' @family Clustering functions
#'
#' @examples
#'
#' example.data <- data.frame(x = c(18, 21, 22, 24, 26, 26, 27, 30, 31,
#'                                  35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54, 35, 30),
#'                           y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44,
#'                                 27, 29, 20, 28, 21, 30, 31, 23, 24, 40, 45))
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#'
#' #Results should say that 3 clusters is optimal
#' output <- CalcOptimalNumClustersForKMeans(example.data, c("x", "y"))
#'
#' elbow.plot <- output[[1]]
#'
#' ch.and.asw.plot <- output[[2]]
#'
#' dev.new()
#' elbow.plot
#'
#' dev.new()
#' ch.and.asw.plot
#'
CalcOptimalNumClustersForKMeans <- function(inputted.data, clustering.columns){


  working.data <- inputted.data


  elbow.plot <- factoextra::fviz_nbclust(working.data[,clustering.columns], stats::kmeans, method = "wss") + ggplot2::labs(subtitle = "Elbow method")

  #grDevices::dev.new()
  #elbow.plot

  # Two other methods
  kclust.ch <- fpc::kmeansruns(working.data[,clustering.columns],krange=1:10,criterion='ch')
  kclust.ch$bestk
  kclust.asw <- fpc::kmeansruns(working.data[,clustering.columns],krange=1:10,criterion='asw')
  kclust.asw$bestk
  ##Looks like 2 is a good number to use.
  crit.df <- data.frame(k=1:10, ch=scale(kclust.ch$crit), asw=scale(kclust.asw$crit))
  crit.df <- reshape2::melt(crit.df, id.vars=c('k'), variable.name = "measure", value.name="score")

  #grDevices::dev.new()
  ch.and.asw.plot <- ggplot2::ggplot(crit.df, ggplot2::aes(x=k, y=score, color=measure))+
    ggplot2::geom_point(ggplot2::aes(shape=measure))+
    ggplot2::geom_line(ggplot2::aes(linetype=measure))+
    ggplot2::scale_x_continuous(breaks = 1:10, labels=1:10)

  return(list(elbow.plot, ch.and.asw.plot))

}


#' Compare clusters
#'
#' For each cluster as specified by rows.for.each.cluster, box plots are generated
#' for each column as specified in name.of.values.to.compare.
#'
#' @param data.input A dataframe
#' @param rows.for.each.cluster A numerical vector that indicates which cluster each observation (row in data.input) belongs to
#' @param name.of.values.to.compare A vector of strings that indicates the columns in the data.input that should be
#'
#' @return No objects are returned, but a plot will be outputted.
#'
#'
#' @export
#'
#' @family Clustering functions
#'
#' @examples
#'
#' example.data <- data.frame(x = c(18, 21, 22, 24, 26, 26, 27, 30, 31,
#'                                  35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54, 35, 30),
#'                           y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44,
#'                                 27, 29, 20, 28, 21, 30, 31, 23, 24, 40, 45))
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#'
#' km.res <- stats::kmeans(example.data[,c("x", "y")], 3, nstart = 25, iter.max=10)
#'
#' grouped <- km.res$cluster
#'
#' generate.plots.comparing.clusters(example.data, grouped, c("x", "y"))
#'
#'
generate.plots.comparing.clusters <- function(data.input, rows.for.each.cluster, name.of.values.to.compare){

  number.of.clusters.to.use <- length(unique(rows.for.each.cluster))

  grDevices::dev.new()
  graphics::par(mfrow=c(1,number.of.clusters.to.use))

  running.vals <- NULL

  ##Use this loop to figure out axes
  for (K in 1:number.of.clusters.to.use) {
    d0 <- data.input[rows.for.each.cluster==K,name.of.values.to.compare]
    running.vals <- c(running.vals, d0)
  }
  lmts <- range(running.vals)

  ##Use this loop to plot
  for (K in 1:number.of.clusters.to.use) {
    d0 <- data.input[rows.for.each.cluster==K,name.of.values.to.compare]
    graphics::boxplot(d0, main=paste(c("Cluster ", K)), ylim=lmts)
  }
}


#' Generate parallel plot to show each observation and which cluster they belong in.
#'
#' @param inputted.data A dataframe where there are numerical columns for clustering.
#' @param cluster.assignment.column A string that specifies the name of the column in inputted.data that specify which cluster each observation in inputted.data belongs to.
#' @param x_axis_features A vector of strings that specify which features from inputted.data should be displayed on the x-axis of the parallel plot.
#'
#' @return No object is returned but a plot will be created.
#'
#'
#' @export
#'
#' @family Clustering functions
#'
#' @examples
#'
#' example.data <- data.frame(x = c(18, 21, 22, 24, 26, 26, 27, 30, 31,
#'                                  35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54, 35, 30),
#'                           y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44,
#'                                 27, 29, 20, 28, 21, 30, 31, 23, 24, 40, 45))
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#'
#' km.res <- stats::kmeans(example.data[,c("x", "y")], 3, nstart = 25, iter.max=10)
#'
#' grouped <- km.res$cluster
#'
#' example.data <- cbind(example.data, grouped)
#'
#' GenerateParcoordForClusters(example.data, "grouped", c("x", "y"))
#'
#'
GenerateParcoordForClusters <- function(inputted.data, cluster.assignment.column, x_axis_features){

  temp.data <- inputted.data
  grouped <- inputted.data[,cluster.assignment.column]

  #Will be subsetted for only features of interest and will contain avg lines.
  temp.data2 <- inputted.data[,x_axis_features]

  cluster.categories <- sort(unique(temp.data[,cluster.assignment.column]))
  num.clusters <- length(cluster.categories)
  parcoord.col <- c(grouped+1)
  parcoord.lwd <- c(rep(1, times = length(grouped)) )

  legend.lab <- c(sort(unique(grouped)) ) #c(cluster.categories,)
  legend.col <- c(sort(unique(grouped+1)) )
  legend.lty <- rep(1, times = length(c(sort(unique(grouped+1)))))
  legend.lwd <- rep(1, times = length(c(sort(unique(grouped+1)))))

  color.num <- 5

  for(cluster.index in cluster.categories)
  {
    cluster.avg.line <- colMeans(subset(temp.data, temp.data[,cluster.assignment.column]==cluster.index)[,x_axis_features])

    temp.data2 <- rbind(temp.data2, cluster.avg.line)
    parcoord.col <- c(parcoord.col, color.num)
    parcoord.lwd <- c(parcoord.lwd, 6)

    label <- paste("Avg line for ", cluster.index)
    legend.lab <- c(legend.lab, label)
    legend.col <- c(legend.col, color.num)
    legend.lty <- c(legend.lty, 1)
    legend.lwd <- c(legend.lwd, 6)

    color.num <- color.num + 2

  }

  grDevices::dev.new()
  MASS::parcoord(temp.data2 , col= parcoord.col, lwd = parcoord.lwd)

  graphics::legend("topright", legend=legend.lab,
         col=legend.col, lty=legend.lty, lwd = legend.lwd, cex=0.8)


}


#' Make a 2D scatter plot that shows the data as represented by PC1 and PC2 and color labels clusters.
#'
#' After clustering of a dataset with two or more dimensions, we often want to
#' visualize the result of the clustering on a 2D plot. If there are more than
#' two dimensions, we want to first reduce the data down to two dimensions.
#' This can be done with PCA. After PCA is completed, the data can be plotted
#' with this function.
#'
#' This function plots PC1 vs PC2 as well as PC1 vs PC3. This function uses
#' the output of stat::prcomp(). The input into prcomp() needs to have
#' at least 3 dimensions.
#'
#' @param pca.results.input An object outputted by stats::prcomp(). The PCA of all the features used for clustering. There should be at least 3 features.
#' @param cluster.labels.input A vector of integers that specify which cluster each observation belongs to
#' (order of observations must match the data inputted to prcomp() to generate pca.results.input).
#' @param subgroup.labels.input A vector of integers that specify an additional label for each observations. Should only have two levels (0 or 1). If observation has value of 1, then it will be circled.
#' @param name A string that is used for the title of the plot.
#'
#' @return No object is outputted, but a plot is outputted.
#'
#' @export
#'
#' @family Clustering functions
#'
#' @examples
#' example.data <- data.frame(x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35,
#'                                  39, 40, 41, 42, 44, 46, 47, 48, 49, 54, 35, 30),
#'                            y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 27,
#'                                  29, 20, 28, 21, 30, 31, 23, 24, 40, 45),
#'                            z = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'                                  1, 1, 1, 1, 1, 1, 1, 1, 1))
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#'
#' km.res <- stats::kmeans(example.data[,c("x", "y", "z")], 3, nstart = 25, iter.max=10)
#'
#' grouped <- km.res$cluster
#'
#' pca.results <- prcomp(example.data[,c("x", "y", "z")], scale=FALSE)
#'
#' actual.group.label <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' generate.2D.clustering.with.labeled.subgroup(pca.results, grouped, actual.group.label,
#'                                              "Cluster results with actual group label")
#'
generate.2D.clustering.with.labeled.subgroup <- function(pca.results.input, cluster.labels.input, subgroup.labels.input, name){

  tbl <- table(subgroup.labels.input, cluster.labels.input)
  chisq.res <- stats::chisq.test(tbl)
  main.text <- paste(as.character(name),"Chi-square p-value=", as.character(chisq.res$p.value))

  ##Plot
  print(tbl)
  utils::str(tbl)

  grDevices::dev.new()
  graphics::par(mfrow=c(1,3), xpd=TRUE)

  totalvar <- (pca.results.input[[1]]^2)
  variancePer <- round(totalvar/sum(totalvar)*100,1)

  xlab <- paste(c("Principal Component 1 (",variancePer[1],"%)"),collapse="")
  ylab <- paste(c("Principal Component 2 (",variancePer[2],"%)"),collapse="")
  zlab <- paste(c("Principal Component 3 (",variancePer[3],"%)"),collapse="")

  plot(pca.results.input$x[,c(1,2)], col=(cluster.labels.input+1), pch=20, cex=2, xlab=xlab, ylab=ylab, main=main.text)
  graphics::symbols(pca.results.input$x[c((subgroup.labels.input==1)),c(1)], pca.results.input$x[c((subgroup.labels.input==1)),c(2)], circles=rep(1,sum(((subgroup.labels.input==1)))),add=T, inches=F)

  plot(pca.results.input$x[,c(1,3)], col=(cluster.labels.input+1), pch=20, cex=2, xlab=xlab, ylab=zlab, main=main.text)
  graphics::symbols(pca.results.input$x[c((subgroup.labels.input==1)),c(1)], pca.results.input$x[c((subgroup.labels.input==1)),c(3)], circles=rep(1,sum(((subgroup.labels.input==1)))),add=T, inches=F)

  ##The padding (cell size) also depends on the padding used when creating original table.
  ##table doesn't support row names.
  plot(1, type="n")
  plotrix::addtable2plot(x=0.7, y=1, xpad=0, ypad=1, table=tbl, cex=1.5,
                title="Chi-Square Table", hlines=TRUE, vlines=TRUE, bty="o", lwd=1.5)

}

#' Make a 3D scatter plot that shows the data as represented by PC1, PC2, and PC3 and color labels clusters.
#'
#' After clustering of a dataset with three or more dimensions, we often want to
#' visualize the result of the clustering on a 3D plot. If there are more than
#' three dimensions, we want to first reduce the data down to three dimensions.
#' This can be done with PCA. After PCA is completed, the data can be plotted
#' with this function.
#'
#' This function plots PC1 vs PC2 vs PC3. This function uses
#' the output of stat::prcomp(). The input into prcomp() needs to have
#' at least 3 dimensions.
#'
#' @param pca.results.input An object outputted by stats::prcomp(). The PCA of all the features used for clustering. There should be at least 3 features.
#' @param cluster.labels.input A vector of integers that specify which cluster each observation belongs to
#' @param subgroup.labels.input A vector of integers that specify an additional label for each observations. Should only have two levels (0 or 1). If observation has value of 1, then it will be circled. If it's 2 then it'll be triangled.
#' @param name A string used for the title of the plot.
#'
#' @return No object is returned, but a rgl plot is displayed.
#'
#' @export
#'
#' @family Clustering functions
#'
#' @examples
#'
#' example.data <- data.frame(x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35,
#'                                  39, 40, 41, 42, 44, 46, 47, 48, 49, 54, 35, 30),
#'                            y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44,
#'                                  27, 29, 20, 28, 21, 30, 31, 23, 24, 40, 45),
#'                            z = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3,
#'                                  3, 3, 3, 3, 3, 3, 3, 3))
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#'
#' km.res <- stats::kmeans(example.data[,c("x", "y", "z")], 3, nstart = 25, iter.max=10)
#'
#' grouped <- km.res$cluster
#'
#' pca.results <- prcomp(example.data[,c("x", "y", "z")], scale=FALSE)
#'
#' actual.group.label <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' generate.3D.clustering.with.labeled.subgroup(pca.results, grouped, actual.group.label,
#'                                              "Cluster results with actual group label")
#'
generate.3D.clustering.with.labeled.subgroup <- function(pca.results.input, cluster.labels.input, subgroup.labels.input, name){

  tbl <- table(subgroup.labels.input, cluster.labels.input)
  chisq.res <- stats::chisq.test(tbl)
  main.text <- paste(as.character(name),"Chi-square p-value=", as.character(chisq.res$p.value))

  totalvar <- (pca.results.input[[1]]^2)
  variancePer <- round(totalvar/sum(totalvar)*100,1)

  xlab <- paste(c("Principal Component 1 (",variancePer[1],"%)"),collapse="")
  ylab <- paste(c("Principal Component 2 (",variancePer[2],"%)"),collapse="")
  zlab <- paste(c("Principal Component 3 (",variancePer[3],"%)"),collapse="")

  ##Plot
  rgl::rgl.open()
  rgl::rgl.bg(color = "white")
  rgl::plot3d(x= pca.results.input$x[,c(1)], y= pca.results.input$x[,c(2)], z= pca.results.input$x[,c(3)],
         xlab = xlab, ylab = ylab, zlab = zlab, col=(cluster.labels.input+1), pch=20, cex=2, main=main.text)
  rgl::pch3d(x= pca.results.input$x[,c(1)], y= pca.results.input$x[,c(2)], z= pca.results.input$x[,c(3)], pch= subgroup.labels.input, cex=0.3)

}

#' Automated hierarchical clustering with labeling of observations and groups
#'
#' Versatile hierarchical clustering function that can use correlation or distance
#' clustering. The linkage can also be specified.
#'
#' Correlation type, distance type, linkage type, and coloring of groups can
#' all be specified. The result is a dendrogram of the hierarchical clustering with
#' coloring scheme that shows which observations belong to which cluster. Additionally,
#' coloring of the terminal branches can be done with meta data so that the
#' meta data can be compared to the cluster assignment. Links to resources
#' used to make this function are provided in the code.
#'
#' @param working.data A dataframe of data
#' @param clustering.columns A vector of strings that indicate the names of columns to be used for clustering. The columns should be numerical.
#' @param label.column.name A string that indicates the name of column to be used for labeling the terminal branches of the dendrogram.
#' @param grouping.column.name A string that indicates the name of column to be used for coloring terminal branches. The column should contain numerical values and should be a factor. A value of 0 will result in a black terminal branch. A value of 1 will result in a red terminal branch.
#' @param number.of.clusters.to.use A numerical value indicating how many clusters (main branches) to be colored.
#' @param distance_method A string that specifies the distance method for clustering. Default option is "euclidean". See documentation for stats::dist() for all available options. This is only used if Use.correlation.for.hclust is FALSE.
#' @param correlation_method A string that specifies the correlation method to be used for clustering. Default option is "spearman". See documentation for stats::cor() for all available options. This is only used if Use.correlation.for.hclust is TRUE.
#' @param linkage_method_type A string that specifies the linkage method to be used for clustering. See documentation for stats::hclust() for all available options. Examples are "ward.D", "complete".
#' @param Use.correlation.for.hclust A boolean specifying if correlation between observations should be used to cluster. If correlation is not used, then distance between observations is used instead.
#' @param terminal.branch.font.size A numeric value specifying font size of the labels for the terminal branches as specified by label.column.name. Default value is 1.
#' @param title.to.use A string that indicates the title of the plot.
#'
#' @return A list with three objects:
#' 1.hclust.res: The object outputted from stats::hclust().
#' 2.dend1: The object outtputed from converting hclust.res into a dendrogram.
#' 3.kcboot: The results from fpc::clusterboot() which evaluates the stability of the clusters.
#'
#' Additionally, the dendrogram will be displayed and a table summarizing
#' stability of clusters will also be displayed.
#'
#' @export
#'
#' @family Clustering functions
#'
#' @import dendextend
#'
#' @examples
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "3h", "3i", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "2g", "2h")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54, 35, 30)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 27, 29, 20, 28, 21, 30, 31, 23, 24, 40, 45)
#'
#' color = as.factor(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
#'
#' example.data <- data.frame(id, x, y, color)
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#' text(example.data$x, example.data$y, labels = id, cex=0.9, font=2)
#'
#' HierarchicalClustering(working.data = example.data,
#'                        clustering.columns = c("x", "y"),
#'                        label.column.name = "id",
#'                        grouping.column.name = "color",
#'                        number.of.clusters.to.use = 3,
#'                        distance_method = "euclidean",
#'                        correlation_method = NULL,
#'                        linkage_method_type = "ward.D",
#'                        Use.correlation.for.hclust = FALSE,
#'                        terminal.branch.font.size = 1,
#'                        title.to.use = "Clustering based on x and y data")
#'
HierarchicalClustering <- function(working.data, clustering.columns, label.column.name,
                                   grouping.column.name, number.of.clusters.to.use,
                                   distance_method = "euclidean", correlation_method,
                                   linkage_method_type, Use.correlation.for.hclust,
                                   terminal.branch.font.size, title.to.use){

  #Assumes that data is already normalized.

  #------------------------------------------------------------------------------
  # Clustering with Hierarchical clustering
  #------------------------------------------------------------------------------
  #Resources:
  #https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
  #https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html. How to color some labels
  #How to use correlation matrix: https://www.datanovia.com/en/blog/clustering-using-correlation-as-distance-measures-in-r/


  # #Testing conditions
  # working.data <- working.data.scaled.columns
  # distance_method <- c("euclidean")
  # correlation_method <- c("spearman")
  # linkage_methods <- c("ward.D", "ward.D2")
  # clustering.columns <- names.of.dependent.variables
  # label.column.name <- "ID"
  # grouping.column.name <- "HIV"
  # Use.correlation.for.hclust <- FALSE
  # number.of.clusters.to.use <- 2
  # title.to.use <- "HIV clustering based on volumetric data"
  # #linkage_methods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")


  if(Use.correlation.for.hclust == TRUE)
  {
    data.dist = stats::as.dist(1 - stats::cor(t(working.data[,clustering.columns]), method = correlation_method))
  }else{
    data.dist = stats::dist(working.data[,clustering.columns], method = distance_method)
  }


  #Label the observations
  set.seed(1)
  hclust.res <- stats::hclust(data.dist, method = linkage_method_type)
  #hclust.res.list[[list.index]] <- hclust.res
  dend <- stats::as.dendrogram(hclust.res)

  #Get the order that should occur on the dendrogram: https://stackoverflow.com/questions/33611111/how-to-change-dendrogram-labels-in-r
  #and add labels.
  #labels() is from the dendextend package.
  labels(dend) <- working.data[,label.column.name][hclust.res$order]

  #Color by an additional group label
  #labels_colors is from dendextend package.
  labels_colors(dend) <- as.integer(as.character(working.data[,grouping.column.name][hclust.res$order])) + 1 #col = 1 is black. col = 2 is red.
  dend <- dendextend::set(dend, "labels_cex", value = terminal.branch.font.size)
  dend1 <- dendextend::color_branches(dend, k = number.of.clusters.to.use)

  #Calculate the quality of clusters using Dunn's index
  #Dunn's index is the ratio between the minimum inter-cluster distances to the maximum intra-cluster diameter.
  #The diameter of a cluster is the distance between its two furthermost points. In order to have well separated
  #and compact clusters you should aim for a higher Dunn's index.
  set.seed(1)
  tree <- stats::hclust(data.dist, method = linkage_method_type)
  cluster <- stats::cutree(tree, k = number.of.clusters.to.use) #LEFT OFF HERE. CLUSTER LABEL AND COLORING ON DEND DO NOT MATCH
  dunn.index <- clValid::dunn(data.dist, cluster) #From 0 to inf. Should be maximized.

  grDevices::dev.new()
  title <- paste(title.to.use, linkage_method_type, " linkage. ", distance_method, " distance. ", correlation_method, " correlation.\n", "Correlation Used", Use.correlation.for.hclust, ". Dunn's index= ", dunn.index)
  plot(dend1, main = title)

  #Add legend to dendrogram
  legend.labels.to.use <- levels(working.data[,grouping.column.name])
  col.to.use <- as.integer(levels(working.data[,grouping.column.name])) + 1
  pch.to.use <- rep(20, times = length(legend.labels.to.use))
  graphics::legend("topright",
         legend = legend.labels.to.use,
         col = col.to.use,
         pch = pch.to.use, bty = "n",  pt.cex = 1.5, cex = 0.8 ,
         text.col = "black", horiz = FALSE, inset = c(0, 0.1),
         title = grouping.column.name)

  #Get the cluster assignment for each leaf
  ##The name labels of the leaves on the outputted dendrogram reflects the cluster assignment
  cluster.labels.renamed <- cluster
  names(cluster.labels.renamed) <- working.data[,label.column.name]
  cluster.labels.renamed.reordered <- cluster.labels.renamed[hclust.res$order]
  print(title)
  print("Cluster assignment")
  print(cluster.labels.renamed.reordered)

  ##Evaluate stability of clusters
  #https://win-vector.com/2015/09/04/bootstrap-evaluation-of-clusters/
  #disthclustCBI needs to be used because the input matrix is already a distance (dissimilarity)
  #matrix. https://rdrr.io/cran/fpc/src/R/clusterboot.R
  set.seed(1)
  kcboot <- fpc::clusterboot(data.dist, distances = TRUE, B=100, clustermethod = fpc::disthclustCBI ,method=linkage_method_type, k=number.of.clusters.to.use, seed=1)
  kcboot$bootmean
  kcboot$bootbrd
  ##Display stability
  stability.table <- rbind(kcboot$bootmean, kcboot$bootbrd)
  rownames(stability.table) <- c("bootmean", "bootbrd")
  colnames(stability.table) <- c(1:number.of.clusters.to.use)
  grDevices::dev.new()
  d <- stability.table
  gridExtra::grid.table(d)

  #From the stability output, how do you tell which cluster on the dendrogram
  #is stable?
  groups <- kcboot$result$partition
  groups_relabeled <- groups
  names(groups_relabeled) <- working.data[,label.column.name]
  groups_relabeled_reordered <- groups_relabeled[hclust.res$order]

  #Checked that group assignment from hclust and cluster boot match by
  #comparing cluster.labels.renamed.reordered and groups_relabeled_reordered

  output <- list(hclust.res, dend1, kcboot)

  return(output)

}
