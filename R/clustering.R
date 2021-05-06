
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


  elbow.plot <- factoextra::fviz_nbclust(working.data[,clustering.columns], kmeans, method = "wss") + ggplot2::labs(subtitle = "Elbow method")

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
#' generate.2D.clustering.with.labeled.subgroup(pca.results, grouped, actual.group.label, "Cluster results with actual group label")
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
#' generate.3D.clustering.with.labeled.subgroup(pca.results, grouped, actual.group.label, "Cluster results with actual group label")
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


#Own separate file
#HierarchicalClustering()
