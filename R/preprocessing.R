
#' Remove columns with all zeros
#'
#' @param inputted.data A dataframe.
#' @param columns.to.look.at A vector of strings that specify the column names to look at. If no value is provided, then all columns are evaluated.
#'
#' @return A new dataframe with all-zero columns removed.
#'
#' @export
#'
RemoveColWithAllZeros <- function(inputted.data, columns.to.look.at=NULL){

  modified.input.data <- NULL

  #If columns.to.look.at is set to NULL, then just look at all columns.
  if(is.null(columns.to.look.at))
  {

    keep.nonzero.cols <- apply(inputted.data, 2, max)!=0
    modified.input.data <- inputted.data[,keep.nonzero.cols]

  }else{

    subset.inputted.data <- inputted.data[, columns.to.look.at]
    keep.nonzero.cols <- apply(subset.inputted.data, 2, max)!=0
    subset.inputted.data.to.keep <- subset.inputted.data[,keep.nonzero.cols]

    columns.to.keep.not.from.subset <- setdiff(names(inputted.data), names(subset.inputted.data))
    columns.to.keep.from.subset <- intersect(names(subset.inputted.data), names(subset.inputted.data.to.keep))

    all.columns.to.keep <- c(columns.to.keep.not.from.subset,columns.to.keep.from.subset)

    modified.input.data <- inputted.data[,all.columns.to.keep]

  }

  return(modified.input.data)
}

#' Perform PCA
#'
#' @param inputted.data A dataframe.
#' @param columns.to.do.PCA.on A vector of strings that specify the column names to use for PCA.
#' @param scale.boolean Boolean specifying if data should be scaled before PCA.
#' @param minimum.variance.percent Value from 0-100 specifying how much percentage of total variance a principal component needs to meet or exceed to be kept.
#'
#' @return A List object that contains 3 elements:
#' 1. A dataframe with the PCs added to the end and the names of the PC columns.
#' 2. A vector with the names of PCs.
#' 3. The pca.results object from prcomp().
#'
#' @export
#'
AddPCsToEnd <- function(inputted.data, columns.to.do.PCA.on, scale.boolean, minimum.variance.percent){

  pca.results <- stats::prcomp(inputted.data[, columns.to.do.PCA.on], scale=scale.boolean)
  totalvar <- (pca.results[[1]]^2)
  varianceper <- round((totalvar/sum(totalvar))*100,1)
  pca.x <- pca.results$x[,varianceper >= minimum.variance.percent]

  modified.input.data <- cbind(inputted.data, pca.x)
  PC.names <- colnames(pca.x)

  output <- list(modified.input.data, PC.names, pca.results)

  return(output)

}


#' Assess stability of values that correspond to a single identifier
#'
#' @param inputted.data A dataframe.
#' @param col.name.of.unique.identifier A string that specifies name of column in inputted.data containing unique identifiers.
#' @param value.to.evaluate A string that specifies name of column in inputted.data to look at for stability of values.
#'
#' @return A dataframe with the unique.identifiers as rows and the standard deviation of the values.
#' If a individual only has one row in the original dataframe, the SD value will be NA.
#'
#' @export
#'
#' @examples
#' identifier.col <- c("a", "a", "a", "b", "b", "b", "c")
#' value.col <- c(1, 2, 3, 1, 1, 1, 5)
#' input.data.frame <- as.data.frame(cbind(identifier.col, value.col))
#'
#' results <- StabilityTestingAcrossVisits(input.data.frame, "identifier.col", "value.col")
#'
#' results
#'
#'
StabilityTestingAcrossVisits <- function(inputted.data, col.name.of.unique.identifier, value.to.evaluate){

  unique.identifier <- unique(inputted.data[,col.name.of.unique.identifier])

  running.ID.values <- data.frame(ID=unique.identifier, standard.deviation.of.val=numeric(length(unique.identifier)))
  #Go through each sample ID
  for(ID.index in 1:length(unique.identifier))
  {
    vals.for.sample <- NULL

    #Go through each row of the data
    for(data.index in 1:dim(inputted.data)[1])
    {
      if(unique.identifier[ID.index] == inputted.data[data.index, col.name.of.unique.identifier])
      {
        vals.for.sample <- c(vals.for.sample, inputted.data[data.index, value.to.evaluate])
      }
    }

    standard.deviation.for.samp <- stats::sd(vals.for.sample)
    running.ID.values[ID.index, c("standard.deviation.of.val")] <- standard.deviation.for.samp

  }

  return(running.ID.values)
}


#' Remove samples that have multiple values for a single column and those
#' values are unstable
#'
#' This function uses the StabilityTestingAcrossVisits() function, and then uses the results
#' to subset the inputted data.
#'
#' Samples with only a single visit are removed. Additionally, samples that
#' have values that differ significantly (stddev greater than a specified threshold)
#' are also removed.
#'
#' @param inputted.data A dataframe
#' @param col.name.of.unique.identifier A string that specifies name of column in inputted.data containing unique identifiers.
#' @param value.to.evaluate A string that specifies name of column in inputted.data to look at for stability of values.
#' @param standard.deviation.threshold A numeric value that specifies the value of the standard
#' deviation that is considered large enough to say vists for a single sample is too unstable.
#'
#' @return A dataframe where only rows from stable samples remain.
#'
#' @export
#'
#' @examples
#'
#' identifier.col <- c("a", "a", "a", "b", "b", "b", "c")
#' value.col <- c(1, 2, 3, 1, 1, 1, 5)
#' input.data.frame <- as.data.frame(cbind(identifier.col, value.col))
#'
#' results <- RemoveSamplesWithInstability(input.data.frame, "identifier.col", "value.col", 0.5)
#'
#' results
#'
RemoveSamplesWithInstability <- function(inputted.data, col.name.of.unique.identifier, value.to.evaluate, standard.deviation.threshold){

  working.data <- inputted.data

  standard.deviation.vals <- StabilityTestingAcrossVisits(inputted.data, col.name.of.unique.identifier, value.to.evaluate)

  #Remove samples with instability from the data
  samples.with.multiple.visits <- stats::na.omit(standard.deviation.vals) #NA for SD means there was only one visit
  samples.with.multiple.visits.and.stable <- subset(samples.with.multiple.visits, samples.with.multiple.visits[,2]<standard.deviation.threshold)
  samples.with.multiple.visits.and.stable.names <- samples.with.multiple.visits.and.stable[,1]
  data.from.stable.samples <- subset(working.data, is.element(working.data[,col.name.of.unique.identifier], samples.with.multiple.visits.and.stable.names))

  return(data.from.stable.samples)

}



#' Randomly select one row
#'
#' If multiple rows contain the same identifier for a column, then
#' randomly select just one row. Do this for all identifiers and output a new dataframe
#' where each identifier now only has one row.
#'
#' @param inputted.data A dataframe.
#' @param col.name.of.unique.identifier Name of column in inputted.data containing identifiers.
#' @param seed Number indicating the seed to set for random number generation.
#'
#' @return A dataframe where a single row remains for each identifier.
#'
#' @export
#'
#' @examples
#' identifier.col <- c("a", "a", "a", "b", "b", "b", "c")
#' value.col <- c(1, 2, 3, 1, 1, 1, 5)
#' input.data.frame <- as.data.frame(cbind(identifier.col, value.col))
#'
#' results <- RanomlySelectOneRowForEach(input.data.frame, "identifier.col", 1)
#'
#' results
RanomlySelectOneRowForEach <- function(inputted.data, col.name.of.unique.identifier, seed){

  working.data <- inputted.data

  rows.to.keep <- NULL
  for(ID in unique(working.data[,col.name.of.unique.identifier]))
  {
    #Select all row numbers for the unique ID
    #all.rows.with.ID <- rownames(subset(working.data, working.data[,col.name.of.unique.identifier]==ID))
    all.rows.with.ID <- rownames(subset(working.data[working.data[,col.name.of.unique.identifier]==ID, ]))

    #Sample the row numbers to randomly select 1 row by discarding the other rows.
    set.seed(seed)
    single.row.to.keep <- sample(all.rows.with.ID, 1)
    rows.to.keep <- c(rows.to.keep, single.row.to.keep)
  }

  working.data <- working.data[rows.to.keep,]

  return(working.data)

}


#' Remove outliers based on Z score of a particular variable
#'
#' If the Z-score of a sample for the selected column corresponds with a p-value
#' less than 0.05, then the sample is considered an outlier and removed.
#'
#' @param inputted.data A dataframe
#' @param column.to.perform.outlier.analysis Name of column in dataframe to evaluate for outliers. The column should contain continuous data.
#'
#' @return A dataframe with outlier rows removed.
#'
#' @export
#'
#' @examples
#'
#'
#' identifier.col <- c("a", "a", "a", "b", "b", "b", "c")
#' value.col <- c(1, 2, 3, 1, 1, 1, 100)
#' input.data.frame <- as.data.frame(cbind(identifier.col, value.col))
#'
#' results <- ZScoreChallengeOutliers(input.data.frame, "value.col")
#'
#' results
#'
ZScoreChallengeOutliers <- function(inputted.data, column.to.perform.outlier.analysis){

  temp1 <- as.numeric(inputted.data[, column.to.perform.outlier.analysis])
  running.results <- NULL
  for (i in 1:length(temp1)) {
    temp2 <- temp1[-c(i)]
    temp3 <- mean(temp2)
    temp4 <- stats::sd(temp2)
    temp5 <- temp1[c(i)]
    temp6 <- 1 - stats::pnorm((abs(temp5-temp3))/temp4)
    running.results <- c(running.results,temp6)
  }
  modified.input.data <- inputted.data[running.results>=0.05,]

  return(modified.input.data)

}


#' Generate PC1 vs PC2 plots with and without outliers.
#'
#' Generate PC1 vs PC2 plots to visualize data with and without outliers.
#' also ouputs dataset with outliers removed.
#'
#' Outliers are defined as samples with either PC1 or PC2 values that
#' have a standard deviation value that meets a specified p-value
#' threshold.
#'
#' @param inputted.data A dataframe.
#' @param columns.to.do.PCA.on A vector of strings that specify the column names that should be used for doing PCA.
#' @param scale.PCA Boolean to specify whether or not to scale columns before doing PCA.
#' @param p.value.for.outliers Outliers are defined as samples with either PC1 or PC2 values that
#' have a standard deviation value that meets a specified p-value
#' threshold.
#'
#' @return A List with two objects:
#' 1. Data after removing outliers for PC1 and PC2.
#' 2. Data from outliers.
#'
#' Plots will also be displayed.
#'
#' @export
#'
#'
GeneratePC1andPC2PlotsWithAndWithoutOutliers <- function(inputted.data, columns.to.do.PCA.on, scale.PCA, p.value.for.outliers){

  original.working.data <- inputted.data
  working.data <- inputted.data

  #------------------------------------------------------------------------------
  # Exploratory PCA On Samples Using Non Zero Volume Data (1 of 3)
  #------------------------------------------------------------------------------
  volume.data <- working.data[,columns.to.do.PCA.on]
  pca.results <- stats::prcomp(volume.data,scale=scale.PCA)
  totalvar <- (pca.results[[1]]^2)
  variancePer <- round(totalvar/sum(totalvar)*100,1)
  variancePer <- variancePer[1:3]
  pca.x <- pca.results$x[,c(1:3)]
  grDevices::palette("default")
  x.range <- range(pca.x[,1])
  y.range <- range(pca.x[,2])
  xlab <- paste(c("Principal Component 1 (",variancePer[1],"%)"),collapse="")
  ylab <- paste(c("Principal Component 2 (",variancePer[2],"%)"),collapse="")
  grDevices::dev.new()
  grDevices::dev.new()
  graphics::par(mfrow=c(1,3))
  plot(pca.x[,1],pca.x[,2],type='n',xlab=xlab,ylab=ylab,bg="gray",main="PCA (Pre Outlier Curation)")
  graphics::points(pca.x[,1],pca.x[,2],lwd=0.5,col=8,cex=2,pch=19)
  graphics::points(pca.x[,1],pca.x[,2],lwd=0.5,col=1,cex=2,pch=21)


  #------------------------------------------------------------------------------
  # Zscore Challenge Samples For Outliers Using PC1
  #------------------------------------------------------------------------------

  temp1 <- pca.x[,1]
  running.results <- NULL
  for (i in 1:length(temp1)) {
    temp2 <- temp1[-c(i)]
    temp3 <- mean(temp2)
    temp4 <- stats::sd(temp2)
    temp5 <- temp1[c(i)]
    temp6 <- 1 - stats::pnorm((abs(temp5-temp3))/temp4)
    running.results <- c(running.results,temp6)
  }
  working.data <- working.data[running.results>=p.value.for.outliers,]
  dim(working.data)[1]

  #------------------------------------------------------------------------------
  # Zscore Challenge Samples For Outliers Using PC2
  #------------------------------------------------------------------------------

  temp1 <- pca.x[,2]
  running.results <- NULL
  for (i in 1:length(temp1)) {
    temp2 <- temp1[-c(i)]
    temp3 <- mean(temp2)
    temp4 <- stats::sd(temp2)
    temp5 <- temp1[c(i)]
    temp6 <- 1 - stats::pnorm((abs(temp5-temp3))/temp4)
    running.results <- c(running.results,temp6)
  }
  temp7 <- names(running.results)[running.results>=p.value.for.outliers]
  temp8 <- intersect(dimnames(working.data)[[1]],temp7)
  working.data <- working.data[temp8,]
  dim(working.data)[1]

  #------------------------------------------------------------------------------
  # Generate Remaining 2 PCAs
  #------------------------------------------------------------------------------

  temp1 <- dimnames(working.data)[[1]]
  temp2 <- dimnames(pca.x)[[1]]
  temp3 <- setdiff(temp2,temp1)
  temp4 <- rep(8,length(temp1))
  names(temp4) <- temp1
  temp5 <- rep(2,length(temp3))
  names(temp5) <- temp3
  temp6 <- c(temp4,temp5)
  temp6 <- temp6[temp2]
  temp6 <- as.numeric(temp6)
  plot(pca.x[,1],pca.x[,2],type='n',xlab=xlab,ylab=ylab,bg="gray",main="PCA (Outliers Identified = Red)")
  graphics::points(pca.x[,1],pca.x[,2],lwd=0.5,col=temp6,cex=2,pch=19)
  graphics::points(pca.x[,1],pca.x[,2],lwd=0.5,col=1,cex=2,pch=21)
  volume.data <- working.data[,columns.to.do.PCA.on]
  pca.results <- stats::prcomp(volume.data,scale=scale.PCA)
  totalvar <- (pca.results[[1]]^2)
  variancePer <- round(totalvar/sum(totalvar)*100,1)
  pca.x <- pca.results$x[,c(1:3)]
  grDevices::palette("default")
  x.range <- range(pca.x[,1])
  y.range <- range(pca.x[,2])
  xlab <- paste(c("Principal Component 1 (",variancePer[1],"%)"),collapse="")
  ylab <- paste(c("Principal Component 2 (",variancePer[2],"%)"),collapse="")
  plot(pca.x[,1],pca.x[,2],type='n',xlab=xlab,ylab=ylab,bg="gray",main="PCA (Post Outlier Curation)")
  graphics::points(pca.x[,1],pca.x[,2],lwd=0.5,col=8,cex=2,pch=19)
  graphics::points(pca.x[,1],pca.x[,2],lwd=0.5,col=1,cex=2,pch=21)

  working.data.without.outliers <- working.data

  outlier.data.ID <- setdiff(row.names(original.working.data), row.names(working.data))
  outlier.data <- original.working.data[outlier.data.ID,]


  output <- list(working.data.without.outliers, outlier.data)

  return(output)

}


#' Create elbow plot to see how much total variance is explained by the components
#'
#' @param inputted.data A dataframe.
#' @param column.to.do.PCA.on A vector of strings that specify the column names that should be used for doing PCA.
#' @param scale.PCA Boolean to specify whether or not to scale columns before doing PCA.
#'
#' @return No object is returned. A plot will be created.
#'
#' @export
#'
GenerateElbowPlotPCA <- function(inputted.data, column.to.do.PCA.on, scale.PCA){

  volume.data <- inputted.data[,column.to.do.PCA.on]
  pca.results <- stats::prcomp(volume.data,scale=TRUE)
  totalvar <- (pca.results[[1]]^2)
  variancePer <- round(totalvar/sum(totalvar)*100,1)

  grDevices::dev.new()
  graphics::par(mfrow=c(1,2))
  plot(variancePer,type='b',xlab="PC",ylab="% Total Variance Explained",main="Elbow Plot (All PCs)")
  plot(variancePer[1:20],type='b',xlab="PC",ylab="% Total Variance Explained",main="Elbow Plot (First 20 PCs)")

}


#' Subset data by two bounds on a continuous column
#'
#' Example. If you want to only keep the bottom 25% and top 75% of data, then
#' the lower.bound.percent will be 0.25 and the upper.bound.percent will be 0.75.
#'
#' @param inputted.data A dataframe.
#' @param col.name.to.subset.on Name of column with continuous data for subsetting.
#' @param lower.bound.percent A numeric value from 0 to 1 that specifies the first bound.
#' @param upper.bound.percent A numeric value from 0 to 1 that specifies the second bound.
#'
#' @return A subsetted dataframe.
#'
#' @export
#'
SubsetDataByContinuousCol <- function(inputted.data, col.name.to.subset.on, lower.bound.percent, upper.bound.percent){

  working.data <- inputted.data

  lower.bound = as.numeric(stats::quantile(working.data[,col.name.to.subset.on], lower.bound.percent))
  upper.bound = as.numeric(stats::quantile(working.data[,col.name.to.subset.on], upper.bound.percent))
  working.data <- working.data[as.logical((working.data[,col.name.to.subset.on]<lower.bound) + (working.data[,col.name.to.subset.on]>upper.bound)),]

  return(working.data)

}

#' Do Log2 transformation on a column, and then compare with and without log2 transformation
#'
#' @param inputted.data A dataframe.
#' @param col.name.to.log Name of column to do log transformation on. Column should be numerical continuous.
#'
#' @return No object is returned. A plot will be made.
#'
#'
#' @export
#'
Log2TargetDensityPlotComparison <- function(inputted.data, col.name.to.log){

  working.data <- inputted.data

  grDevices::dev.new()
  graphics::par(mfrow=c(1,2))
  plot(stats::density(working.data[,col.name.to.log]),main="No Log2 Transform")
  graphics::abline(v=mean(working.data[,col.name.to.log],trim=0.05),lty=2)
  plot(stats::density(log2(working.data[,col.name.to.log])), main="With Log2 Transform")
  graphics::abline(v=mean(log2(working.data[,col.name.to.log]),trim=0.05),lty=2)

}


#' Bin the values of a selected continuous column into 4 bins (quartiles) and add the bin assignments as a new column
#'
#'
#' @param inputted.data A dataframe.
#' @param col.name.to.bin Name of a continuous column to use the values for binning.
#' @param name.of.new.col Name of the new column that will be added to the end of the dataframe. A suitable name would be "TARGET.quartile"
#'
#' @return A dataframe with bin assignment added as a factor column.
#'
#' @export
#'
AddColBinnedToQuartiles <- function(inputted.data, col.name.to.bin, name.of.new.col){

  working.data <- inputted.data

  ##summary() function. Setting levels to min-25%, >=25%-50%, >=50%-75%, >=75%
  first.qu = summary(working.data[,col.name.to.bin])[[2]]
  median = summary(working.data[,col.name.to.bin])[[3]]
  third.qu = summary(working.data[,col.name.to.bin])[[5]]
  TARGET.quartile <- rep(NA, dim(working.data)[1])
  for (index in 1:dim(working.data)[1]) {
    if(working.data[index,col.name.to.bin]<first.qu)
    {
      TARGET.quartile[index] = 1
    } else if(working.data[index,col.name.to.bin]<median) {
      TARGET.quartile[index] = 2
    } else if(working.data[index,col.name.to.bin]<third.qu) {
      TARGET.quartile[index] = 3
    } else{
      TARGET.quartile[index] = 4
    }
  }
  working.data <- cbind(working.data, TARGET.quartile)
  working.data <- data.frame(working.data)
  working.data[,"TARGET.quartile"] <- as.factor(working.data[,"TARGET.quartile"])

  colnames(working.data)[which(names(working.data) == "TARGET.quartile")] <- name.of.new.col

  return(working.data)

}


#' Bin the values of a selected continuous column into 2 bins (halves) and add the bin assignments as a new column
#'
#' @param inputted.data A dataframe.
#' @param col.name.to.bin Name of a continuous column to use the values for binning.
#' @param name.of.new.col Name of the new column that will be added to the end of the dataframe.
#'
#' @return A dataframe with bin assignment added as a factor column.
#'
#' @export
#'
AddColBinnedToBinary <- function(inputted.data, col.name.to.bin, name.of.new.col){

  working.data <- inputted.data

  TARGET.bin <- ifelse(working.data[,col.name.to.bin] < mean(working.data[,col.name.to.bin], trim=0.05), 0, 1)
  working.data <- cbind(working.data, TARGET.bin)
  working.data[,"TARGET.bin"] <- as.factor(working.data[,"TARGET.bin"])

  colnames(working.data)[which(names(working.data) == "TARGET.bin")] <- name.of.new.col

  return(working.data)

}

#LookAtPCFeatureLoadings
#' Principal component feature loadings
#'
#' @param pca.results Object outputted from stats::prcomp()
#' @param pc.to.look.at Numeric value indicating which principal component to look at.
#'
#' @return No objects are outputted but a plot is created.
#'
#' @export
#'
#'
LookAtPCFeatureLoadings <- function(pca.results, pc.to.look.at) {

  dev.new()
  pc.to.grab.and.look <- pc.to.look.at
  loadings <- pca.results[[2]]
  lookie <- loadings[,pc.to.grab.and.look]
  par(cex.axis=0.8, mar=c(15, 5, 5, 2))
  barplot(rev(sort(lookie)),las=2,ylab="Loading",main=paste(c("Feature loadings for PC ",pc.to.grab.and.look),collapse="",sep=""))
  abline(h=0)
  box()

}


#' Split into train and test
#'
#' @param inputted.data A dataframe.
#' @param percent.to.train.with A number from 0 to 1 indicating percentage of data to be used for training.
#' @param seed A number to set the seed for random number generation.
#'
#' @return List with two objects. Train and test data, in that order.
#'
#' @export
#'
SplitIntoTrainTest <- function(inputted.data, percent.to.train.with, seed) {

  set.seed(seed)
  train <- sample(c(1:dim(inputted.data)[1]), round(dim(inputted.data)[1]*percent.to.train.with, 0))
  test <- c(1:dim(inputted.data)[1])[-c(train)]
  train <- inputted.data[train,]
  test <- inputted.data[test,]

  output <- list(train, test)

  return(output)
}


#' Remove rows from the dataframe if the row contains a value in the specified columns
#'
#' @param inputted.data A dataframe.
#' @param columns.to.look.at A vector of strings with each string specifying the name of columns to look at.
#' @param val.to.remove Value to look for. If a row contains this value in the specified column(s), then the row is removed.
#'
#' @return Dataframe with some rows removed.
#' @export
#'
RemoveRowsBasedOnCol <- function(inputted.data, columns.to.look.at, val.to.remove) {

  working.data <- inputted.data

  for(column.name in columns.to.look.at)
  {
    working.data <- working.data[working.data[,column.name] != val.to.remove, ]
  }

  return(working.data)

}

#' Use histograms and boxplots to get an general idea of what data looks like
#'
#' Creates a subplot for each specified feature. Each subplot can be histogram
#' or a boxplot.
#'
#' @param inputted.data A dataframe.
#' @param overall.plot.layout A vector with two elements to specify how many rows and columns to include in the par plot. c(rows, cols)
#' @param plot.type A vector that specifies, for each subplot, should features be displayed in histogram or boxplot form. Elements can be "histogram" or "boxplot".
#' @param plot.features A vector that specifies, for each subplot, what features should be analyzed. The elements should be names of columns that contain continous numerical data.
#' @param plot.name A string for the name of the plot.
#'
#' @return No object is outputted, but a plot is displayed.
#'
#' @export
#'
#' @examples
#'
#' first.col <- c(10, 10, 30, 50, 20 , 10, 30)
#' second.col <- c(1, 2, 3, 1, 1, 1, 1)
#' input.data.frame <- as.data.frame(cbind(first.col, second.col))
#'
#' generate.descriptive.plots(input.data.frame,
#'                           c(1, 2),
#'                           c("boxplot", "histogram"),
#'                           c("first.col", "second.col"),
#'                           "Example")
#'
#'
generate.descriptive.plots <- function(inputted.data, overall.plot.layout, plot.type, plot.features, plot.name){

  dev.new()
  par(mfrow = overall.plot.layout)
  for(index in 1:length(plot.type))
  {

    if(plot.type[[index]] == "boxplot")
    {
      boxplot(inputted.data[,plot.features[[index]]], main=c(plot.features[[index]],plot.name))
    }else if(plot.type[[index]] == "histogram")
    {
      hist(inputted.data[,plot.features[[index]]], main=c(plot.features[[index]],plot.name))
    }
    else
    {
      stop('Invalid entry in the plot.type vector. Can only be boxplot or histogram')
    }
  }

}

#' Use histograms and boxplots to get an general idea of what data looks like
#'
#' Creates a subplot for each specified feature. Each subplot can be histogram
#' or a boxplot. Same as the generate.descriptive.plots() function except
#' this function saves the plot to a pdf file.
#'
#' @param inputted.data A dataframe.
#' @param overall.plot.layout A vector with two elements to specify how many rows and columns to include in the par plot. c(rows, cols)
#' @param plot.type A vector that specifies, for each subplot, should features be displayed in histogram or boxplot form. Elements can be "histogram" or "boxplot".
#' @param plot.features A vector that specifies, for each subplot, what features should be analyzed. The elements should be names of columns that contain continous numerical data.
#' @param plot.name A string for the name of the plot. Will be used for the name of the output pdf.
#' @param width Number indicating the width of plot. Default is 5.
#' @param height Number indicating the width of the plot. Default is 8.
#'
#' @return No object is outputted, but a plot is saved as a pdf.
#'
#' @export
#'
#'
#'
generate.descriptive.plots.save.pdf <- function(inputted.data, overall.plot.layout, plot.type,
                                                plot.features, plot.name, width = 5, height = 8){

  dev.new()
  par(mfrow = overall.plot.layout)
  for(index in 1:length(plot.type))
  {

    if(plot.type[[index]] == "boxplot")
    {
      boxplot(inputted.data[,plot.features[[index]]], main=c(plot.features[[index]],plot.name))
    }else if(plot.type[[index]] == "histogram")
    {
      hist(inputted.data[,plot.features[[index]]], main=c(plot.features[[index]],plot.name))
    }
    else
    {
      stop('Invalid entry in the plot.type vector. Can only be boxplot or histogram')
    }
  }

  plot.name <- paste(plot.name, ".pdf")
  dev.copy2pdf(file=plot.name, width = width, height = height)

}


#' Given two numerical data vector, determine the correlation
#'
#' This is a work in progress function.
#'
#' @param data1 Numerical data vector.
#' @param data1.type A string to specify what type of data is data1. Options are "continuous", "ordinal", "categorical".
#' @param data1.name String that specifies name to associate with data1.
#' @param data2 Numerical data vector.
#' @param data2.type A string to specify what type of data is data1. Options are "continuous", "ordinal", "categorical".
#' @param data2.name String that specifies name to associate with data1.
#'
#' @return A vector of strings with two elements:
#' 1. Description of test performed
#' 2. P-value
#'
#' @export
#'
correlation.association.test <- function(data1, data1.type, data1.name, data2, data2.type, data2.name){

  p.value.captured <- NULL
  test.type <- NULL

  #3 * 3 = 9 possible combinations.
  temp0 <- data1
  temp1 <- data2

  if((data1.type == "continuous") && (data2.type == "continuous"))
  {
    #Do Pearson
    test.type <- "Pearson"
    temp2 <- cor.test(temp0,temp1,method="pearson",na.rm=T)
    temp3 <- temp2$estimate
    p.value.captured <- temp2$p.value

  }else if((data1.type == "continuous") && (data2.type == "ordinal"))
  {
    #Do Spearman
    test.type <- "Spearman"
    temp2 <- cor.test(temp0,temp1,method="spearman",na.rm=T)
    temp3 <- temp2$estimate #Rho is the test statistic for Spearman method.
    p.value.captured <- temp2$p.value

  }else if((data1.type == "continuous") && (data2.type == "categorical"))
  {
    #Do ANOVA
    test.type <- "ANOVA"
    temp2 <- data.frame(temp0=temp0,temp1=factor(temp1))
    temp3 <- aov(temp0 ~ temp1, data=temp2,na.rm=T)
    temp4 <- Anova(temp3)
    p.value.captured <- temp4$P[1]

  }else if((data1.type == "ordinal") && (data2.type == "continuous"))
  {
    #Do Spearman. #1.21.21 update: Polyserial could be better.
    test.type <- "Spearman"
    temp2 <- cor.test(temp0,temp1,method="spearman",na.rm=T)
    temp3 <- temp2$estimate #Rho is the test statistic for Spearman method.
    p.value.captured <- temp2$p.value

  }else if((data1.type == "ordinal") && (data2.type == "ordinal"))
  {
    #Do Spearman.
    test.type <- "Spearman"
    temp2 <- cor.test(temp0,temp1,method="spearman",na.rm=T)
    temp3 <- temp2$estimate #Rho is the test statistic for Spearman method.
    p.value.captured <- temp2$p.value

  }else if((data1.type == "ordinal") && (data2.type == "categorical"))
  {
    #Do????????
    warning(paste("Need to setup test in correlation.association.test() for ", data1.name, " and ", data2.name))

  }else if((data1.type == "categorical") && (data2.type == "continuous"))
  {
    #Do ANOVA
    test.type <- "ANOVA"
    temp2 <- data.frame(temp0=factor(temp0),temp1=temp1)
    temp3 <- aov(temp1 ~ temp0, data=temp2,na.rm=T)
    temp4 <- Anova(temp3)
    p.value.captured <- temp4$P[1]

  }else if((data1.type == "categorical") && (data2.type == "ordinal"))
  {
    #Do????????
    warning(paste("Need to setup test in correlation.association.test() for ", data1.name, " and ", data2.name))

  }else if((data1.type == "categorical") && (data2.type == "categorical"))
  {
    test.type <- "Chi-Square"
    tbl <- table(data1, data2)
    chi <- chisq.test(tbl)
    p.value.captured <- chi$p.value

  }

  name.of.test <- paste(test.type, "_", data1.name, "_", data2.name)
  return(c(name.of.test, p.value.captured))

}



#' Takes multiple vectors and do correlation/association testing with all of them
#'
#' Uses correlation.association.test() for multiple pairs of data vectors and
#' then outputs results into a text file.
#'
#' This function can be used for batch effect testing. Example: We can ask
#' if the dependent variable significantly differs based on gender or race.
#' If it does, then it could impact the relationship of the independent
#' variable with the dependent variable. See ancovall for functions that
#' can adjust for covariates like gender and race.
#'
#' @param data.vectors1 List of numerical vectors.
#' @param variable.types1 A vector of strings where each element can be: "continuous", "categorical", or "ordinal". Specifies data type of vectors in data.vectors1.
#' @param names1 A vector strings that specify the names that should go with each numerical vector in data.vectors1.
#' @param data.vectors2 List of numerical vectors.
#' @param variable.types2 A vector of strings where each element can be: "continuous", "categorical", or "ordinal". Specifies data type of vectors in data.vectors2.
#' @param names2 A vector strings that specify the names that should go with each numerical vector in data.vectors2.
#' @param table.name Name of the outputted text file.
#'
#' @return No object is returned, but function creates a text file with the p-values from the testings.
#'
#' @export
#'
CorAssoTestMultipleWithErrorHandling <- function(data.vectors1, variable.types1, names1, data.vectors2, variable.types2, names2, table.name){

  #Columns are data.vectors1
  #Rows are data.vectors2

  #First column just contains name of the second set of variables
  table.to.output <- c("Variable Names", names2)

  column.p.values <- NULL

  for(data.vector1.index in 1:length(data.vectors1))
  {
    #Add the name of the variable as the first row in new column
    col.to.add.to.table <- c(names1[[data.vector1.index]])

    for(data.vector2.index in 1:length(data.vectors2))
    {
      corr.asso.output <- NULL

      tryCatch({

        #Will catch any error from correlation.association.test function
        corr.asso.output <- correlation.association.test(data.vectors1[[data.vector1.index]], variable.types1[[data.vector1.index]], names1[[data.vector1.index]],
                                                         data.vectors2[[data.vector2.index]], variable.types2[[data.vector2.index]], names2[[data.vector2.index]])

      }, error = function(err){

        corr.asso.output <<- c("","Error in correlation.association.test()") #Notice the double arrow, ensures scope at the parent level.
        message(paste("Could not calculate asso/cor for this pair:", names1[[data.vector1.index]], " and ", names2[[data.vector2.index]]))
        message("Here's the original error message:")
        message(err)

        #Does not work
        #corr.asso.output <- c("","Error in correlation.association.test()") #Notice the double arrow, ensures scope at the parent level.
        #return(corr.asso.output = corr.asso.output)

      }, finally = {

      })


      ##Add each p-value as a new row
      col.to.add.to.table <- c(col.to.add.to.table, corr.asso.output[[2]])
    }

    table.to.output <- cbind(table.to.output, col.to.add.to.table)

  }

  write.table(table.to.output, table.name, sep="\t")


}

#' Down sample an imbalanced dataset to get a balanced dataset
#'
#' @param input.data A dataframe.
#' @param name.of.feature.to.balance.on Name of column in the input dataframe. Column has to be a factor. This column is the membership column that you want to balance for.
#' @param seedVal Numeric value to set seed for random number generator.
#'
#' @return List with two objects:
#' 1. ordered_balanced_data Dataframe with order of columns the same as the original data. Dataframe is a subset of the original dataframe because some observations were left out in order to balance the values in the selected column.
#' 2. left_out_data Dataframe of the left out observations.
#'
#' @export
#'
DownSampleDataframe <- function(input.data, name.of.feature.to.balance.on, seedVal){

  #Add index column to observations so that I can use this to determine the left
  #out observations later. Name the index column something that is unique and won't
  #be found in the input dataframe.
  num_of_obs <- dim(input.data)[[1]]
  obs_index_1234 <- 1:num_of_obs

  #Add index to dataframe
  input.data.with.index <- cbind(obs_index_1234, input.data)

  col_order <- colnames(input.data.with.index) #After downsampling, I want the order of features in the dataframe to remain the same.
  x <- input.data.with.index[,setdiff(colnames(input.data.with.index), name.of.feature.to.balance.on)]
  y <- input.data.with.index[,name.of.feature.to.balance.on]

  set.seed(seedVal)
  balanced_data <- caret::downSample(x, y, FALSE, name.of.feature.to.balance.on)

  ordered_balanced_data <- balanced_data[, col_order]

  #The order of arguments in setdiff() matters.
  left_out_data_indices <-  setdiff(obs_index_1234, ordered_balanced_data[,"obs_index_1234"])
  #To get the left out data, we want the rows corresponding to indices not in the balanced data set
  left_out_data <- input.data.with.index[is.element(input.data.with.index$obs_index_1234, left_out_data_indices),]

  #Remove the index column for the two dataframes
  ordered_balanced_data <- ordered_balanced_data[,colnames(input.data)]
  left_out_data <- left_out_data[,colnames(input.data)]

  output <- list(ordered_balanced_data, left_out_data)

  return(output)
}

#' Recode the identifier column of a dataset
#'
#' To protect patient confidentiality, sometimes the identifier in datasets
#' should be recoded.
#'
#' @param input.data Dataframe with the data, including column to recode.
#' @param name.of.identifier String specifying the column name of the column with data to recode.
#' @param file.name Name of the file or absolute path of the file to write the output to.
#'
#' @return A modified dataframe with recoded identifier will be outputted. The recoded data will
#' also be written into a text file.
#'
#' @export
#'
RecodeIdentifier <- function(input.data, name.of.identifier, file.name){

  vector_recoded123 <- as.integer(as.factor(input.data[,name.of.identifier]))

  #Replace column in dataframe with the recoded vector
  recoded_data <- input.data
  recoded_data[, name.of.identifier] <- vector_recoded123

  #Rename the modified column to the original column name.
  names(recoded_data)[names(recoded_data) == 'vector_recoded123'] <- name.of.identifier

  #Write the new dataframe to a new file
  write.table(recoded_data, file.name, append = FALSE, sep = "\t", dec = ".",
              row.names = FALSE, col.names = TRUE)

  return(recoded_data)
}


#' Capture session info
#'
#' Capture session info to keep track of what packages and what versions are
#' being used in the current session. Writes the session info output to a txt
#' file.
#'
#' @return No object is returned. A txt file is created that captures the sessionInfo() output.
#'
#' @export
#'
captureSessionInfo <- function(){

  sink("my_session_info.txt")
  print(sessionInfo())
  sink()
  #closeAllConnections()

  #save.image(file = "my_work_space.RData")

}



#' Describe each numerical feature. Mean, stddev, median, skewness (symmetry), kurtosis (flatness), pass normality?
#'
#' @param input.data A dataframe.
#' @param column.names.to.use Vector of strings of column names. Function will only describe these specified columns (features).
#' @param file.name Name of the text file or absolute path of the file to write the output to. If the input is NULL, then
#' no file is generated.
#'
#' @return A dataframe where the rows are the descriptions and columns are the features.
#' Additionally, a text file will be created which also captured the function output.
#'
#' @export
#'
describeNumericalColumns <- function(input.data, column.names.to.use, file.name = NULL){

  #captured.output <- c("Value", "Mean", "Standard Deviation", "Median", "Min", "Max", "Skewness", "Kurtosis", "Normal?", "Shapiro Test p-value")

  captured.output <- c("Value", "Mean", "Standard Deviation", "Median", "Min", "Max", "Skewness",
                       "Kurtosis", "Shapiro Test p-value")

  for(i in 1:length(column.names.to.use))
  {
    column.to.look.at <- as.numeric(input.data[,column.names.to.use[i]])

    normal.or.not <- "Normal"

    ##THIS NEEDS TO BE RE-EVALUATED
    #if( (abs(moments::skewness(column.to.look.at))) > 1 || (abs(moments::kurtosis(column.to.look.at)) > 1))
    #{
    #  normal.or.not <- "Not_Normal"
    #}

    #Use shapiro test for normality. Note that small sample sizes don't work well for this test.
    shapiro.result.for.col <- shapiro.test(column.to.look.at)

    captured.output.for.column <- c(column.names.to.use[i],
                                    mean(column.to.look.at),
                                    sd(column.to.look.at),
                                    median(column.to.look.at),
                                    min(column.to.look.at),
                                    max(column.to.look.at),
                                    moments::skewness(column.to.look.at),
                                    moments::kurtosis(column.to.look.at),
                                    shapiro.result.for.col$p.value)


    captured.output <- cbind(captured.output, captured.output.for.column)

  }

  if(!is.null(file.name)){

    #Write the new dataframe to a new file
    write.table(captured.output, file.name, append = FALSE, sep = "\t", dec = ".",
                row.names = FALSE, col.names = TRUE)
  }

  return(captured.output)

}



#' For each level, describe each numerical feature. Mean, sd, median, skewness (symmetry), kurtosis (flatness), pass normality?
#'
#' Uses describeNumericalColumns() separately for each unique level in the column
#' corresponding to column.name.containing.levels.within.feature. The results
#' for each level are separated in the output by blank rows.
#'
#'
#' @param input.data A dataframe.
#' @param column.names.to.use Vector of strings of column names. Function will only describe these specified columns (features).
#' @param column.name.containing.levels.within.feature Name of column that contains the levels that you want to separate by.
#' @param file.name Name of the text file or absolute path of the file to write the output to. If the input is NULL, then
#' no file is generated.
#'
#' @return A dataframe where the rows are the descriptions and columns are the features.
#' Additionally, a text file will be created which also captured the function output.
#'
#' The output for each level is separated by blank rows.
#'
#' @export
#'
describeNumericalColumnsWithLevels <- function(input.data, column.names.to.use,
                                               column.name.containing.levels.within.feature, file.name=NULL){

  ##Re-use the above function. Subset data first. Do function on each subset,
  #then combine the results from the subsets by rbind.

  captured.output <- NULL

  levels.in.column <- sort(unique(input.data[,column.name.containing.levels.within.feature]))

  for(i in 1:length(levels.in.column))
  {
    subset.data <- subset(input.data, input.data[,column.name.containing.levels.within.feature]==levels.in.column[i])

    temp.df <- describeNumericalColumns(subset.data, column.names.to.use, file.name)

    #Add a row to specify a name for the output for this level of the factor
    name.of.output <- paste(column.name.containing.levels.within.feature, " value of ", levels.in.column[i])
    temp.df.with.name <- rbind(c(name.of.output, rep(c(""), length(column.names.to.use))), temp.df)

    captured.output <- rbind(captured.output, temp.df.with.name)

    #Add a blank row to separate the different levels
    captured.output <- rbind(captured.output,
                             rep(c(""), length(column.names.to.use)+1) )

  }

  if(!is.null(file.name)){

    #Write the new dataframe to a new file
    write.table(captured.output, file.name, append = FALSE, sep = "\t", dec = ".",
                row.names = FALSE, col.names = FALSE)
  }

  return(captured.output)

}


#' Checks if the data is normally distributed using Shapiro test. If not normal, then boxcox transform.
#'
#' Cannot use boxcox if data has zeroes. If data has zeros, then do this:
#' Add 1 if most values are greater than 1, else  if most of values <1,
#' multiply 10 or 100, then add 1. Then do boxcox.
#'
#'
#' @param input.data A numerical data vector.
#' @param alpha.for.shapiro Numerical value from 0 to 1. Threshold for what is considered not normal.
#' If p-value is less than this threshold, then the data is considered not normal.
#'
#' @return A List with 4 elements:
#' 1. If data is non-normal, then a vector of the transformed data is outputted. If data is normal, then this is NULL.
#' 2. If data is non-normal, then a number specifying the lambda used for boxcox is outputted. If data is normal, then this is NULL.
#' 3. P-value from the Shapiro test.
#' 4. Boolean indicating if boxcox transformation was performed.
#'
#' @export
#'
NormalCheckThenBoxCoxTransform <- function(input.data, alpha.for.shapiro){

  #Check if vector is already normally distributed
  shapiro.result <- shapiro.test(input.data)
  shapiro.result$p.value

  is.transform.true <- shapiro.result$p.value < alpha.for.shapiro

  transformed.vector <- NULL
  lambda <- NULL

  if(is.transform.true == TRUE)
  {
    #If not already normally distributed, then do box cox transform

    feature.to.normalize <- input.data

    #boxcox_results <- boxcox(lm(feature.to.normalize~1))

    #Source: https://stackoverflow.com/questions/26617587/finding-optimal-lambda-for-box-cox-transform-in-r
    #confidence_interval <- range(boxcox_results$x[boxcox_results$y > max(boxcox_results$y)-qchisq(0.95,1)/2])

    #Automatically pick optimal lambda
    lambda = forecast::BoxCox.lambda(feature.to.normalize, method = "loglik")

    #If the data contains zero values, then HERE

    #Transform vector. InvBoxCox() reverses the transformation.
    transformed.vector = forecast::BoxCox(feature.to.normalize, lambda)

    ##How to create plots for the transformed vector
    #c_transformed <- as.data.frame(transformed.vector)
    #feature.to.normalize_description_transformed <- describeNumericalColumns(c_transformed, "transformed.vector", NULL)
  }

  output <- list(transformed.vector, lambda, shapiro.result$p.value, is.transform.true)

  return(output)

}



#' Checks multiple columns in a dataframe to see if each is normally distributed. If not, then box-cox transform
#'
#' @param input.data A dataframe.
#' @param names.of.dependent.variables Vector of strings where each element is the name of a column to assess for normality and potentially transform.
#' @param alpha.for.shapiro Numerical value from 0 to 1. Threshold for what is considered not normal.
#' If p-value is less than this threshold, then the data is considered not normal.
#' @param output.lambda.in.col.name Boolean indicating if the lambda used for boxcox should be included in the column name.
#'
#' @return A dataframe with the columns specified in names.of.dependent.variables.
#' @export
#'
MultipleColumnsNormalCheckThenBoxCox <- function(input.data,
                                                 names.of.dependent.variables,
                                                 alpha.for.shapiro,
                                                 output.lambda.in.col.name = TRUE){

  captured.output <- NULL

  index <- 1

  #Check for normality. If not normal, then transform with boxcox
  for(column.name in names.of.dependent.variables){

    single.column <- input.data[,column.name]

    output <- NormalCheckThenBoxCoxTransform(single.column, alpha.for.shapiro)

    #This will be null if the NormalCheckThenBoxCoxTransform() function determines that
    #the column values are already normal.
    single.column.transformed <- unlist(output[1])

    if(is.null(single.column.transformed)){

      captured.output <- cbind(captured.output, single.column)
      colnames(captured.output)[index] <- column.name

    } else{

      #If transformation is required, then
      captured.output <- cbind(captured.output, single.column.transformed)

      #Determine if lambda should be added to col name
      if(output.lambda.in.col.name)
      {

        column.name.with.lambda <- paste(column.name, unlist(output[2]))
        colnames(captured.output)[index] <- column.name.with.lambda

      }else{

        column.name <- paste(column.name)
        colnames(captured.output)[index] <- column.name

      }


    }

    index = index + 1
  }

  return(as.data.frame(captured.output))

}


#' Use percentiles to assess for outliers in multidimensional data
#'
#' Takes a dataframe of values where the columns are continuous features
#' to be assess for outliers and rows are observations. For each feature, each observation
#' will receive a percentile rank for that feature. At the end, a column will be added
#' to the data that tallies how many features in a single observation has percentile
#' ranks in the top and bottom specified percentiles; this will be done for every observation.
#' Observations with many features in the top and bottom percentiles will be considered as
#' potential outliers. The tally column can be used to locate observations
#' that have many features with extreme values. As a result, the column
#' can be used to assess for potential outliers.
#'
#' @param input.data A dataframe with the columns as continous values to be converted to percentile rank
#' @param upper_lower_bound_threshold A number from 0 to 1. The tails that should be considered as percentiles that are
#' too large or too small (upper_lower_bound_threshold, 1-upper_lower_bound_threshold)
#'
#' @return Original dataframe but values are converted to percentile and an additional column
#' is added to tally up how many features in each observation has an usually large or small percentile rank.
#'
#' @export
#'
ConvertDataToPercentiles <- function(input.data, upper_lower_bound_threshold){

  #1.apply ecdf()() to each column.
  input.data.ranked <- apply(input.data, 2, function(c) ecdf(c)(c))

  #2.for each observation, go through all the columns and tally up
  #how many features pass the threshold.
  captured.tallies <- NULL

  #Go through each observation
  for(i in 1:dim(input.data)[1])
  {
    tallied <- 0

    #Go through each feature
    for(x in 1:dim(input.data)[2])
    {
      if((input.data.ranked[i,x] < upper_lower_bound_threshold) || (input.data.ranked[i,x] > (1-upper_lower_bound_threshold)))
      {
        tallied <- tallied + 1
      }
    }

    captured.tallies <- rbind(captured.tallies, tallied)
  }

  #3.add tally to the dataframe
  input.data.ranked <- cbind(input.data.ranked, captured.tallies)

  #Add column name to the tallied column
  col.name.tally <- paste("Tallied features with percentile rank >", (1-upper_lower_bound_threshold),
                          "or <", (upper_lower_bound_threshold))

  colnames(input.data.ranked)[dim(input.data)[2] + 1] <- col.name.tally

  return(input.data.ranked)

}




#' Performs two sample t-test on multiple features
#'
#'
#' @param input.data A dataframe.
#' @param column.names.to.use Vector of strings where each string is the name of a column of numerical data to use for t-test.
#' @param name.of.class.column Name of column that contains labels for group1 and labels for group2. The labels can be any value.
#' For example, for group1 observations can have value of "1" for this column while group2 observations can have value of  "2" for this column.
#' @param file.name Name of the file to write output to.
#'
#' @return Dataframe where the rows are statistic values from the t-test and columns
#' are the features.
#'
#' @export
#'
TwoSampleTTest <- function(input.data, column.names.to.use, name.of.class.column, file.name){

  #column.names.to.use are the features

  #Testing conditions
  #input.data <- working.data
  #column.names.to.use <- names.of.dependent.variables
  #name.of.class.column <- target.column.name
  #file.name <- "TwosampleTTest.txt"

  captured.output <- c("Value", "t-value", "df", "p-value")

  class.column.as.factor <- as.factor(input.data[,name.of.class.column] )
  class.levels <- levels(class.column.as.factor)

  #Values for class 1
  values.for.class.one <- subset(input.data, input.data[,name.of.class.column]==class.levels[1])

  #Values for class 2
  values.for.class.two <- subset(input.data, input.data[,name.of.class.column]==class.levels[2])

  for(i in 1:length(column.names.to.use))
  {
    feature.values.for.class.one <- values.for.class.one[,column.names.to.use[i]]

    feature.values.for.class.two <- values.for.class.two[,column.names.to.use[i]]


    #For each column, do two sample t-test
    res <- t.test(feature.values.for.class.one, feature.values.for.class.two)

    captured.output.for.column <- c(column.names.to.use[i],
                                    res$statistic,
                                    res$parameter,
                                    res$p.value)


    captured.output <- cbind(captured.output, captured.output.for.column)

  }

  if(!is.null(file.name)){

    #Write the new dataframe to a new file
    write.table(captured.output, file.name, append = FALSE, sep = "\t", dec = ".",
                row.names = FALSE, col.names = TRUE)
  }

  return(captured.output)

}
