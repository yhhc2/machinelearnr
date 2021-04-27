
#' Remove columns with all zeros
#'
#' @param inputted.data A dataframe.
#' @param columns.to.look.at A vector of strings that specify the column names to look at. If no value is provided, then all columns are evaluated.
#'
#' @return A new dataframe with all-zero columns removed.
#'
#' @export
#'
#' @examples
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
#' @examples
AddPCsToEnd <- function(inputted.data, columns.to.do.PCA.on, scale.boolean, minimum.variance.percent){

  pca.results <- prcomp(inputted.data[, columns.to.do.PCA.on], scale=scale.boolean)
  totalvar <- (pca.results[[1]]^2)
  varianceper <- round((totalvar/sum(totalvar))*100,1)
  pca.x <- pca.results$x[,varianceper >= minimum.variance.percent]

  modified.input.data <- cbind(inputted.data, pca.x)
  PC.names <- colnames(pca.x)

  output <- list(modified.input.data, PC.names, pca.results)

  return(output)

}

#StabilityTestingAcrossVisits()

#RemoveSamplesWithInstability()

#RanomlySelectOneRowForEach()

#ZScoreChallengeOutliers()

#GeneratePC1andPC2PlotsWithAndWithoutOutliers()

#GenerateElbowPlotPCA()

#RandomizeRows()

#SubsetDataByContinuousCol()

#Log2TargetDensityPlotComparison

#AddColBinnedToQuartiles()

#AddColBinnedToBinary()

#AddBinaryDummyCol()

#RemoveRowsBasedOnCol()

#generate.descriptive.plots()

#generate.descriptive.plots.save.pdf()

#correlation.association.test()

#CorAssoTestMultiple()

#CorAssoTestMultipleWithErrorHandling()

#downSampleDataframe()

#RecodeIdentifier()

#captureSessionInfo()

#describeNumericalColumns()

#describeNumericalColumnsWithLevels()

#NormalCheckThenBoxCoxTransform()

#NormalCheckThenYeoJohnsonTransform()

#MultipleColumnsNormalCheckThenBoxCox()

#ConvertDataToPercentiles()

#TwoSampleTTest()
