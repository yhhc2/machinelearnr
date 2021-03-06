% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{RanomlySelectOneRowForEach}
\alias{RanomlySelectOneRowForEach}
\title{Randomly select one row}
\usage{
RanomlySelectOneRowForEach(inputted.data, col.name.of.unique.identifier, seed)
}
\arguments{
\item{inputted.data}{A dataframe.}

\item{col.name.of.unique.identifier}{Name of column in inputted.data containing identifiers.}

\item{seed}{Number indicating the seed to set for random number generation.}
}
\value{
A dataframe where a single row remains for each identifier.
}
\description{
If multiple rows contain the same identifier for a column, then
randomly select just one row. Do this for all identifiers and output a new dataframe
where each identifier now only has one row.
}
\examples{
identifier.col <- c("a", "a", "a", "b", "b", "b", "c")
value.col <- c(1, 2, 3, 1, 1, 1, 5)
input.data.frame <- as.data.frame(cbind(identifier.col, value.col))

results <- RanomlySelectOneRowForEach(input.data.frame, "identifier.col", 1)

results
}
\seealso{
Other Preprocessing functions: 
\code{\link{AddColBinnedToBinary}()},
\code{\link{AddColBinnedToQuartiles}()},
\code{\link{AddPCsToEnd}()},
\code{\link{ConvertDataToPercentiles}()},
\code{\link{CorAssoTestMultipleWithErrorHandling}()},
\code{\link{DownSampleDataframe}()},
\code{\link{GenerateElbowPlotPCA}()},
\code{\link{GeneratePC1andPC2PlotsWithAndWithoutOutliers}()},
\code{\link{Log2TargetDensityPlotComparison}()},
\code{\link{LookAtPCFeatureLoadings}()},
\code{\link{MultipleColumnsNormalCheckThenBoxCox}()},
\code{\link{NormalCheckThenBoxCoxTransform}()},
\code{\link{RecodeIdentifier}()},
\code{\link{RemoveColWithAllZeros}()},
\code{\link{RemoveRowsBasedOnCol}()},
\code{\link{RemoveSamplesWithInstability}()},
\code{\link{SplitIntoTrainTest}()},
\code{\link{StabilityTestingAcrossVisits}()},
\code{\link{SubsetDataByContinuousCol}()},
\code{\link{TwoSampleTTest}()},
\code{\link{ZScoreChallengeOutliers}()},
\code{\link{captureSessionInfo}()},
\code{\link{correlation.association.test}()},
\code{\link{describeNumericalColumnsWithLevels}()},
\code{\link{describeNumericalColumns}()},
\code{\link{generate.descriptive.plots.save.pdf}()},
\code{\link{generate.descriptive.plots}()}
}
\concept{Preprocessing functions}
