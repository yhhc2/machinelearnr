% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{RemoveSamplesWithInstability}
\alias{RemoveSamplesWithInstability}
\title{Remove samples that have multiple values for a single column and those
values are unstable}
\usage{
RemoveSamplesWithInstability(
  inputted.data,
  col.name.of.unique.identifier,
  value.to.evaluate,
  standard.deviation.threshold
)
}
\arguments{
\item{inputted.data}{A dataframe}

\item{col.name.of.unique.identifier}{A string that specifies name of column in inputted.data containing unique identifiers.}

\item{value.to.evaluate}{A string that specifies name of column in inputted.data to look at for stability of values.}

\item{standard.deviation.threshold}{A numeric value that specifies the value of the standard
deviation that is considered large enough to say vists for a single sample is too unstable.}
}
\value{
A dataframe where only rows from stable samples remain.
}
\description{
This function uses the StabilityTestingAcrossVisits() function, and then uses the results
to subset the inputted data.
}
\details{
Samples with only a single visit are removed. Additionally, samples that
have values that differ significantly (stddev greater than a specified threshold)
are also removed.
}
\examples{

identifier.col <- c("a", "a", "a", "b", "b", "b", "c")
value.col <- c(1, 2, 3, 1, 1, 1, 5)
input.data.frame <- as.data.frame(cbind(identifier.col, value.col))

results <- RemoveSamplesWithInstability(input.data.frame, "identifier.col", "value.col", 0.5)

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
\code{\link{RanomlySelectOneRowForEach}()},
\code{\link{RecodeIdentifier}()},
\code{\link{RemoveColWithAllZeros}()},
\code{\link{RemoveRowsBasedOnCol}()},
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
