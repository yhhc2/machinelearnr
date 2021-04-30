#' Example dataset to demonstrate clustering functions.
#'
#' The dataset has data for two classes (1 and 2). 7 columns of continuous
#' data are provided with values that range from most separation of the two
#' classes to less separation of the two classes.
#'
#' @format A data frame with 102 rows and 9 variables:
#' \describe{
#'   \item{SampleID}{ID from 1 to 101}
#'   \item{Target}{Class assigned to each observation}
#'   \item{Measurement1_Exact_to_Target}{Copied target column}
#'   \item{Measurement2_0.01_sum_Correlation_to_Target}{Copied Measurement7_Complete_Random, then added 0.1 to only rows corresponding with Target 1}
#'   \item{Measurement2_0.01_sum_Correlation_to_Target}{Copied Measurement7_Complete_Random, then added 0.1 to only rows corresponding with Target 1}
#'   ...
#' }
#' @source Generated using Excel with RAND()
"Example.data.for.cluster"
