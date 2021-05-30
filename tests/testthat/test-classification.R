library(machinelearnr)

#DONE find.best.number.of.trees 
#Test if the output is the expected type. 

#eval.classification.results #No need to test

#DONE RandomForestAutomaticMtryAndNtree 
#Test if the output is the expected type.


#DONE LOOCVPredictionsRandomForestAutomaticMtryAndNtree
#Test if the output is the expected type. 
##Expected number of objects in list
###Expected number of observations in first object


#DONE RandomForestClassificationGiniMatrixForPheatmap
#Test if output is what you would get if you did things manually. 

#DONE RandomForestClassificationPercentileMatrixForPheatmap
##Test if output is what you would get if you did things manually. 


#DONE LOOCVRandomForestClassificationMatrixForPheatmap
##Test if output is what you would get if you did things manually. 



#-----------------------------------------------
#New functions to add. Specify the cross validation
#fold. If the function works, then N fold
#should have the same results as using the LOOCV
#function


#DONE CVPredictionsRandomForestAutomaticMtryAndNtree

#DONE CVRandomForestClassificationMatrixForPheatmap



#------------------------------------------------
# Tests begin here
#------------------------------------------------

test_that("find.best.number.of.trees works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  set.seed(1)
  rf.result <- randomForest::randomForest(x=example.data[,c("x", "y", "a", "b")], y=example.data[,"actual"])
  
  error.oob <- rf.result[[4]][,1]
  
  result <- find.best.number.of.trees(error.oob)
  
  expect_equal(length(result), 1)
  
})


test_that("RandomForestAutomaticMtryAndNtree works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  rf.result <- RandomForestAutomaticMtryAndNtree(example.data, c("x", "y", "a", "b"), "actual", seed=2)
  
  #Should result in a rf object
  expect_equal(class(rf.result), "randomForest")
  
  #The error rate of the classification model should be very low because 
  #the test data purposely has several features that can separate the target. 
  expect_equal(rf.result$err.rate[length(rf.result$err.rate[,1])] < 0.1, TRUE)
  
})


test_that("LOOCVPredictionsRandomForestAutomaticMtryAndNtree works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  invisible(capture.output(
    result <- LOOCVPredictionsRandomForestAutomaticMtryAndNtree(example.data,
                              predictors.that.PCA.can.be.done.on = c("x", "y", "a", "b"),
                              predictors.that.should.not.PCA = NULL,
                              should.PCA.be.used = FALSE,
                              target.column.name = "actual",
                              seed=2,
                              percentile.threshold.to.keep = 0.5)
  ))
  
  #Output should be a list with two objects
  expect_equal(length(result), 2)
  
  #First object should be a vector of predicted values for each observation
  expect_equal(length(result[[1]]), length(example.data[,1]))
  
  #Second object should be table with number of entries equal to number of features
  #identified as important.
  expect_equal(class(result[[2]]), "table")
  
  #The predicted values should be very close to the actual values
  #because the test data purposely has several features that can separate the target.
  actual <- example.data$actual
  predicted <- result[[1]]
  expect_equal(mltools::mcc(preds = as.integer(predicted), actuals = as.integer(actual))>0.8, TRUE)
  
})


test_that("RandomForestClassificationGiniMatrixForPheatmap works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  invisible(capture.output(
    matrix.for.pheatmap <- RandomForestClassificationGiniMatrixForPheatmap(input.data = example.data,
                                            factor.name.for.subsetting = "sep.xy.ab",
                                            name.of.predictors.to.use = c("x", "y", "a", "b"),
                                            target.column.name = "actual",
                                            seed = 2)
  ))
  
  #The resulting matrix should have 5 rows (4 rows for features and 1 row for MCC)
  expect_equal(dim(matrix.for.pheatmap)[1], 5)
  
  #The resulting matrix should have 2 columns. 1 column for each level of the factor
  #used for subetting
  expect_equal(dim(matrix.for.pheatmap)[2], 2)
  
  #The last row of the column should hold the MCC value
  expect_equal(row.names(matrix.for.pheatmap)[[length(row.names(matrix.for.pheatmap))]], "MCC.val")
  
  #The MCC values for each subset should be very high because the testing data
  #was created in a way where this is true.
  MCC_val_row <- matrix.for.pheatmap[dim(matrix.for.pheatmap)[1],]
  expect_equal(MCC_val_row[[1]]>0.8, TRUE)
  expect_equal(MCC_val_row[[2]]>0.8, TRUE)
  
  #-------------------------------------------------------------------------
  #See if the results from this function are equal to the results
  #if pheatmap matrix is created manually. Not mtry and ntree optimization
  #-------------------------------------------------------------------------
  
  #Subset 1/2/3
  
  subset.123 <- subset(example.data, example.data[,"sep.xy.ab"]=="1/2/3")
  subset.123$actual <- as.factor(as.numeric(subset.123$actual))
  
  set.seed(2)
  rf.result.subset.123 <- randomForest::randomForest(x=subset.123[,c("x", "y", "a", "b")], y=subset.123[,"actual"])
  
  importance.values.from.123 <- rf.result.subset.123$importance
  predicted <- rf.result.subset.123$predicted
  actual <- subset.123[,"actual"]
  MCC.val <- MCC.val.123 <- mltools::mcc(preds=predicted, actuals=actual)
  
  #Subset 4/5
  
  subset.45 <- subset(example.data, example.data[,"sep.xy.ab"]=="4/5")
  subset.45$actual <- as.factor(as.numeric(subset.45$actual))
  
  set.seed(2)
  rf.result.subset.45 <- randomForest::randomForest(x=subset.45[,c("x", "y", "a", "b")], y=subset.45[,"actual"])
  
  importance.values.from.45 <- rf.result.subset.45$importance
  predicted <- rf.result.subset.45$predicted
  actual <- subset.45[,"actual"]
  MCC.val.45 <- mltools::mcc(preds=predicted, actuals=actual)

  
  #Make the dataframe so that it imitates the pheatmap output
  column.123 <- rbind(importance.values.from.123, MCC.val)
  column.45 <- rbind(importance.values.from.45, MCC.val.45)
  
  combined.result <- cbind(column.123, column.45)
  
  #Change column names
  colnames(combined.result)[1] <- "sep.xy.ab 1/2/3"
  colnames(combined.result)[2] <- "sep.xy.ab 4/5"
  
  expect_equal(matrix.for.pheatmap, combined.result)
  
  #-------------------------------------------------------------------------
  #See if the results from this function are equal to the results
  #if pheatmap matrix is created manually. With mtry and ntree optimization
  #-------------------------------------------------------------------------
  
  #Subset 1/2/3
  
  subset.123 <- subset(example.data, example.data[,"sep.xy.ab"]=="1/2/3")
  subset.123$actual <- as.factor(as.numeric(subset.123$actual))
  
  set.seed(2)
  rf.result.subset.123 <- RandomForestAutomaticMtryAndNtree(subset.123, c("x", "y", "a", "b"), "actual", seed=2)
  
  importance.values.from.123 <- rf.result.subset.123$importance
  predicted <- rf.result.subset.123$predicted
  actual <- subset.123[,"actual"]
  MCC.val <- MCC.val.123 <- mltools::mcc(preds=predicted, actuals=actual)
  
  #Subset 4/5
  
  subset.45 <- subset(example.data, example.data[,"sep.xy.ab"]=="4/5")
  subset.45$actual <- as.factor(as.numeric(subset.45$actual))
  
  set.seed(2)
  rf.result.subset.45 <- RandomForestAutomaticMtryAndNtree(subset.45, c("x", "y", "a", "b"), "actual", seed=2)
  
  importance.values.from.45 <- rf.result.subset.45$importance
  predicted <- rf.result.subset.45$predicted
  actual <- subset.45[,"actual"]
  MCC.val.45 <- mltools::mcc(preds=predicted, actuals=actual)
  
  
  #Make the dataframe so that it imitates the pheatmap output
  column.123 <- rbind(importance.values.from.123, MCC.val)
  column.45 <- rbind(importance.values.from.45, MCC.val.45)
  
  combined.result <- cbind(column.123, column.45)
  
  #Change column names
  colnames(combined.result)[1] <- "sep.xy.ab 1/2/3"
  colnames(combined.result)[2] <- "sep.xy.ab 4/5"
  
  #Toggle TRUE for mtry and ntree optimization
  invisible(capture.output(
    matrix.for.pheatmap <- RandomForestClassificationGiniMatrixForPheatmap(input.data = example.data,
                                                                           factor.name.for.subsetting = "sep.xy.ab",
                                                                           name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                           target.column.name = "actual",
                                                                           seed = 2,
                                                                           should.mtry.and.ntree.be.optimized = TRUE)
  ))
  
  expect_equal(matrix.for.pheatmap, combined.result)
  
  

})


test_that("RandomForestClassificationPercentileMatrixForPheatmap works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  invisible(capture.output(
    percentile.result <- RandomForestClassificationPercentileMatrixForPheatmap(input.data = example.data,
                                                                    factor.name.for.subsetting = "sep.xy.ab",
                                                                    name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                    target.column.name = "actual",
                                                                    seed = 2,
                                                                    should.mtry.and.ntree.be.optimized = FALSE)
  ))
  
  invisible(capture.output(
    gini.result <- RandomForestClassificationGiniMatrixForPheatmap(input.data = example.data,
                                                                               factor.name.for.subsetting = "sep.xy.ab",
                                                                               name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                               target.column.name = "actual",
                                                                               seed = 2,
                                                                               should.mtry.and.ntree.be.optimized = FALSE)
  ))
  
  #Check if MCC is placed in column name.
  MCC.val.in.col1.name <- as.numeric(strsplit(colnames(percentile.result)[[1]], " ")[[1]][[6]])
  MCC.val.in.col2.name <- as.numeric(strsplit(colnames(percentile.result)[[2]], " ")[[1]][[6]])
  MCC.val.gini.col1 <- as.numeric(gini.result[5,1])
  MCC.val.gini.col2 <- as.numeric(gini.result[5,2])
  
  expect_equal(MCC.val.in.col1.name, MCC.val.gini.col1, tolerance = 0.01)
  expect_equal(MCC.val.in.col2.name, MCC.val.gini.col2, tolerance = 0.01)
  
  
  #Is the matrix to be expected given the gini matrix results.
  #In the first column, x and y should both have larger
  #percentiles than a and b.
  expect_equal(percentile.result[1,1] > percentile.result[3,1] & percentile.result[1,1] > percentile.result[4,1], TRUE)
  expect_equal(percentile.result[2,1] > percentile.result[3,1] & percentile.result[2,1] > percentile.result[4,1], TRUE)
  
  #In the second column, a and b should both have larger
  #percentiles than x and y
  expect_equal(percentile.result[3,2] > percentile.result[1,2] & percentile.result[3,2] > percentile.result[2,2], TRUE)
  expect_equal(percentile.result[4,2] > percentile.result[1,2] & percentile.result[4,2] > percentile.result[2,2], TRUE)

})

#LOOCVRandomForestClassificationMatrixForPheatmap
#Expected output is a matrix. 

test_that("LOOCVRandomForestClassificationMatrixForPheatmap works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  invisible(capture.output(
    result <- LOOCVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
                                                                               factor.name.for.subsetting = "sep.xy.ab",
                                                                               name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                               target.column.name = "actual",
                                                                               seed = 2,
                                                                               should.mtry.and.ntree.be.optimized = FALSE,
                                                                               percentile.threshold.to.keep = 0.5)
  ))
  
  percentile.result <- result[[1]]
  
  #Is the matrix to be expected given the gini matrix results.
  #In the first column, x and y should both have larger
  #percentiles than a and b.
  expect_equal(percentile.result[1,1] > percentile.result[3,1] & percentile.result[1,1] > percentile.result[4,1], TRUE)
  expect_equal(percentile.result[2,1] > percentile.result[3,1] & percentile.result[2,1] > percentile.result[4,1], TRUE)
  
  #In the second column, a and b should both have larger
  #percentiles than x and y
  expect_equal(percentile.result[3,2] > percentile.result[1,2] & percentile.result[3,2] > percentile.result[2,2], TRUE)
  expect_equal(percentile.result[4,2] > percentile.result[1,2] & percentile.result[4,2] > percentile.result[2,2], TRUE)
  
  #MCC values should be very large
  MCC.val.in.col1.name <- as.numeric(strsplit(colnames(percentile.result)[[1]], " ")[[1]][[6]])
  MCC.val.in.col2.name <- as.numeric(strsplit(colnames(percentile.result)[[2]], " ")[[1]][[6]])
  expect_equal(MCC.val.in.col1.name>0.8, TRUE)
  expect_equal(MCC.val.in.col2.name>0.8, TRUE)
  
  #Are there two subsetted dataframes
  subset.result <- result[[2]]
  expect_equal(length(subset.result), 2)
  
  
})



test_that("CVPredictionsRandomForest works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  invisible(capture.output(
    result.LOOCV <- LOOCVPredictionsRandomForestAutomaticMtryAndNtree(inputted.data = example.data,
                                                                      predictors.that.should.not.PCA = c("x", "y", "a", "b"),
                                                                      predictors.that.PCA.can.be.done.on = NULL,
                                                                      should.PCA.be.used = FALSE,
                                                                      target.column.name = "actual",
                                                                      seed = 2,
                                                                      should.mtry.and.ntree.be.optimized = FALSE,
                                                                      percentile.threshold.to.keep = 0.5)
  ))
  
  invisible(capture.output(
    result.CV <- CVPredictionsRandomForest(inputted.data = example.data,
                                           name.of.predictors.to.use = c("x", "y", "a", "b"),
                                           target.column.name = "actual",
                                           seed = 2,
                                           percentile.threshold.to.keep = 0.5,
                                           number.of.folds = nrow(example.data))
  ))
  
  #Results from CVPredictionsRandomForest should be equivalent to results
  #from LOOCVPredictionsRandomForestAutomaticMtryAndNtree if
  #1. Number of folds is equal to number of observations.
  #2. PCA is not used.
  #3. Default mtry and ntree is used.
  expect_equal(result.LOOCV[[1]], result.CV[[1]])
  expect_equal(result.LOOCV[[2]], result.CV[[2]])
  
  #Check if 10 fold works. Should not get any errors. 
  invisible(capture.output(
    result.CV.ten.fold <- CVPredictionsRandomForest(inputted.data = example.data,
                                           name.of.predictors.to.use = c("x", "y", "a", "b"),
                                           target.column.name = "actual",
                                           seed = 2,
                                           percentile.threshold.to.keep = 0.5,
                                           number.of.folds = 10)
  ))
  
  #Check if 4 fold works. Should not get any errors.
  invisible(capture.output(
    result.CV.four.fold <- CVPredictionsRandomForest(inputted.data = example.data,
                                                    name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                    target.column.name = "actual",
                                                    seed = 2,
                                                    percentile.threshold.to.keep = 0.5,
                                                    number.of.folds = 4)
  ))
  
  #Check if 3 fold works. Should not get any errors.
  invisible(capture.output(
    result.CV.three.fold <- CVPredictionsRandomForest(inputted.data = example.data,
                                                     name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                     target.column.name = "actual",
                                                     seed = 2,
                                                     percentile.threshold.to.keep = 0.5,
                                                     number.of.folds = 3)
  ))
  
  #Check if 2 fold works. Should not get any errors.
  invisible(capture.output(
    result.CV.two.fold <- CVPredictionsRandomForest(inputted.data = example.data,
                                                      name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                      target.column.name = "actual",
                                                      seed = 2,
                                                      percentile.threshold.to.keep = 0.5,
                                                      number.of.folds = 2)
  ))
  two.fold.predicted <- as.numeric(result.CV.two.fold[[1]])
  two.fold.actual <- as.numeric(example.data[,"actual"])
  two.fold.mcc <- mltools::mcc(preds = two.fold.predicted, actuals = two.fold.actual)
  
  #Randomly shuffle the data, this should improve the two fold performance. 
  #This is because the CVPredictionsRandomForest() should not automatically shuffle
  #and since the example.data was created sequentially with first half focused on
  #predicting categories 1/2/3 with x/y and the second half on predicting categories 4/5
  #with a/b, two fold CV should result in very poor performance.
  set.seed(1)
  example.data.shuffled <- example.data[sample(nrow(example.data)),]
  #Check if 2 fold works. Should not get any errors.
  invisible(capture.output(
    result.CV.two.fold <- CVPredictionsRandomForest(inputted.data = example.data.shuffled,
                                                    name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                    target.column.name = "actual",
                                                    seed = 2,
                                                    percentile.threshold.to.keep = 0.5,
                                                    number.of.folds = 2)
  ))
  two.fold.predicted <- as.numeric(result.CV.two.fold[[1]])
  two.fold.actual <- as.numeric(example.data.shuffled[,"actual"])
  two.fold.mcc.shuffled <- mltools::mcc(preds = two.fold.predicted, actuals = two.fold.actual)
  
  expect_equal(two.fold.mcc.shuffled > two.fold.mcc, TRUE)
  
})




test_that("CVRandomForestClassificationMatrixForPheatmap works", {
  
  example.data <- GenerateExampleDataMachinelearnr()
  
  # set.seed(1)
  # example.data <- example.data[sample(nrow(example.data)),]
  
  
  invisible(capture.output(
    result.LOOCV <- LOOCVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
                                                                     factor.name.for.subsetting = "sep.xy.ab",
                                                                     name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                     target.column.name = "actual",
                                                                     seed = 2,
                                                                     should.mtry.and.ntree.be.optimized = FALSE,
                                                                     percentile.threshold.to.keep = 0.5)
  ))
  
  invisible(capture.output(
    result.CV <- CVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
                                                               factor.name.for.subsetting = "sep.xy.ab",
                                                               name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                               target.column.name = "actual",
                                                               seed = 2,
                                                               percentile.threshold.to.keep = 0.5,
                                                               number.of.folds = -1)
  ))
  
  #Results from CVRandomForestClassificationMatrixForPheatmap should be equivalent to results
  #from LOOCVRandomForestClassificationMatrixForPheatmap if
  #1. Number of folds is equal to number of observations.
  #2. PCA is not used.
  #3. Default mtry and ntree is used.
  expect_equal(result.LOOCV, result.CV)
  
  #Check if 10 fold works. Should not get any errors. 
  invisible(capture.output(
    result.ten.fold.CV <- CVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
                                                               factor.name.for.subsetting = "sep.xy.ab",
                                                               name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                               target.column.name = "actual",
                                                               seed = 2,
                                                               percentile.threshold.to.keep = 0.5,
                                                               number.of.folds = 10)
  ))
  
  #Check if 4 fold works. Should not get any errors.
  invisible(capture.output(
    result.four.fold.CV <- CVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
                                                               factor.name.for.subsetting = "sep.xy.ab",
                                                               name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                               target.column.name = "actual",
                                                               seed = 2,
                                                               percentile.threshold.to.keep = 0.5,
                                                               number.of.folds = 4)
  ))
  
  #Check if 3 fold works. Should not get any errors.
  invisible(capture.output(
    result.three.fold.CV <- CVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
                                                                         factor.name.for.subsetting = "sep.xy.ab",
                                                                         name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                         target.column.name = "actual",
                                                                         seed = 2,
                                                                         percentile.threshold.to.keep = 0.5,
                                                                         number.of.folds = 3)
  ))
  
  #Should expect an error because subsetting and then doing two-fold CV will cause
  #the two folds of the 4/5 subsetted data to each have only one target value (4 or 5).
  expect_error(
  invisible(capture.output(
    result.two.fold.CV <- CVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
                                                                         factor.name.for.subsetting = "sep.xy.ab",
                                                                         name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                         target.column.name = "actual",
                                                                         seed = 2,
                                                                         percentile.threshold.to.keep = 0.5,
                                                                         number.of.folds = 2)
  ))
  )
  
  #Error should be gone after shuffling the data
  set.seed(1)
  example.data.shuffled <- example.data[sample(nrow(example.data)),]
  invisible(capture.output(
    result.two.fold.CV <- CVRandomForestClassificationMatrixForPheatmap(input.data = example.data.shuffled,
                                                                        factor.name.for.subsetting = "sep.xy.ab",
                                                                        name.of.predictors.to.use = c("x", "y", "a", "b"),
                                                                        target.column.name = "actual",
                                                                        seed = 2,
                                                                        percentile.threshold.to.keep = 0.5,
                                                                        number.of.folds = 2)
  ))

  
  #Check if the function outputs a list of two dataframes as its other output
  expect_equal(length(result.two.fold.CV[[2]]), 2)
  
})
