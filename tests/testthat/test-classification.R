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

#RandomForestClassificationPercentileMatrixForPheatmap
##Test if output is what you would get if you did things manually. 


#LOOCVRandomForestClassificationMatrixForPheatmap
##Test if output is what you would get if you did things manually. 



#-----------------------------------------------
#New functions to add. Specify the cross validation
#fold. If the function works, then N fold
#should have the same results as using the LOOCV
#function


#CVPredictionsRandomForestAutomaticMtryAndNtree

#CVRandomForestClassificationMatrixForPheatmap



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


# test_that("RandomForestAutomaticMtryAndNtree works", {
#   
#   example.data <- GenerateExampleDataMachinelearnr()
#   
#   rf.result <- RandomForestAutomaticMtryAndNtree(example.data, c("x", "y", "a", "b"), "actual", seed=2)
#   
#   #Should result in a rf object
#   expect_equal(class(rf.result), "randomForest")
#   
#   #The error rate of the classification model should be very low because 
#   #the test data purposely has several features that can separate the target. 
#   expect_equal(rf.result$err.rate[length(rf.result$err.rate[,1])] < 0.1, TRUE)
#   
# })
# 
# 
# test_that("LOOCVPredictionsRandomForestAutomaticMtryAndNtree works", {
#   
#   example.data <- GenerateExampleDataMachinelearnr()
#   
#   invisible(capture.output(
#     result <- LOOCVPredictionsRandomForestAutomaticMtryAndNtree(example.data,
#                               predictors.that.PCA.can.be.done.on = c("x", "y", "a", "b"),
#                               predictors.that.should.not.PCA = NULL,
#                               should.PCA.be.used = FALSE,
#                               target.column.name = "actual",
#                               seed=2,
#                               percentile.threshold.to.keep = 0.5)
#   ))
#   
#   #Output should be a list with two objects
#   expect_equal(length(result), 2)
#   
#   #First object should be a vector of predicted values for each observation
#   expect_equal(length(result[[1]]), length(example.data[,1]))
#   
#   #Second object should be table with number of entries equal to number of features
#   #identified as important.
#   expect_equal(class(result[[2]]), "table")
#   
#   #The predicted values should be very close to the actual values
#   #because the test data purposely has several features that can separate the target.
#   actual <- example.data$actual
#   predicted <- result[[1]]
#   expect_equal(mltools::mcc(preds = as.integer(predicted), actuals = as.integer(actual))>0.9, TRUE)
#   
# })
# 
# 
# test_that("RandomForestClassificationGiniMatrixForPheatmap works", {
#   
#   example.data <- GenerateExampleDataMachinelearnr()
#   
#   invisible(capture.output(
#     matrix.for.pheatmap <- RandomForestClassificationGiniMatrixForPheatmap(input.data = example.data,
#                                             factor.name.for.subsetting = "sep.xy.ab",
#                                             name.of.predictors.to.use = c("x", "y", "a", "b"),
#                                             target.column.name = "actual",
#                                             seed = 2)
#   ))
#   
#   #The resulting matrix should have 5 rows (4 rows for features and 1 row for MCC)
#   expect_equal(dim(matrix.for.pheatmap)[1], 5)
#   
#   #The resulting matrix should have 2 columns. 1 column for each level of the factor
#   #used for subetting
#   expect_equal(dim(matrix.for.pheatmap)[2], 2)
#   
#   #The last row of the column should hold the MCC value
#   expect_equal(row.names(matrix.for.pheatmap)[[length(row.names(matrix.for.pheatmap))]], "MCC.val")
#   
#   #The MCC values for each subset should be very high because the testing data
#   #was created in a way where this is true.
#   MCC_val_row <- matrix.for.pheatmap[dim(matrix.for.pheatmap)[1],]
#   expect_equal(MCC_val_row[[1]]>0.9, TRUE)
#   expect_equal(MCC_val_row[[2]]>0.9, TRUE)
#   
#   #-------------------------------------------------------------------------
#   #See if the results from this function are equal to the results
#   #if pheatmap matrix is created manually
#   #-------------------------------------------------------------------------
#   
#   #Subset 1/2/3
#   
#   subset.123 <- subset(example.data, example.data[,"sep.xy.ab"]=="1/2/3")
#   subset.123$actual <- as.factor(as.numeric(subset.123$actual))
#   
#   rf.result.subset.123 <- RandomForestAutomaticMtryAndNtree(subset.123, 
#                                                             c("x", "y", "a", "b"), 
#                                                             "actual", seed=2)
#   
#   importance.values.from.123 <- rf.result.subset.123$importance
#   predicted <- rf.result.subset.123$predicted
#   actual <- subset.123[,"actual"]
#   MCC.val <- MCC.val.123 <- mltools::mcc(preds=predicted, actuals=actual)
#   
#   #Subset 4/5
#   
#   subset.45 <- subset(example.data, example.data[,"sep.xy.ab"]=="4/5")
#   subset.45$actual <- as.factor(as.numeric(subset.45$actual))
#   
#   ##NEED TO RERUN UNIT TESTS AFTER REINTALLING PACKAGE BECAUSE
#   #IT"S STILL USING MTRY OPTIMIZATION
#   
#   rf.result.subset.45 <- RandomForestAutomaticMtryAndNtree(subset.45, 
#                                                             c("x", "y", "a", "b"), 
#                                                             "actual", seed=2)
#   
#   importance.values.from.45 <- rf.result.subset.45$importance
#   predicted <- rf.result.subset.45$predicted
#   actual <- subset.45[,"actual"]
#   MCC.val.45 <- mltools::mcc(preds=predicted, actuals=actual)
# 
#   
#   #Make the dataframe so that it imitates the pheatmap output
#   column.123 <- rbind(importance.values.from.123, MCC.val)
#   column.45 <- rbind(importance.values.from.45, MCC.val.45)
#   
#   combined.result <- cbind(column.123, column.45)
#   
#   #Change column names
#   colnames(combined.result)[1] <- "sep.xy.ab 1/2/3"
#   colnames(combined.result)[2] <- "sep.xy.ab 4/5"
#   
#   expect_equal(matrix.for.pheatmap, combined.result)
# 
# })
# 
