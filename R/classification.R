

#find.best.number.of.trees()
#In the example, show a plot of the x=number of trees and y=error.oob

#' Using the classification error rate for each number of trees, find
#' the optimal number of trees to use for random forest classifier
#'
#' Find a plateau that corresponds with the minimum error. Uses a sliding window approach
#' where the window has a width of 3 trees.
#'
#' Select windows with lowest mean. From these windows, I select the windows with
#' lowest standard deviation (indicates plateau). If multiple plateaus exist, select
#' the one with the fewest number of trees. Then select the tree corresponding to the
#' center of the window as the optimal number of trees.
#'
#' @param error.oob A vector of numbers. Should be the $err.rate from a randomForest::randomForest object.
#'
#' @return A numerical value specifying the optimal number of trees to use in random forest.
#'
#' @export
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
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3", "2", "2"))
#'
#' color = as.factor(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
#'
#' example.data <- data.frame(id, x, y, a, b, actual, color)
#'
#' set.seed(1)
#' rf.result <- randomForest::randomForest(x=example.data[,c("x", "y", "a", "b")], y=example.data[,"actual"], proximity=TRUE, ntree=50)
#' error.oob <- rf.result[[4]][,1]
#'
#' best.tree <- find.best.number.of.trees(error.oob)
#'
#' trees <- 1:length(error.oob)
#'
#' plot(trees, error.oob, type = "l")
#'
#'
find.best.number.of.trees <- function(error.oob) {
  ##Find optimal number of trees---------------------------------------
  window.size = 3;
  index.vals <- numeric(0)
  window.vals <- numeric(0)
  window.vals.mean <- numeric(0)
  window.vals.sd <- numeric(0)
  for (index in 1:length(error.oob))
  {
    if(index < length(error.oob)-window.size+2)
    {
      index.vals.temp = index:(index+window.size-1)
      index.vals = rbind(index.vals, index.vals.temp)

      window.vals.temp = error.oob[index.vals.temp]
      window.vals = rbind(window.vals, window.vals.temp)

      window.vals.sd.temp = sd(window.vals.temp)
      window.vals.sd = rbind(window.vals.sd, window.vals.sd.temp)

      window.vals.mean.temp = mean(window.vals.temp)
      window.vals.mean = rbind(window.vals.mean, window.vals.mean.temp)

    }
  }

  windows.tracker = data.frame(index.vals, window.vals, window.vals.mean, window.vals.sd)
  ##Round mean and SD values. For classification, we need more decimal places because values for error are from 0-1.
  windows.tracker = data.frame(index.vals, window.vals, round(window.vals.mean,3), round(window.vals.sd,3))

  ##Rename rows and columns
  ##Be careful with this step, the column names might be different.
  rownames(windows.tracker) = paste("window num",1:dim(windows.tracker)[1])
  colnames(windows.tracker)[colnames(windows.tracker) == "round.window.vals.mean..3."] <- "Window.mean"
  colnames(windows.tracker)[colnames(windows.tracker) == "round.window.vals.sd..3."] <- "Window.SD"

  ##Select window with lowest error
  index.for.min.mean.error = which(windows.tracker$Window.mean == min(windows.tracker$Window.mean))
  ##Of the windows with lowest error, which has the lowest SD
  index.for.min.mean.error.with.lowest.SD = which(windows.tracker$Window.SD[index.for.min.mean.error] == min(windows.tracker$Window.SD[index.for.min.mean.error]))
  ##window or windows with lowest error and then lowest SD if multiple windows share same minimum error
  window.index.with.lowest.mean.and.SD = index.for.min.mean.error[index.for.min.mean.error.with.lowest.SD]

  ##Use the lowest index to select the window to use in picking the number of trees for an optimal model
  best.window = min(window.index.with.lowest.mean.and.SD)
  windows.tracker[best.window,]

  ##Use the tree in the center of window as the best tree
  best.tree = best.window + as.integer(window.size/2)
  best.tree

  return(best.tree)
}


#eval.classification.results()
#' Determine the performance of classification
#'
#' The Matthew's Correlation Coefficient, proportion of total classifications
#' that are correct, and proportions of classifications correct for each
#' class are all displayed by this function. Plots are also displayed
#' to show performance of classification.
#'
#' @param actual_input A vector that indicates the actual class for each observation.
#' @param predicted_input A vector that indicates the predicted class for each observation.
#' @param name A string that specifies the title to be used for plotting.
#'
#' @return No objects are created, but numbers and plots will be displayed.
#'
#' @export
#'
#' @examples
#'
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "3h", "3i", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "2g", "2h")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3"))
#'
#' color = as.factor(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
#'
#' example.data <- data.frame(id, x, y, a, b, actual, color)
#'
#' set.seed(1)
#' rf.result <- randomForest::randomForest(x=example.data[,c("x", "y", "a", "b")], y=example.data[,"actual"], proximity=TRUE, ntree=50)
#' predicted <- rf.result$predicted
#' actual <- example.data[,"actual"]
#'
#' eval.classification.results(actual, predicted, "Example")
#'
#'
eval.classification.results <- function(actual_input, predicted_input, name) {

  actual <- as.numeric(actual_input)
  predicted <- as.numeric(predicted_input)

  print(paste("The MCC (Matthews Correlation Coefficient is: ", mltools::mcc(preds=predicted, actuals=actual)))
  print(mltools::mcc(preds=predicted, actuals=actual))

  print(name)
  print(paste("proportion of total classification correct: ", ((sum(predicted==actual))/length(actual))*100))
  print(((sum(predicted==actual))/length(actual))*100)

  for(class in unique(actual))
  {
    print(paste("proportion of classification correct for class", class, ":", ((sum(predicted[actual==class]==class))/sum(actual==class))*100))
    print(((sum(predicted[actual==class]==class))/sum(actual==class))*100)
  }

  #Make a plot. The color represents the actual class of sample. The y-axis is the
  #predicted value of the sample. Red = 1, blue = 2.
  grDevices::dev.new()
  cx  <- cbind(actual,predicted)
  ccc <- c(actual)
  plot(cx[,2],type='p',col=ccc*2,pch=19,cex=2, main=name, ylab="Predicted") ##red=1, blue=2
  points(cx[,2],type='p',col=1,pch=21,cex=2)

  legend("right", legend = unique(actual),
         col=unique(ccc*2), pch=(rep(19,length(unique(actual)))), cex=0.8, title="Actual")

  print("Color represents the actual class. Y-value represents predicted class")

  #print the keys for classes.
  print("Classes used by function")
  print(unique(actual))
  print("Original classes in data")
  print(unique(actual_input))

}


#' Create random forest classification model after optimizing mtry and ntree
#'
#' @param inputted.data A dataframe.
#' @param name.of.predictors.to.use A vector of strings that specifies the columns with values that we want to use for prediction.
#' @param target.column.name A string that specifies the column with values that we want to predict for. This column should be a factor.
#' @param seed A integer that specifies the seed to use for random number generation.
#'
#' @return A randomForest object is returned
#'
#' @export
#'
#' @examples
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "3h", "3i", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "2g", "2h")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3"))
#'
#' color = as.factor(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
#'
#' example.data <- data.frame(id, x, y, a, b, actual, color)
#'
#' rf.result <- RandomForestAutomaticMtryAndNtree(example.data, c("x", "y", "a", "b"), "actual", seed=2)
#'
#' predicted <- rf.result$predicted
#' actual <- example.data[,"actual"]
#'
#' #Result is not perfect because RF model does not over fit to the training data.
#' eval.classification.results(actual, predicted, "Example")
#'
RandomForestAutomaticMtryAndNtree <- function(inputted.data, name.of.predictors.to.use, target.column.name, seed) {

  working.data <- inputted.data

  set.seed(seed)
  invisible(capture.output(explore.4.best.mtry <- randomForest::tuneRF(working.data[,name.of.predictors.to.use],
                                                                       working.data[,target.column.name], ntreeTry=500, trace = FALSE, plot = FALSE)))
  explore.4.best.mtry <- explore.4.best.mtry[order(explore.4.best.mtry[,2], decreasing=FALSE), ][1,1]

  ##Perform tree selection to build optimal model
  set.seed(seed)
  rf.result <- randomForest::randomForest(x=working.data[,name.of.predictors.to.use], y=working.data[,target.column.name], proximity=TRUE, ntree=1000, mtry = explore.4.best.mtry)
  error.oob <- rf.result[[4]][,1]
  best.tree <- find.best.number.of.trees(error.oob)

  #Build RF model
  set.seed(seed)
  rf.result <- randomForest::randomForest(x=working.data[,name.of.predictors.to.use], y=working.data[,target.column.name], proximity=TRUE, ntree=5, mtry = explore.4.best.mtry)

  return(rf.result)

}


#' Create random forest cross-validated model after optimizing mtry and ntree.
#'
#' For a single input dataset with N observations, each observation is removed one at a time.
#' This creates N sub-datasets Using each sub-dataset, we can build a RF model and predict the one observation
#' that's left out. We can also determine the features in each round that was deemed to be important.
#' At the end, we get a list of predictions for every observation (because every observation was left out exactly one time)
#' and we get a list of variables that were considered important.
#'
#' This function uses RandomForestAutomaticMtryAndNtree() for each round of LOOCV.
#' For each round of LOOCV, the mean decrease in gini index is used to determine
#' which features (predictors) are important for predicting the classes. The code
#' for this function has functionality for doing PCA to reduce the dimensionality
#' and then using the PCs as predictors.
#'
#' Cross-validation allows you to do feature selection and assess if the model is
#' overfit.
#'
#' @param inputted.data A dataframe
#' @param predictors.that.PCA.can.be.done.on A vector of strings that specify the name of columns that has data that should undergo PCA.
#' @param predictors.that.should.not.PCA A vector of strings that specify the name of columns that has data that should not undergo PCA.
#' @param should.PCA.be.used A boolean indicating if PCA should be used. If this is FALSE, then it doesn't matter whether predictors as listed in predictors.that.PCA.can.be.done.on or predictors.that.should.not.PCA
#' @param target.column.name A string that specifies the column with values that we want to predict for. This column should be a factor.
#' @param seed A integer that specifies the seed to use for random number generation.
#'
#' @return A list with two objects:
#' 1. running.pred: predicted values for each observation.
#' 2. var.tally: the percentage of LOOCV rounds that the features had importance values above the 80th percentile.
#'
#' @export
#'
#' @examples
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "3h", "3i", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "2g", "2h")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3"))
#'
#' color = as.factor(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1))
#'
#' example.data <- data.frame(id, x, y, a, b, actual, color)
#'
#' result <- LOOCVPredictionsRandomForestAutomaticMtryAndNtree(example.data,
#'                            predictors.that.PCA.can.be.done.on = c("x", "y", "a", "b"),
#'                            predictors.that.should.not.PCA = NULL,
#'                            should.PCA.be.used = FALSE,
#'                            target.column.name = "actual",
#'                             seed=2)
#'
#' predicted <- result[[1]]
#' actual <- example.data[,"actual"]
#'
#' #Result is not perfect because RF model does not over fit to the training data.
#' eval.classification.results(actual, predicted, "Example")
#'
#' #Feature selection
#' result[[2]]
#'
#'
LOOCVPredictionsRandomForestAutomaticMtryAndNtree <- function(inputted.data, predictors.that.PCA.can.be.done.on, predictors.that.should.not.PCA, should.PCA.be.used, target.column.name, seed) {

  #Test function
  #inputted.data <- working.data
  #predictors.that.PCA.can.be.done.on <- names.of.dependent.variables
  #predictors.that.should.not.PCA <- NULL
  #should.PCA.be.used <- FALSE
  #target.column.name <- "HIV"
  #seed <- 1
  #

  train <- inputted.data
  #volume.column.nums <- name.of.predictors.to.use

  min.pc.percent.to.use <- 1
  running.pred <- NULL
  #running.importance <- NULL
  #running.treeSize <- NULL
  variables.with.sig.contributions <- NULL

  ##Go through each row of the dataset
  for (l in 1:dim(train)[1]) {

    print(l)
    loo <- train[-c(l),] ##Select all rows other than one

    #If PCA is to be used, then we need to remove the sample and then calculate the PC
    if(should.PCA.be.used == FALSE){
      name.of.predictors.to.use <- c(predictors.that.should.not.PCA, predictors.that.PCA.can.be.done.on)
    } else{
      pc.addition.output <- AddPCsToEnd(loo, predictors.that.PCA.can.be.done.on, TRUE, 1)
      loo <- pc.addition.output[[1]]
      pc.names <- pc.addition.output[[2]]
      pca.results <- pc.addition.output[[3]]

      name.of.predictors.to.use <- c(predictors.that.should.not.PCA, pc.names)
    }

    #Perform RF
    final.model <- RandomForestAutomaticMtryAndNtree(loo, name.of.predictors.to.use, target.column.name, seed)

    #------------------------------------------------------------------------------
    # Predict left out sample
    #------------------------------------------------------------------------------

    #We now want to predict the sample we left out.
    #If PCA is used as predictors, then we need to first calculate the PC values for left out sample
    if(should.PCA.be.used == FALSE){
      predictions_loo <- predict(final.model, newdata = train[,name.of.predictors.to.use])
    } else{
      #Calculate PC values for left out sample
      non.PC.values <- train[,predictors.that.should.not.PCA]
      predicted.PC.values <- predict(pca.results, newdata=train)

      temp3 <- cbind(non.PC.values, predicted.PC.values)
      #I did not do feature selection, but if I did, then I would need to use this commented out code
      #temp3 <- temp3[,predictor.features.2.keep,drop=FALSE]

      predictions_loo <- predict(final.model, newdata = temp3)
    }

    running.pred <- c(running.pred, predictions_loo[c(l)])

    #------------------------------------------------------------------------------
    # Record which features were important for this round of LOOCV
    #------------------------------------------------------------------------------

    importance.of.variables <- randomForest::importance(final.model)
    importance.of.variables.sorted <- importance.of.variables[order(importance.of.variables[,1], decreasing=TRUE), ]

    #calculate percentile
    features.2.keep <- ecdf(importance.of.variables.sorted)(importance.of.variables.sorted)

    #add names back to the features
    names(features.2.keep) <- names(importance.of.variables.sorted)

    #Only include the features in the top percentiles
    percentile.threshold.to.keep <- 0.8 #Value of importance value has to be in the top 80% ##<--------------ADJUST HERE
    features.2.keep <- names(features.2.keep[features.2.keep>percentile.threshold.to.keep])

    variables.with.sig.contributions <- c(variables.with.sig.contributions, features.2.keep)
  }

  #Calculate the percentage of LOOCV rounds that the features had importance
  #values above a certain percentile
  var.tally <- round((rev(sort(table(variables.with.sig.contributions)))/dim(train)[1])*100,2)

  output <- list(running.pred, var.tally)

  return(output)

  ###------------------------------------------------------------------------------------
  ##Count the number of rounds of LOOCV a feature has importance scores
  ##in the top 20th percentile by tallying up values in variables.with.sig.contributions

  ##Use running.pred and the actual values to calculate MCC.

  # #------------------------------------------------------------------------------
  # # Inspect LOOCV Performance for RF Classification
  # #------------------------------------------------------------------------------
  #
  # ##known.response and target.response are used interchangeably below.
  # known.response <- train[,target.to.predict.column.num]
  # target.response <- train[,target.to.predict.column.num]
  #
  # ##The i-th predicted value returns the mean of the predicted values across all trees that have the i-th sample value as OOB.
  # predicted <- running.pred
  # ##last column of data.rf is the class column.
  # actual <- target.response
  #
  #
  # eval.classification.results(actual,predicted,"Performance with LOOCV")
  #
  #
  # #------------------------------------------------------------------------------
  # # Inspect Features Selected Under LOOCV Modeling Condition
  # #------------------------------------------------------------------------------
  # sort(table(variables.with.sig.contribution.to.loadings), decreasing = TRUE)
  # var.tally <- round((rev(sort(table(variables.with.sig.contribution.to.loadings)))/dim(train)[1])*100,2)
  # var.tally
  # var.percent.time <- Use.percentage.for.LOOCV.feature.selection ##<-----------------------------------------------------------ADJUST HERE
  # predictors.to.model.with <- names(var.tally[var.tally>=var.percent.time])

}


#RandomForestClassificationGiniMatrixForPheatmap()

#RandomForestClassificationPercentileMatrixForPheatmap()

#LOOCVRandomForestClassificationMatrixForPheatmap()

#
