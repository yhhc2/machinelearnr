

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
#' @family Classification functions
#'
#' @examples
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e",
#'        "2f", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46,
#' 47, 48, 49, 54)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21,
#' 30, 31, 23, 24)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2",
#' "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3"))
#'
#' example.data <- data.frame(id, x, y, a, b, actual)
#'
#' set.seed(1)
#' rf.result <- randomForest::randomForest(x=example.data[,c("x", "y", "a", "b")],
#' y=example.data[,"actual"], proximity=TRUE, ntree=50)
#'
#' error.oob <- rf.result[[4]][,1]
#'
#' best.tree <- find.best.number.of.trees(error.oob)
#'
#' trees <- 1:length(error.oob)
#'
#' plot(trees, error.oob, type = "l")
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#' text(example.data$x, example.data$y,labels=example.data$id)
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

      window.vals.sd.temp = stats::sd(window.vals.temp)
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
#' @family Classification functions
#'
#' @examples
#'
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e", "2f", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3"))
#'
#' example.data <- data.frame(id, x, y, a, b, actual)
#'
#' set.seed(2)
#' rf.result <- randomForest::randomForest(x=example.data[,c("x", "y", "a", "b")],
#' y=example.data[,"actual"], proximity=TRUE)
#'
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
  graphics::points(cx[,2],type='p',col=1,pch=21,cex=2)

  graphics::legend("right", legend = unique(actual),
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
#' For this function, mtry is optimized using randomForest::tuneRF() and
#' ntree is optimized using find.best.number.of.trees() function on the out-of-bag
#' error. After optimizing for mtry and ntree, the optimal values are used
#' to create a new random forest model, and this model is outputted.
#'
#' However, the default values of mtry and ntree from randomForest() are actually
#' preferred in most cases.
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
#' @family Classification functions
#'
#' @examples
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e", "2f", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2",
#'        "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3"))
#'
#' example.data <- data.frame(id, x, y, a, b, actual)
#'
#' rf.result <- RandomForestAutomaticMtryAndNtree(example.data, c("x", "y", "a", "b"),
#' "actual", seed=2)
#'
#' predicted <- rf.result$predicted
#' actual <- example.data[,"actual"]
#'
#' #Result is not perfect because RF model does not over fit to the training data.
#' eval.classification.results(actual, predicted, "Example")
#'
RandomForestAutomaticMtryAndNtree <- function(inputted.data, name.of.predictors.to.use,
                                              target.column.name, seed) {

  working.data <- inputted.data

  set.seed(seed)
  invisible(utils::capture.output(explore.4.best.mtry <- randomForest::tuneRF(working.data[,name.of.predictors.to.use],
                                                                       working.data[,target.column.name], ntreeTry=500, trace = FALSE, plot = FALSE)))
  explore.4.best.mtry <- explore.4.best.mtry[order(explore.4.best.mtry[,2], decreasing=FALSE), ][1,1]

  ##Perform tree selection to build optimal model
  set.seed(seed)
  rf.result <- randomForest::randomForest(x=working.data[,name.of.predictors.to.use], y=working.data[,target.column.name], proximity=TRUE, ntree=1000, mtry = explore.4.best.mtry)
  error.oob <- rf.result[[4]][,1]
  best.tree <- find.best.number.of.trees(error.oob)

  #Build RF model
  set.seed(seed)
  rf.result <- randomForest::randomForest(x=working.data[,name.of.predictors.to.use], y=working.data[,target.column.name], proximity=TRUE, ntree=best.tree, mtry = explore.4.best.mtry)

  return(rf.result)


  #ADD IF STATEMENT TO HANDLE WHAT HAPPENS IF TUNERF DOESNT WORK
  #JUST USE DEFAULT MTRY

  #ADD IF STATEMENT TO INDICATE WHAT HAPPENS IF NUMBER OF TREES IS NOT ENOUGH.
  #COULD ALSO JUST USE DEFAULT MTRY

}


#' Create random forest leave-one-out-cross-validated model
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
#' and then using the PCs as predictors. Can choose to optimize for mtry and ntree.
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
#' @param should.mtry.and.ntree.be.optimized A boolean to indicate if RandomForestAutomaticMtryAndNtree() should be used to optimize ntree and mtry. Default is FALSE.
#' @param percentile.threshold.to.keep A number from 0-1 indicating the percentile to use for feature selection. The percentage of
#' LOOCV rounds that the feature has importance values above the percentile.threshold.to.keep will be counted in the var.tally of the
#' output. Example: If there are 4 features (x, y, a, b) and the mean decrease in gini index in one LOOCV round are 4, 3, 2, 1 respectively,
#' then this means feature x is in the 100th percentile (percentile value of 1), y is in the 75th percentile (percentile value of 0.75), etc.
#' If the threshold is set at 0.75, then both x and y will be tallied for this single LOOCV round.
#'
#' @return A list with two objects:
#' 1. running.pred: predicted values for each observation. A vector. 
#' 2. var.tally: the percentage of LOOCV rounds that the features had importance values above the percentile.threshold.to.keep percentile. A table.
#'
#' @export
#'
#' @family Classification functions
#'
#' @examples
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e", "2f", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2",
#'        "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3"))
#'
#' example.data <- data.frame(id, x, y, a, b, actual)
#'
#' result <- LOOCVPredictionsRandomForestAutomaticMtryAndNtree(example.data,
#'                            predictors.that.PCA.can.be.done.on = c("x", "y", "a", "b"),
#'                            predictors.that.should.not.PCA = NULL,
#'                            should.PCA.be.used = FALSE,
#'                            target.column.name = "actual",
#'                            seed=2,
#'                            percentile.threshold.to.keep = 0.5)
#'
#' predicted <- result[[1]]
#' actual <- example.data[,"actual"]
#'
#' eval.classification.results(actual, predicted, "Example")
#'
#' #Feature selection
#' #As expected, only features x and y are indicated as important.
#' result[[2]]
#'
#'
LOOCVPredictionsRandomForestAutomaticMtryAndNtree <- function(inputted.data,
                                                              predictors.that.PCA.can.be.done.on,
                                                              predictors.that.should.not.PCA,
                                                              should.PCA.be.used,
                                                              target.column.name,
                                                              seed,
                                                              should.mtry.and.ntree.be.optimized = FALSE,
                                                              percentile.threshold.to.keep = 0.8) {

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
    if(should.mtry.and.ntree.be.optimized == TRUE){

      final.model <- RandomForestAutomaticMtryAndNtree(loo, name.of.predictors.to.use, target.column.name, seed)

    } else{

      set.seed(seed)
      final.model <- randomForest::randomForest(x=loo[,name.of.predictors.to.use], y=loo[,target.column.name], proximity=TRUE)
    }


    #------------------------------------------------------------------------------
    # Predict left out sample
    #------------------------------------------------------------------------------

    #We now want to predict the sample we left out.
    #If PCA is used as predictors, then we need to first calculate the PC values for left out sample
    if(should.PCA.be.used == FALSE){
      predictions_loo <- stats::predict(final.model, newdata = train[,name.of.predictors.to.use])
    } else{
      #Calculate PC values for left out sample
      non.PC.values <- train[,predictors.that.should.not.PCA]
      predicted.PC.values <- stats::predict(pca.results, newdata=train)

      temp3 <- cbind(non.PC.values, predicted.PC.values)
      #I did not do feature selection, but if I did, then I would need to use this commented out code
      #temp3 <- temp3[,predictor.features.2.keep,drop=FALSE]

      predictions_loo <- stats::predict(final.model, newdata = temp3)
    }

    running.pred <- c(running.pred, predictions_loo[c(l)])

    #------------------------------------------------------------------------------
    # Record which features were important for this round of LOOCV
    #------------------------------------------------------------------------------

    importance.of.variables <- randomForest::importance(final.model)
    importance.of.variables.sorted <- importance.of.variables[order(importance.of.variables[,1], decreasing=TRUE), ]

    #calculate percentile
    features.2.keep <- stats::ecdf(importance.of.variables.sorted)(importance.of.variables.sorted)

    #add names back to the features
    names(features.2.keep) <- names(importance.of.variables.sorted)

    #Only include the features in the top percentiles
    percentile.threshold.to.keep <- percentile.threshold.to.keep #Value of importance value has to be in the top 80% ##<--------------ADJUST HERE
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


#' Generate a random forest model for different subsets of the data and display
#' results into a matrix
#'
#' RandomForestAutomaticMtryAndNtree() or randomForest() is used on each subset of the data. For
#' each subset of the data: A random forest model is generated. This random forest
#' model is used to make a single column in the final matrix which can be
#' used to generate a pheatmap.
#'
#' In each column of the final matrix, the Matthew's Correlation Coefficient (MCC)
#' is calculated and displayed as a row titled MCC.val to specify the performance
#' of the model to predict the target in that particular data subset. Each column
#' of the final matrix also has others rows, with each row specifying a single predictor
#' used. For each predictor, the mean decrease in gini index is indicated in the
#' matrix and can be used to determine how important the predictor is for
#' predicting the target in that particular data subset.
#'
#' The result from this function should be used with the
#' RandomForestClassificationPercentileMatrixForPheatmap() function to create
#' a pheatmap.
#'
#'
#' @param input.data A dataframe.
#' @param factor.name.for.subsetting String to specify name of column to use for subsetting. The column should be a factor. Each column of the generated pheatmap will correspond to a level in the factor.
#' @param name.of.predictors.to.use A vector of strings to specify name of columns to use as predictors for random forest model. Each column should be numeric.
#' @param target.column.name A string to specify the column with values the random forest model is trying to predict for. The column should be a factor.
#' @param seed A number to set for random number generation.
#' @param should.mtry.and.ntree.be.optimized A boolean to indicate if RandomForestAutomaticMtryAndNtree() should be used to optimize ntree and mtry. Default is FALSE.
#'
#' @return A numerical matrix that can be used for pheatmap generation.
#'
#' @export
#'
#' @family Classification functions
#'
#' @examples
#'
#' #Make example data where samples with 1, 2, and 3 in their ID names can be
#' #predicted using features x and y, while samples with 4 and 5 in their ID names
#' #can be predicted using features a and b.
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e", "2f", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i",
#'        "4a", "4b", "4c", "4d", "4e", "4f", "4g", "5a", "5b", "5c",
#'        "5d", "5e", "5f", "5g")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54,
#' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24,
#' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#' 18, 21, 22, 24, 26, 26, 27, 40, 41, 42, 44, 46, 47, 48)
#'
#' b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#' 10, 11, 22, 15, 12, 13, 14, 27, 29, 20, 28, 21, 30, 31)
#'
#' sep.xy.ab = c("1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5",
#' "4/5", "4/5", "4/5")
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", "4", "4", "5", "5", "5", "5", "5",
#'        "5", "5"))
#'
#' example.data <- data.frame(id, x, y, a, b, sep.xy.ab, actual)
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#' text(example.data$x, example.data$y,labels=example.data$id)
#'
#' dev.new()
#' plot(example.data$a, example.data$b)
#' text(example.data$a, example.data$b,labels=example.data$id)
#'
#' matrix.for.pheatmap <- RandomForestClassificationGiniMatrixForPheatmap(input.data = example.data,
#'                                        factor.name.for.subsetting = "sep.xy.ab",
#'                                        name.of.predictors.to.use = c("x", "y", "a", "b"),
#'                                        target.column.name = "actual",
#'                                        seed = 2)
#'
#'
#' #Add MCC to column names
#' for(i in 1:dim(matrix.for.pheatmap)[2])
#' {
#'    old.name <- colnames(matrix.for.pheatmap)[[i]]
#'    MCC.val <- matrix.for.pheatmap[dim(matrix.for.pheatmap)[1],][[i]]
#'    new.name <- paste(old.name, ", MCC: ", format(round(MCC.val, 2), nsmall = 2))
#'    colnames(matrix.for.pheatmap)[[i]] <- new.name
#' }
#'
#' #Remove MCC row from matrix
#' matrix.for.pheatmap.MCC.row.removed <- matrix.for.pheatmap[1:(dim(matrix.for.pheatmap)[1]-1),]
#'
#' pheatmap_RF <- pheatmap::pheatmap(matrix.for.pheatmap.MCC.row.removed, fontsize_col = 12,
#' fontsize_row=12)
#'
#'
#' #The pheatmap shows that the points in groups 1, 2, and 3 can be predicted
#' #with features x and y. While points in group 4 and 5 can be predicted with
#' #features a and b.
#' dev.new()
#' pheatmap_RF
#'
#' #Note that the 1/2/3 column is exactly the same as the random forest
#' #model created in the example for eval.classification.results.
#' #Since this pheatmap is using the number of LOOCV rounds
#' #as the input matrix, the colors can be misleading. It looks
#' #like features x and y are more important for 1/2/3 than features a and b
#' #is for 4/5. However, this is not the case. The mean decrease
#' #in gini index cannot be used to compare features between models. It
#' #can only be used to compare features within a single model.
#'
#'
#'
RandomForestClassificationGiniMatrixForPheatmap <- function(input.data,
                                                            factor.name.for.subsetting,
                                                            name.of.predictors.to.use,
                                                            target.column.name, seed,
                                                            should.mtry.and.ntree.be.optimized = FALSE){

  # input.data <- working.data
  # factor.name.for.subsetting <- "GENDER"
  # name.of.predictors.to.use <- names.of.dependent.variables
  # target.column.name <- "HIV"
  # seed <- 1

  #For classification, the target column needs to be a factor, but
  #when the data is subsetted, an error will occur if levels in the factor
  #are not represented, so the target column has to be converted to a factor
  #after subsetting
  input.data[,target.column.name] <- as.character(input.data[,target.column.name])

  #Converting factor column to factor is the easiest way to identify the levels.
  vector_recoded123 <- as.factor(input.data[,factor.name.for.subsetting])
  levels.in.column <- levels(vector_recoded123)

  #Captures all the subsets of data
  subsets.of.data <- list()

  #Captures all the importance values
  captured.importance.values <- NULL

  #For each level of the factor, subset the data and add the subsetted data into
  #a list.
  for(i in 1:length(levels.in.column)){

    subset.data <- subset(input.data, input.data[,factor.name.for.subsetting]==levels.in.column[i])

    subset.data[,target.column.name] <- as.factor(subset.data[,target.column.name])

    subsets.of.data[[i]] <- subset.data

    #For each subset data, do random forest and capture the importance value
    #of the features.
    if(should.mtry.and.ntree.be.optimized == TRUE){

      subset.data.rf.result <- RandomForestAutomaticMtryAndNtree(subset.data, name.of.predictors.to.use, target.column.name, seed)

    } else{

      set.seed(seed)
      subset.data.rf.result <- randomForest::randomForest(x=subset.data[,name.of.predictors.to.use], y=subset.data[,target.column.name], proximity=TRUE)
    }



    #Importance values
    subset.importance.values <- subset.data.rf.result$importance

    #MCC
    predicted <- subset.data.rf.result$predicted
    actual <- subset.data[,target.column.name]
    MCC.val <- mltools::mcc(preds=predicted, actuals=actual)

    #Combine importance values and MCC into one column.
    subset.importance.values <- rbind(subset.importance.values, MCC.val)

    #Capture result for each subset
    captured.importance.values <- cbind(captured.importance.values, subset.importance.values)

    #Add name to column
    col.name.for.subset <- paste(factor.name.for.subsetting, levels.in.column[i])
    colnames(captured.importance.values)[i] <- col.name.for.subset

  }

  return(captured.importance.values)

}



#' Generate a random forest model for different subsets of the data and display
#' results in a pheatmap to easily compare the different subsets
#'
#' RandomForestAutomaticMtryAndNtree() or randomForest() is used on each subset of the data. For
#' each subset of the data: A random forest model is generated. This random forest
#' model is used to make a single column in the final matrix which can be
#' used to generate a pheatmap.
#'
#' In each column of the final matrix, the Matthew's Correlation Coefficient (MCC)
#' is calculated and displayed in the column name to specify the performance
#' of the model to predict the target in that particular data subset. Each column
#' of the final matrix also has rows, with each row specifying a single predictor
#' used. The mean decrease in gini index for each feature is inputted this function.
#' Within each column the mean decrease in gini index is converted to a percentile,
#' with the feature with the highest mean decrease in gini index receiving a 100th
#' percentile.
#'
#'
#' @param input.data A dataframe.
#' @param factor.name.for.subsetting String to specify name of column to use for subsetting. The column should be a factor. Each column of the generated pheatmap will correspond to a level in the factor.
#' @param name.of.predictors.to.use A vector of strings to specify name of columns to use as predictors for random forest model. Each column should be numeric.
#' @param target.column.name A string to specify the column with values the random forest model is trying to predict for. The column should be a factor.
#' @param seed A number to set for random number generation.
#' @param should.mtry.and.ntree.be.optimized A boolean to indicate if RandomForestAutomaticMtryAndNtree() should be used to optimize ntree and mtry. Default is FALSE.
#'
#'
#' @return A numerical matrix that can be used for pheatmap generation.
#'
#'
#' @export
#'
#' @family Classification functions
#'
#' @examples
#'
#' #Make example data where samples with 1, 2, and 3 in their ID names can be
#' #predicted using features x and y, while samples with 4 and 5 in their ID names
#' #can be predicted using features a and b.
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e", "2f", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i",
#'        "4a", "4b", "4c", "4d", "4e", "4f", "4g", "5a", "5b", "5c",
#'        "5d", "5e", "5f", "5g")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54,
#' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24,
#' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#' 18, 21, 22, 24, 26, 26, 27, 40, 41, 42, 44, 46, 47, 48)
#'
#' b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#' 10, 11, 22, 15, 12, 13, 14, 27, 29, 20, 28, 21, 30, 31)
#'
#' sep.xy.ab = c("1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5",
#' "4/5", "4/5", "4/5")
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2",
#'        "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", "4", "4", "5", "5", "5",
#'        "5", "5", "5", "5"))
#'
#' example.data <- data.frame(id, x, y, a, b, sep.xy.ab, actual)
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#' text(example.data$x, example.data$y,labels=example.data$id)
#'
#' dev.new()
#' plot(example.data$a, example.data$b)
#' text(example.data$a, example.data$b,labels=example.data$id)
#'
#' matrix.for.pheatmap <- RandomForestClassificationPercentileMatrixForPheatmap(
#'                                        input.data = example.data,
#'                                        factor.name.for.subsetting = "sep.xy.ab",
#'                                        name.of.predictors.to.use = c("x", "y", "a", "b"),
#'                                        target.column.name = "actual",
#'                                        seed = 2)
#'
#'
#' pheatmap_RF <- pheatmap::pheatmap(matrix.for.pheatmap, fontsize_col = 12, fontsize_row=12)
#'
#'
#' #The pheatmap shows that the points in groups 1, 2, and 3 can be predicted
#' #with features x and y. While points in group 4 and 5 can be predicted with
#' #features a and b.
#'
#' dev.new()
#' pheatmap_RF
#'
RandomForestClassificationPercentileMatrixForPheatmap <- function(input.data,
                                                                  factor.name.for.subsetting,
                                                                  name.of.predictors.to.use,
                                                                  target.column.name, seed,
                                                                  should.mtry.and.ntree.be.optimized = FALSE){

  #The values are the mean decrease in gini index. Do percentile within each column
  #using the lines of code below.
  matrix.for.pheatmap <- RandomForestClassificationGiniMatrixForPheatmap(input.data,
                                                                         factor.name.for.subsetting,
                                                                         name.of.predictors.to.use, target.column.name, seed,
                                                                         should.mtry.and.ntree.be.optimized )

  #Add MCC to column names
  for(i in 1:dim(matrix.for.pheatmap)[2])
  {
    old.name <- colnames(matrix.for.pheatmap)[[i]]
    MCC.val <- matrix.for.pheatmap[dim(matrix.for.pheatmap)[1],][[i]]
    new.name <- paste(old.name, ", MCC: ", format(round(MCC.val, 2), nsmall = 2))
    colnames(matrix.for.pheatmap)[[i]] <- new.name
  }

  #Remove MCC row from matrix
  matrix.for.pheatmap.MCC.row.removed <- matrix.for.pheatmap[1:(dim(matrix.for.pheatmap)[1]-1),]

  #Convert values from mean decrease in gini index to percentile within each column.
  #Doing percentile instead of using the raw score because we are interested in
  #relative values.
  matrix.for.pheatmap.percentile <- ConvertDataToPercentiles(matrix.for.pheatmap.MCC.row.removed, 1)
  #Add row names back
  rownames(matrix.for.pheatmap.percentile) <- rownames(matrix.for.pheatmap.MCC.row.removed)
  #Remove tally column
  matrix.for.pheatmap.percentile <- matrix.for.pheatmap.percentile[,1:(dim(matrix.for.pheatmap.percentile)[2]-1)]

  return(matrix.for.pheatmap.percentile)

}



#' Generate a random forest model under leave-one-out-cross-validation (LOOCV) for different
#' subsets of the data and display results in a pheatmap to easily compare the different subsets
#'
#' LOOCVPredictionsRandomForestAutomaticMtryAndNtree() is used on each subset of the data. For
#' each subset of the data: A random forest model with LOOCV is generated. This random forest
#' model is used to make a single column in the final matrix which can be
#' used to generate a pheatmap.
#'
#' In each column of the final matrix, the Matthew's Correlation Coefficient (MCC)
#' is calculated and displayed in the column name to specify the performance
#' of the model to predict the target in that particular data subset. MCC is calculated
#' with the predicted and actual values of the samples left out during each round
#' of LOOCV.
#'
#' Each column
#' of the final matrix also has rows, with each row specifying a single predictor
#' used. The row values indicate the percentage (0-100) of LOOCV rounds that the features
#' had importance values above the percentile.threshold.to.keep percentile.
#'
#' @param input.data A dataframe.
#' @param factor.name.for.subsetting String to specify name of column to use for subsetting. The column should be a factor. Each column of the generated pheatmap will correspond to a level in the factor.
#' @param name.of.predictors.to.use A vector of strings to specify name of columns to use as predictors for random forest model. Each column should be numeric.
#' @param target.column.name A string to specify the column with values the random forest model is trying to predict for. The column should be a factor.
#' @param seed A number to set for random number generation.
#' @param should.mtry.and.ntree.be.optimized A boolean to indicate if RandomForestAutomaticMtryAndNtree() should be used to optimize ntree and mtry. Default is FALSE.
#' @param percentile.threshold.to.keep A number from 0-1 indicating the percentile to use for feature selection. This is used by the LOOCVPredictionsRandomForestAutomaticMtryAndNtree() function.
#'
#' @return A numerical matrix that can be used for pheatmap generation.
#'
#'
#' @export
#'
#' @family Classification functions
#'
#' @examples
#'
#' #Make example data where samples with 1, 2, and 3 in their ID names can be
#' #predicted using features x and y, while samples with 4 and 5 in their ID names
#' #can be predicted using features a and b.
#'
#' id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e", "2f", "3a",
#'        "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i",
#'        "4a", "4b", "4c", "4d", "4e", "4f", "4g", "5a", "5b", "5c",
#'        "5d", "5e", "5f", "5g")
#'
#' x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54,
#' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24,
#' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#'
#' a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#' 18, 21, 22, 24, 26, 26, 27, 40, 41, 42, 44, 46, 47, 48)
#'
#' b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#' 10, 11, 22, 15, 12, 13, 14, 27, 29, 20, 28, 21, 30, 31)
#'
#' sep.xy.ab = c("1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
#' "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5",
#' "4/5", "4/5", "4/5")
#'
#' actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2",
#'        "2", "2", "3", "3", "3",
#'        "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", "4", "4", "5",
#'        "5", "5", "5", "5", "5", "5"))
#'
#' example.data <- data.frame(id, x, y, a, b, sep.xy.ab, actual)
#'
#' dev.new()
#' plot(example.data$x, example.data$y)
#' text(example.data$x, example.data$y,labels=example.data$id)
#'
#' dev.new()
#' plot(example.data$a, example.data$b)
#' text(example.data$a, example.data$b,labels=example.data$id)
#'
#' matrix.for.pheatmap <- LOOCVRandomForestClassificationMatrixForPheatmap(input.data = example.data,
#'                                        factor.name.for.subsetting = "sep.xy.ab",
#'                                        name.of.predictors.to.use = c("x", "y", "a", "b"),
#'                                        target.column.name = "actual",
#'                                        seed = 2,
#'                                        should.mtry.and.ntree.be.optimized = FALSE,
#'                                        percentile.threshold.to.keep = 0.5)
#'
#'
#' pheatmap_RF <- pheatmap::pheatmap(matrix.for.pheatmap, fontsize_col = 12, fontsize_row=12)
#'
#'
#' #The pheatmap shows that the points in groups 1, 2, and 3 can be predicted
#' #with features x and y. While points in group 4 and 5 can be predicted with
#' #features a and b.
#'
#' dev.new()
#' pheatmap_RF
#'
LOOCVRandomForestClassificationMatrixForPheatmap <- function(input.data,
                                                             factor.name.for.subsetting,
                                                             name.of.predictors.to.use,
                                                             target.column.name, seed,
                                                             should.mtry.and.ntree.be.optimized = FALSE,
                                                             percentile.threshold.to.keep){

  #Testing conditions
  #input.data <- working.data
  #factor.name.for.subsetting <- "GENDER"
  #name.of.predictors.to.use <- names.of.dependent.variables
  #target.column.name <- "HIV"
  #seed <- 1

  #For classification, the target column needs to be a factor
  #input.data[,target.column.name] <- as.factor(input.data[,target.column.name])

  #For classification, the target column needs to be a factor, but
  #when the data is subsetted, an error will occur if levels in the factor
  #are not represented, so the target column has to be converted to a factor
  #after subsetting
  input.data[,target.column.name] <- as.character(input.data[,target.column.name])

  #Converting factor column to factor is the easiest way to identify the levels.
  vector_recoded123 <- as.factor(input.data[,factor.name.for.subsetting])
  levels.in.column <- levels(vector_recoded123)

  #Captures all the subsets of data
  subsets.of.data <- list()

  #Captures all the importance values
  captured.importance.values <- NULL

  #For each level of the factor, subset the data and add the subsetted data into
  #a list.
  for(i in 1:length(levels.in.column)){

    subset.data <- subset(input.data, input.data[,factor.name.for.subsetting]==levels.in.column[i])

    subset.data[,target.column.name] <- as.factor(subset.data[,target.column.name])

    subsets.of.data[[i]] <- subset.data

    ##-------------------------------------**

    #For each subset data, do random forest with LOOCV
    LOOCV.results <- LOOCVPredictionsRandomForestAutomaticMtryAndNtree(inputted.data = subset.data,
                                                                       predictors.that.PCA.can.be.done.on = name.of.predictors.to.use,
                                                                       predictors.that.should.not.PCA = NULL,
                                                                       should.PCA.be.used = FALSE,
                                                                       target.column.name = target.column.name,
                                                                       seed = seed,
                                                                       should.mtry.and.ntree.be.optimized = should.mtry.and.ntree.be.optimized,
                                                                       percentile.threshold.to.keep = percentile.threshold.to.keep)



    LOOCV.predictions <- LOOCV.results[[1]]

    LOOCV.features.selected <- LOOCV.results[[2]]

    ##MCC
    predicted <- as.integer(LOOCV.predictions)
    actual <- as.integer(subset.data[,target.column.name])
    MCC.val <- mltools::mcc(preds=predicted, actuals=actual)

    #Importance of features
    #1. If feature is not selected at all, then assign it a value of 0 in a new dataframe.
    names.of.features.selected <- names(LOOCV.features.selected)
    names.of.features.not.selected.at.all <- setdiff(name.of.predictors.to.use, names.of.features.selected)

    #Create data frame for features not selected at all
    variables.with.sig.contributions <- names.of.features.not.selected.at.all
    Freq <- rep(0, length(names.of.features.not.selected.at.all))
    features.not.selected.dataframe <- data.frame(variables.with.sig.contributions, Freq)

    #Convert the table that describes selected features into a dataframe so that
    #it can be combined with the dataframe for features not selected at all
    LOOCV.features.selected.dataframe <- as.data.frame(LOOCV.features.selected)
    
    #If only one feature was selected, then the results have to be formatted differently
    if(length(LOOCV.features.selected.dataframe) == 1){
      LOOCV.features.selected.dataframe <- data.frame(colone = row.names(LOOCV.features.selected.dataframe),
                                                      coltwo = LOOCV.features.selected.dataframe[1,1])
      
      colnames(LOOCV.features.selected.dataframe) <- c("variables.with.sig.contributions", "Freq")
    }

    #Combine the two dataframes above
    LOOCV.features.selected.dataframe.with.features.not.selected <- rbind(LOOCV.features.selected.dataframe,
                                                                          features.not.selected.dataframe)

    #2. Sort so that for each subset the features are in the same order. This is necessary
    #because for the pheatmap, we need all features in each column to be in the same order.
    target.order.of.features <- name.of.predictors.to.use
    sorted.dataframe <- LOOCV.features.selected.dataframe.with.features.not.selected[match(target.order.of.features,
                                                                                           LOOCV.features.selected.dataframe.with.features.not.selected$variables.with.sig.contributions),]


    #3. Use first column as row names, then delete first column.
    row.names(sorted.dataframe) <- sorted.dataframe$variables.with.sig.contributions
    subset.importance.values <- sorted.dataframe[2]

    #Combine importance values and MCC into one column.
    subset.importance.values <- rbind(subset.importance.values, MCC.val = MCC.val)

    #Convert dataframe to matrix first then...
    #Capture result for each subset
    subset.importance.values <- as.matrix(subset.importance.values)
    captured.importance.values <- cbind(captured.importance.values, subset.importance.values)

    #Add name to column
    col.name.for.subset <- paste(factor.name.for.subsetting, levels.in.column[i])
    colnames(captured.importance.values)[i] <- col.name.for.subset

  }

  ##Just make the pheatmap matrix in this command. When doing RF without LOOCV,
  #the gini index function is used first before another command converts
  #values to percentiles. For LOOCV pheatmap, the importance of features
  #is the percentage of time it occurs in the top percentile of importance values,
  #taking all rounds of LOOCV into consideration, want to use this directly without
  #needing to convert to percentile.

  matrix.for.pheatmap <- captured.importance.values

  #Add MCC to column names
  for(i in 1:dim(matrix.for.pheatmap)[2])
  {
    old.name <- colnames(matrix.for.pheatmap)[[i]]
    MCC.val <- matrix.for.pheatmap[dim(matrix.for.pheatmap)[1],][[i]]
    new.name <- paste(old.name, ", MCC: ", format(round(MCC.val, 2), nsmall = 2))
    colnames(matrix.for.pheatmap)[[i]] <- new.name
  }

  #Remove MCC row from matrix
  matrix.for.pheatmap.MCC.row.removed <- matrix.for.pheatmap[1:(dim(matrix.for.pheatmap)[1]-1),]

  return(matrix.for.pheatmap.MCC.row.removed)

}


#' Create random forest cross-validated model
#' 
#' For a single input dataset, the dataset is divided
#' sequentially into number.of.folds specified subsets. Each subset is left out as 
#' testing data while the remaining subsets are used to train a random forest model
#' with default mtry and ntree. The left out data is then predicted by
#' the model. 
#' 
#' This function assumes that the data is already randomly shuffled. This 
#' function is based off of the LOOCVPredictionsRandomForestAutomaticMtryAndNtree()
#' function, but this function does not have the ability to do PCA or
#' optimize for mtry and ntree. 
#'
#' @param inputted.data A dataframe that should already have the rows randomly shuffled.
#' @param name.of.predictors.to.use A vector of strings that specify the name of columns to use as predictors. Each column should be numerical. 
#' @param target.column.name A string that specifies the column with values that we want to predict for. This column should be a factor.
#' @param seed A integer that specifies the seed to use for random number generation.
#' @param percentile.threshold.to.keep A number from 0-1 indicating the percentile to use for feature selection. The percentage of CV rounds that the feature has importance values above the percentile.threshold.to.keep will be counted in the var.tally of the output. Example: If there are 4 features (x, y, a, b) and the mean decrease in gini index in one LOOCV round are 4, 3, 2, 1 respectively, then this means feature x is in the 100th percentile (percentile value of 1), y is in the 75th percentile (percentile value of 0.75), etc. If the threshold is set at 0.75, then both x and y will be tallied for this single CV round.
#' @param number.of.folds An integer from 1 to nrow(inputted.data) to specify the fold for CV. If This number is set to nrow(inputted.data), then the function will be LOOCV. 
#'
#' @return A list with two objects:
#' running.pred: predicted values for each observation. A vector.
#' var.tally: the percentage of CV rounds that the features had importance values above the percentile.threshold.to.keep percentile. A table.
#' 
#' @export
#'
#' @examples
CVPredictionsRandomForest <- function(inputted.data,
                                      name.of.predictors.to.use,
                                      target.column.name,
                                      seed,
                                      percentile.threshold.to.keep = 0.8,
                                      number.of.folds) {
  
  
  test.data <- inputted.data
  running.pred <- NULL
  variables.with.sig.contributions <- NULL
  
  #Create equally sized folds
  folds <- cut(seq(1,nrow(inputted.data)),breaks=number.of.folds,labels=FALSE)
  
  ##Go through each fold of CV
  for (l in 1:number.of.folds) {
    
    print(l)
    
    #Segment your data by fold using the which() function 
    testIndexes <- which(folds==l,arr.ind=TRUE)
    train.data <- inputted.data[-testIndexes, ]
    train.data[,target.column.name] <- as.factor(as.character(train.data[,target.column.name]))
    
    set.seed(seed)
    final.model <- randomForest::randomForest(x=train.data[,name.of.predictors.to.use], y=train.data[,target.column.name], proximity=TRUE)
    
    #------------------------------------------------------------------------------
    # Predict left out sample
    #------------------------------------------------------------------------------
    
    #We now want to predict the samples we left out. So we use the model on the entire dataset and then only get the predicted
    #values corresponding to the samples we left out. 
    predictions_loo <- stats::predict(final.model, newdata = test.data[,name.of.predictors.to.use])
    running.pred <- c(running.pred, predictions_loo[c(testIndexes)])
    
    #------------------------------------------------------------------------------
    # Record which features were important for this round of LOOCV
    #------------------------------------------------------------------------------
    
    importance.of.variables <- randomForest::importance(final.model)
    importance.of.variables.sorted <- importance.of.variables[order(importance.of.variables[,1], decreasing=TRUE), ]
    
    #calculate percentile
    features.2.keep <- stats::ecdf(importance.of.variables.sorted)(importance.of.variables.sorted)
    
    #add names back to the features
    names(features.2.keep) <- names(importance.of.variables.sorted)
    
    #Only include the features in the top percentiles
    percentile.threshold.to.keep <- percentile.threshold.to.keep
    features.2.keep <- names(features.2.keep[features.2.keep>percentile.threshold.to.keep])
    
    variables.with.sig.contributions <- c(variables.with.sig.contributions, features.2.keep)
  }
  
  #Calculate the percentage of LOOCV rounds that the features had importance
  #values above a certain percentile
  var.tally <- round((rev(sort(table(variables.with.sig.contributions)))/number.of.folds)*100,2)
  
  output <- list(running.pred, var.tally)
  
  return(output)
  
}


#I just copied the LOOCVRandomForestClassificationMatrixForPheatmap() function
# and substituted out LOOCVPredictionsRandomForestAutomaticMtryAndNtree() for
#CVPredictionsRandomForest()
#If number.of.folds == -1, then this means LOOCV should be used for each subset.
CVRandomForestClassificationMatrixForPheatmap <- function(input.data,
                                                             factor.name.for.subsetting,
                                                             name.of.predictors.to.use,
                                                             target.column.name, 
                                                             seed,
                                                             percentile.threshold.to.keep,
                                                             number.of.folds){
  
  
  #For classification, the target column needs to be a factor, but
  #when the data is subsetted, an error will occur if levels in the factor
  #are not represented, so the target column has to be converted to a factor
  #after subsetting
  input.data[,target.column.name] <- as.character(input.data[,target.column.name])
  
  #Converting factor column to factor is the easiest way to identify the levels.
  vector_recoded123 <- as.factor(input.data[,factor.name.for.subsetting])
  levels.in.column <- levels(vector_recoded123)
  
  #Captures all the subsets of data
  subsets.of.data <- list()
  
  #Captures all the importance values
  captured.importance.values <- NULL
  
  #For each level of the factor, subset the data and add the subsetted data into
  #a list.
  for(i in 1:length(levels.in.column)){
    
    subset.data <- subset(input.data, input.data[,factor.name.for.subsetting]==levels.in.column[i])
    
    subset.data[,target.column.name] <- as.factor(subset.data[,target.column.name])
    
    subsets.of.data[[i]] <- subset.data
    
    ##-------------------------------------**
    
    #If LOOCV should be used for each subset, then do this
    if(number.of.folds == -1){
      
      #For each subset data, do random forest
      CV.results <- CVPredictionsRandomForest(inputted.data = subset.data,
                                              name.of.predictors.to.use = name.of.predictors.to.use,
                                              target.column.name = target.column.name,
                                              seed = seed,
                                              percentile.threshold.to.keep = percentile.threshold.to.keep,
                                              number.of.folds = nrow(subset.data))
      
    } else{
      
      #For each subset data, do random forest
      CV.results <- CVPredictionsRandomForest(inputted.data = subset.data,
                                              name.of.predictors.to.use = name.of.predictors.to.use,
                                              target.column.name = target.column.name,
                                              seed = seed,
                                              percentile.threshold.to.keep = percentile.threshold.to.keep,
                                              number.of.folds = number.of.folds)
      
    }
    
    
    CV.predictions <- CV.results[[1]]
    
    CV.features.selected <- CV.results[[2]]
    
    ##MCC
    predicted <- as.integer(CV.predictions)
    actual <- as.integer(subset.data[,target.column.name])
    MCC.val <- mltools::mcc(preds=predicted, actuals=actual)
    
    #Importance of features
    #1. If feature is not selected at all, then assign it a value of 0 in a new dataframe.
    names.of.features.selected <- names(CV.features.selected)
    names.of.features.not.selected.at.all <- setdiff(name.of.predictors.to.use, names.of.features.selected)
    
    #Create data frame for features not selected at all
    variables.with.sig.contributions <- names.of.features.not.selected.at.all
    Freq <- rep(0, length(names.of.features.not.selected.at.all))
    features.not.selected.dataframe <- data.frame(variables.with.sig.contributions, Freq)
    
    #Convert the table that describes selected features into a dataframe so that
    #it can be combined with the dataframe for features not selected at all
    CV.features.selected.dataframe <- as.data.frame(CV.features.selected)
    
    #If only one feature was selected, then the results have to be formatted differently
    if(length(CV.features.selected.dataframe) == 1){
      CV.features.selected.dataframe <- data.frame(colone = row.names(CV.features.selected.dataframe),
                                                      coltwo = CV.features.selected.dataframe[1,1])
      
      colnames(CV.features.selected.dataframe) <- c("variables.with.sig.contributions", "Freq")
    }
    
    #Combine the two dataframes above
    CV.features.selected.dataframe.with.features.not.selected <- rbind(CV.features.selected.dataframe,
                                                                          features.not.selected.dataframe)
    
    #2. Sort so that for each subset the features are in the same order. This is necessary
    #because for the pheatmap, we need all features in each column to be in the same order.
    target.order.of.features <- name.of.predictors.to.use
    sorted.dataframe <- CV.features.selected.dataframe.with.features.not.selected[match(target.order.of.features,
                                                                                           CV.features.selected.dataframe.with.features.not.selected$variables.with.sig.contributions),]
    
    
    #3. Use first column as row names, then delete first column.
    row.names(sorted.dataframe) <- sorted.dataframe$variables.with.sig.contributions
    subset.importance.values <- sorted.dataframe[2]
    
    #Combine importance values and MCC into one column.
    subset.importance.values <- rbind(subset.importance.values, MCC.val = MCC.val)
    
    #Convert dataframe to matrix first then...
    #Capture result for each subset
    subset.importance.values <- as.matrix(subset.importance.values)
    captured.importance.values <- cbind(captured.importance.values, subset.importance.values)
    
    #Add name to column
    col.name.for.subset <- paste(factor.name.for.subsetting, levels.in.column[i])
    colnames(captured.importance.values)[i] <- col.name.for.subset
    
  }
  
  ##Just make the pheatmap matrix in this command. When doing RF without LOOCV,
  #the gini index function is used first before another command converts
  #values to percentiles. For LOOCV pheatmap, the importance of features
  #is the percentage of time it occurs in the top percentile of importance values,
  #taking all rounds of LOOCV into consideration, want to use this directly without
  #needing to convert to percentile.
  
  matrix.for.pheatmap <- captured.importance.values
  
  #Add MCC to column names
  for(i in 1:dim(matrix.for.pheatmap)[2])
  {
    old.name <- colnames(matrix.for.pheatmap)[[i]]
    MCC.val <- matrix.for.pheatmap[dim(matrix.for.pheatmap)[1],][[i]]
    new.name <- paste(old.name, ", MCC: ", format(round(MCC.val, 2), nsmall = 2))
    colnames(matrix.for.pheatmap)[[i]] <- new.name
  }
  
  #Remove MCC row from matrix
  matrix.for.pheatmap.MCC.row.removed <- matrix.for.pheatmap[1:(dim(matrix.for.pheatmap)[1]-1),]
  
  return(matrix.for.pheatmap.MCC.row.removed)
  
}




#' Produce example data set for demonstrating package functions
#'
#' @return A dataframe.
#' 
#' @export
#'
GenerateExampleDataMachinelearnr <- function(){
  
  
  id = c("1a", "1b", "1c", "1d", "1e", "1f", "1g", "2a", "2b", "2c", "2d", "2e", "2f", "3a",
         "3b", "3c", "3d", "3e", "3f", "3g", "3h", "3i",
         "4a", "4b", "4c", "4d", "4e", "4f", "4g", "5a", "5b", "5c",
         "5d", "5e", "5f", "5g")
  
  x = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 35, 30, 40, 41, 42, 44, 46, 47, 48, 49, 54,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  
  y = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 40, 45, 27, 29, 20, 28, 21, 30, 31, 23, 24,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  
  a = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        18, 21, 22, 24, 26, 26, 27, 40, 41, 42, 44, 46, 47, 48)
  
  b = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        10, 11, 22, 15, 12, 13, 14, 27, 29, 20, 28, 21, 30, 31)
  
  sep.xy.ab = c("1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
                "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
                "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3", "1/2/3",
                "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5", "4/5",
                "4/5", "4/5", "4/5")
  
  actual = as.factor(c("1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2",
                       "2", "2", "3", "3", "3",
                       "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", "4", "4", "5",
                       "5", "5", "5", "5", "5", "5"))
  
  example.data <- data.frame(id, x, y, a, b, sep.xy.ab, actual)
  
  return(example.data)
  
}

