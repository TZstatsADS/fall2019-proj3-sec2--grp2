###########################################################
### Train a classification model with training features ###
###########################################################
trainBaseline <- function(trainingData, desiredDistribution, desiredNumberOfTrees, desiredShrinkage, desiredInteractionDepth, desiredNobs, desiredCVFolds){
  ### Train an GBM model using processed features from training images
  
  ### Input:
  ### - a data frame containing features and labels
  ### - a parameter list
  ### Output: trained model
  
  ### load libraries
  library("gbm")
  
  ### Train with GBM
  ftGBM <- gbm(
    formula = as.factor(emotion_idx) ~ .,
    data = trainingData,
    distribution = desiredDistribution,
    n.trees = desiredNumberOfTrees,
    bag.fraction=0.65,
    shrinkage = 0.1,
    cv.folds=3)

  return(ftGBM)
}

