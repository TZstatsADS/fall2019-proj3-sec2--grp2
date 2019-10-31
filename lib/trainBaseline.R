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
    n.trees = desiredNumberOfTrees
  )

  return(ftGBM)
}

