###########################################################
### Train a classification model with training features ###
###########################################################
trainOurModel <- function(trainingData, desiredKernel, desiredCostRange){
  ### Train an SVM model using processed features from training images
  
  ### Input:
  ### - a data frame containing features and labels
  ### - a parameter list
  ### Output: trained model
  
  ### load libraries
  library("e1071")
  
  ### Train with SVM
  fitSVM <- svm(as.factor(emotion_idx) ~ ., 
                data=trainingData, 
                kernel = desiredKernel, 
                ranges = desiredCostRange)

  return(fitSVM)
}

