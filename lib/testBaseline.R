########################################
### Classification with testing data ###
########################################

testBaseline <- function(model, testingData, ntrees){
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  - processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  library("gbm")
  
  ### make predictions
  predBaseline <- predict(model, testingData, n.trees = 100)
  return(predBaseline)
}
