########################################
### Classification with testing data ###
########################################

testOurModel <- function(model, testingData){
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  - processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  library("e1071")
  
  ### make predictions
  pred <- predict(model, testingData)
  return(pred)
}
