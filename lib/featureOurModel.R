#############################################################
### Construct features and responses for training images  ###
#############################################################

featureOurModel <- function(input_list=fiducial_pt_list, index){
  
  distance <- function(data){
    n<-78
    dist <- vector()
    for (i in 1:(n-1)){
      for (j in (i+1):n){
        ab <- sqrt((data[i,1]-data[j,1])^2+(data[i,2]-data[j,2])^2)
        dist <- c(dist,ab)
      }
    }
    return(dist)
  }
  
  pairwise_dist_feature <- t(sapply(input_list[index], distance))
  pairwise_data <- cbind(pairwise_dist_feature, info$emotion_idx[index])
  colnames(pairwise_data) <- c(paste("feature", 1:(ncol(pairwise_data)-1), sep = ""), "emotion_idx")
  pairwise_data <- as.data.frame(pairwise_data)
  # pairwise_data$emotion_idx <- as.factor(pairwise_data$emotion_idx)
  return(feature_df = pairwise_data)
}
