feature_hog <- function(img_dir, index){
  hog <- vector()
  for (i in index){
    a <- readImage(paste0(img_dir, sprintf("%04d", i), ".jpg"))
    hog <- rbind(hog, HOG(a))
  }
  colnames(hog) <- c(paste("HOG", 1:(ncol(hog)), sep = ""))
  hog <- data.frame(hog)
  return(feture_hog = hog)
}