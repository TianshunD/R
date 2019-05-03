# use a neural
#	net classifier to classify an image
# and report the overall accuracy, kappa
# coefficient, and confusion matrix of
#	the classification.
install.packages("nnet")
install.packages("e1071")

library("nnet")
library("raster")
library("rgdal")
library("class")
library("maptools")
library("caret")
library("e1071")

x <- brick("tahoe_highrez.tif")
training <- readOGR(dsn=getwd(),layer="tahoe_highrez_training_points")
testing <- readOGR(dsn=getwd(),layer="tahoe_highrez_testing_points")

machine_learning_classification = function(x, training, testing) 
{
  #extract the 3-bands of data at the training locations
  tahoe_knn_training = extract(x, training, df=T)
  x_extract_training = cbind(tahoe_knn_training[c(2,3,4)], data.frame(training)[2])
  
  #extract the 3-bands of data at the testing locations.
  tahoe_knn_test = extract(x, testing, df=T)
  x_extract_testing = cbind(tahoe_knn_test[c(2,3,4)], data.frame(testing)[2])
  
  #create the classifier based on the training data
  classifier = nnet(SPECIES ~ ., x_extract_training, size=2, rang=0.1, decay=5e-4, maxit=1000)
  x_predict = predict(classifier, tahoe_knn_test, type = "class")
  
  #use spCbind to cbind to a Spatial DataFrame
  training_points_w_spectra = spCbind(training, tahoe_knn_training)
  testing_points_w_spectra = spCbind(testing, tahoe_knn_test)
  
  tahoe_knn_training_classes = training_points_w_spectra$SPECIES
  set.seed(1)
  tahoe_knn = knn(train=tahoe_knn_training[c(2,3,4)], test=tahoe_knn_test[c(2,3,4)],cl=tahoe_knn_training_classes)
  
  knn_calc_function <- function(x){
    set.seed(1)
    tahoe_knn_factors <- knn(train=tahoe_knn_training,test=x,cl=tahoe_knn_training_classes)
    
    tahoe_knn_factors <- predict(classifier, x, type = "class")
    return(as.numeric(tahoe_knn_factors))
  }
  
  confusion_matrix <- confusionMatrix(table(x_predict, x_extract_testing$SPECIES))
  #return a list with element named "confusion_matrix" which are the confusionMatrix() function output
  returned_list = list("confusion_matrix"=confusion_matrix)
  return(returned_list)
}




