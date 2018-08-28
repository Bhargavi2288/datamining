myliverdata <- read.csv("C:/Users/bhargavinagaraj/Desktop/bupadata.csv", header=FALSE)
dim(myliverdata)

myresponse <- factor(myliverdata[,7])

mydf_liver <- data.frame(myresponse, 
                   myliverdata[,2:ncol(myliverdata)])
numobs <- nrow(mydf_liver)
numobs

                                                                                    
# train, validate, test framework

set.seed(123)
mylevels<-c(1,2)
library(kernlab)
randomsort <- sample(1:numobs, numobs, replace=FALSE)
randomsort
# lists of observations for train, validate, test
trainloc <- randomsort[1:floor(0.5*numobs)]
valloc <- randomsort[(floor(0.5*numobs)+1):floor(0.75*numobs)]
testloc <- randomsort[(floor(0.75*numobs)+1):numobs]
trainloc
valloc
testloc

Cparams <- c(0.01,0.1,1,10,100)
sigmas <- c(0.01,0.1,1,10,100)

trainSVM <- function(i) {
  cat(Cparams[i])
  # linear kernel
  mysvmlinear <- ksvm(myresponse ~ ., data=mydf_liver[trainloc,], 
                type="C-svc", kernel="vanilladot",
                C = Cparams[i]/length(trainloc)) 
  svmValPredict <- predict(mysvmlinear, newdata=mydf_liver[valloc,], 
                           type="response")
  misclassLin <- sum(svmValPredict != mydf_liver[valloc,7])/length(valloc)
  # Gaussian kernel
  misclassGau <- lapply(1:length(sigmas), FUN=tuneSigma, i)
  
  list(misclassrateLin=misclassLin, misclassrateGau=misclassGau)
}

tuneSigma <- function(k, i) {


myresults <- lapply(1:length(Cparams), FUN=trainSVM)
myresults

misclassrateLin <- sapply(myresults, 
                          FUN=function(x) x$misclassrateLin)
misclassrateGau <- sapply(myresults, 
                          FUN=function(x) x$misclassrateGau)

misclassrateLin
misclassrateGau

# train best settings on test data
mysvmTest <- ksvm(myresponse ~ ., data=mydf_liver[testloc,], 
                  type="C-svc", kernel="rbfdot",
                  C = Cparams[1]/length(testloc)
                  ,kpar=list(sigma=100)
                  )

# generate predictions on test data
svmTestPredict <- predict(mysvmTest, newdata=mydf_liver[testloc,],
                          type="response")
# report confusion matrix on test data
svmTestCM <- table(factor(mydf_liver[testloc,7], levels=mylevels),
                   factor(svmTestPredict, levels=mylevels))
svmTestCM
svmTestCM/rowSums(svmTestCM)

propcorr=(svmTestCM[1,1]+svmTestCM[2,2])/length(testloc)
propcorr
misclassrate<-1-propcorr
misclassrate





# install.packages("kernlab")
