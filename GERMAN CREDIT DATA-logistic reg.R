credit <- read.csv("germancredit.csv")
credit
credit$Default <- factor(credit$Default)

## re-level the credit history and a few other variables
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), 
                         labels=c("foreign","german"))


credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",
                            NA,"edu","biz","biz")

## for demonstration, cut the dataset to these variables
### select only these columns
credit <- credit[,c("Default","duration","amount","installment","age","history", 
                    "purpose","foreign","rent")]
credit[1:3,]
summary(credit) # check out the data

## create a design matrix 
## factor variables are turned into indicator variables 
## the first column of ones is omitted
Xcred <- model.matrix(Default~.,data=credit)[,-1] 
Xcred[1:3,]

## creating training and prediction datasets
## select 900 rows for estimation and 100 for testing
set.seed(1)
train <- sample(1:1000,900)
xtrain <- Xcred[train,]
xnew <- Xcred[-train,]
ytrain <- credit$Default[train]
ynew <- credit$Default[-train]
credglm=glm(Default~.,family=binomial,data=data.frame(Default=ytrain,xtrain))
summary(credglm)

## prediction: predicted default probabilities for cases in test set
ptest <- predict(credglm,newdata=data.frame(xnew),type="response")
data.frame(ynew,ptest)

## What are our misclassification rates on that training set? 
## We use probability cutoff 1/6
## coding as 1 (predicting default) if probability 1/6 or larger
gg1=floor(ptest+(5/6))

ttt=table(ynew,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/100
error
