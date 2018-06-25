install.packages("xlsx")
require(xlsx)
mydata<- read.csv("QualityMsrMDS_Download-12-11.csv")
mydata
dim(mydata)
#### data for only Missouri state
mydata_MO =mydata[mydata$STATE == "MO",]
