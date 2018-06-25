install.packages("lpSolve")
library(lpSolve)
f.obj<-c(40,50)
f.obj
f.con <-matrix(c(1,2,4,3), nrow =2 , byrow =TRUE)
f.con
f.rhs<- c(40, 120)
f.rhs
f.dir<-c("<=","<=")
myout <-lp("max",f.obj, f.con,f.dir,f.rhs)

?lp
summary(myout)
myout$solution
myout$objval

###############################

f.con1 <-matrix (c ( 6, 4, 2, 3, 1, 0, 1, 0, 0, 1, 20, 96, 12, 8, 30, 0, 52, 150, 3, 26, 3, 4, 5, 6, 7, 2, 1, 9, 1, 3, 5, 2, 3, 4, 0, 0, 1, 0, 0, 3) , nrow =4, byrow =TRUE )
f.obj1 <-  c(0.25, 0.22, 0.1, 0.12,0.1, 0.09, 0.4, 0.27, 0.5, 0.12)
f.rhs1 <- c( 17, 400, 20, 12)
f.dir1<- c(">=", ">=", ">=", ">=")

myout1 <-lp("min",f.obj1, f.con1,f.dir1,f.rhs1)

summary(myout1)
myout1$solution
myout1$objval

#####dual of the LP

f.con2<-t(f.con1)  #Transpose the coefficient matrix
f.temp2 <- f.obj1
f.obj2 <- f.rhs1
f.rhs2 <- f.temp2
f.dir2 <- c("<=", "<=", "<=", "<=","<=", "<=", "<=", "<=","<=", "<=")
Out2<-lp ("max", f.obj2, f.con2, f.dir2, f.rhs2)
Out2$solution

Out2$objval

