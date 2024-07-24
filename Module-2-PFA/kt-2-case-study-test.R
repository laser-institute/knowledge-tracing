library(LKT)
set.seed(41)
val<-largerawsample
print(val)
val$KC..Default.<-val$Problem.Name
val$fold<-sample(1:5,length(val$Anon.Student.Id),replace=T)
