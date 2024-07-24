library(LKT)
set.seed(41)
val<-largerawsample
val = setDT(val)
# make student stratified folds (for crossvalidation for unseen sample)
unq = sample(unique(val$Anon.Student.Id))
sfold = rep(1:5,length.out=length(unq))
val$fold = rep(0,length(val[,1]))
for(i in 1:5){val$fold[which(val$Anon.Student.Id %in% unq[which(sfold==i)])]=i}
#Clean it up
val$KC..Default.<-val$Problem.Name

# get the times of each trial in seconds from 1970
val$CF..Time.<-as.numeric(as.POSIXct(as.character(val$Time),format="%Y-%m-%d %H:%M:%S"))

#make sure it is ordered in the way the code expects
val<-val[order(val$Anon.Student.Id, val$CF..Time.),]

#create a binary response column to predict and extract only data with a valid value
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val<-val[val$CF..ansbin.==0 | val$CF..ansbin.==1,]

# create durations
val$Duration..sec.<-(val$CF..End.Latency.+val$CF..Review.Latency.+500)/1000
val_actual <- val[, c('Anon.Student.Id', 'Duration..sec.', 'Outcome', 'KC..Default.')]
data <- val_actual[, c('Duration..sec.', 'Outcome')]
data$NOutcome<-ifelse(tolower(data$Outcome)=="correct",1,0)
data_a <- data[, c('Duration..sec.', 'NOutcome')]
data_a <- as.matrix(data_a[sample(nrow(data_a), 1000), ])
heatmap(data_a)