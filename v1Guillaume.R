### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work5/Problem-5-1")
#
toenail <- read.table("data.txt", header = TRUE)

dim(toenail)
summary(toenail)

### Derived variables
### +++++++++++++++++++++++++++
toenail <- transform(toenail, 
                     trt = factor(trt, levels = 0:1, labels = c("Control", "Testing")),
                     infect=factor(infect))
summary(toenail)
length(unique(toenail[, "idnr"]))  ### number of patients = 294


###analyse groupe test

toenail_Test <-subset(toenail,toenail$trt=="Testing")

#Let's keep only the individuals having made 7 visits.
t7<-toenail_Test[which(toenail_Test$visit==7),]$idnr #number individu with 7 visit

toenail_Test7 <- subset(toenail_Test,toenail_Test$idnr %in% t7) 

#individuals with all the data.
t7a<-as.numeric(names(which(sort(table(toenail_Test7$idnr))==7)))

toenail_Test7a <- subset(toenail_Test7,toenail_Test7$idnr %in% t7a) 
#We have 117 individual