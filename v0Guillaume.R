### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
setwd("C:/Users/guill/OneDrive/Documents/Charles_University/Advanced Regression Models/Work5")
#
toenail <- read.table("data.txt", header = TRUE)

dim(toenail)
summary(toenail)

### Derived variables
### +++++++++++++++++++++++++++
toenail <- transform(toenail, 
                     trt = factor(trt, levels = 0:1, labels = c("Control", "Testing")),
                     visit = factor(visit),infect=factor(infect))
summary(toenail)
length(unique(toenail[, "idnr"]))  ### number of patients = 294

### Jittered version of infect variable
### (useful for plotting)
### ++++++++++++++++++++++++++++++++++++++++++++

#So we have 4 variables. 

#Y : infect
# X : trt, time, visit

#####link beetween Y and X variable#### : 

###Y vs trt
# 2 factors variables 

table1<-with(toenail,table(trt,infect))
round(prop.table(table1,margin=1)*100,2)

#The control group appears more infected than the test group. 
mosaicplot(table1, las = 3, shade = TRUE)
with(toenail,chisq.test(trt,infect))
#The variables seem to have very low independence
#maybe normal

###Y vs time
# 1 factors variables and 1 continues.

with(toenail,boxplot(time ~ infect))
#we note a difference.
#The time is on average lower for infected persons


COL=c("blue","red")
quanti <- quantile(toenail$time,seq(0,1,length.out = 7))
toenail <- transform(toenail, time6 = cut(time, c(-Inf,quanti[2:(length(quanti)-1)],Inf)))

round(prop.table(table(toenail$time6,toenail$infect),1)*100,1)

# <!-- Graph -->
plot(infect~time6, data = toenail,col=COL)
#We can note a decline in growth. MOre the time it's importante, 
#more proportion is infect=0


#test
with(toenail,t.test(time~infect)) #p-value < 2.2e-16 #a big difference.

#What relationship

fit_time0<-glm(infect~time,data=toenail,family = binomial())
fit_time1<-glm(infect~I(time^2),data=toenail,family = binomial())
anova(fit_time0,fit_time1,test="LRT")
#fit_time0 better

fit_time2<-glm(infect~log(time-min(time)+1),data=toenail,family = binomial())
anova(fit_time0,fit_time2,test="LRT")
#fit_time0 better
fit_time3<-glm(infect~time+log(time-min(time)+1),data=toenail,family = binomial())
anova(fit_time0,fit_time3,test="LRT")
#fit_time3 better (not a lot)




###Y vs visit
# 2 factors variables 

table2<-with(toenail,table(infect,visit))
round(prop.table(table2,margin=1)*100,2)
#Infected people have a low number of visits in contrast to the uninfected.

#difference

mosaicplot(table2, las = 3, shade = TRUE)
with(toenail,chisq.test(infect,visit))
#The variables seem to have very dependence p-value < 2.2e-16

#first model

fit0 <- glm(infect ~  visit + time,data=toenail,family = binomial() )
fit1 <- glm(infect ~ visit + time -1,data=toenail,family = binomial() )

summary(fit0)
summary(fit1)
anova(fit0,fit1,test="LRT")
#same

fit2 <- glm(infect ~ visit + time+log(time-min(time)+1),data=toenail,family = binomial() )
anova(fit0,fit2,test="LRT") #fit2 better

#interaction

fit3 <- glm(infect ~ visit + time + log(time-min(time)+1) + visit:time ,data=toenail,family = binomial() )

anova(fit2,fit3,test="LRT") #fit2 better

fit4 <- glm(infect ~ visit + time + log(time-min(time)+1)+visit:log(time-min(time)+1),data=toenail,family = binomial() )
anova(fit2,fit4,test="LRT") #fit2 better



