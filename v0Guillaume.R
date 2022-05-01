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
                     visit = visit,infect=factor(infect))
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
with(toenail,boxplot(visit ~ infect))
#we note a difference.
#The visite is on average lower for infected persons


COL=c("blue","red")
quanti <- quantile(toenail$visit,seq(0,1,length.out = 7))
toenail <- transform(toenail, vis6 = cut(time, c(-Inf,quanti[2:(length(quanti)-1)],Inf)))

round(prop.table(table(toenail$vis6,toenail$infect),1)*100,1)


# <!-- Graph -->
plot(infect~vis6, data = toenail,col=COL)
#We can note a decline in growth. MOre the visit number it's importante, 
#more proportion is infect=0


#test
with(toenail,t.test(visit~infect)) #p-value < 2.2e-16 #a big difference.


#What relationship

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

#No visit
fit5 <- glm(infect ~time+log(time-min(time)+1),data=toenail,family = binomial() )
anova(fit2,fit5,test="LRT") #fit5 better without visit


#create database

toenail_Control <-subset(toenail,toenail$trt=="Control")
toenail_Test <-subset(toenail,toenail$trt=="Testing")

#compare modele

fit_con<-glm(infect ~ time+log(time-min(time)+1),data=toenail_Control,family = binomial() )
fit_test<-glm(infect ~ time+log(time-min(time)+1),data=toenail_Test,family = binomial() )
summary(fit_con)
summary(fit_test)

#The model seems much better fitted to the test observation.

##with trt

fit6 <- glm(infect ~trt+time+log(time-min(time)+1),data=toenail,family = binomial() )

###Analyse

summary(fit6) #p_values trtTesting = 10%

###find the proba for each time

##Testing
t<-data.frame(time=seq(0,14,by=1),trt=rep("Testing",15))
##control
c<-data.frame(time=seq(0,14,by=1),trt=rep("Control",15))


lt<-predict(fit6, newdata = t, interval = "confidence",se.fit=TRUE)
lc<-predict(fit6, newdata = c, interval = "confidence",se.fit=TRUE)

logtest<-data.frame(test0.025=(lt$fit - 1.96*lt$se.fit),test=c((lt$fit)),test0.975=(lt$fit + 1.96*lt$se.fit))
logcontrol<-data.frame(cont0.025=(lc$fit - 1.96*lc$se.fit),cont=c((lc$fit)),cont0.975=(lc$fit + 1.96*lc$se.fit))


par(mar = c(4, 4, 1, 1) + 0.1)
plot(c(0,14), c(-4,0), xlab = "Time [months]", ylab = "logit prob. of infection", type = "n")
lines(seq(0,14,by=1), logcontrol$cont, col = "red", lwd = 2)
lines(seq(0,14,by=1), logcontrol$cont0.025, col = "orange", lwd = 2)
lines(seq(0,14,by=1), logcontrol$cont0.975, col = "orange", lwd = 2)
lines(seq(0,14,by=1), logtest$test, col = "darkgreen", lwd = 2)
lines(seq(0,14,by=1), logtest$test0.025, col = "green", lwd = 2)
lines(seq(0,14,by=1), logtest$test0.975, col = "green", lwd = 2)
legend(9, -1, legend = c("Control","interval Control","Testing","interval Testing"), col = c("red","orange","darkgreen","green"), lty = 1, lwd = 2)




#The probability of infection of group Control is always higher than
#group Testing  at any time.

###empirique

iTAB <- with(toenail, table(time6, infect, trt))
(pCont <- prop.table(iTAB[,,1], margin = 1)[,2])
(logitCont <- log(pCont / (1 - pCont)))
x1<-with(toenail,tapply(time, time6, mean))

(pTest <- prop.table(iTAB[,,2], margin = 1)[,2])
(logitTest <- log(pTest / (1 - pTest)))


par(mar = c(4, 4, 1, 1) + 0.1)
plot(c(0,14), c(-3,0), xlab = "Time [months]", ylab = "logit prob. of infection", type = "n")
lines(seq(0,14,by=1), logcontrol$cont, col = "red", lwd = 2)
lines(x1, logitCont, col = "orange", lwd = 2)
lines(seq(0,14,by=1), logtest$test, col = "darkgreen", lwd = 2)
lines(x1, logitTest, col = "green", lwd = 2)
legend(8, 0, legend = c("Control-model","Control-empirique","Testing-model","Testing-empirique"), col = c("red","orange","darkgreen","green"), lty = 1, lwd = 2)

###
library(xtable)
xtable(summary(fit5))

##confidance interval

confint(fit5)
