rm(list=ls())
toenail <- read.table("toenail.txt", header = TRUE)

#create database

toenail_Control <-subset(toenail,toenail$trt=="Control")
toenail_Test <-subset(toenail,toenail$trt=="Testing")

fit_con<-glm(infect ~  time+log(time-min(time)+1),data=toenail_Control,family = binomial() )
fit_test<-glm(infect ~  time+log(time-min(time)+1),data=toenail_Test,family = binomial() )
summary(fit_con)
summary(fit_test)

?predict
fitted = predict(fit_con)
fitted_2 = fit_con$fitted.values

plot(fitted~ toenail_Control$time)
plot(fitted_2~ toenail_Control$time)

(CI_con = round(exp(confint(fit_con)),2))
(exp(fit_con$coefficients))

#Package preload
install.packages("dotwhisker")
library(dotwhisker)
library(dplyr)

fit = glm(infect ~time + ftrt, data = toenail, family = binomial)
summary(fit)
exp(fit$coefficients)

str(toenail)

# install.packages("magrittr")
library(magrittr)
install.packages("gee")
library(gee)
?gee
dep_gee <- gee(infect ~ visit + time,
               data = toenail, 
               family = binomial,
               id = idnr)
summary(dep_gee)
exp(dep_gee$coefficients)

URL <- "http://static.lib.virginia.edu/statlab/materials/data/depression.csv"
dat <- read.csv(URL, stringsAsFactors = TRUE)
dat$id <- factor(dat$id)
dat$drug <- relevel(dat$drug, ref = "standard")
head(dat, n = 3)
str(dat)

summary(dat)

par(mfrow=c(1,2))
fit_con2 <- glm(infect ~time:visit, data= toenail_Control, family = binomial)
summary(fit_con2)
plot(exp(fit_con2$coefficients[-1]))

fit_test2 <- glm(infect ~time:visit, data= toenail_Test, family = binomial)
summary(fit_test2)
plot(exp(fit_test2$coefficients[-1]))
