setwd("C:/Unizeug/Übungen/CU_AdvRegressionModels/Problem-5/")
rm(list=ls())

library("Epi")
library("splines")
library("foreign")
library(questionr)
library("latex2exp")

# initial exploration
toenail <- read.table("toenail.txt", header = TRUE)

dim(toenail)
head(toenail)
summary(toenail)

toenail[1:12,]

#set up Graphics device outside of RStudio
dev.new() # a new window should pop up


### Derived variables
### +++++++++++++++++++++++++++
toenail <- transform(toenail, 
                     ftrt = factor(trt, levels = 0:1, labels = c("Control", "Testing")),
                     fvisit = factor(visit))
summary(toenail)
length(unique(toenail[, "idnr"]))  ### number of patients


### Jittered version of infect variable
### (useful for plotting)
### ++++++++++++++++++++++++++++++++++++++++++++
set.seed(951913282)
toenail <- transform(toenail, jinfect = infect + runif(nrow(toenail), -0.1, 0.1))


### Scatterplot of jittered version of infect variable
### ++++++++++++++++++++++++++++++++++++++++++++
COL <- c("red3", "darkgreen")
BG <- c("pink", "aquamarine")
PCH <- c(21, 23)
names(COL) <- names(BG) <- names(PCH) <- levels(toenail[, "ftrt"])
#
par(mar = c(4, 4, 1, 1) + 0.1)
with(toenail, plot(time, jinfect, pch = PCH[ftrt], col = COL[ftrt], bg = BG[ftrt], xlab = "Time [months]", ylab = "Infection"))
abline(h = c(0, 1), col = "grey40", lty = 2)
legend(13, 0.75, legend = names(PCH), pch = PCH, col = COL, pt.bg = BG)


### Empirical probabilities of infection
### and related quantities for additional plotting
### ++++++++++++++++++++++++++++++++++++++++++++++++

### Frequency tables
iTAB <- with(toenail, table(visit, infect, ftrt))
print(iTAB)

### Empirical probabilities of infection
prop.table(iTAB[,,1], margin = 1)     ### proportions in Control group
prop.table(iTAB[,,2], margin = 1)     ### proportions in Testing group

### Empirical probabilities of infection again
(pCont <- prop.table(iTAB[,,1], margin = 1)[,2])
(pTest <- prop.table(iTAB[,,2], margin = 1)[,2])

### Mean time of each visit
(tCont <- with(subset(toenail, trt == 0), tapply(time, visit, mean))) #control
(tTest <- with(subset(toenail, trt == 1), tapply(time, visit, mean))) #testing
(TLIM <- range(c(tCont, tTest)))


### Scatterplot with empirical probabilities of infection
### per visit
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
COL2  <- c("red3", "darkgreen")
names(COL2)  <- levels(toenail[, "ftrt"])
#
par(mar = c(4, 4, 1, 1) + 0.1)
with(toenail, plot(time, jinfect, pch = PCH[ftrt], col = COL[ftrt], bg = BG[ftrt], xlab = "Time [months]", ylab = "Infection"))
abline(h = c(0, 1), col = "grey40", lty = 2)
lines(tCont, pCont, col = COL2["Control"], lwd = 2)
points(tCont, pCont, pch = PCH["Control"], bg = COL2["Control"], col = COL["Control"], cex = 2)
lines(tTest, pTest, col = COL2["Testing"], lwd = 2)
points(tTest, pTest, pch = PCH["Testing"], bg = COL2["Testing"], col = COL["Testing"], cex = 2)
legend(13, 0.75, legend = names(PCH), pch = PCH, col = COL, pt.bg = BG, bty = "n")
legend(11.5, 0.75, legend = c("", ""), pch = PCH, col = COL2, pt.bg = COL2, lwd = 2, bty = "n")


### Empirical logits of infection
### +++++++++++++++++++++++++++++++++++++++++
(logitCont <- log(pCont / (1 - pCont)))
(logitTest <- log(pTest / (1 - pTest)))
(LLIM <- range(c(logitCont, logitTest)))

### Plot
par(mar = c(4, 4, 1, 1) + 0.1)
plot(TLIM, LLIM, xlab = "Time [months]", ylab = "Logit of prob. of infection", type = "n")
lines(tCont, logitCont, col = COL2["Control"], lwd = 2)
points(tCont, logitCont, pch = PCH["Control"], col = COL["Control"], bg = BG["Control"], cex = 1.5)
lines(tTest, logitTest, col = COL2["Testing"], lwd = 2)
points(tTest, logitTest, pch = PCH["Testing"], col = COL["Testing"], bg = BG["Testing"], cex = 1.5)
legend(7, -0.5, legend = names(PCH), col = COL2, lty = 1, lwd = 2)

#TASK FOR YOU: Use standard logistic model (in which independence of observations is assumed) and develop reasonable model capturing evolution of probabilities (1) of infection over time. Explain, how those probabilities are modelled (in your final model) and provide estimates of the model parameters (including confidence intervals) obtained using a method of maximum-likelihood while assuming independence of observations. Plot estimated versions of the two functions (1) in one plot with empirical probabilities of infection per visit.

#split data
toenail_control = subset(toenail, ftrt == 'Control')
toenail_testing = subset(toenail, ftrt == 'Testing')
#try overall model
mod_all_time <- glm(formula = infect ~ (time + ftrt)^2, family = binomial, data = toenail)
summary(mod_all_time) #well could be better
drop1(mod_all_time)

mod_all_fvisit <- glm(formula = infect ~ (fvisit + ftrt)^2, family = binomial, data = toenail)
summary(mod_all_fvisit)
drop1(mod_all_fvisit)

mod_log <- glm(formula = infect ~ log(time+1) + ftrt + ftrt:log(time+1), family = binomial, data = toenail) #no significant improvement

#do separate models for control and testing group
tspline_control <- splinefun(tCont, logitCont)
tspline_test <- splinefun(tTest, logitTest)

mod_c.0 <- glm(formula = infect ~ time, family = binomial, data = toenail_control)
mod_c.10 <- glm(formula = infect ~ log(time+1), family = binomial, data = toenail_control)
anova(mod_c.0, mod_c.10, test = "LRT") # worse deviance, rejected
mod_c.10a <- glm(formula = infect ~ log(time+1) + time, family = binomial, data = toenail_control) ##try with log + linear
anova(mod_c.0, mod_c.10a, test = "LRT") # no significance, rejected

mod_c.11 <- glm(formula = infect ~ I(time^2)+time, family = binomial, data = toenail_control) # try quadratic transformation
anova(mod_c.0, mod_c.11, test = "LRT") # significance, take this model of quadratic transformation

mod_c.12 <- glm(formula = infect ~ I(time^2)+time + log(time+1), family = binomial, data = toenail_control)
summary(mod_c.12) #coef for log looks interesting
anova(mod_c.11, mod_c.12, test = "LRT") # on the edge, but without log(.) should be kept

#plot transformation
trafo_time <- function(x) coef(mod_c.11)['(Intercept)'] + coef(mod_c.11)['time'] * x + coef(mod_c.11)['I(time^2)'] * x*x
x_seq = seq(from = 0, to = 13)
lines(x_seq, trafo_time(x_seq))

mod_t.0 <- glm(formula = infect ~ time, family = binomial, data = toenail_testing)
mod_t.11 <- glm(formula = infect ~ I(time^2)+time, family = binomial, data = toenail_testing) # try quadratic transformation
anova(mod_t.0, mod_t.11, test = "LRT") # edge, but keep it for comparison reasons

#models for each group
summary(mod_c.11)
summary(mod_t.11)

expcoef_control <- round(exp(coef(mod_c.11)),4)
expcoef_testing <- round(exp(coef(mod_t.11)),4)
