### Initial operations
### ++++++++++++++++++++++++++++++++++++++++++++
#setwd("/home/komarek/teach/mff_2021/nmst432_AdvRegr/Problem_5/")
#
toenail <- read.table("toenail.txt", header = TRUE)

dim(toenail)
head(toenail)
summary(toenail)

toenail[1:12,]


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


### Scatterplot
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
(tCont <- with(subset(toenail, trt == 0), tapply(time, visit, mean)))
(tTest <- with(subset(toenail, trt == 1), tapply(time, visit, mean)))
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

