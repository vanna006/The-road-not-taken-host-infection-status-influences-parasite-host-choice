## General Linear mixed models
### with Chamber as variable (to nest each miracidia within chamber), and direction of chamber as a variable (to test for random preferences/bias, if any)

setwd("F:/A_in_prep/Chambers/Thomas")

# Download packages and load libraries: 

library(glmm)
library(lme4)
library(nlme)
library(arm)

#Data should be formatted so the choices are bernoulli (1 or 0 for direction chose). '1' was our predicted direction, 0 was opposite.

## Ex data sheet column names:     Choice(1 or 0)       Treatment       Chamber(e.g. trial)  


# load data

data <- read.csv("chambers_R.csv", header = TRUE)
schisto <- data[1:632,]
echino <- data[632:1356,]
# Run model
attach(data)

cham <- as.factor(trial_num)

model <- glmm(Choice ~ 0 + Treatment,
              random = list(~0 + cham),
              varcomps.names = c("Trial"),
              data = data, family.glmm = bernoulli.glmm,
              m = 10^3, debug = TRUE)

# varcomps.names are T for trial and D for direction but can be whatever you want. 

# summary of results
summary(model)

detach(data)

zscores <- read.csv("zscores.csv", header = TRUE)
attach(zscores)
summary(zscores)

hist(econtrol, breaks = 10)
shapiro.test(econtrol)
hist(eecontrol, breaks = 10)
shapiro.test(eecontrol)
hist(eepcontrol, breaks = 10)
shapiro.test(eepcontrol)
hist(ees, breaks = 10)
shapiro.test(ees)
hist(eesun, breaks = 10)
shapiro.test(eesun)
hist(eeun, breaks = 10)
shapiro.test(eeun)
hist(evs, breaks = 10)
shapiro.test(evs)
hist(evun, breaks = 10)
shapiro.test(evun)
hist(pcontrol, breaks = 10)
shapiro.test(pcontrol)
hist(schistoun, breaks = 10)
shapiro.test(schistoun)
hist(Trial, breaks = 10)
shapiro.test(Trial)

#calculate the p value from the average z score from 30 runs of the MCML model
#insert z value where 'z' is located
m1 <- mean(econtrol)
2*pnorm(-abs(m1))
m2 <- mean(eecontrol)
2*pnorm(-abs(m2))
m3 <- mean(eepcontrol)
2*pnorm(-abs(m3))
m4 <- mean(ees)
2*pnorm(-abs(m4))
m5 <- mean(eesun)
2*pnorm(-abs(m5))
m6 <- mean(eeun)
2*pnorm(-abs(m6))
m7 <- mean(evs)
2*pnorm(-abs(m7))
m8 <- mean(evun)
2*pnorm(-abs(m8))
m9 <- mean(pcontrol)
2*pnorm(-abs(m9))
m10 <- mean(schistoun)
2*pnorm(-abs(m10))
m11 <- mean(Trial)
2*pnorm(-abs(m11))

