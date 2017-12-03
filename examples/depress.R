setwd("/Users/jzhou/Documents/Bin/SASUniversityEdition/myfolders/CPH684/Lec27/")

depress <- read.table("depress.txt",col.names=c("ID", "y", "severe", "drug", "time", "dt"))

library(lme4)

#Laplace approximation ##
fit.glmer <- glmer(y ~ 1 + severe + drug + time + dt + (1 | ID), data=depress, nAGQ=1,family = binomial(link = "logit"))
summary(fit.glmer)
confint(fit.glmer)

#Gaussian quadrature to implement ML -- 
#this is only implemented in the case of a single scalar random effect for the intercept
fit.glmer <- glmer(y ~ 1 + severe + drug + time + dt + (1 | ID), data=depress, nAGQ=25,family = binomial(link = "logit"))
summary(fit.glmer)
confint(fit.glmer)

