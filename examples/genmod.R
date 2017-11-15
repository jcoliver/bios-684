library(dplyr)
library(gee)
library(geepack)
#install.packages("alr", repos="http://R-Forge.R-project.org")
#library(alr)

setwd("/Users/jzhou/GoogleDrive/Teaching/CPH684/JinZhou-17Fall/Lec24-GEELab/")
obesity<-read.table("muscatine.dat",na=".")
names(obesity) <- c("id", "gender", "baseage", "age", "occasion", "y") 
obesity=obesity[which(!is.na(obesity$y)),]
cage=obesity$age-12
cage2=cage*cage
gender=as.factor(gender)

#fit using package gee
fit.1.gee <- gee(y~gender+cage+cage2+gender:cage+gender:cage2, 
                 id=id,
                 data=obesity, 
                 family=binomial,
                 corstr="unstructured")
summary(fit.1.gee)
cc=coef(summary(fit.1.gee))
#CI:
CI.gender=c(cc[2,1]-1.96*cc[2,4],cc[2,1]+1.96*cc[2,4]);CI.gender
CI.cage=c(cc[3,1]-1.96*cc[3,4],cc[3,1]+1.96*cc[3,4]);CI.cage
#p-values
2*pnorm(abs(coef(summary(fit.1.gee))[,5]), lower.tail = FALSE)

#Alternative logistic regression for GEE (logOR for association)
#a1 <- alr(y~gender+cage+cage2+gender:cage+gender:cage2, 
#          id=id,
#         data=obesity, 
#          depm="exchangeable", 
#          ainit=0.01)
#cc=coef(summary(a1))
#CI:
#CI.gender=c(cc[2,1]-1.96*cc[2,2],cc[2,1]+1.96*cc[2,2]);CI.gender
#CI.cage=c(cc[3,1]-1.96*cc[3,2],cc[3,1]+1.96*cc[3,2]);CI.cage
#p-values
#2*pnorm(abs(coef(summary(a1))[,3]), lower.tail = FALSE)




