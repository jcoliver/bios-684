rm(list=ls())

#pull in data #
setwd("/Users/jzhou/Dropbox/Teaching/CPH684/JinZhou-15Fall/Lec7-StatBasis-II/Example/")

tlc.wide <- read.table("lead.txt")
colnames(tlc.wide) <- c("id","y1","y2","y3","y4")

tlc.long <- reshape(tlc.wide,varying=c("y1","y2","y3","y4"), v.names="y",
                    timevar="time",
                    times = seq(1,4), direction="long")

tlc.long.sort <- tlc.long[order(tlc.long$id),]
tlc.long.sort[1:10,]

a=aggregate(y~as.numeric(time),data=tlc.long.sort,FUN=mean) 
aggregate(y~as.numeric(time),data=tlc.long.sort,FUN=sd) 
#plot means
plot(a[,1],a$y,pch=19,type="o",main="Mean BL",xlab="Time",ylab="BL")

library(nlme)

time.f <- as.character(tlc.long.sort$time)
tlc.long.sort <- cbind(tlc.long.sort,time.f)
tlc.long.sort <- within(tlc.long.sort, time.f <- relevel(time.f, ref = "1"))

fit.full.ML <- gls(y~time.f-1,data=tlc.long.sort,
                   corr=corSymm(,form=~1|id),
                   weights = varIdent(form = ~ 1 | time.f),method="ML")

fit.full.REML <- gls(y~time.f-1,data=tlc.long.sort,
                     corr=corSymm(,form=~1|id),
                     weights = varIdent(form = ~ 1 | time.f),method="REML")

fit.linear.wint.ML <- gls(y~time,data=tlc.long.sort,
                          corr=corSymm(,form=~1|id),
                          weights = varIdent(form = ~ 1 | time),
                          method="ML")

summary(fit.full.ML)
summary(fit.full.REML)
summary(fit.linear.wint.ML)
getVarCov(fit.full.ML)
getVarCov(fit.full.REML)
getVarCov(fit.linear.wint.ML)

anova(fit.linear.wint.ML,fit.full.ML)