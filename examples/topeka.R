rm(list=ls())
setwd("/Users/jzhou/GoogleDrive/Teaching/CPH684/JinZhou-Fall17/Lec4-IntroLinearRegressionANOVA/Example/")

topeka <- read.table("topeka.txt")
names(topeka) <- c("id", "height", "age", "logfev1")
topeka$logheight <- log(topeka$height)

mod.1 <- lm(logfev1~age+logheight,data=topeka)
summary(mod.1)
plot(mod.1)

tox <- read.table("tox.txt")
names(tox) <- c("y","drug")
tox <- within(tox, drug<- as.factor(drug))

mod.2 <- lm(y~drug,data=tox)
plot(y~drug,data=tox)
anova(mod.2)