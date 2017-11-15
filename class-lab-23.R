# Class lecture 24 lab
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2017-11-15

rm(list = ls())

################################################################################
library(dplyr)
library(gee)
library(geepack)

################################################################################
# Probability, Bernoulli example
obesity <- read.table("data/muscatine.dat", na=".")
names(obesity) <- c("id", "gender", "baseage", "age", "occasion", "y") 
obesity <- obesity[which(!is.na(obesity$y)),]
# TODO: check if this centering value is correct
# Good chance it is supposed to be 10
obesity$cage <- obesity$age - 12 # "cage" stands for "centered age"
obesity$cage2 <- obesity$cage * obesity$cage
obesity$gender <- as.factor(obesity$gender)

fit.1.gee <- gee(y ~ gender + cage + cage2 + gender*cage + gender*cage2, 
                 id = id,
                 data = obesity, 
                 family = binomial, # Distributional assumption, handles BOTH link and variance function
                 corstr = "unstructured") # Correlation structure

# Summary for coefficient estimates
fit.1.summary <- summary(fit.1.gee)
fit.1.coef <- coef(fit.1.summary)

# Confidence intervals
CI.gender <- c(fit.1.coef[2, 1] + (c(-1, 1) * 1.96 * fit.1.coef[2, 4]))
CI.age <- c(fit.1.coef[3, 4] + c(-1, 1) * 1.96 * fit.1.coef[3, 4])

# p-values (two-sided)
p.values <- 2 * pnorm(abs(coef(fit.1.summary)[, 5]), lower.tail = FALSE)

rm(list = ls())
################################################################################
# Count, Poisson example
seizure <- read.table(file = "data/seizure.txt")
# Probably use "exchangable" correlation structure
