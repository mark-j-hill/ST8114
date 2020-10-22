## Script written for ST 8114 group project: Cohort study on health drivers associated with diastolic blood pressure.
## by Mark Hill


# library/read in of data -------------------------------------------------

rm(list = ls())
library(tidyverse)
library(ggpubr)
library(car)


dat <- read.csv("cohort_DIABP_female.txt", header = TRUE, sep = "")
dat$SMOKING <- as.factor(dat$SMOKING)
dat$HTNMED <- as.factor(dat$HTNMED)
attach(dat)

# Test assumptions of normality

densityPlot(DIABP)

ggplot(dat)+
  stat_density(aes(x= DIABP, fill= HTNMED) )

ggplot(dat, aes(sample = DIABP))+
  stat_qq()+
  stat_qq_line()

qqPlot(DIABP)

shapiro.test(DIABP)


### i: how is each explanatory variable associated with the response variable (c(AGE,SMOKING,HTNMED) ~ DIABP)

# DIABP ~ AGE

cor(AGE, DIABP)
lm(DIABP ~ AGE)

ggplot(dat,
       aes(x = AGE, y= DIABP))+
  geom_point()+
  geom_smooth(method = "lm")

# DIABP ~ SMOKING
compare_means(DIABP ~ SMOKING, data = dat)

tests <- list(c("current-smoker", "non-smoker"), c("current-smoker", "past-smoker"), c("non-smoker","past-smoker"))

ggplot(dat, aes(y= DIABP, x = SMOKING, fill= SMOKING))+
  geom_boxplot()+
  
    ggtitle("Comparison of Diastolic Blood Pressure distribution for factor \n SMOKING")

model <- lm(DIABP ~ SMOKING)
summary(model)$coef


# DIABP ~ HTNMED

ggplot(dat,
      aes(x= HTNMED, y= DIABP, fill= HTNMED))+
    geom_boxplot()+
    ggtitle("Comparison of Diastolic Blood Pressure distribution for factor \n HYPERTENSION MEDS")

model2 <- lm(DIABP ~ HTNMED)
summary(model2)$coef


t.test(DIABP ~ HTNMED, alternative = "less", var.equal =  TRUE)

# Model which contains both predictor vars

model3 <- lm(DIABP ~ SMOKING + HTNMED)
summary(model3)$coef
Anova(model3)
