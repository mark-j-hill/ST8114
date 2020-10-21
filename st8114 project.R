## Script written for ST 8114 group project: Cohort study on health drivers associated with diastolic blood pressure.
## by Mark Hill


# library/read in of data -------------------------------------------------

rm(list = ls())
library(tidyverse)
library(ggpubr)

dat <- read.csv("cohort_DIABP_female.txt", header = TRUE, sep = "")
dat$SMOKING <- as.factor(dat$SMOKING)
dat$HTNMED <- as.factor(dat$HTNMED)
attach(dat)
### i: how is each explanatory variable associated with the response variable (c(AGE,SMOKING,HTNMED) ~ DIABP)

# DIABP ~ AGE

cor(AGE, DIABP)
lm(DIABP ~ AGE)

ggplot(dat,
       aes(x = AGE, y= DIABP))+
  geom_point()+
  geom_smooth(method = "lm")

# SMOKING ~ DIABP

ggplot(dat)+
  geom_boxplot(
    aes(y= DIABP, x = SMOKING, fill= SMOKING))+
    ggtitle("Comparison of Diastolic Blood Pressure distribution for factor \n SMOKING")

model <- lm(DIABP ~ SMOKING)
summary(model)$coef

# SMOKING ~ HTNMED

ggplot(dat,
      aes(x= HTNMED, y= DIABP, fill= HTNMED))+
    geom_boxplot()+
    ggtitle("Comparison of Diastolic Blood Pressure distribution for factor \n HYPERTENSION MEDS")

model2 <- lm(DIABP ~ HTNMED)
summary(model2)$coef


# Model which contains both predictor vars

model3 <- lm(DIABP ~ SMOKING + HTNMED)
Anova(model3)
