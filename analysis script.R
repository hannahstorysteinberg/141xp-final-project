library(readr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(pROC)
library(ggplot2)
# df <- read_csv("JTmotility with blank rows removed.csv", col_names = TRUE)
# head(df)
# df_numeric <- select(df,-c(Patient_ID, Laterality, Global_Motility, Floor, Roof, Lateral, Medial))
# cor_mat <- cor(df_numeric, use = "pairwise.complete.obs")
# (sum(cor_mat > .6) - ncol(df_numeric))/2 # restriction and upgaze are highly negatively correlation 
# 

df <- read_csv("Motility_final with blank rows removed.csv", col_names = TRUE)
head(df)
df$Global_Motility[is.na(df$Global_Motility)] <- 0


# Logistic Regression Model 1: Predicting Restrict Up
model_up <- glm(restrict_Up ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_up)

#Upgaze+Downgaze+ADduction+ABduction+Retrobulbar_hemorrhage+Emphysema+fSR+fIR+fMR+fLR+restrict_Down+restrict_AB+restrict_AD+SR_size+IR_size+MR_size+LR_size+sum_fracture+muscle_bi
# Logistic Regression Model 1: Predicting Restrict Down
model_down <- glm(restrict_Down ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_down)

# Logistic Regression Model 1: Predicting Restrict AB
model_AB <- glm(restrict_AB ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_AB)

# Logistic Regression Model 1: Predicting Restrict AD
model_AD <- glm(restrict_AD ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_AD)


# Logistic Regression Model 1: Predicting Any Restriction
model_any <- glm(Restriction ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_any)


# ROC/AUC for any restriction
test_prob = predict(model_any, newdata = df, type = "response")
test_roc = roc(df$Restriction ~ test_prob, plot = TRUE, print.auc = TRUE)

# get threshold that render best results
coords(test_roc, "best", "threshold")
coords(test_roc)

# df$Patient_ID <- as.factor(df$Patient_ID)
# cor(df$Restriction, df$muscle_bi)
# model <- glmer(Restriction~muscle_bi + (1|Patient_ID), data = df, family = "binomial")
# summary(model)
# 
# 
# table(df$Restriction)
# table(df$muscle_bi)
