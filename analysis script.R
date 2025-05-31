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

#AD = MR
#AB = LR
#UP = SR
#DOWN = IR

df$SR_score <- df$fSR - df$cSR
df$IR_score <- df$fIR - df$cIR
df$LR_score <- df$fLR - df$cLR
df$MR_score <- df$fMR - df$cMR


# Logistic Regression Model 1: Predicting Restrict Up
model_up <- glm(restrict_Up ~ SR_score+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_up)

#Upgaze+Downgaze+ADduction+ABduction+Retrobulbar_hemorrhage+Emphysema+fSR+fIR+fMR+fLR+restrict_Down+restrict_AB+restrict_AD+SR_size+IR_size+MR_size+LR_size+sum_fracture+muscle_bi
# Logistic Regression Model 1: Predicting Restrict Down
model_down <- glm(restrict_Down ~ IR_score+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_down)

# Logistic Regression Model 1: Predicting Restrict AB
model_AB <- glm(restrict_AB ~ LR_score+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_AB)

# Logistic Regression Model 1: Predicting Restrict AD
model_AD <- glm(restrict_AD ~ MR_score+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_AD)


# Predict probabilities from each model
prob_up <- predict(model_up, newdata=df, type = "response")
prob_down <- predict(model_down,newdata=df, type = "response")
prob_AB <- predict(model_AB,newdata=df, type = "response")
prob_AD <- predict(model_AD,newdata=df, type = "response")

# Create ROC objects
roc_up <- roc(df$restrict_Up, prob_up)
roc_down <- roc(df$restrict_Down, prob_down)
roc_AB <- roc(df$restrict_AB, prob_AB)
roc_AD <- roc(df$restrict_AD, prob_AD)

# Plot all ROC curves on the same graph
plot(roc_up, col = "blue", legacy.axes = TRUE, print.auc = TRUE, main = "ROC Curves for Restriction Models")
plot(roc_down, col = "red", add = TRUE, print.auc = TRUE, print.auc.y = 0.4)
plot(roc_AB, col = "green", add = TRUE, print.auc = TRUE, print.auc.y = 0.3)
plot(roc_AD, col = "purple", add = TRUE, print.auc = TRUE, print.auc.y = 0.2)

# Add legend
legend("bottomright", legend = c("Restrict Up", "Restrict Down", "Restrict AB", "Restrict AD"),
       col = c("blue", "red", "green", "purple"), lwd = 2)


library(sjPlot)
plot_model(model_up)
plot_model(model_down)
plot_model(model_AD)
plot_model(model_AB)

library(ggplot2)

p <- plot_model(model_down, type = "est", transform = "exp")  # 'transform = "exp"' for odds ratios
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")


# Logistic Regression Model 1: Predicting Any Restriction
model_any <- glm(Restriction ~ cSR+cIR+cMR+cLR+fSR+fIR+fMR+fSR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_any)


# ROC/AUC for any restriction
test_prob = predict(model_any, newdata = df, type = "response")
test_roc = roc(df$Restriction ~ test_prob, plot = TRUE, print.auc = TRUE)

# get threshold that render best results
# coords(test_roc, "best", "threshold")
# coords(test_roc)

# df$Patient_ID <- as.factor(df$Patient_ID)
# cor(df$Restriction, df$muscle_bi)
# model <- glmer(Restriction~muscle_bi + (1|Patient_ID), data = df, family = "binomial")
# summary(model)
# 
# 
# table(df$Restriction)
# table(df$muscle_bi)

# 
# # Logistic Regression Model 1: Predicting Restrict Up
# model_up <- glm(restrict_Up ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
# summary(model_up)
# 
# #Upgaze+Downgaze+ADduction+ABduction+Retrobulbar_hemorrhage+Emphysema+fSR+fIR+fMR+fLR+restrict_Down+restrict_AB+restrict_AD+SR_size+IR_size+MR_size+LR_size+sum_fracture+muscle_bi
# # Logistic Regression Model 1: Predicting Restrict Down
# model_down <- glm(restrict_Down ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
# summary(model_down)
# 
# # Logistic Regression Model 1: Predicting Restrict AB
# model_AB <- glm(restrict_AB ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
# summary(model_AB)
# 
# # Logistic Regression Model 1: Predicting Restrict AD
# model_AD <- glm(restrict_AD ~ CSA_SR+CSA_IR+CSA_MR+CSA_LR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
# summary(model_AD)