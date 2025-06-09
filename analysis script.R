library(readr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(pROC)
library(ggplot2)
library(sjPlot)

# Read in Data
df <- read_csv("Motility_final with blank rows removed.csv", col_names = TRUE)
head(df)

# Set Gobal Motility NAs to 0
df$Global_Motility[is.na(df$Global_Motility)] <- 0

#AD = MR
#AB = LR
#UP = SR
#DOWN = IR


# Calculate absolute scores, using a difference of fracture and control surface area
df$SR_score <- df$fSR - df$cSR
df$IR_score <- df$fIR - df$cIR
df$LR_score <- df$fLR - df$cLR
df$MR_score <- df$fMR - df$cMR


# Logistic Regression Model 1: Predicting Restrict Up
model_up <- glm(restrict_Up ~ SR_score+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_up)

# Logistic Regression Model 2: Predicting Restrict Down
model_down <- glm(restrict_Down ~ IR_score+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_down)

# Logistic Regression Model 3: Predicting Restrict AB
model_AB <- glm(restrict_AB ~ LR_score+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_AB)

# Logistic Regression Model 4: Predicting Restrict AD
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


# Odds plots
plot_model(model_up)
plot_model(model_down)
plot_model(model_AD)
plot_model(model_AB)



# Odds plots with line at 1
plot_model(model_up, type = "est", transform = "exp") + geom_hline(yintercept = 1, linetype = "dashed", color = "red")
plot_model(model_down, type = "est", transform = "exp") + geom_hline(yintercept = 1, linetype = "dashed", color = "red")
plot_model(model_AB, type = "est", transform = "exp") + geom_hline(yintercept = 1, linetype = "dashed", color = "red")
plot_model(model_AD, type = "est", transform = "exp") + geom_hline(yintercept = 1, linetype = "dashed", color = "red")


# Logistic Regression Model 5: Predicting Any Restriction (Not reported in the Final Report Slides)
model_any <- glm(Restriction ~ cSR+cIR+cMR+cLR+fSR+fIR+fMR+fSR+sum_fracture+Retrobulbar_hemorrhage, data= df, family = "binomial")
summary(model_any)


# ROC/AUC for any restriction
test_prob = predict(model_any, newdata = df, type = "response")
test_roc = roc(df$Restriction ~ test_prob, plot = TRUE, print.auc = TRUE)
