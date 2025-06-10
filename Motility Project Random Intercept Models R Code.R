data <- read.csv("CvF Split Dataset with zeroed restrictions and fractures.csv")
library(lme4)

# Models for matching the muscle to the restriction in the specific eye direction

model_mr <- glmer(restrict_AD ~ MR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_mr)

model_lr <- glmer(restrict_AB ~ LR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_lr)

model_sr <- glmer(restrict_Up ~ SR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_sr)

model_ir <- glmer(restrict_Down ~ IR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_ir)

# General Model for Restriction due to muscle enlargement and other eye conditions

model <- glmer(Restriction ~ muscle_bi + Retrobulbar_hemorrhage + Emphysema + (1|Patient_ID), data = data, family = "binomial")
summary(model)

# Models for matching the opposite muscle to the restriction in each eye direction

model_mr_opp <- glmer(restrict_AD ~ LR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_mr_opp)

model_lr_opp <- glmer(restrict_AB ~ MR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_lr_opp)

model_sr_opp <- glmer(restrict_Up ~ IR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_sr_opp)

model_ir_opp <- glmer(restrict_Down ~ SR_size + (1|Patient_ID), data = data, family = "binomial")
summary(model_ir_opp) # Will not work properly due to no patient having an enlarged SR Muscle


# Odds Plots for all working models
library(sjPlot)
library(ggplot2)

p <- plot_model(model, type = "est", transform = "exp")  # 'transform = "exp"' for odds ratios
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")

p <- plot_model(model_lr, type = "est", transform = "exp")  
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")

p <- plot_model(model_lr_opp, type = "est", transform = "exp")
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")

p <- plot_model(model_mr, type = "est", transform = "exp")
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")

p <- plot_model(model_mr_opp, type = "est", transform = "exp")
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")

p <- plot_model(model_sr_opp, type = "est", transform = "exp")
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")

p <- plot_model(model_ir, type = "est", transform = "exp")
p + geom_hline(yintercept = 1, linetype = "dashed", color = "red")

# ROC Curves for the Models
library(pROC)
# General Model
plot(roc(data$Restriction, predict(model,newdata = data, type = "response")), 
     main = "ROC Curve for General Random Intercepts Model")

# Paired Muscles and Restrictions
plot(roc(data$Restriction, predict(model_mr,newdata = data, type = "response")), 
     main = "ROC Curve for MR & Adduction Model")
plot(roc(data$Restriction, predict(model_lr,newdata = data, type = "response")), 
     main = "ROC Curve for LR & Abduction Model")
plot(roc(data$Restriction, predict(model_ir,newdata = data, type = "response")), 
     main = "ROC Curve for IR & Down Gaze Model")

# Opposite Muscles and Restrictions
plot(roc(data$Restriction, predict(model_mr_opp,newdata = data, type = "response")), 
     main = "ROC Curve for MR & Abduction Model")
plot(roc(data$Restriction, predict(model_lr_opp,newdata = data, type = "response")), 
     main = "ROC Curve for LR & Adduction Model")
plot(roc(data$Restriction, predict(model_sr_opp,newdata = data, type = "response")), 
     main = "ROC Curve for SR & Downgaze Model")
