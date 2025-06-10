#load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(broom)
library(gt)

# load dataset
motility <- read_csv("motility_fin.csv",  show_col_types = FALSE)
#head(motility)

# change NAs to 0s or unknown
motility <- motility %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  mutate(across(where(is.character), ~ replace_na(., "unknown"))) 

# make factors
motility <- motility %>%
  mutate(across(c(Sex, Laterality, Restriction, Global_Motility, Floor, Roof, Lateral, Medial, Retrobulbar_hemorrhage, Emphysema, SR_size, IR_size, MR_size, LR_size, sum_fracture, muscle_bi), as.factor))

#head(motility)

# upgaze lm model 
lm_up_model <- lm(Upgaze ~ Age + Sex + Laterality + Global_Motility + Floor + Roof + Lateral + Medial + Retrobulbar_hemorrhage + Emphysema + Thickness_cut + RSR + RIR + RLR + RMR + LSR + LIR + LLR + LMR + CSA_SR + CSA_IR + CSA_MR + CSA_LR + SR_size + IR_size + MR_size + LR_size + sum_fracture + muscle_bi, data=motility)
summary(lm_up_model)
step_up <- stepAIC(lm_up_model, direction="both") # stepwise regression
summary(step_up)

final_up <- lm(formula = Upgaze ~ Global_Motility + Roof + Retrobulbar_hemorrhage + 
                 RIR + RLR + LMR + CSA_IR + SR_size + MR_size + sum_fracture + 
                 muscle_bi, data = motility)

tidy_up <- tidy(final_up)

tidy_up %>%
  gt() %>%
  tab_header(
    title = "Final Linear Model: Upgaze",
    subtitle = "Model Coefficients"
  ) %>%
  fmt_number(columns = vars(estimate, std.error, statistic, p.value), decimals = 3) %>%
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  )

# downgaze lm model 
lm_down_model <- lm(Downgaze ~ Age + Sex + Laterality + Global_Motility + Floor + Roof + Lateral + Medial + Retrobulbar_hemorrhage + Emphysema + Thickness_cut + RSR + RIR + RLR + RMR + LSR + LIR + LLR + LMR + CSA_SR + CSA_IR + CSA_MR + CSA_LR + SR_size + IR_size + MR_size + LR_size + sum_fracture + muscle_bi, data=motility)
summary(lm_down_model)
step_down <- stepAIC(lm_down_model, direction="both")
summary(step_down)

final_down <- lm(formula = Downgaze ~ Sex + Global_Motility + Floor + CSA_IR + 
                   CSA_LR + sum_fracture, data = motility)

tidy_down <- tidy(final_down)

tidy_down %>%
  gt() %>%
  tab_header(
    title = "Final Linear Model: Downgaze",
    subtitle = "Model Coefficients"
  ) %>%
  fmt_number(columns = vars(estimate, std.error, statistic, p.value), decimals = 3) %>%
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  )

# adduction lm model
lm_ad_model <- lm(ADduction ~ Age + Sex + Laterality + Global_Motility + Floor + Roof + Lateral + Medial + Retrobulbar_hemorrhage + Emphysema + Thickness_cut + RSR + RIR + RLR + RMR + LSR + LIR + LLR + LMR + CSA_SR + CSA_IR + CSA_MR + CSA_LR + SR_size + IR_size + MR_size + LR_size + sum_fracture + muscle_bi, data=motility)
summary(lm_ad_model)
step_ad <- stepAIC(lm_ad_model, direction="both")
summary(step_ad)
final_ad <- lm(formula = ADduction ~ Age + Global_Motility + Lateral + Thickness_cut + 
                 LSR + CSA_SR + CSA_LR + IR_size + MR_size + LR_size, data = motility)

tidy_ad <- tidy(final_ad)

tidy_ad %>%
  gt() %>%
  tab_header(
    title = "Final Linear Model: ADduction",
    subtitle = "Model Coefficients"
  ) %>%
  fmt_number(columns = vars(estimate, std.error, statistic, p.value), decimals = 3) %>%
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  )

# abduction lm model 
lm_ab_model <- lm(ABduction ~ Age + Sex + Laterality + Global_Motility + Floor + Roof + Lateral + Medial + Retrobulbar_hemorrhage + Emphysema + Thickness_cut + RSR + RIR + RLR + RMR + LSR + LIR + LLR + LMR + CSA_SR + CSA_IR + CSA_MR + CSA_LR + SR_size + IR_size + MR_size + LR_size + sum_fracture + muscle_bi, data=motility)
summary(lm_ab_model)
step_ab <- stepAIC(lm_ab_model, direction="both")
summary(step_ab)
final_ab <- lm(formula = ABduction ~ Global_Motility + Retrobulbar_hemorrhage + 
                 CSA_IR + IR_size + Medial, data = motility)

tidy_ab <- tidy(final_ab)

tidy_ab %>%
  gt() %>%
  tab_header(
    title = "Final Linear Model: ABduction",
    subtitle = "Model Coefficients"
  ) %>%
  fmt_number(columns = vars(estimate, std.error, statistic, p.value), decimals = 3) %>%
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "Std. Error",
    statistic = "t-value",
    p.value = "p-value"
  )

