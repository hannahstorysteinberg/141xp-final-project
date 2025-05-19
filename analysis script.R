library(readr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
# df <- read_csv("JTmotility with blank rows removed.csv", col_names = TRUE)
# head(df)
# df_numeric <- select(df,-c(Patient_ID, Laterality, Global_Motility, Floor, Roof, Lateral, Medial))
# cor_mat <- cor(df_numeric, use = "pairwise.complete.obs")
# (sum(cor_mat > .6) - ncol(df_numeric))/2 # restriction and upgaze are highly negatively correlation 
# 

df <- read_csv("Motility_final with blank rows removed.csv", col_names = TRUE)
head(df)



df$Patient_ID <- as.factor(df$Patient_ID)
cor(df$Restriction, df$muscle_bi)
model <- glmer(Restriction~muscle_bi + (1|Patient_ID), data = df, family = "binomial")
summary(model)


table(df$Restriction)
table(df$muscle_bi)
