library(readr)
library(dplyr)
library(lme4)
library(lmerTest)
# df <- read_csv("JTmotility with blank rows removed.csv", col_names = TRUE)
# head(df)
# df_numeric <- select(df,-c(Patient_ID, Laterality, Global_Motility, Floor, Roof, Lateral, Medial))
# cor_mat <- cor(df_numeric, use = "pairwise.complete.obs")
# (sum(cor_mat > .6) - ncol(df_numeric))/2 # restriction and upgaze are highly negatively correlation 
# 

df_other <- read_csv("Motility_final with blank rows removed.csv", col_names = TRUE)
head(df_other)
df_other$Patient_ID <- as.factor(df_other$Patient_ID)
cor(df_other$Restriction, df_other$muscle_bi)
model <- glm(Restriction~muscle_bi, data = df_other, family = "binomial")
summary(model)


table(df_other$Restriction)
table(df_other$muscle_bi)
