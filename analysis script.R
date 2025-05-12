library(readr)
library(dplyr)
df <- read_csv("JTmotility with blank rows removed.csv", col_names = TRUE)
head(df)
df_numeric <- select(df,-c(Patient_ID, Laterality, Global_Motility, Floor, Roof, Lateral, Medial))
cor_mat <- cor(df_numeric, use = "pairwise.complete.obs")
(sum(cor_mat > .6) - ncol(df_numeric))/2 # restriction and upgaze are highly negatively correlation 
