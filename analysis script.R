library(readr)
library(dplyr)
df <- read_csv("JTmotility with blank rows removed.csv", col_names = TRUE)
head(df)
df_numeric <- select(df,-c(Patient_ID, Laterality))

