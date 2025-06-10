setwd("~/Documents/1School/24-25/25S/STATS 141XP/Final Project")
library(readr)
library(dplyr)

motility <- read_csv("motility_final.csv")
motility

# change NAs under Global_Motility to 0s
motility$Global_Motility[is.na(motility$Global_Motility)] <- 0

# summary statistics
str(motility)

# convert variables to binary/categorical/ordinal if needed
motility <- motility %>%
  mutate(across(c(Sex, Laterality, Restriction, Global_Motility, 
                  Floor, Roof, Lateral, Medial,
                  Retrobulbar_hemorrhage, Emphysema,
                  restrict_Up, restrict_Down, restrict_AB, restrict_AD,
                  SR_size, IR_size, MR_size, LR_size,
                  sum_fracture, muscle_bi), as.factor))
motility$sum_fracture <- factor(motility$sum_fracture, 
                                levels = c("1", "2", "3", "4"), 
                                ordered = TRUE)
motility <- motility %>%
  mutate(across(ends_with("_size"), 
                ~ factor(., levels = c("normal", "large"), ordered = TRUE)))

# look at structure and variable types
str(motility) # include in report
glimpse(motility)
summary(motility) # include in report

# which columns have the most missing data
colSums(is.na(motility))
