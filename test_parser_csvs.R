# quick script to test that the csvs produced by the parser are actually usable in R

library(dplyr)
library(ggplot2)
getwd()

basefname <- "work/phd/eyetracking/data/data/Results/English_Fixations.csv"
df <- read.csv(basefname, sep=",")
colnames(df)
hist(df$Duration)


saccades <- "work/phd/eyetracking/data/data/Results/English_Saccades.csv"
sacc <- read.csv(saccades, sep=",")
colnames(sacc)

hist(sacc$Duration)
hist(sacc$StartX)

hex <- ggplot(sacc, aes(StartX, EndX))
hex + geom_hex()

filt <- subset(sacc, sacc$StartX <= 1024 & sacc$EndX <= 1024 & sacc$StartX >= -1024 & sacc$EndX > -1024)
hex <- ggplot(filt, aes(StartX, EndX))
hex + geom_hex()