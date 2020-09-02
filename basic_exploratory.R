
library(dplyr)

filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
colnames(data)
nrow(data)
mean(data$L_DeltaTime)
mean(data$R_DeltaTime)
