#Analysis of fixation durations
library(dplyr)

filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
colnames(data)

mean(data$L_DeltaTime)
range(data$L_DeltaTime)
sd(data$L_DeltaTime)

hist(data$L_DeltaTime)
nrow(subset(data, data$L_DeltaTime <=70))
data <-data %>% 
  group_by(ID) %>%
  mutate(LPrevFixDuration = lag(L_DeltaTime))
nrow(data)
data <- na.omit(data)
nrow(data)
physsaccades <- subset(data, data$LPrevFixDuration <= 50)
nrow(physsaccades)
mean(physsaccades$L_DeltaTime)
mean(physsaccades$L_deltaX)
mean(data$L_deltaX)
mean(data$L_DeltaTime)
fracregressions <- nrow(subset(physsaccades, physsaccades$L_deltaX < 0))/ nrow(physsaccades)
fracregressions
# about 60% regressions 
data.fracRegressions <- nrow(subset(data, data$L_deltaX < 0)) / nrow(data)
data.fracRegressions

#  The hypothesis is that before lexical information is extracted, quick saccade
# should have a different distirbution of something since they are determined by primarily 
# physiological effects rather than lexical ones!

data <-data %>% 
  group_by(ID) %>%
  mutate(RPrevFixDuration = lag(R_DeltaTime))
nrow(data)
# omit the first fixations for each person
data <- na.omit(data)
nrow(data)
physsaccades <- subset(data, data$RPrevFixDuration <= 50)
nrow(physsaccades)
# so only 5000
# now let's look at their distributions of saccades
mean(physsaccades$R_DeltaTime)
mean(physsaccades$R_deltaX)
mean(data$R_deltaX)
mean(data$R_DeltaTime)
fracregressions <- nrow(subset(physsaccades, physsaccades$R_deltaX < 0))/ nrow(physsaccades)
fracregressions
data.fracRegressions <- nrow(subset(data, data$R_deltaX < 0)) / nrow(data)
data.fracRegressions
