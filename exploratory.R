#Exploratory data analysis on the eyetracking dataset

library(lme4)
library(ggplot2)

getwd()
setwd("/home/beren/work/phd/eyetracking")
getwd()
data = read.csv("EnglishTypical.csv", sep = ",")
head(data)
str(data)

boxplot(data$L_MeanX ~ data$ID)
boxplot(data$Age ~ data$L_MeanX) 
hist(data$Age)

boxplot(data$L_MeanX ~ data$Sex)
str(data)
# let's see if differences among the groups
boxplot(data$L_MeanX ~ data$Group)
data$Group
hist(data$L_Block)
boxplot(data$L_MeanX ~ data$L_Block)
boxplot(data$R_MeanX ~ data$L_Block)
plot(data$ID, data$L_MeanX)
hist(data$L_Angle) # farly normal
hist(data$L_MeanX)
hist(data$R_MeanX) 
hist(data$L_MeanY) 
hist(data$R_MeanY) 
hist(data$L_Angle) 
hist(data$R_Angle) 

boxplot(data$L_Angle + data$R_Angle ~ data$ID)
boxplot(data$L_Angle  ~ data$ID)
# there appear to be some significant individual differences here in the left eye angle
boxplot(data$R_Angle ~ data$ID)
boxplot(data$L_Angle ~ data$Sex)
boxplot(data$R_Angle ~ data$Sex)#
# the interaction with sex and angle here is also really weird... females are a bit larger on left angle and lower on right angle while men

boxplot(data$L_MeanX ~ data$ScreenNo)
boxplot(data$L_Angle ~ data$ScreenNo)
# no seeming interaction here
boxplot(data$R_Angle ~ data$ScreenNo)
# probably nor an interactino here either
# wonder what the relation is between the two angles, if any... presumably a linear correlation
plot(data$L_Angle[1:30000],data$R_Angle[1:30000])

plot(data$L_MeanX[1:10000], data$R_MeanX[1:10000])

plot(as.factor(data$Age), data$L_MeanX)
# so age doesn't seem to ahve an impact on mean x, what about angle
plot(as.factor(data$Age), data$L_Angle)
# so seems much more variable, and weird, as expected, but generally don't seem to have too much impact!
plot(as.factor(data$Age), data$R_Angle)

# let's check by story
boxplot(data$L_MeanX ~ data$StoryNo)
# story number seems to have no major effect on mean x - what about angle
boxplot(data$R_MeanX ~ data$StoryNo) # or on right mean x

boxplot(data$L_Angle ~ data$StoryNo) # or on left angle

boxplot(data$R_Angle ~ data$StoryNo) # or on right angle

boxplot(data$L_Distance ~ data$StoryNo) 
# let's do a plot of L distance against R distance
plot(data$L_Distance, data$R_Distance)

plot(data$L_Distance, data$L_MeanX)
hist(data$L_Distance)
mean(data$L_Distance) # mean is very low
range(data$L_Distance) 
# try R distance
hist(data$R_Distance)
mean(data$R_Distance)

# let's do a histogram of pupils
hist(data$R_Pupil)
hist(data$L_Pupil) # effectively same result for L pupil
plot(data$L_Pupil, data$R_Pupil)
cor(data$L_Pupil, data$R_Pupil)

plot(data$L_MeanX, data$R_MeanX)
# so that's definitely a very strong correlation there.
cor(data$L_MeanX, data$R_MeanX)
# so quite high, let's look at the ys
plot(data$L_MeanY, data$R_MeanY)
cor(data$L_MeanY, data$R_MeanY)
hist(data$L_MeanX)
hist(data$L_MeanY)
hist(data$R_MeanX)
hist(data$R_MeanY)

# let's look at the time distribution histograms
hist(data$L_DeltaTime)
hist(data$R_DeltaTime)
deltL <- data$L_DeltaTime[data$L_DeltaTime <=500]
hist(deltL)
mean(data$L_DeltaTime)
sd(data$L_DeltaTime)
deltR <- data$R_DeltaTime[data$R_DeltaTime <=500]
hist(deltR)
