# this is just code for some quick analysis on  binocular disparity

#libraries
library(dplyr)  # for data manipulation
library(ggplot2) # plotting data
library(hexbin)
library(lme4)
library(lmerTest)
library(lattice)
library(autoimage)
data = read.csv("/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv", sep='\t')
summary(data)
# clean by screen resolution
colnames(data)

data <- subset(data, R_StartX >= 0 & R_StartX <= 1024 & R_StartY >= 0 & R_StartY <= 1024) # I'm changing this to 1024 for some reason... I don't understand why it's 768
data <- subset(data, L_StartX >= 0 & L_StartX <= 1024 & L_StartY >= 0 & L_StartY <= 1024)
data <- subset(data, R_EndX >= 0 & R_EndX <= 1024 & R_EndX >= 0 & R_EndX <= 1924)
data <- subset(data, L_EndX >= 0 & L_EndX <= 1024 & L_EndX >= 0 & L_EndX <= 768)



# remove NAs already
data <- na.omit(data)

#calculate the binocular disparities of various kinds
data["Binocular_Disparity_Start"] <- sqrt((data$R_StartX - data$L_StartX)^2 + (data$R_StartY - data$L_StartY)^2)
data["Binocular_Disparity_End"] <- sqrt((data$R_EndX - data$L_EndX)^2 + (data$R_EndY - data$L_EndY)^2)
data["Binocular_Disparity_StartX"] <- sqrt((data$R_StartX - data$L_StartX)^2)
data["Binocular_Disparity_StartY"] <- sqrt((data$R_StartY - data$L_StartY)^2)
data["Binocular_Disparity_EndX"] <- sqrt((data$R_EndX - data$L_EndX)^2)
data["Binocular_Disparity_EndY"] <- sqrt((data$R_EndY - data$L_EndY)^2)
data["Binocular_Disparity_Difference"] <- data$Binocular_Disparity_Start - data$Binocular_Disparity_End

# now for some exploratory data analysis!
hist(data$Binocular_Disparity_Difference)
range(data$Binocular_Disparity_Difference)
mean(data$Binocular_Disparity_Difference)
hist(data$Binocular_Disparity_Start)

hist(data$Binocular_Disparity_End)
mean(data$Binocular_Disparity_End)
mean(data$Binocular_Disparity_StartX)
mean(data$Binocular_Disparity_StartY)
mean(data$Binocular_Disparity_EndX)
mean(data$Binocular_Disparity_EndY)
mean(data$Binocular_Disparity_Difference)
hist(data$Binocular_Disparity_Difference)
median(data$Binocular_Disparity_Start)
median(data$Binocular_Disparity_End)

mean(data$Binocular_Disparity_Start)

data["Binocular_Disparity_Absolute_StartX"] <- data$R_StartX - data$L_StartX
data["Binocular_Disparity_Absolute_StartY"] <- data$R_StartY - data$L_StartY
data["Binocular_Disparity_Absolute_EndX"] <- data$R_EndX - data$L_EndX
data["Binocular_Disparity_Absolute_EndY"] <- data$R_EndY - data$L_EndY

mean(data$Binocular_Disparity_Absolute_StartX) 
mean(data$Binocular_Disparity_Absolute_StartY) 
mean(data$Binocular_Disparity_Absolute_EndX)
mean(data$Binocular_Disparity_Absolute_EndY)

colnames(data)
data["Mean_DeltaTime"] <- (data$L_DeltaTime + data$R_DeltaTime) /2
mean(data$Mean_DeltaTime)
cor(data$Binocular_Disparity_Difference, data$Mean_DeltaTime)
cor(data$L_DeltaTime, data$R_DeltaTime)
data["Binocular_Absolute_Disparity_DifferenceX"] <- data$Binocular_Disparity_Absolute_StartX - data$Binocular_Disparity_Absolute_EndX
cor(data$Binocular_Absolute_Disparity_DifferenceX, data$Mean_DeltaTime)

# split into screen sides
data$ScreenSide<- ifelse(data$L_StartX<= 341 | data$R_StartX<= 341,"Left",
                                    ifelse(data$L_StartX> 341& data$L_StartX< 682 | data$R_StartX> 341& data$R_StartX< 682,"Middle",
                                           ifelse(data$L_StartX>= 682 | data$R_StartX>= 682,"Right",NA)))

data$ScreenSide<- as.factor(data$ScreenSide)
colnames(data)

# calculates disparities by screen side
data.left <- subset(data, ScreenSide=="Left")
data.middle <- subset(data, ScreenSide == "Middle")
data.right <- subset(data, ScreenSide == "Right")
# calculate the means of the disparities
mean(data.left$Binocular_Disparity_Start)
mean(data.middle$Binocular_Disparity_Start)
mean(data.right$Binocular_Disparity_Start)
#t-tests
t.test(data.left$Binocular_Disparity_Start, data.middle$Binocular_Disparity_Start)
t.test(data.middle$Binocular_Disparity_Start, data.right$Binocular_Disparity_Start)

# a clear pattern is observed of decreasing start binocular disparities to the left!
mean(data.left$Binocular_Disparity_End)
mean(data.middle$Binocular_Disparity_End)
mean(data.right$Binocular_Disparity_End)

data.typical = subset(data, Group == "English" | Group == "Chinese") # filters out the dyslexics
data.typical.left <- subset(data, ScreenSide=="Left")
data.typical.middle <- subset(data, ScreenSide == "Middle")
data.typical.right <- subset(data, ScreenSide == "Right")
# calculate the means of the disparities
mean(data.typical.left$Binocular_Disparity_Start)

t.test(data.left$Binocular_Disparity_End, data.middle$Binocular_Disparity_End)
t.test(data.middle$Binocular_Disparity_End , data.right$Binocular_Disparity_End)

sd(data$Binocular_Disparity_Start)
sd(data$Binocular_Disparity_End)
sd(data.left$Binocular_Disparity_Start)
sd(data.middle$Binocular_Disparity_Start)
sd(data.right$Binocular_Disparity_Start)

mean(data.typical.middle$Binocular_Disparity_Start)
mean(data.typical.right$Binocular_Disparity_Start)
# a clear pattern is observed of decreasing start binocular disparities to the left!
mean(data.typical.left$Binocular_Disparity_End)
mean(data.typical.middle$Binocular_Disparity_End)
mean(data.typical.right$Binocular_Disparity_End)
# it seems not to change the effect!
# let's look at the dyslexics
data.dyslexic = subset(data, Group=="Dyslexic")
data.dyslexic.left <- subset(data, ScreenSide=="Left")
data.dyslexic.middle <- subset(data, ScreenSide == "Middle")
data.dyslexic.right <- subset(data, ScreenSide == "Right")
# calculate the means of the disparities
mean(data.dyslexic.left$Binocular_Disparity_Start)
mean(data.dyslexic.middle$Binocular_Disparity_Start)
mean(data.dyslexic.right$Binocular_Disparity_Start)
# a clear pattern is observed of decreasing start binocular disparities to the left!
mean(data.dyslexic.left$Binocular_Disparity_End)
mean(data.dyslexic.middle$Binocular_Disparity_End)
mean(data.dyslexic.right$Binocular_Disparity_End)
# the pattern is aso the same for dyslexcivs... now just need to chose english and chinese!S
data.english <- subset(data, Group=="English")
data.english.left <- subset(data, ScreenSide=="Left")
data.english.middle <- subset(data, ScreenSide == "Middle")
data.english.right <- subset(data, ScreenSide == "Right")
# calculate the means of the disparities
mean(data.english.left$Binocular_Disparity_Start)
mean(data.english.middle$Binocular_Disparity_Start)
mean(data.english.right$Binocular_Disparity_Start)
# a clear pattern is observed of decreasing start binocular disparities to the left!
mean(data.english.left$Binocular_Disparity_End)
mean(data.english.middle$Binocular_Disparity_End)
mean(data.english.right$Binocular_Disparity_End)

data.chinese = subset(data, Group=="Chinese")
data.chinese.left <- subset(data, ScreenSide=="Left")
data.chinese.middle <- subset(data, ScreenSide == "Middle")
data.chinese.right <- subset(data, ScreenSide == "Right")
# calculate the means of the disparities
mean(data.chinese.left$Binocular_Disparity_Start)
mean(data.chinese.middle$Binocular_Disparity_Start)
mean(data.chinese.right$Binocular_Disparity_Start)
# a clear pattern is observed of decreasing start binocular disparities to the left!
mean(data.chinese.left$Binocular_Disparity_End)
mean(data.chinese.middle$Binocular_Disparity_End)
mean(data.chinese.right$Binocular_Disparity_End)
# okay, identical for the chinese too! that's good


boxplot(data$Binocular_Disparity_Start ~ data$ScreenSide)
boxplot(data$Binocular_Disparity_End  ~ data$ScreenSide)
# so generally disparity decreases. start decreases the disparity over the middle which is interesting the end does not.

# create unique numerical ids for each group
data$ID <- ifelse(data$Group == "Chinese", data$ID + 100, 
                  ifelse(data$Group == "Dyslexic", data$ID + 200,
                         data$ID + 0))
data$ID <- as.numeric(data$ID)
#check that it worked
range(subset(data$ID, data$Group=="English"))
range(subset(data$ID, data$Group == "Chinese"))
range(subset(data$ID, data$Group == "Dyslexic"))

# try the autoimage solution here
#autoimage(x=data$L_StartX, y = data$L_StartY, main=data$Binocular_Disparity_Start)

# let's try a simpel density colour

hexbindf.L <- data.frame(x = data$L_StartX,
                       y = data$L_StartY,
                       z = data$Binocular_Disparity_Start)
hexbindf.L.plot <- ggplot(data=hexbindf.L,
               aes(x=x,
                   y=y,
                   z=z)) +
  stat_summary_hex(fun = function(x) sum(x))
hexbindf.R <- data.frame(x = data$R_StartX,
                         y = data$R_StartY,
                         z = data$Binocular_Disparity_Start)
hexbindf.R.plot <- ggplot(data=hexbindf.R,
                          aes(x=x,
                              y=y,
                              z=z)) +
  stat_summary_hex(fun = function(x) sum(x))
print(hexbindf.L.plot)
print(hexbindf.R.plot)
hexbindf.end.L <- data.frame(x = data$L_EndX,
                         y = data$L_EndY,
                         z = data$Binocular_Disparity_End)
hexbindf.end.L.plot <- ggplot(data=hexbindf.end.L,
                          aes(x=x,
                              y=y,
                              z=z)) +
  stat_summary_hex(fun = function(x) sum(x))
print(hexbindf.end.L.plot)

hexbindf.end.R <- data.frame(x = data$R_EndX,
                         y = data$R_EndY,
                         z = data$Binocular_Disparity_End)
hexbindf.end.R.plot <- ggplot(data=hexbindf.end.R,
                          aes(x=x,
                              y=y,
                              z=z)) +
  stat_summary_hex(fun = function(x) sum(x))
plot(hexbindf.end.R.plot)



hexbindf.median <- data.frame(x = data$L_StartX,
                         y = data$L_StartY,
                         z = data$Binocular_Disparity_StartX)
hexbindf.median.plot <- ggplot(data=hexbindf.median,
                          aes(x=x,
                              y=y,
                              z=z)) +
  stat_summary_hex(fun = function(x) median(x))
print(hexbindf.median.plot)

filtX <- subset(data, L_StartX <= 750 & L_StartY <=600)
filtY <- filtX$L_StartY
filtDisp <- filtX$Binocular_Disparity_StartX
filtX <- filtX$L_StartX
hexbindf.filt <- data.frame(x = filtX,
                              y = filtY,
                              z = filtDisp)
hexbindf.filt.plot <- ggplot(data=hexbindf.filt,
                               aes(x=x,
                                   y=y,
                                   z=z)) +
  stat_summary_hex(fun = function(x) median(x))
print(hexbindf.filt.plot)

dispsd <- sd(data$Binocular_Disparity_Start)
dispsd
dispmu <- mean(data$Binocular_Disparity_Start)
dispmu
#filter by two lots of the standard deviations
sdfilt <- subset(data, Binocular_Disparity_Start > dispmu - (2 * dispsd) &
                   Binocular_Disparity_Start < dispmu + (2 * dispsd))
sdfilt <- data.frame(x = sdfilt$L_StartX,
                     y = sdfilt$L_StartY,
                     z = sdfilt$Binocular_Disparity_Start)
sdfilt.plot <- ggplot(data=sdfilt,
                      aes(x=x,
                          y=y,
                          z=z)) +
  stat_summary_hex(fun = function(x) median(x))
print(sdfilt.plot)

hist(data$Binocular_Disparity_Difference)
mean(data.left$Binocular_Disparity_Difference)
mean(data.middle$Binocular_Disparity_Difference)
mean(data.right$Binocular_Disparity_Difference)

colnames(data)
cor(data$Binocular_Disparity_Difference, data$L_deltaX)
cor(data$Binocular_Disparity_Difference, data$R_deltaX)
cor(data$Binocular_Disparity_Difference, data$L_deltaY)
cor(data$Binocular_Disparity_Difference, data$R_deltaY)
hist(data$L_deltaX)
hist(data$L_deltaY)
hist(data$R_deltaX)
hist(data$R_deltaY)
range(data$L_deltaX)
range(data$R_deltaX)
range(data$L_deltaY)
range(data$R_deltaY)

boxplot(data$Binocular_Disparity_Difference ~ data$ScreenSide, data=data)
boxplot(data$Binocular_Disparity_Start ~ data$ScreenSide, data=data)
boxplot(data$Binocular_Disparity_End ~ data$ScreenSide, data=data)
boxplot(data$Binocular_Disparity_Start ~ data$ScreenNo)
colnames(data)
boxplot(data$Binocular_Disparity_Start ~ data$Sex)
boxplot(data$Binocular_Disparity_Start ~ data$Age)

plot(data$Binocular_Disparity_Start, data$L_deltaX)
plot(data$Binocular_Disparity_Start, data$R_deltaX)
plot(data$Binocular_Disparity_End, data$L_deltaX)
plot(data$Binocular_Disparity_End, data$R_deltaX)
plot(data$Binocular_Disparity_Difference, data$L_deltaX)
plot(data$Binocular_Disparity_Difference, data$R_deltaX)


# export the file as a csv
help(write.csv)

write_csv <- function(data, filename) {
  if (file.exists(filename)) {
    dir.create(filename)
    write.csv(data, filename)
  } else {
    write.csv(data, filename)
  }
}


filename <- "/home/beren/work/phd/eyetracking/data/binocular_disparity_augmented.csv"
#write_csv(data, filename)
write.csv(data,filename)

print("Done!")

# check differences by id
screenDiffsById <- function(id) {
  df.left <- subset(data.left, data$ID == id)
  df.middle <- subset(data.middle, data$ID == id)
  df.right <- subset(data.right, data$ID == id)
  print(mean(df.left$Binocular_Disparity_Start))
  print(mean(df.middle$Binocular_Disparity_Start))
  print(mean(df.right$Binocular_Disparity_Start))
}


# let's try plotting various other relationships to see what's up
plot(data$Binocular_Disparity_Start ~ data$Mean_DeltaTime)
cor(data$Binocular_Disparity_Start , data$Mean_DeltaTime)
cor(data$Binocular_Disparity_End , data$Mean_DeltaTime)
cor(data$Binocular_Disparity_Start, data$L_Angle)
cor(data$Binocular_Disparity_Start, data$R_Angle)
cor(data$Binocular_Disparity_End, data$L_Angle)
cor(data$Binocular_Disparity_End, data$R_Angle)
plot(data$Binocular_Disparity_Start ~ data$L_Angle)
plot(data$Binocular_Disparity_Start ~ data$R_Angle)

# try truncating the data for more sense
trunc <- data[0:10000,]
plot(trunc$Binocular_Disparity_Start ~ trunc$R_Angle)

length(data)
nrow(data)
plot(trunc$Binocular_Disparity_Start ~ trunc$L_Pupil)
cor(data$Binocular_Disparity_Start, data$L_Pupil)
cor(data$Binocular_Disparity_Start, data$R_Pupil)

cor(data$Binocular_Disparity_Start, data$L_MeanX)
cor(data$Binocular_Disparity_Start, data$L_StartTime)
cor(data$Binocular_Disparity_Start, data$L_Angle)
cor(data$Binocular_Disparity_Start, data$L_DeltaTime)
cor(data$Binocular_Disparity_Start, data$L_Distance)
# there's a strong correlation with distance it seems
plot(trunc$Binocular_Disparity_Start ~ trunc$L_Distance)
# looks like it could be a linear trend
plot(data$Binocular_Disparity_Start ~ data$L_Distance)
print("done!")
cor(data$Binocular_Disparity_Start, data$R_Distance)
cor(data$Binocular_Disparity_End, data$L_Distance)
cor(data$Binocular_Disparity_End, data$R_Distance)

plot(trunc$Binocular_Disparity_Difference ~ trunc$L_deltaX)
plot(data$Binocular_Disparity_Difference ~ data$L_deltaX)
plot(data$Binocular_Disparity_Difference ~ data$R_deltaY)

# simple t-test on the start and end times
t.test(data$Binocular_Disparity_Start , data$Binocular_Disparity_End)
# extremely highly significant!
# what are the hemispheric effects!?
cor(data$Binocular_Disparity_Start, data$L_deltaX)
cor(data$Binocular_Disparity_Start, data$R_deltaX)
cor(data$Binocular_Disparity_End, data$L_deltaX)
cor(data$Binocular_Disparity_End, data$R_deltaX)
# effect of angle
cor(data$Binocular_Disparity_Start, data$L_Angle)
cor(data$Binocular_Disparity_Start, data$R_Angle)
cor(data$Binocular_Disparity_End, data$L_Angle)
cor(data$Binocular_Disparity_End, data$R_Angle)
plot(data$Binocular_Disparity_Start ~ data$L_Angle)
# distances
cor(data$Binocular_Disparity_Start, data$L_Distance)
cor(data$Binocular_Disparity_Start, data$R_Distance)
cor(data$Binocular_Disparity_End, data$L_Distance)
cor(data$Binocular_Disparity_End, data$R_Distance)
plot(data$Binocular_Disparity_Start ~ data$L_Distance)
# no effect of distances except for first one!
cor(data$Binocular_Disparity_Start, data$L_Pupil)
cor(data$Binocular_Disparity_Start, data$R_Pupil)
cor(data$Binocular_Disparity_End, data$L_Pupil)
cor(data$Binocular_Disparity_Start, data$R_Pupil)
#no effect of pupil!
cor(data$Binocular_Disparity_Start, data$L_DeltaTime)
cor(data$Binocular_Disparity_Start, data$R_DeltaTime)
cor(data$Binocular_Disparity_End, data$L_DeltaTime)
cor(data$Binocular_Disparity_End, data$R_DeltaTime)
# no effect of delta time - i.e. fixation length!

# check significant effects of factors on means via boxplots
boxplot(data$Binocular_Disparity_Start ~ data$Sex)
boxplot(data$Binocular_Disparity_Start ~ data$Age)
boxplot(data$Binocular_Disparity_Start ~ data$StoryNo)
boxplot(data$Binocular_Disparity_Start ~ data$ScreenNo)
cor(data$Binocular_Disparity_Start, data$L_StartY)
plot(data$Binocular_Disparity_Start, data$L_StartY)
cor(data$Binocular_Disparity_Start, data$L_deltaY)
cor(data$Binocular_Disparity_End, data$L_EndY)
boxplot(data$Binocular_Disparity_Start ~ data$Group)
