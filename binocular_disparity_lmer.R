# lmer models and other statistical tests for the binocular disparity analysis - separated from the exploratory file because that was becoming too unwieldy!

library(ggplot2)
library(dplyr)
library(lme4)

filename <- "/home/beren/work/phd/eyetracking/data/binocular_disparity_augmented.csv"
data <- read.csv(filename, sep=",")
colnames(data)

# let's just try binocular disparity with screen side!
nullLmer <- lmer(Binocular_Disparity_Start ~ (1|ID), data=data)
summary(nullLmer)

lmerScreenSide <- lmer(Binocular_Disparity_Start ~ ScreenSide + (1|ID), data=data, REML=FALSE)
summary(lmerScreenSide)
# anova to compare
anova(nullLmer, lmerScreenSide)
# the screen side lmer is vastly more significant - which is good!
# now try adding groups!
lmerScreenGroups <- lmer(Binocular_Disparity_Start ~ ScreenSide + (1|ID) + (1|Group), data=data)
anova(lmerScreenSide, lmerScreenGroups)
#try adding screen number
lmerScreenNo <- lmer(Binocular_Disparity_Start ~ ScreenSide +(1|ID) + (1|ScreenNo), data=data, REML=FALSE)
anova(lmerScreenSide, lmerScreenNo)
# screen number is highly significant... I think
lmerScreenNoGroup <- lmer(Binocular_Disparity_Start ~ ScreenSide + (1|ID) + (1|ScreenNo) + (1|Group), data=data, REML=FALSE)
anova(lmerScreenNo, lmerScreenNoGroup)
# once again not significant.

# next try story number
lmerStoryNo <- lmer(Binocular_Disparity_Start ~ ScreenSide +(1|ID) + (1|ScreenNo) + (1|StoryNo), data=data, REML=FALSE)
anova(lmerScreenNo, lmerStoryNo)
lmerAge <- lmer(Binocular_Disparity_Start ~ ScreenSide +(1|ID) + (1|ScreenNo) + (1|StoryNo) + (1|Age), data=data, REML=FALSE)
anova(lmerStoryNo, lmerAge)
# no effect of age!
#and sex
lmerSex <- lmer(Binocular_Disparity_Start ~ ScreenSide +(1|ID) + (1|ScreenNo) + (1|StoryNo) + (1|Sex), data=data, REML=FALSE)
anova(lmerStoryNo, lmerSex)
# not significant = p = 0.4198!
colnames(data)
lmerLBlock <- lmer(Binocular_Disparity_Start ~ ScreenSide +(1|ID) + (1|ScreenNo) + (1|StoryNo) + (1|L_Block), data=data, REML=FALSE)
anova(lmerStoryNo, lmerLBlock)
# l block is very significant also.
lmerLPage <- lmer(Binocular_Disparity_Start ~ ScreenSide +(1|ID) + (1|ScreenNo) + (1|StoryNo) + (1|L_Page), data=data, REML=FALSE)
anova(lmerStoryNo, lmerLPage)
lmerSide <- lmer(Binocular_Disparity_Start ~ relevel(ScreenSide, ref="Middle") + (1|ID) + (1|ScreenNo) + (1|StoryNo), data=data, REML=FALSE)
summary(lmerSide)
lmerEndSide <- lmer(Binocular_Disparity_End ~ relevel(ScreenSide, ref="Middle") +(1|ID) + (1|ScreenNo) + (1|StoryNo), data=data, REML=FALSE)
summary(lmerEndSide)
lmerEndNull <- lmer(Binocular_Disparity_End ~ 1 + (1|ID) + (1|ScreenNo) + (1|StoryNo), data=data, REML=FALSE)
anova(lmerEndNull, lmerEndSide)

plot(lmerEndSide)
print("Done")
plot(fitted(lmerEndSide), residuals(lmerEndSide))
res <- residuals(lmerEndSide)
hist(res)
qqnorm(res)
diffnull <- lmer(Binocular_Disparity_Difference ~ 1 + (1|ID) + (1|ScreenNo) + (1|StoryNo), data=data, REML=FALSE)
diffscreenside <- lmer(Binocular_Disparity_Difference ~ relevel(ScreenSide, ref="Middle") + (1|ID) + (1|ScreenNo) + (1|StoryNo), data=data, REML=FALSE)
anova(diffnull, diffscreenside)

