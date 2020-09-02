
# steps.
  # - do exploratory data analysis to get some idea
  # - do data cleaning at least to some basic level
  # - recreate pupil size analysis
  # - check for correlation w/ typology also
  # - check for word-frequency correlations
  # - anything else that appeas interesting

library(dplyr)
library(ggplot2)
library(lme4)

fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English.csv"
d <- read.csv(fname, sep=",")
colnames(d)

mean(d$meanP_l) # 1151.299
mean(d$meanP_r)# 1176.588
sd(d$meanP_l) #  361.1135
sd(d$meanP_r) # 452.0393

t.test(d$meanP_l, d$meanP_r) # extremely significant in t-test
# what about handedness correlation
d$pupil_disparity <- d$meanP_r - d$meanP_l
mean(d$pupil_disparity) # 25.289
sd(d$pupil_disparity) # 240.3743
# quite a large standard deviation
# let's do a correlation with handedness
unique(d$handedness)
cor(d$pupil_disparity, as.numeric(d$handedness)) # -0.0964
# so little apparent correlation but could be because correlation is wrong
boxplot(d$pupil_disparity ~ d$handedness)
unique(d$handednessValue)

# calculate correlation between pupil disparity and handedness value only for those handedness values which are not NA
d.withhandedness <- d[!is.na(d$handednessValue),]
cor(d.withhandedness$pupil_disparity, d.withhandedness$handednessValue) # -0.11623...

# let's try filtering out those without a visual disorder
unique(d$visualDisorder)
nodis <- subset(d, d$visualDisorder == 'N')
nrow(nodis)
nrow(d)
boxplot(nodis$pupil_disparity ~ nodis$handedness)

nodis.withhandedness <- nodis[!is.na(nodis$handednessValue),]
cor(nodis.withhandedness$pupil_disparity, nodis.withhandedness$handednessValue) # -0.112456

# let's check out the hexbin of the meanx vs meany to determine how to set up screen side and the like
range(d$meanX_l)
range(d$meanX_r)
range(d$meanY_l)
hex <- ggplot(d, aes(meanX_l, meanY_l))
hex + geom_hex()
d.left <- subset(d, d$ScreenSide == "Left")
d.middle <- subset(d, d$ScreenSide == "Middle")
d.right <- subset(d, d$ScreenSide == "Right")

mean(d.left$meanP_l) # 1180.09
mean(d.left$meanP_r) # 1169.921
sd(d.left$meanP_l) #370.8977
sd(d.left$meanP_r) # 463.0138

mean(d.middle$meanP_l) # 1148.941
mean(d.middle$meanP_r) # 1179.610
sd(d.middle$meanP_l) # 359.1006
sd(d.middle$meanP_r) # 451.4678

mean(d.right$meanP_l) # 1105.671
mean(d.right$meanP_r) # 1183.172
sd(d.right$meanP_l) #341.7702
sd(d.right$meanP_r) # 433.3778

#t tests of significance
t.test(d.left$meanP_l, d.left$meanP_r) #= t = 4.1744, p = 2.99*10-5
t.test(d.middle$meanP_r, d.right$meanP_r) # t = -1.1673, p = 02429

plot(d$meanP_l, d$thisSaccStartX_l)
cor(d$meanP_l, d$thisSaccStartX_l)

mean(d$pupil_disparity)
d$pupil_divergence <- d$pupil_disparity - mean(d$pupil_disparity)
mean(d$pupil_divergence)

# first create the typology subsets
d.syn <- subset(d, d$Type == "Syn")
d.T1 <- subset(d, d$Type == "T1")
d.T2 <- subset(d, d$Type == "T2")
d.T3 <- subset(d, d$Type == "T3")
d.T4 <- subset(d, d$Type == "T4")
d.T5 <- subset(d, d$Type == "T5")
d.T6 <- subset(d, d$Type == "T6")
d.T7 <- subset(d, d$Type == "T7")
d.T8 <- subset(d, d$Type == "T8")


lefteyes <- subset(d, d$eyePreferred == "L")
righteyes <-subset(d, d$eyePreferred == "R")

mean(d$meanP_l) # 1151.299
mean(d$meanP_r)# 1176.588

mean(lefteyes$meanP_l) # 1195.546
mean(lefteyes$meanP_r) # 1293.163
mean(righteyes$meanP_l) # 1149.953
mean(righteyes$meanP_r) # 1137.801

sd(lefteyes$meanP_l) # 421.7412
sd(lefteyes$meanP_r) # 609.9321
sd(righteyes$meanP_l) # 337.0896
sd(righteyes$meanP_r) # 346.5546
t.test(lefteyes$meanP_l, lefteyes$meanP_r) # t = -27.757, p = 0
t.test(righteyes$meanP_l, righteyes$meanP_r) # t = 7.044, p = 1.865e-12 

mean(lefties$meanP_l) # 865.17
mean(lefties$meanP_r) # 937.8086
mean(righties$meanP_l) # 1173.073
mean(righties$meanP_r) # 1187.183
unique(d$haveDyslexia)
dys <- subset(d, d$haveDyslexia != "N")
dys$haveDyslexia 
head(dys) 
unique(d$visualDisorder)
disorder <- subset(d, d$visualDisorder != "N")
nrow(disorder)
mean(disorder$meanP_l)
mean(disorder$meanP_r) 

unique(d$gender)
male <- subset(d, d$gender == "M")
female <- subset(d, d$gender == "F")
nrow(male) / nrow(d)
nrow(female) / nrow(d)
mean(male$meanP_l) # 1125.218
mean(male$meanP_r) # 1100.882
mean(female$meanP_l) # 1182.052
mean(female$meanP_r) # 1258.473
unique(d$handedness)
nrow(subset(male, male$handedness == "L")) /nrow(male)
nrow(subset(female, female$handedness == "L")) / nrow(female)
nrow(subset(female, female$handedness == "R")) / nrow(female)

nrow(subset(lefties, lefties$glassesOrContacts != "N")) / nrow(lefties) # 0.326243
nrow(subset(righties, righties$glassesOrContacts != "N")) / nrow(righties) # 0.2881965


nrow(subset(lefties, lefties$glassesOrContacts == "contacts")) / nrow(lefties) # 0.326243
nrow(subset(righties, righties$glassesOrContacts == "contacts")) / nrow(righties) # 0.2881965

unique(d$HandledBy)
nrow(subset(lefties, lefties$HandledBy == "Clare")) / nrow(lefties)
nrow(subset(righties, righties$HandledBy == "Clare")) / nrow(righties)
leftclare <- subset(lefties, lefties$HandledBy == "Clare")
leftmonica <- subset(lefties, lefties$HandledBy == "Monica")
mean(leftclare$meanP_l)
mean(leftmonica$meanP_l)
unique(leftclare$subjectID)

rightclare <- subset(righties, righties$HandledBy == "Clare")
rightmonica <- subset(righties, righties$HandledBy == "Monica")
mean(rightclare$meanP_l)
mean(rightmonica$meanP_l)

nrow(lefties)
head(lefties$han)

hist(lefties$meanP_l)
hist(righties$meanP_l)

unique(d$glassesOrContacts)

cor(d$meanP_l, d$meanX_l)
nrow(lefteyes)

lefteyes.left <- subset(lefteyes, lefteyes$ScreenSide == "Left")
lefteyes.middle <- subset(lefteyes, lefteyes$ScreenSide == "Middle")
lefteyes.right <- subset(lefteyes, lefteyes$ScreenSide == "Right")

nrow(lefteyes.left)

mean(lefteyes.left$meanP_l) # 1229.584
mean(lefteyes.left$meanP_r) # 1299.439

mean(lefteyes.middle$meanP_l) # 1188.975
mean(lefteyes.middle$meanP_r) # 1293.738

mean(lefteyes.right$meanP_l) #1149.414
mean(lefteyes.right$meanP_r) # 1281.874


boxplot(lefteyes$meanP_l ~ lefteyes$ScreenSide)

righteyes.left <- subset(righteyes, righteyes$ScreenSide == "Left")
righteyes.middle <- subset(righteyes, righteyes$ScreenSide == "Middle")
righteyes.right <- subset(righteyes, righteyes$ScreenSide == "Right")

mean(righteyes.left$meanP_l) # 1177.838
mean(righteyes.left$meanP_r) # 1129.806

mean(righteyes.middle$meanP_l) # 1147.429
mean(righteyes.middle$meanP_r) # 1140.009

mean(righteyes.right$meanP_l) #1104.09
mean(righteyes.right$meanP_r) # 1148.383



# let's do the pupil divergences
mean(d.syn$pupil_divergence) # 3.25882 # #right, but nearly correct
mean(d.T1$pupil_divergence) # -20.676 # left, correct
mean(d.T2$pupil_divergence) # 41.93079 # right, correct
mean(d.T3$pupil_divergence) # 17.89961 # going rightwards, correct
mean(d.T4$pupil_divergence) # -6.899244 # staying leftwards, correct
mean(d.T5$pupil_divergence) # 57.46767 # going rightwards, correct
mean(d.T6$pupil_divergence) # -42.894 # going leftwards, correct
mean(d.T7$pupil_divergence) # -61.2524 # going leftwards moreso correct!
mean(d.T8$pupil_divergence) # -18.37 # staying rightwards, correct

sd(d.syn$pupil_divergence) # 240.2468
sd(d.T1$pupil_divergence)  # 237.5679
sd(d.T2$pupil_divergence)  # 261.3709
sd(d.T3$pupil_divergence)  # 240.4002
sd(d.T4$pupil_divergence) # 229.8682
sd(d.T5$pupil_divergence)  # 260.6302
sd(d.T6$pupil_divergence)  # 224.3622
sd(d.T7$pupil_divergence) # 231.0414
sd(d.T8$pupil_divergence) # 231.5063

boxplot(d$pupil_divergence ~ d$ScreenSide)
mean(d.left$pupil_divergence)#-35.45944
mean(d.middle$pupil_divergence) # 5.41031
mean(d.right$pupil_divergence) # 52.21053
sd(d.left$pupil_divergence) # 243.9076
sd(d.middle$pupil_divergence) #237.5833
sd(d.right$pupil_divergence) # 228.1228

lefties <- subset(d, d$handedness == "L")
righties <- subset(d, d$handedness == "R")
mean(lefties$pupil_divergence) # 47.24514
mean(righties$pupil_divergence) # -11.17917
t.test(lefties$pupil_divergence, righties$pupil_divergence) # p ~ 0 so extremely significant!

nrow(lefties)
nrow(righties)
mean(d$meanP_l)
mean(d$meanP_r)
mean(lefties$meanP_l)
mean(lefties$meanP_r)
mean(righties$meanP_l)
mean(righties$meanP_r)


unique(d$eyePreferred)
lefteye <- subset(d, d$eyePreferred == "L")
righteye <- subset(d, d$eyePreferred == "R")
eithereye <- subset(d, d$eyePreferred == "Either")
mean(lefteye$pupil_divergence) # 72.32689
mean(righteye$pupil_divergence) # -37.4429
mean(eithereye$pupil_divergence) # -83.04804
unique(d$earPrephered)
leftear <- subset(d, d$earPrephered == "L")
rightear <-subset(d, d$earPrephered == "R")
eitherear <- subset(d, d$earPrephered == "Either")
mean(leftear$pupil_divergence) # 4.005667
mean(rightear$pupil_divergence) # -2.091535
mean(eitherear$pupil_divergence) # -49.03402

nrow(eitherear)
nrow(eithereye)
nrow(lefteye)

t.test(leftear$pupil_divergence, rightear$pupil_divergence)
cor(d$pupil_divergence, d$thisSaccStartX_l) # 0.1471489
cor(d$pupil_divergence, d$thisSaccStartX_r) # 0.1431985
cor(d$pupil_divergence, d$thisSaccEndX_l) # 0.1442194
cor(d$pupil_divergence, d$thisSaccEndX_r) # 0.05720263 
cor(d$pupil_divergence, d$thisSaccDuration_l) # 0.06200679 # no appreaciable correlation with duration
cor(d$pupil_divergence, d$thisSaccDuration_r) # 0.07073231
cor(d$pupil_divergence, d$binocular_disparity_start) # -0.02831622 # no appreciablecorrelation with binocular disparity

# let's check out correlations between handedness and eyedness and earedness
cor(as.numeric(d$handedness), as.numeric(d$eyePreferred)) # 0.7410936 -  it's significant
cor(as.numeric(d$handedness), as.numeric(d$earPrephered)) #0.6523815 a bit less big still significant


d$binocular_disparity_start <- d$thisSaccStartX_r - d$thisSaccStartX_l

colnames(d)
unique(d$subID_x)
unique(d$subID_y)
nrow(subset(d, d$subID_x != d$subID_y))
unique(d$subjectID)
nrow(subset(d, d$subID_x != d$subjectID)) 
d$subID_x[1:100]mmmvmmmmmmvvv
d$subjectID[1:100]
nrow(subset(d, as.numeric(d$subID_x) != as.numeric(d$subjectID))) # 0 
# just assume subject ID is the correct thing here
head(d$subjectID.1)
nrow(subset(d, d$subjectID != d$subjectID.1))

basescreensidelmer <- lmer(pupil_divergence ~ ScreenSide + (1|subjectID), data=d, REML=FALSE)
basescreensideplustype <- lmer(pupil_divergence ~ ScreenSide + Type + (1|subjectID), data=d, REML=FALSE)
anova(basescreensidelmer, basescreensideplustype) 
base_handedness <- lmer(pupil_divergence ~ ScreenSide + Type + (1|subjectID) + (1|handedness), data=d, REML=FALSE)
anova(basescreensideplustype, base_handedness) # handedness appears to have no effect(!!!!)
base_experimenter <-lmer(pupil_divergence ~ ScreenSide + Type + (1|subjectID) + (1|HandledBy), data=d, REML=FALSE)
anova(basescreensideplustype, base_experimenter) # no effect of experimenter
lmer_null <- lmer(pupil_divergence ~  (1|subjectID), data=d, REML=FALSE)
anova(basescreensidelmer, lmer_null) # huge effect of screenside
basejusttype <- lmer(pupil_divergence ~ Type + (1|subjectID), data=d, REML=FALSE)
anova(basejusttype, lmer_null) # extremely significant
base_eyepreferred <- lmer(pupil_divergence ~ ScreenSide + Type + (1|subjectID) + (1|eyePreferred), data=d, REML=FALSE)
anova(basetype_eyepreferred, basescreensideplustype) 
basetype_eyepreferred <- lmer(pupil_divergence ~ Type + (1|subjectID), data=d, REML=FALSE)
anova(basetype_eyepreferred, basejusttype)
eyepreffered_justscreen <- lmer(pupil_divergence ~ ScreenSide + (1|subjectID) + (1|eyePreferred), data=d, REML=FALSE)
anova(basescreensidelmer, eyepreffered_justscreen)












