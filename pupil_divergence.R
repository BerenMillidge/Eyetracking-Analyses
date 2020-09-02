
library(dplyr)
library(lme4)

filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
colnames(data)


cor(data$L_Pupil, data$R_Pupil)
sd(data$L_Pupil)
sd(data$R_Pupil)
mean(data$L_Pupil)
mean(data$R_Pupil)
t.test(data$L_Pupil, data$R_Pupil)

# split up into different groups:
mean(eng$L_Pupil)
sd(eng$L_Pupil)
mean(eng$R_Pupil)
sd(eng$R_Pupil)
t.test(eng$L_Pupil, eng$R_Pupil)

mean(chin$L_Pupil)
sd(chin$L_Pupil)
mean(chin$R_Pupil)
sd(chin$R_Pupil)
t.test(chin$L_Pupil, chin$R_Pupil)

dys <- subset(data, Group=="Dyslexic")
nrow(dys)

mean(dys$L_Pupil)
sd(dys$L_Pupil)
mean(dys$R_Pupil)
sd(dys$R_Pupil)

t.test(dys$L_Pupil, dys$R_Pupil)

hex.T8 <- ggplot(data, aes(L_Pupil, R_Pupil))
hex.T8 + geom_hex()

data$ScreenSide<- ifelse(data$L_StartX<= 341 | data$R_StartX<= 341,"Left",
                         ifelse(data$L_StartX> 341& data$L_StartX< 682 | data$R_StartX> 341& data$R_StartX< 682,"Middle",
                                ifelse(data$L_StartX>= 682 | data$R_StartX>= 682,"Right",NA)))

mean(subset(data$L_Pupil, data$ScreenSide == "Left"))
mean(subset(data$L_Pupil, data$ScreenSide == "Middle"))
mean(subset(data$L_Pupil, data$ScreenSide == "Right"))

mean(subset(data$R_Pupil, data$ScreenSide == "Left"))
mean(subset(data$R_Pupil, data$ScreenSide == "Middle"))
mean(subset(data$R_Pupil, data$ScreenSide == "Right"))


boxplot(data$L_Pupil ~ data$Group + data$ScreenSide)
boxplot(data$R_Pupil ~ data$Group + data$ScreenSide)
boxplot(data$L_Pupil ~ data$ID)
data["Mean_Pupil"] <- (data$L_Pupil + data$R_Pupil)/ 2

lmermodel <- lmer(Mean_Pupil ~ ScreenSide +  (1| Group) + (1|ID), data=data, REML=FALSE)
summary(lmermodel)
lmer_no_ID <- lmer(Mean_Pupil ~ ScreenSide +  (1| Group), data=data, REML=FALSE) 
anova(lmermodel, lmer_no_ID)
lmer_no_group <- lmer(Mean_Pupil ~ ScreenSide + (1|ID), data=data, REML=FALSE)
lmer_no_effect <- lmer(Mean_Pupil ~ (1|ID), data=data, REML=FALSE)
anova(lmermodel, lmer_no_group)
anova(lmer_no_group, lmer_no_effect)
lmer_gender <- lmermodel <- lmer(Mean_Pupil ~ ScreenSide +  (1| Group) + (1|ID) + (1|Sex), data=data, REML=FALSE)
anova(lmermodel, lmer_gender)
# no effect of gender
lmer_block <- lmermodel <- lmer(Mean_Pupil ~ ScreenSide +  (1| Group) + (1|ID) + (1|L_Block), data=data, REML=FALSE)
anova(lmermodel, lmer_block)


dyslexics <- subset(data, Group == "English")
nrow(subset(data, data$L_Block == "1("))
nrow(subset(data, data$L_Block == "12"))

boxplot(dyslexics$Mean_Pupil ~ dyslexics$L_Block)
range(data$L_Block)
unique(data$L_Block)

# filter by screen size!
data <- subset(data, R_StartX >= 0 & R_StartX <= 1024 & R_StartY >= 0 & R_StartY <= 1024) # I'm changing this to 1024 for some reason... I don't understand why it's 768
data <- subset(data, L_StartX >= 0 & L_StartX <= 1024 & L_StartY >= 0 & L_StartY <= 1024)
data <- subset(data, R_EndX >= 0 & R_EndX <= 1024 & R_EndX >= 0 & R_EndX <= 1024)
data <- subset(data, L_EndX >= 0 & L_EndX <= 1024 & L_EndX >= 0 & L_EndX <= 1024)

data["StampStart"]<- data$R_StartTime - data$L_StartTime
data["StampEnd"]<- data$R_EndTime - data$L_EndTime

# subset the synchronized fixations
NormalSYN<- subset(data, StampStart==0 & StampEnd==0)

# Change the name of duration before set the types
data$ST_duration<- data$StampStart
data$ET_duration<- data$StampEnd

data["STtype"]<- ifelse(as.numeric(data$ST_duration)>0,"RW",
                        ifelse(as.numeric(data$ST_duration)<0,"LW","Syn"))
data["ETtype"]<- ifelse(as.numeric(data$ET_duration)>0,"RW",
                        ifelse(as.numeric(data$ET_duration)<0,"LW","Syn"))

head(data$STtype)
head(data$ETtype)

data["Type"]<- ifelse(data$STtype=="Syn"&data$ETtype=="Syn",data$Type<- "Syn",
                      ifelse(data$STtype=="RW"&data$ETtype=="RW",data$Type<- "T8",
                             ifelse(data$STtype=="RW"&data$ETtype=="LW",data$Type<- "T7",
                                    ifelse(data$STtype=="LW"&data$ETtype=="LW",data$Type<-"T4",
                                           ifelse(data$STtype=="LW"&data$ETtype=="RW",data$Type<- "T5",
                                                  ifelse(data$STtype=="Syn"&data$ETtype=="LW",data$Type<- "T1",
                                                         ifelse(data$STtype=="Syn"&data$ETtype=="RW",data$Type<- "T2",
                                                                ifelse(data$STtype=="LW"&data$ETtype=="Syn",data$Type<- "T3",
                                                                       ifelse(data$STtype=="RW"&data$ETtype=="Syn",data$Type<- "T6",NA
                                                                       )))))))))

head(data$Type)
filtpupdiff <-subset(data, data$Pupil_Diff > -50 & data$Pupil_Diff < 50)
boxplot(filtpupdiff$Pupil_Diff ~ filtpupdiff$Type)

data["Start_Time_diff"] <- data$L_StartTime - data$R_StartTime
mean(data$Start_Time_diff)
sd(data$Start_Time_diff)
nrow(subset(data,data$Start_Time_diff == 0))/ nrow(data)


median(subset(data$Pupil_Diff, data$Type == "Syn"))
median(subset(data$Pupil_Diff, data$Type == "T1"))

median(subset(data$Pupil_Diff, data$Type == "T3"))
median(subset(data$Pupil_Diff, data$Type == "T6"))

data.syn <- subset(data, Type=="Syn")
data.T8 <- subset(data, Type=="T8")
data.T7 <- subset(data, Type=="T7")
data.T6 <- subset(data, Type=="T6")
data.T5 <- subset(data, Type=="T5")
data.T4 <- subset(data, Type=="T4")
data.T3 <- subset(data, Type=="T3")
data.T2 <- subset(data, Type=="T2")
data.T1 <- subset(data, Type=="T1")

t.test(data.syn$Pupil_Diff, data.T3$Pupil_Diff)
t.test(data.syn$Pupil_Diff, data.T6$Pupil_Diff)
t.test(data.syn$Pupil_Diff, data.T1$Pupil_Diff)

data["Pupil_Diff"] <- data$L_Pupil - data$R_Pupil
pupil_diff <- na.omit(data$Pupil_Diff)
mean(data$Pupil_Diff)
boxplot(data$L_MeanX ~ data$Type_factor)
data["Type_factor"] <- as.factor(data$Type)
boxplot(pupil_diff ~ data$Type_factor)

mean(data$Pupil_Diff)
sd(data$Pupil_Diff)

data["Pupil_divergence"] <- data$Pupil_Diff - mean(data$Pupil_Diff)
mean(data$Pupil_divergence)
unique(data$Group)


mean(data.syn$Pupil_divergence)
t.test(data.syn$Pupil_divergence)

mean(data.T1$Pupil_divergence)
t.test(data.T1$Pupil_divergence)

mean(data.T2$Pupil_divergence)
t.test(data.T2$Pupil_divergence)

mean(data.T3$Pupil_divergence)
t.test(data.T3$Pupil_divergence)

mean(data.T4$Pupil_divergence)
t.test(data.T4$Pupil_divergence)

mean(data.T5$Pupil_divergence)
t.test(data.T5$Pupil_divergence)

mean(data.T6$Pupil_divergence)
t.test(data.T6$Pupil_divergence)

mean(data.T7$Pupil_divergence)
t.test(data.T7$Pupil_divergence)

mean(data.T8$Pupil_divergence)
t.test(data.T8$Pupil_divergence)

eng <- subset(data, Group=="English")
eng["Pupil_divergence"] <- eng$Pupil_Diff - mean(eng$Pupil_Diff)
eng.syn <- subset(eng, Type=="Syn")
eng.T8 <- subset(eng, Type=="T8")
eng.T7 <- subset(eng, Type=="T7")
eng.T6 <- subset(eng, Type=="T6")
eng.T5 <- subset(eng, Type=="T5")
eng.T4 <- subset(eng, Type=="T4")
eng.T3 <- subset(eng, Type=="T3")
eng.T2 <- subset(eng, Type=="T2")
eng.T1 <- subset(eng, Type=="T1")



# data just for english speakers, to see if the typology is respected
mean(eng.syn$Pupil_divergence)
t.test(eng.syn$Pupil_divergence)

mean(eng.T1$Pupil_divergence)
t.test(eng.T1$Pupil_divergence)

mean(eng.T2$Pupil_divergence)
t.test(eng.T2$Pupil_divergence)

mean(eng.T3$Pupil_divergence)
t.test(eng.T3$Pupil_divergence)

mean(eng.T4$Pupil_divergence)
t.test(eng.T4$Pupil_divergence)

mean(eng.T5$Pupil_divergence)
t.test(eng.T5$Pupil_divergence)

mean(eng.T6$Pupil_divergence)
t.test(eng.T6$Pupil_divergence)

mean(eng.T7$Pupil_divergence)
t.test(eng.T7$Pupil_divergence)

mean(eng.T8$Pupil_divergence)
t.test(eng.T8$Pupil_divergence)


#chinese data
chin <- subset(data, Group=="Chinese")
chin["Pupil_divergence"] <- chin$Pupil_Diff - mean(chin$Pupil_Diff)
chin.syn <- subset(chin, Type=="Syn")
chin.T8 <- subset(chin, Type=="T8")
chin.T7 <- subset(chin, Type=="T7")
chin.T6 <- subset(chin, Type=="T6")
chin.T5 <- subset(chin, Type=="T5")
chin.T4 <- subset(chin, Type=="T4")
chin.T3 <- subset(chin, Type=="T3")
chin.T2 <- subset(chin, Type=="T2")
chin.T1 <- subset(chin, Type=="T1")



# data just for english speakers, to see if the typology is respected
t.test(chin.syn$Pupil_divergence)

t.test(chin.T1$Pupil_divergence)

t.test(chin.T2$Pupil_divergence)

t.test(chin.T3$Pupil_divergence)

t.test(chin.T4$Pupil_divergence)

t.test(chin.T5$Pupil_divergence)

t.test(chin.T6$Pupil_divergence)

t.test(chin.T7$Pupil_divergence)

t.test(chin.T8$Pupil_divergence)


dyslexics <- subset(data, data$Group == "Dyslexic")
nrow(dyslexics)
nrow(data)

dyslexics["Pupil_divergence"] <- dyslexics$Pupil_Diff - mean(dyslexics$Pupil_Diff)
dys <- dyslexics
dyslexics.syn <- subset(dyslexics, Type=="Syn")
dyslexics.T8 <- subset(dyslexics, Type=="T8")
dyslexics.T7 <- subset(dyslexics, Type=="T7")
dyslexics.T6 <- subset(dyslexics, Type=="T6")
dyslexics.T5 <- subset(dyslexics, Type=="T5")
dyslexics.T4 <- subset(dyslexics, Type=="T4")
dyslexics.T3 <- subset(dyslexics, Type=="T3")
dyslexics.T2 <- subset(dyslexics, Type=="T2")
dyslexics.T1 <- subset(dyslexics, Type=="T1")

mean(dyslexics.syn$Pupil_divergence)
mean(dyslexics.T1$Pupil_divergence)
mean(dyslexics.T2$Pupil_divergence)
mean(dyslexics.T3$Pupil_divergence)
mean(dyslexics.T4$Pupil_divergence)
mean(dyslexics.T5$Pupil_divergence)
mean(dyslexics.T6$Pupil_divergence)
mean(dyslexics.T7$Pupil_divergence)
mean(dyslexics.T8$Pupil_divergence)

t.test(dyslexics.syn$Pupil_divergence)

t.test(dyslexics.T1$Pupil_divergence)

t.test(dyslexics.T2$Pupil_divergence)

t.test(dyslexics.T3$Pupil_divergence)

t.test(dyslexics.T4$Pupil_divergence)

t.test(dyslexics.T5$Pupil_divergence)

t.test(dyslexics.T6$Pupil_divergence)

t.test(dyslexics.T7$Pupil_divergence)

t.test(dyslexics.T8$Pupil_divergence)

typenum <- as.numeric(as.factor(data$Type))
head(typenum)
cor(data$Binocular_Disparity, typenum)
# so no correlatoin of binocular disparity with fixation type
cor(data$Binocular_Disparity_End, typenum)
filt <- subset(data, Pupil_divergence >= -50000 & Pupil_divergence <= 50000)
male <- subset(filt, Sex == "Male")
mean(male$Pupil_divergence)
sd(male$Pupil_divergence)
female <- subset(filt, Sex == "Female")
mean(female$Pupil_divergence)
sd(female$Pupil_divergence)
eng <- subset(filt, Group=="English")
chin <- subset(filt, Group=="Chinese")
dys <- subset(filt,Group == "Dyslexic")
nrow(dys)
mean(eng$Pupil_divergence)
mean(chin$Pupil_divergence)
mean(dys$Pupil_divergence)
sd(eng$Pupil_divergence)
sd(chin$Pupil_divergence)
sd(dys$Pupil_divergence)
chin["Pupil_divergence"] <- chin$Pupil_Diff - mean(filt$Pupil_Diff)
chinmale <- subset(chin, data$Sex == "Male")
chinfemale <- subset(chin,Sex == "Female")
unique(as.factor(chinmale$ID)) #  19 male chinese - 12 male chinese
unique(as.factor(chinfemale$ID)) # 34 female chinese 25 female chinese 
head(chinmale)
head(chinmale$Pupil_divergence)
test <- na.omit(chinmale$Pupil_divergence)
range(test)
mean(test)
mean(na.omit(chinmale$Pupil_divergence))
mean(na.omit(chinfemale$Pupil_divergence))

sd(na.omit(chinmale$Pupil_divergence))
sd(na.omit(chinfemale$Pupil_divergence))

eng["Pupil_divergence"] <- eng$Pupil_Diff - mean(data$Pupil_Diff)
engmale <- subset(eng, dys$Sex == "Male")
unique(as.factor(engmale$ID)) # 18 unique males
engfemale <- subset(eng,data$Sex == "Female")
unique(as.factor(engfemale$ID)) # 35
mean(na.omit(engmale$Pupil_divergence))
mean(na.omit(engfemale$Pupil_divergence)) # 22 unique females

sd(na.omit(engmale$Pupil_divergence))
sd(na.omit(engfemale$Pupil_divergence)) # 22 unique females

dys["Pupil_divergence"] <- dys$Pupil_Diff - mean(filt$Pupil_Diff)
dysmale <- subset(dys, data$Sex == "Male")
dysfemale <- subset(dys,data$Sex =="Female")
unique(as.factor(dysmale$ID)) # 25 # 20
unique(as.factor(dysfemale$ID)) # 51 # 34
mean(na.omit(dysmale$Pupil_divergence))
mean(na.omit(dysfemale$Pupil_divergence))
sd(na.omit(dysmale$Pupil_divergence))
sd(na.omit(dysfemale$Pupil_divergence))

nrow(subset(eng, eng$Pupil_divergence <= 0)) / nrow(eng)
nrow(subset(chin, chin$Pupil_divergence <=0)) / nrow(chin)
nrow(subset(dys, dys$Pupil_divergence <= 0)) / nrow(dys)

t.test(male$Pupil_divergence, female$Pupil_divergence)
nondys <- subset(data, Group == "English" | Group == "Chinese")
eng <- subset(data, Group=="English")
male <- subset(data, Sex == "Male")


hex.T8 <- ggplot(dyslexics.syn, aes(L_StartX, L_StartY))
hex.T8 + geom_hex()

hex.T1 <- ggplot(dyslexics.T1, aes(L_StartX, L_StartY))
hex.T1 + geom_hex()

hex.T2 <- ggplot(dyslexics.T2, aes(L_StartX, L_StartY))
hex.T2 + geom_hex()

hex.T3 <- ggplot(dyslexics.T3, aes(L_StartX, L_StartY))
hex.T3 + geom_hex()

hex.T4 <- ggplot(dyslexics.T4, aes(L_StartX, L_StartY))
hex.T4 + geom_hex()

hex.T5 <- ggplot(dyslexics.T5, aes(L_StartX, L_StartY))
hex.T5 + geom_hex()

hex.T6 <- ggplot(dyslexics.T6, aes(L_StartX, L_StartY))
hex.T6 + geom_hex()

hex.T7 <- ggplot(dyslexics.T7, aes(L_StartX, L_StartY))
hex.T7 + geom_hex()

hex.T8 <- ggplot(dyslexics.T8, aes(L_StartX, L_StartY))
hex.T8 + geom_hex()


mean(male$Pupil_divergence)
sd(male$Pupil_divergence)
female <- subset(data, Sex=="Female")
mean(female$Pupil_divergence)
median(male$Pupil_divergence)
median(female$Pupil_divergence)
sd(female$Pupil_divergence)
boxplot(data$Pupil_divergence ~ data$Sex)
unique(as.factor(eng$ID))
range(eng$ID)
unique(as.factor(chin$ID))
range(chin$ID)
range(dyslexics$ID)
unique(as.factor(dyslexics$ID))
mean(eng$Pupil_divergence)
chin <- subset(data, Group=="Chinese")
mean(chin$Pupil_divergence)
data["Binocular_Disparity"] <- data$L_StartX - data$R_StartX
data["Binocular_Disparity_End"] <- data$L_EndX - data$R_EndX
plot(data$Pupil_divergence, data$Binocular_Disparity)
cor(data$Pupil_divergence, data$Binocular_Disparity)
cor(data$Pupil_divergence, data$L_deltaX)
cor(data$Pupil_divergence, data$L_DeltaTime)
cor(data$Pupil_Diff, data$R_deltaX)
cor(data$Pupil_divergence, data$R_deltaX)
cor(data$Pupil_divergence, data$R_DeltaTime)
cor(data$Pupil_divergence, data$L_Angle)
cor(data$Pupil_divergence, data$R_Angle)
boxplot(data$Pupil_divergence ~ data$Sex)
nrow(nondys)
mean(nondys$Pupil_divergence)
mean(dyslexics$Pupil_divergence)
d <- na.omit(data)
colnames(d)
lmer <- lmer(Pupil_divergence ~ Type_factor +(1|ID), data=d, REML=FALSE)
lmer_no_group <- lmer(Pupil_divergence ~ Type , data=d, REML=FALSE)
anova(lmer, lmer_no_group)

data <- na.omit(data)

lmermodel <- lmer(Pupil_divergence ~ Type +  (1| Group) + (1|ID), data=data, REML=FALSE)
summary(lmermodel)
lmer_no_group <- lmer(Pupil_divergence ~ Type + (1|ID), data=data, REML=FALSE)
lmer_just_type <- lmer(Pupil_divergence ~ Type, data=data, REML=FALSE)
lmer_group <- lmer(Pupil_divergence ~ Type +  (1| Group), data=data, REML=FALSE)
anova(lmermodel, lmer_no_group)
lmer_sex <- lmer(Pupil_divergence ~ Type +  (1| Group) + (1|ID) + (1|Sex), data=data, REML=FALSE)
lmer_block <- lmer(Pupil_divergence ~ Type +  (1| Group) + (1|ID) + (1|Sex) + (1|L_Block), data=data, REML=FALSE)
lmer_null <- lmer(Pupil_divergence ~ (1|ID), data=data, REML=FALSE)
lmer_screenside <- lmer(Pupil_divergence ~ Type +  (1| Group) + (1|ID) + (1|ScreenSide), data=data, REML=FALSE)
anova(lmer_null, lmer_no_group)
anova(lmer_no_group, lmermodel)
anova(lmermodel, lmer_sex)
anova(lmer_sex, lmer_block)
anova(lmer_screenside, lmer_sex)

mean(data.T6$Pupil_divergence)range(data$Pupil_Diff)
head(data$Pupil_Diff)
min(data$Pupil_Diff)
max(data$Pupil_Diff)
head(data$Type)
