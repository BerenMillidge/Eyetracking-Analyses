# calculatw proportions of crossed vs uncrossed in dataset.


library(dplyr)
library(lme4)
library(ggplot2)

filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
colnames(data)

# filter the data first which is reasonable!
data <- subset(data, R_StartX >= 0 & R_StartX <= 1024 & R_StartY >= 0 & R_StartY <= 1024) 
data <- subset(data, L_StartX >= 0 & L_StartX <= 1024 & L_StartY >= 0 & L_StartY <= 1024)
data <- subset(data, R_EndX >= 0 & R_EndX <= 1024 & R_EndX >= 0 & R_EndX <= 1024)
data <- subset(data, L_EndX >= 0 & L_EndX <= 1024 & L_EndX >= 0 & L_EndX <= 1024)

data["Start_Crossed"] = ifelse(data$L_StartX > data$R_StartX, data$Start_Crossed <- "Crossed", data$Start_Crossed <- "Uncrossed")
data["End_Crossed"] <- ifelse(data$L_EndX > data$R_EndX, data$End_Crossed <- "Crossed", data$End_Crossed <- "Uncrossed")


data$ScreenSide<- ifelse(data$L_StartX<= 341 | data$R_StartX<= 341,"Left",
                         ifelse(data$L_StartX> 341& data$L_StartX< 682 | data$R_StartX> 341& data$R_StartX< 682,"Middle",
                                ifelse(data$L_StartX>= 682 | data$R_StartX>= 682,"Right",NA)))


crossednum <- nrow(subset(data, data$Start_Crossed == "Crossed"))
crossednum / nrow(data)
nrow(data)
uncrossednum <- nrow(subset(data, data$Start_Crossed == "Uncrossed"))
crossednum + uncrossednum

crossendnum <- nrow(subset(data, data$End_Crossed == "Crossed"))
crossendnum / nrow(data)


cor(data$Start_Crossed, data$End_Crossed)

crossednum / nrow(data)
crossed <- subset(data, data$Start_Crossed == "Crossed")
uncrossed <- subset(data, data$Start_Crossed == "Uncrossed")
mean(crossed$L_MeanX)
sd(crossed$L_MeanX)
mean(uncrossed$L_MeanX)
sd(uncrossed$L_MeanX)
t.test(crossed$L_MeanX, uncrossed$L_MeanX)
hist(crossed$L_MeanX)
hist(uncrossed$L_MeanX)

# look vaguely similar
mean(crossed$L_DeltaTime)
sd(crossed$L_DeltaTime)
mean(uncrossed$L_DeltaTime)
sd(uncrossed$L_DeltaTime)
t.test(crossed$L_DeltaTime, uncrossed$L_DeltaTime)
data$Start_Crossed_Numeric <- as.numeric(as.factor(data$Start_Crossed))
data$End_Crossed_Numeric <- as.numeric(as.factor(data$End_Crossed))
boxplot(data$L_MeanX ~ data$Start_Crossed)
head(data$Start_Crossed_Numeric)

crossed.hex <- ggplot(crossed, aes(L_StartX, L_StartY))
crossed.hex + geom_hex()

uncrossed.hex <- ggplot(uncrossed, aes(L_StartX, L_StartY))
uncrossed.hex + geom_hex()

mean(crossed$L_Pupil)
sd(crossed$L_Pupil)
mean(uncrossed$L_Pupil)
sd(uncrossed$L_Pupil)
mean(crossed$R_Pupil)
sd(crossed$R_Pupil)
mean(uncrossed$R_Pupil)
sd(uncrossed$R_Pupil)
t.test(crossed$L_Pupil, uncrossed$L_Pupil)
head(data$End_Crossed_Numeric)
head(data$Start_Crossed_Numeric)
sd(data$Start_Crossed_Numeric)
mean(data$Start_Crossed_Numeric)
cor(data$Start_Crossed_Numeric, data$End_Crossed_Numeric)
mean(data$End_Crossed_Numeric)
endcrossed <- subset(data, data$End_Crossed == "Crossed")
enduncrossed <- subset(data, data$End_Crossed == "Uncrossed")

crossedend.hex <- ggplot(crossed, aes(L_StartX, L_StartY))
crossedend.hex + geom_hex()

uncrossedend.hex <- ggplot(uncrossed, aes(L_StartX, L_StartY))
uncrossedend.hex + geom_hex()

mean(endcrossed$L_Pupil)
mean(enduncrossed$L_Pupil)
mean(crossed$L_Pupil)
mean(uncrossed$L_Pupil)
crossedSame <- subset(data, data$L_MeanX == data$R_MeanX)
nrow(crossedSame)
threshold <- 50
convergedWithinThreshold <- subset(data, data$L_MeanX < data$R_MeanX + threshold & data$L_MeanX> data$R_MeanX - threshold)
nrow(convergedWithinThreshold) / nrow(data)
thresholds <- c(0.1, 0.5, 1, 5,10,15,20,25,40,50,70,100)
nums <- vector("list", length(thresholds))
for (i in 1:length(thresholds)){
  threshold <- thresholds[i]
  convergedWithinThreshold <- subset(data, data$L_MeanX < data$R_MeanX + threshold & data$L_MeanX> data$R_MeanX - threshold)
  frac <- nrow(convergedWithinThreshold) / nrow(data)
  nums[i] <- frac
}
nums
pl <- plot(thresholds, nums,type="o", main="Fraction of conjugate fixations vs threshold")

data$Binocular_Disparity <- data$L_MeanX - data$R_MeanX
mean(data$Binocular_Disparity)
sd(data$Binocular_Disparity)
data["Crossing_Type"] <- ifelse(data$Start_Crossed == "Crossed" & data$End_Crossed == "Crossed", data$Crossing_Type <- "Crossed",
                                ifelse(data$Start_Crossed == "Uncrossed" & data$End_Crossed == "Uncrossed", data$Crossing_Type <- "Uncrossed",
                                       ifelse(data$Start_Crossed == "Crossed" & data$End_Crossed == "Uncrossed", data$Crossing_Type <- "MixedCrossed",
                                       ifelse(data$Start_Crossed == "Uncrossed" & data$End_Crossed == "Crossed", data$Crossing_Type <- "MixedUncrossed", NA))))
head(data$Crossing_Type)

bothcrossed <- subset(data, data$Crossing_Type == "Crossed")
bothuncrossed <- subset(data, data$Crossing_Type == "Uncrossed")
crossedmixed <- subset(data, data$Crossing_Type == "MixedCrossed")
uncrossedmixed <- subset(data, data$Crossing_Type == "MixedUncrossed")

nrow(bothcrossed) / nrow(data)
nrow(bothuncrossed) / nrow(data)
nrow(crossedmixed) / nrow(data)
nrow(uncrossedmixed) / nrow(data)

mean(bothcrossed$L_Pupil)
sd(bothcrossed$L_Pupil)
mean(bothuncrossed$L_Pupil)
sd(bothuncrossed$L_Pupil)
mean(crossedmixed$L_Pupil)
sd(crossedmixed$L_Pupil)
mean(uncrossedmixed$L_Pupil)
sd(uncrossedmixed$L_Pupil)

mean(bothcrossed$L_MeanX)
sd(bothcrossed$L_MeanX)
mean(bothuncrossed$L_MeanX)
sd(bothuncrossed$L_MeanX)
mean(crossedmixed$L_MeanX)
sd(crossedmixed$L_MeanX)
mean(uncrossedmixed$L_MeanX)
sd(uncrossedmixed$L_MeanX)
t.test(bothcrossed$L_MeanX, bothuncrossed$L_MeanX)
t.test(crossedmixed$L_MeanX, uncrossedmixed$L_MeanX)

bothcrossed.hex <- ggplot(bothcrossed, aes(L_StartX, L_StartY))
bothcrossed.hex + geom_hex()

bothuncrossed.hex <- ggplot(bothuncrossed, aes(L_StartX, L_StartY))
bothuncrossed.hex + geom_hex()

crossedmixed.hex <- ggplot(crossedmixed, aes(L_StartX, L_StartY))
crossedmixed.hex + geom_hex()

uncrossedmixed.hex <- ggplot(uncrossedmixed, aes(L_StartX, L_StartY))
uncrossedmixed.hex + geom_hex()




boxplot(data$L_MeanY ~ data$Crossing_Type) 
boxplot(data$L_MeanX ~ data$Crossing_Type) 
boxplot(data$R_MeanX ~ data$Crossing_Type)
boxplot(data$R_MeanY ~ data$Crossing_Type) 
boxplot(data$L_Pupil ~ data$Crossing_Type)
boxplot(data$R_Pupil ~ data$Crossing_Type) 
boxplot(data$L_Pupil ~ data$Crossing_Type + data$ScreenSide)
# see any effect of fixatoin duration
boxplot(data$L_DeltaTime ~ data$Crossing_Type)
boxplot(data$R_DeltaTime ~ data$Crossing_Type)

boxplot(data$L_Pupil ~data$Group + data$Crossing_Type)
boxplot(data$L_Angle~ data$Crossing_Type)
mean(bothcrossed$L_Angle)
mean(bothuncrossed$L_Angle)
mean(crossedmixed$L_Angle)
mean(uncrossedmixed$L_Angle)
mean(bothcrossed$R_Angle)
mean(bothuncrossed$R_Angle)
mean(crossedmixed$R_Angle)
mean(uncrossedmixed$R_Angle)
cor(data$L_Angle, data$R_Angle)

data["Mean_X"] <-(data$L_MeanX + data$R_MeanX) / 2
cor(data$L_MeanX, data$R_MeanX)
cor(data$L_Pupil, data$R_Pupil) 
data["Mean_Pupil"] <- (data$L_Pupil + data$R_Pupil) / 2

mean(bothcrossed$L_DeltaTime)
mean(bothuncrossed$L_DeltaTime)
t.test(bothuncrossed$L_DeltaTime, bothcrossed$L_DeltaTime )
eng <- subset(data, data$Group == "English")
chin <- subset(data, data$Group == "Chinese")
dys <- subset(data, data$Group == "Dyslexic")
nrow(subset(eng, eng$Crossing_Type == "Crossed")) / nrow(eng)
nrow(subset(chin, chin$Crossing_Type == "Crossed")) / nrow(chin)
nrow(subset(dys, dys$Crossing_Type == "Crossed")) / nrow(dys)

# see if there are sex differences
male <- subset(data, data$Sex == "Male")
female <- subset(data, data$Sex == "Female")
nrow(subset(male, male$Crossing_Type == "Crossed")) / nrow(male)
nrow(subset(female, female$Crossing_Type == "Crossed")) / nrow(female)


# let's investigate various factors with lmer for the pupil correlations

baselm <- lmer(Mean_Pupil ~ Crossing_Type + (1|ID), data=data, REML=FALSE)
grouplm <- lmer(Mean_Pupil ~ Crossing_Type + (1|ID) + (1|Group), data=data, REML=FALSE)
sexlm <- lmer(Mean_Pupil ~ Crossing_Type + (1|ID) + (1|Sex), data=data, REML=FALSE)
blocklm <- lmer(Mean_Pupil ~ Crossing_Type + (1|ID) + (1|L_Block), data=data, REML=FALSE)
storylm <- lmer(Mean_Pupil ~ Crossing_Type + (1|ID) + (1|StoryNo), data=data, REML=FALSE)
screensidelm <- lmer(Mean_Pupil ~ Crossing_Type + ScreenSide + (1|ID), data=data, REML=FALSE)
!
anova(baselm,grouplm) # group highly significant
anova(baselm, sexlm)
anova(baselm, blocklm) # block highly significant
anova(baselm, storylm) # story highly significant.
anova(baselm, screensidelm) # screenside highly significant

boxplot(data$L_Pupil ~ data$L_Block + data$Crossing_Type)

# typology

data["StampStart"]<- data$R_StartTime - data$L_StartTime
data["StampEnd"]<- data$R_EndTime - data$L_EndTime

# Change the name of duration before set the types
data$ST_duration<- data$StampStart
data$ET_duration<- data$StampEnd

data["STtype"]<- ifelse(as.numeric(data$ST_duration)>0,"RW",
                        ifelse(as.numeric(data$ST_duration)<0,"LW","Syn"))
data["ETtype"]<- ifelse(as.numeric(data$ET_duration)>0,"RW",
                        ifelse(as.numeric(data$ET_duration)<0,"LW","Syn"))

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
library(MASS)
data$Type_factor <- as.factor(data$Type)
data$Crossing_Type_factor <- as.factor(data$Crossing_Type)
typtable <- table(data$Type_factor, data$Crossing_Type_factor)
typtable

table(data$Type, data$Sex)
table(data$Crossing_Type_factor, data$Type_factor)

typtable <- table(data$Crossing_Type, data$Type)
chisq.test(typtable)
typtable

head(data$Crossing_Type_factor)
head(data$Type)
cor(typtable)
library(corrplot)
corrplot(cor(typtable))
df <- data.frame(type=data$Type, crossing=data$Crossing_Type)

data["Disparity"] <- data$L_StartX - data$R_StartX
mean(eng$Disparity)
mean(chin$Disparity)
mean(dys$Disparity)
mean(male$Disparity)
mean(female$Disparity)

cor(data$Disparity, data$R_Pupil)
data["Absolute_disparity"] <- abs(data$L_StartX - data$R_StartX)
cor(data$Absolute_disparity, data$L_Pupil)

cor(data$Disparity, data$L_Angle)
plot(data$Disparity, data$L_Angle)
