# A script to investigate various correlations with the typology
library(ggplot2)
library(hexbin)
library(lme4)
library(dplyr)

#read in data
filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
colnames(data)
nrow(data)


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

# The data to differentiate the STD and ETD into types seperately

data["STtype"]<- ifelse(as.numeric(data$ST_duration)>0,"RW",
                            ifelse(as.numeric(data$ST_duration)<0,"LW","Syn"))
data["ETtype"]<- ifelse(as.numeric(data$ET_duration)>0,"RW",
                            ifelse(as.numeric(data$ET_duration)<0,"LW","Syn"))

# Set the sub-types into TYPE, from T1 to T8

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
colnames(data)

data <- na.omit(data)
# create the subsets by type!
data.syn <- subset(data, Type=="Syn")
data.T8 <- subset(data, Type=="T8")
data.T7 <- subset(data, Type=="T7")
data.T6 <- subset(data, Type=="T6")
data.T5 <- subset(data, Type=="T5")
data.T4 <- subset(data, Type=="T4")
data.T3 <- subset(data, Type=="T3")
data.T2 <- subset(data, Type=="T2")
data.T1 <- subset(data, Type=="T1")

hex.syn <- ggplot(data.syn, aes(L_StartX, L_StartY))
hex.syn + geom_hex()

hex.T1 <- ggplot(data.T1, aes(L_StartX, L_StartY))
hex.T1 + geom_hex()

hex.T2 <- ggplot(data.T2, aes(L_StartX, L_StartY))
hex.T2 + geom_hex()

hex.T3 <- ggplot(data.T3, aes(L_StartX, L_StartY))
hex.T3 + geom_hex()

hex.T4 <- ggplot(data.T4, aes(L_StartX, L_StartY))
hex.T4 + geom_hex()

hex.T5 <- ggplot(data.T5, aes(L_StartX, L_StartY))
hex.T5 + geom_hex()

hex.T6 <- ggplot(data.T6, aes(L_StartX, L_StartY))
hex.T6 + geom_hex()

hex.T7 <- ggplot(data.T7, aes(L_StartX, L_StartY))
hex.T7 + geom_hex()

hex.T8 <- ggplot(data.T8, aes(L_StartX, L_StartY))
hex.T8 + geom_hex()

# End hexplots!

hex.end.syn <- ggplot(data.syn, aes(L_EndX, L_EndY))
hex.end.syn + geom_hex()

hex.end.T1 <- ggplot(data.T1, aes(L_EndX, L_EndY))
hex.end.T1 + geom_hex()

hex.end.T2 <- ggplot(data.T2, aes(L_EndX, L_EndY))
hex.end.T2 + geom_hex()

hex.end.T3 <- ggplot(data.T3, aes(L_EndX, L_EndY))
hex.end.T3 + geom_hex()

hex.end.T4 <- ggplot(data.T4, aes(L_EndX, L_EndY))
hex.end.T4 + geom_hex()

hex.end.T5 <- ggplot(data.T5, aes(L_EndX, L_EndY))
hex.end.T5 + geom_hex()

hex.end.T6 <- ggplot(data.T6, aes(L_EndX, L_EndY))
hex.end.T6 + geom_hex()

hex.end.T7 <- ggplot(data.T7, aes(L_EndX, L_EndY))
hex.end.T7 + geom_hex()

hex.end.T8 <- ggplot(data.T8, aes(L_EndX, L_EndY))
hex.end.T8 + geom_hex()


#Further exlporatory analysis
nrow(data.syn)
nrow(data.T1)
nrow(data.T2)

mean(data.syn$StampStart)
sd(data.syn$StampStart)

mean(data.T1$StampStart)
mean(data.T1$StampStart)
mean(data.T2$StampStart)
mean(data.T3$StampStart)
sd(data.T1$StampStart)

mean(data.T8$StampStart)
sd(data.T8$StampStart)
mean(data.T8$StampEnd)
sd(data.T8$StampEnd)


mean(data.T7$StampStart)
sd(data.T7$StampStart)
mean(data.T7$StampEnd)
sd(data.T7$StampEnd)

mean(data.T6$StampStart)
sd(data.T6$StampStart)
mean(data.T6$StampEnd)
sd(data.T6$StampEnd)

mean(data.T5$StampStart)
sd(data.T5$StampStart)
mean(data.T5$StampEnd)
sd(data.T5$StampEnd)

mean(data.T4$StampStart)
sd(data.T4$StampStart)
mean(data.T4$StampEnd)
sd(data.T4$StampEnd)

mean(data.T3$StampStart)
sd(data.T3$StampStart)
mean(data.T3$StampEnd)
sd(data.T3$StampEnd)

mean(data.T2$StampStart)
sd(data.T2$StampStart)
mean(data.T2$StampEnd)
sd(data.T2$StampEnd)

mean(data.T1$StampStart)
sd(data.T1$StampStart)
mean(data.T1$StampEnd)
sd(data.T1$StampEnd)


boxplot(data$L_DeltaTime ~ data$Type)
data$Type <- as.factor(data$Type)

boxplot(data$L_DeltaTime ~ data$Type)
boxplot(data$R_DeltaTime ~ data$Type)
boxplot(data$L_MeanX ~ data$Type)
