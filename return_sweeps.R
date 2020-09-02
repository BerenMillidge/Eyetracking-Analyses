# look just at the statistical properties of the return sweeps

library(ggplot2)

filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
nrow(d)
colnames(data)

mean(data$L_Pupil)
mean(data$R_Pupil)

cor(data$L_Pupil, data$L_Angle)
cor(data$R_Pupil, data$R_Angle)
cor(data$L_Pupil, data$R_Angle)
cor(data$R_Pupil, data$L_Angle)

back <- subset(data, data$L_deltaX < 0 || data$R_deltaX < 0)
mean(back$L_deltaX)
range(back$L_deltaX)

data <- subset(data, data$L_deltaX <= -500 & data$R_deltaX <=-500)
hist(ret$L_deltaX)
hist(ret$R_deltaX)
nrow(ret)
data$ScreenSide<- ifelse(data$L_StartX<= 341 | data$R_StartX<= 341,"Left",
                         ifelse(data$L_StartX> 341& data$L_StartX< 682 | data$R_StartX> 341& data$R_StartX< 682,"Middle",
                                ifelse(data$L_StartX>= 682 | data$R_StartX>= 682,"Right",NA)))

data["StampStart"]<- data$R_StartTime - data$L_StartTime
data["StampEnd"]<- data$R_EndTime - data$L_EndTime


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

nrow(data)
d <- subset(data, data$L_deltaX <= -500 & data$R_deltaX <=-500)

f <- nrow(data)
data.syn <- subset(data, Type=="Syn")
data.T8 <- subset(data, Type=="T8")
data.T7 <- subset(data, Type=="T7")
data.T6 <- subset(data, Type=="T6")
data.T5 <- subset(data, Type=="T5")
data.T4 <- subset(data, Type=="T4")
data.T3 <- subset(data, Type=="T3")
data.T2 <- subset(data, Type=="T2")
data.T1 <- subset(data, Type=="T1")

fd <- nrow(d)
d.Syn <- subset(d, Type=="Syn")
d.T8 <- subset(d, Type=="T8")
d.T7 <- subset(d, Type=="T7")
d.T6 <- subset(d, Type=="T6")
d.T5 <- subset(d, Type=="T5")
d.T4 <- subset(d, Type=="T4")
d.T3 <- subset(d, Type=="T3")
d.T2 <- subset(d, Type=="T2")
d.T1 <- subset(d, Type=="T1")

nrow(data.syn) / f # 0.5216756
nrow(data.T8) / f # 0.01055648
nrow(data.T7) / f  # 0.01719385
nrow(data.T6) / f # 0.0119835
nrow(data.T5) / f #0.02028863
nrow(data.T4) / f  # 0.02135683
nrow(data.T3) / f #0.1817576
nrow(data.T2) / f #0.04556153
nrow(data.T1) / f #0.06167452

nrow(d.Syn) / fd # 0.3798
nrow(d.T8) / fd # 0.0444715
nrow(d.T7) / fd # 0.01802885
nrow(d.T6) / fd # 0.2259615
nrow(d.T5) / fd # 0.02403846
nrow(d.T4) / fd # 0.0168292
nrow(d.T3) / fd #0.1971154
nrow(d.T2) / fd # 0.06850962
nrow(d.T1) / fd #0.02524038

all <- c(nrow(data.syn) / f,nrow(data.T8) / f,nrow(data.T7) / f,
         nrow(data.T6) / f,nrow(data.T5) / f, nrow(data.T4) / f,
         nrow(data.T3) / f,nrow(data.T2) / f, nrow(data.T1) / f)

return <- c(nrow(d.Syn) / fd,nrow(d.T8) / fd,nrow(d.T7) / fd,
            nrow(d.T6) / fd,nrow(d.T5) / fd, nrow(d.T4) / fd,
            nrow(d.T3) / fd,nrow(d.T2) / fd, nrow(d.T1) / fd)

plot(all, type="l", col="red")
lines(return, type='l', col='blue')
xlab('Type')
ylab('Frequency')