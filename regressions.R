library(dplyr)

filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
colnames(data)

L_reg <- subset(data, data$L_deltaX < 0)
nrow(L_reg)
hist(data$L_deltaX)
range(data$L_deltaX)

frac<- nrow(L_reg) / nrow(data)
frac
R_reg <- subset(data, data$R_deltaX < 0)
nrow(R_reg)
frac<- nrow(R_reg)/nrow(data)
frac

hist(data$R_deltaX)
mean(L_reg$L_deltaX)
mean(subset(data$L_deltaX, data$L_deltaX >=0))
sd(subset(data$L_deltaX, data$L_deltaX >=0))
range(L_reg$L_deltaX)
hist(L_reg$L_deltaX)
# crop out outliers
L_reg.cropped <- subset(L_reg, L_reg$L_deltaX > -100)
hist(L_reg.cropped$L_deltaX)
hist(R_reg$R_deltaX)
R_reg.cropped<- subset(R_reg, R_reg$R_deltaX > -100)
hist(R_reg.cropped$R_deltaX)
mean(R_reg$R_deltaX)


mean(subset(data$R_deltaX, data$R_deltaX >=0))
sd(subset(data$R_deltaX, data$R_deltaX >=0))
sd(L_reg$L_deltaX)
sd(R_reg$R_deltaX)
L_reg["R_Type"] <- ifelse(L_reg$R_deltaX >=0, L_reg$R_Type<-"Positive",L_reg$R_Type<-"Negative")
L_reg.positive <- subset(L_reg, R_Type == "Positive")
nrow(L_reg.positive)
L_reg.negative <-subset(L_reg, R_Type=="Negative")
nrow(L_reg.negative)
data["L_regression"] <- ifelse(data$L_deltaX >=0, data$L_regression <- "No", data$L_regression <- "Yes")
data["R_regression"] <- ifelse(data$R_deltaX >=0, data$R_regression <- "No", data$R_regression <- "Yes")
data$L_regression <- as.numeric(data$L_regression)
data$R_regression <- as.numeric(data$R_regression)
cor(data$L_regression, data$R_regression)
head(L_reg)
nrow(subset(L_reg, Group=="Chinese"))
nrow(subset(L_reg, Group=="English"))
nrow(subset(L_reg, Group=="Dyslexic"))

cor(data$L_regression, data$L_StartX)

data$ScreenSide<- ifelse(data$L_StartX<= 341 | data$R_StartX<= 341,"Left",
                         ifelse(data$L_StartX> 341& data$L_StartX< 682 | data$R_StartX> 341& data$R_StartX< 682,"Middle",
                                ifelse(data$L_StartX>= 682 | data$R_StartX>= 682,"Right",NA)))



head(data$ScreenSide)
nleft <- nrow(subset(data, data$L_regression =="Yes" & data$ScreenSide== "Left"))
nleft


nmid <- nrow(subset(data, data$L_regression == "Yes" & data$ScreenSide == "Middle"))

nright <- nrow(subset(data, data$L_regression == "Yes" & data$ScreenSide == "Right"))
nright
nmid
n_lreg <- nrow(subset(data, data$L_regression=="Yes"))
n_lreg
nleft_all <- nrow(subset(data, data$ScreenSide=="Left"))
nleft_all
nmid_all <- nrow(subset(data, data$ScreenSide=="Middle"))
nmid_all
nright_all <- nrow(subset(data, data$ScreenSide=="Right"))
nright_all
nrow(data)
fracleft <- nleft / nleft_all
fracleft
fracmid <- nmid / nmid_all
fracmid
fracright <- nright / nright_all
fracright

rnleft <- nrow(subset(data, data$R_regression =="Yes" & data$ScreenSide== "Left"))
rnmid <- nrow(subset(data, data$R_regression == "Yes" & data$ScreenSide == "Middle"))
rnright <- nrow(subset(data, data$R_regression == "Yes" & data$ScreenSide == "Right"))

rfracleft <- rnleft / nleft_all
rfracleft
rfracmid <- rnmid / nmid_all
rfracmid
rfracright <- rnright / nright_all
rfracright

rightlreg <- subset(data, data$L_regression == "Yes" & data$ScreenSide == "Right")
hist(rightlreg$L_deltaX)
leftlreg <- subset(data, data$L_regression == "Yes" & data$ScreenSide == "Left")
hist(leftlreg$L_deltaX)
sd(rightlreg$L_deltaX)
sd(leftlreg$L_deltaX)
midlreg <- subset(data, data$L_regression == "Yes" & data$ScreenSide == "Middle")
sd(midlreg$L_deltaX)

rightrreg <- subset(data, data$R_regression == "Yes" & data$ScreenSide == "Right")
sd(rightrreg$R_deltaX)
leftrreg <- subset(data, data$R_regression == "Yes" & data$ScreenSide == "Left")
sd(leftrreg$R_deltaX)
midrreg <- subset(data, data$R_regression == "Yes" & data$ScreenSide == "Middle")
sd(midrreg$R_deltaX)
data["LregNumeric"] <- ifelse(data$L_deltaX >=0, data$L_regression <- -1, data$L_regression <- 1)
data["RregNumeric"] <- ifelse(data$R_deltaX >=0, data$R_regression <- -1, data$R_regression <- 1)
cor(data$LregNumeric, data$RregNumeric)
boxplot(data$LregNumeric ~ data$ScreenSide)
boxplot(data$RregNumeric ~ data$ScreenSide)

data["XDisparity"] <- data$L_StartX - data$R_StartX
cor(data$LregNumeric, data$XDisparity)
cor(data$RregNumeric, data$XDisparity)
plot(data$LregNumeric, data$XDisparity)
data["LregFactor"] <- as.factor(data$LregNumeric)
anova(data$XDisparity , data$LregFactor)
dispnoreg <- subset(data$XDisparity, data$LregNumeric == "-1")
dispreg <- subset(data$XDisparity, data$LregNumeric =="1")
t.test(dispreg, dispnoreg)
mean(dispreg)
mean(dispnoreg)
sd(dispreg)
sd(dispnoreg)
range(data$XDisparity)
mean(data$XDisparity)
rdispnoreg <- subset(data$XDisparity, data$RregNumeric == -1)
rdispreg <- subset(data$XDisparity, data$RregNumeric == 1)
mean(rdispnoreg)
mean(rdispreg)
plot(data$RregNumeric, data$XDisparity)
t.test(rdispreg, rdispnoreg)

data["YregL"] <- ifelse(data$L_deltaY < 0, data$YregL <- -1, data$YregL <- 1)
data["YregR"] <- ifelse(data$R_deltaY < 0, data$YregR <- -1, data$YregR <- 1)
LfracY <- nrow(subset(data, YregL == -1)) / nrow(data)
LfracY
RfracY <- nrow(subset(data, YregR == -1)) / nrow(data)
RfracY
hist(data$YregL)
hist(subset(data, data$L_deltaY < 0))
mean(data$L_deltaY)
range(data$L_deltaY)
data["YDisparity"] <- data$L_StartY - data$R_StartY
cor(data$YDisparity, data$YregL)
cor(data$YDisparity, data$YregR)

data <-data %>% 
  group_by(ID) %>%
  mutate(RPrevLength = lag(R_deltaX))
colnames(lregs)
colnames(data)
data$PrevLength
hist(subset(data$PrevLength, data$PrevLength > -100 & data$PrevLength < 100))
lregs <- subset(data, data$R_regression == "Yes")
lregs.preregnum <- nrow(subset(lregs, lregs$PrevLength < 0))
lregs.preregnum
# 9897
lregs.nopreregnum <- nrow(subset(lregs, lregs$PrevLength >= 0))
lregs.nopreregnum
preforward <- subset(data, data$PrevLength >=0)
cor(preforward$L_deltaX, preforward$LregNumeric)
boxplot(preforward$L_deltaX ~ preforward$LregNumeric)
plot(preforward$LregNumeric, preforward$L_deltaX)
cor(data$L_deltaX, data$PrevLength)
head(data$L_deltaX)
head(data$PrevLength)
nasremoved <- na.omit(data)
cor(nasremoved$L_deltaX, nasremoved$PrevLength)
cor(nasremoved$LregNumeric, nasremoved$PrevLength)
plot(nasremoved$LregFactor, nasremoved$PrevLength)
forward <- subset(nasremoved, nasremoved$PrevLength >= 0 )
cor(forward$LregNumeric, forward$PrevLength)