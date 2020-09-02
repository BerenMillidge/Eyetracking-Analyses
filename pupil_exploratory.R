# exploratory analysis of initial pupil data
library(ggplot2)

d = read.csv("EnglishTypical.csv", sep = ",")
head(d)
plot(d$L_Pupil, d$R_Pupil)

hist(d$L_Pupil)
hist(d$R_Pupil)
mean(d$L_Pupil)
mean(d$R_Pupil)
var(d$L_Pupil)
var(d$R_Pupil)
sd(d$L_Pupil)
sd(d$R_Pupil)
plot(d$L_Pupil[1:10000], d$R_Pupil[1:10000])
boxplot(d$L_Pupil~ d$Sex)
boxplot(d$R_Pupil ~ d$Sex)
# let's try colouring by sex
ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour=Sex)) + geom_point(size=2) + theme_classic()
frp <- d$R_Pupil[d$Sex == "Female"]
flp <- d$L_Pupil[d$Sex == "Female"]
plot(flp, frp)
# so the female distibution does again show the two peaks
# and what about the male
mlp <- d$L_Pupil[d$Sex == "Male"]
mrp <- d$R_Pupil[d$Sex == "Male"]
plot(mlp, mrp)
#  the male version is more split,
# relation to mean x position
mlx <- mean(d$L_MeanX)
mrx <- mean(d$R_MeanX)e
plot(d$L_Pupil, d$R_Pupil, col= ifelse(d$L_Pupil <= 4*mlx, 'red', 'blue'))
mly <- mean(d$L_MeanY)
mry <- mean(d$R_MeanY)
plot(d$L_Pupil, d$R_Pupil, col=ifelse(d$L_Pupil <= 6*mly, 'red','blue'))

rla = range(d$L_Angle)
rla 
rra <- range(d$R_Angle)
rra

mla <- mean(d$L_Angle)
mra <- mean(d$R_Angle)
plot(d$L_Pupil, d$R_Pupil, col=ifelse(d$L_Pupil <= 20* mla, 'red', 'blue'))
plot(flp, frp)
hist(flp)
hist(frp)

hist(mlp)
hist(mrp)
flpout <- flp[flp > 2000]
flpout
hist(flpout)
frpout <- frp[frp > 2000]
hist(frpout)
ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour=ID)) + geom_point(size=2) + theme_classic()

boxplot(frpout ~ d$Age[d$Sex=="Female" && d$R_Pupil > 2000])
ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour= paste0(Age,Sex))) + geom_point(size=2) + theme_classic()


ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour=Age)) + geom_point(size=2) + theme_classic()

plot(flp, frp, col=as.factor(d$Age[d$Sex=="Female"]), legend = levels(d$Age))
legend()
df <- data.frame("flp" = flp, "frp" = frp)
#df$flp <- d$L_Pupil[d$Sex=="Female"]
#df$frp <- d$R_Pupil[d$Sex=="Female"]
str(df)
#ggplot(df,aes(x=d$L_Pupil[d$Sex=="Female"], y=d$R_Pupil[d$Sex=="Female"], colour=d$Age[d$Sex=="Female"])) +  geom_point(size=2) + theme_classic()
ggplot(df,aes(x=flp, y=frp, colour=as.factor(d$Age[d$Sex=="Female"]))) +  geom_point(size=2) + theme_classic()