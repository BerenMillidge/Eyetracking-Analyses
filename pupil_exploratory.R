# okay, the point of this script is to try to figure out what is going on briefly with the pupil data, and how it is weird
# so let's do that!
library(ggplot2)

d = read.csv("EnglishTypical.csv", sep = ",")
# plot the original graph
head(d)
plot(d$L_Pupil, d$R_Pupil)

# okay, let's check the histograms
hist(d$L_Pupil)
hist(d$R_Pupil)
# huh! so they are both normal with tails, but the right pupil seems to have a whole lot more variance generally, which i sweird
# as it's the opposite of what I would be expecting
mean(d$L_Pupil)
mean(d$R_Pupil)
# so means are effectively the same!
var(d$L_Pupil)
var(d$R_Pupil)
# yeah, the R pupil has significantly more variance. which is actually a bit weird because it's the opposite of what the graph finds
sd(d$L_Pupil)
sd(d$R_Pupil)
# a significantly higher standard deviationfor the right pupil
plot(d$L_Pupil[1:10000], d$R_Pupil[1:10000])
# so there is definitely a two line pattern going on here... perhaps its split by gender orsomething
boxplot(d$L_Pupil~ d$Sex)
boxplot(d$R_Pupil ~ d$Sex)
# let's try colouring by sex
ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour=Sex)) + geom_point(size=2) + theme_classic()
# wait a second... it does seem to be a sex difference. and quite robust... males only have the one low one while females have the other
# that is erally really strnge... not sure what is going on there! why would females have the two points... that's really strange... not sure what is going to go onthere!
#let's split this out to plot separately
frp <- d$R_Pupil[d$Sex == "Female"]
flp <- d$L_Pupil[d$Sex == "Female"]
plot(flp, frp)
# so the female distibution does again show the two peaks
# and what about the male
mlp <- d$L_Pupil[d$Sex == "Male"]
mrp <- d$R_Pupil[d$Sex == "Male"]
plot(mlp, mrp)
# so the male version is more split, which is just really really strange... notsure what is going on there? females have more sensitive pupils... but why do females show the doble pattern?
# this difference seems quite miportant overall... so who nkows?

# I wonder if there isa relation to mean x position
# let's just split xposition based on the mean
mlx <- mean(d$L_MeanX)
mrx <- mean(d$R_MeanX)
# let's just look at both and colour the split - just on the l xposition seems reaosnable
plot(d$L_Pupil, d$R_Pupil, col= ifelse(d$L_Pupil <= 4*mlx, 'red', 'blue'))
# right.. so the split is almost entirely for ones greater than the mean# let's try splitting it out!
# so it doesn't seem very obviously based on the x position... which is weird... who knows?
# definitely no effect of x distance... what about angel
# first y differenec
mly <- mean(d$L_MeanY)
mry <- mean(d$R_MeanY)
plot(d$L_Pupil, d$R_Pupil, col=ifelse(d$L_Pupil <= 6*mly, 'red','blue'))
# doesn't necessary look like it splits out in this way... but who knwos!
# so this doesn't seemto have too big an effect, which makes sense... but who knows really?
# okay, yeah, this has no effect... but what about angle?
rla = range(d$L_Angle)
rla # yeah this make ssense... I dno' think the man will necessarily be the best measure here... at all!
rra <- range(d$R_Angle)
rra
# exactly the same range, which makes sense, although it's weird the histograms overall are different
mla <- mean(d$L_Angle)
mra <- mean(d$R_Angle)
plot(d$L_Pupil, d$R_Pupil, col=ifelse(d$L_Pupil <= 20* mla, 'red', 'blue'))
# see if the angle has any kind of effect here?
# so that's interesting... there doensn't seem to be any particulalry obvious relation here that I can see between them
# yeah no relatino here. I do wnoder what's different in the female data
plot(flp, frp)
# so the females show the double trend... I wonder what if any obvious points there are going to be here
hist(flp)
hist(frp)

hist(mlp)
hist(mrp)
# so the issue seems to be that with women, there's a signifiacntly greater tail of large pupil dilatoins... presumably?
# but why? what is causing this issue?
# the cutoff seems to be around 2200 so let's cut it there
flpout <- flp[flp > 2000]
flpout
hist(flpout)
# so it's still a sharp ish decrease... but it does seem almost multimodel?
frpout <- frp[frp > 2000]
hist(frpout)
# so the issue seems almost entirely to be with females' right eyes... which is justrally strange!
# I'm really not sure how this stuff actually ends up working... it probably doesn't
# so the issue does really seem to be that females tend to have large right eye pupil sizes... but why sometimes... it's weird!?
# why is the case just for females, and why does it result in the second distinctoin there anyhow?
# also I'm pretty sure they are not in the plot right?
ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour=ID)) + geom_point(size=2) + theme_classic()
# so let's see if there are any interesting group distinctions!?
#could also try including age... see if it has any sort of reasonable effect
boxplot(frpout ~ d$Age[d$Sex=="Female" && d$R_Pupil > 2000])# not sure why this isn't working... dagnabbit
#anyhow
ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour= paste0(Age,Sex))) + geom_point(size=2) + theme_classic()


ggplot(d, aes(x=L_Pupil, y=R_Pupil, colour=Age)) + geom_point(size=2) + theme_classic()
# not totally sure what'sgong on, but does seem to have some serious relation to age... I might try it just with the female data

plot(flp, frp, col=as.factor(d$Age[d$Sex=="Female"]), legend = levels(d$Age))
legend()
df <- data.frame("flp" = flp, "frp" = frp)
#df$flp <- d$L_Pupil[d$Sex=="Female"]
#df$frp <- d$R_Pupil[d$Sex=="Female"]
str(df)
#ggplot(df,aes(x=d$L_Pupil[d$Sex=="Female"], y=d$R_Pupil[d$Sex=="Female"], colour=d$Age[d$Sex=="Female"])) +  geom_point(size=2) + theme_classic()
# so in the female examplesthere are some  clear age patterns, but I don't know what htey are because it helpfully doesn't provide labels!
# it was so annoying getting to this that it's just ridiculosu... dagnabbit!
ggplot(df,aes(x=flp, y=frp, colour=as.factor(d$Age[d$Sex=="Female"]))) +  geom_point(size=2) + theme_classic()
# so there definitely seems to be some kind of age sepaaration going on here... but god knows what iti s!