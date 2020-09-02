
filename <- "/home/beren/work/phd/eyetracking/data/English_Chinese_Dyslexics.csv"
data <- read.csv(filename, sep="\t")
colnames(data)
nrow(data)
data["Mean_Deltatime"] <- (data$L_DeltaTime + data$R_DeltaTime) / 2
hist(data$Mean_Deltatime)
hist(data$L_DeltaTime)
hist(data$R_DeltaTime)

data["Fixation_Length"] <- sqrt((data$L_deltaX - data$R_deltaX)^2 + (data$L_deltaY - data$R_deltaY)^2 )
hist(data$Fixation_Length)
mean(data$Fixation_Length)
sd(data$Fixation_Length)
#test for normality here
ks.test(data$Fixation_Length, pnorm)
shapiro.test(data$Fixation_Length[0:4999])
length(data$Fixation_Length)
qqnorm(data$Fixation_Length)

sdlen <- sd(data$Fixation_Length)
data.trunc <- subset(data, Fixation_Length < 2 *sdlen)
nrow(data.trunc)
hist(data.trunc$Fixation_Length)
to_filename <- "/home/beren/work/phd/eyetracking/data/FixationLengths.csv"
lengthdf <- data.frame("Lengths" = data$Fixation_Length)
colnames(lengthdf)
nrow(lengthdf)
write.csv(lengthdf, to_filename)
