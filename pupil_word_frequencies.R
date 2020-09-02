# some analysis of the effect of word frequency on pupil size
fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English.csv"
d <- read.csv(fname, sep=",")
colnames(d)

d$pupil_disparity <- d$meanP_r - d$meanP_l
d$pupil_divergence <- d$pupil_disparity - mean(d$pupil_disparity)


unique(d$onSameWord)
unique(d$glassesOrContacts)
unique(d$foreignLanguage) 
range(d$fixInGazeCount_l)
range(d$fixInGazeCount_r)
nrow(subset(d, d$fixInGazeCount_l != d$fixInGazeCount_r))
head(d$fixInGazeCount_l)
unique(d$onSameWord)
unique(d$wordUnique_l)
head(d$wordUnique_l)
head(d$word_r)
head(d$wordFreq_l)
head(d$wordLeft_ic_l)
d$wordLeft_ic_l[1:100]

d$absolute_divergence <- abs(d$pupil_divergence)
head(d$absolute_divergence)
mean(d$absolute_divergence)
mean(d$pupil_divergence)
head(d$pupil_divergence)
range(d$absolute_divergence)
hist(d$absolute_divergence)
hist(d$pupil_divergence)
range(d$meanP_l)
cl <- subset(d, d$absolute_divergence <=500)
nrow(cl) / nrow(d)
mean(cl$absolute_divergence)
cor(d$absolute_divergence, d$wordFreq_l)
cor(d$pupil_divergence, d$wordFreq_l)
cor(d$pupil_disparity, d$wordFreq_l)
cor(d$pupil_disparity, d$wordFreq_r) 
cor(d$meanP_l, d$wordFreq_l)
cor(d$meanP_r, d$wordFreq_r) 
d["normalized_wordFreq_l"] <- d$wordFreq_l / sum(d$wordFreq_l)
head(d$normalized_wordFreq_l)
cor(d$meanP_l, d$normalized_wordFreq_l) 
d["normalized_wordFreq_r"] <- d$wordFreq_r / sum(d$wordFreq_r)
head(d$normalized_wordFreq_r)
unique(d$Type)

d.syn <- subset(d, d$Type == "Syn")
d.T1 <- subset(d, d$Type == "T1")
d.T2 <- subset(d, d$Type == "T2")
d.T3 <- subset(d, d$Type == "T3")
d.T4 <- subset(d, d$Type == "T4")
d.T5 <- subset(d, d$Type == "T5")
d.T6 <- subset(d, d$Type == "T6")
d.T7 <- subset(d, d$Type == "T7")
d.T8 <- subset(d, d$Type == "T8")

mean(d.syn$wordFreq_l) #5.09425
mean(d.syn$wordFreq_r) # 5.238916

mean(d.T1$wordFreq_l) #4.966473
mean(d.T1$wordFreq_r) # 5.161413

mean(d.T2$wordFreq_l) #5.149751
mean(d.T2$wordFreq_r)# 5.356791

mean(d.T3$wordFreq_l) #5.021951
mean(d.T3$wordFreq_r) #5.186255

mean(d.T4$wordFreq_l) # 4.945029
mean(d.T4$wordFreq_r) # 5.008646

mean(d.T5$wordFreq_l) # 5.160776
mean(d.T5$wordFreq_r) #5.244028

mean(d.T6$wordFreq_l) # 5.098116
mean(d.T6$wordFreq_r) # 5.198613

mean(d.T7$wordFreq_l)#5.053839
mean(d.T7$wordFreq_r) #5.180358

mean(d.T8$wordFreq_l) # 5.168492
mean(d.T8$wordFreq_r) #5.184831

cor(d$thisSaccDuration_l, d$wordFreq_l) 
cor(d$total_duration_l, d$wordFreq_l) 
cor(d$thisSaccDuration_r, d$wordFreq_r)
cor(d$total_duration_r, d$wordFreq_r) 
head(d$wordAt_l)
head(d$word_l)
  

fname_norvig <-"/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English_norvig.csv"
norvig <- read.csv(fname_norvig, sep=",")
colnames(norvig)
head(norvig)
suml <- sum(is.na(norvig$norvig_wordFreq_l))
suml
norvig$normalized_norvig_wordfreq_l <- norvig$norvig_wordFreq_l / sum(na.omit(norvig$norvig_wordFreq_l))
norvig$normalized_norvig_wordfreq_r <- norvig$norvig_wordFreq_r / sum(na.omit(norvig$norvig_wordFreq_r))
head(norvig$normalized_norvig_wordfreq_l)
head(norvig$norvig_wordFreq_l)
sum(na.omit(norvig$norvig_wordFreq_l))
nofreq <- subset(norvig, is.na(norvig$norvig_wordFreq_l))
head(nofreq$word_l)
nrow(nofreq)
nofreq$word_l[1:200]
norvig$normalized_wordfreq_l <- norvig$wordFreq_l / sum(norvig$wordFreq_l)
norvig$normalized_wordfreq_r <- norvig$wordFreq_r / sum(norvig$wordFreq_r)

nfull <- subset(norvig, !is.na(norvig$normalized_norvig_wordfreq_l))
nfull <- subset(nfull, !is.na(nfull$normalized_wordfreq_l))
cor(nfull$normalized_norvig_wordfreq_l, nfull$normalized_wordfreq_l) 
nfullright <- subset(norvig, !is.na(norvig$normalized_norvig_wordfreq_r))
nfullright <- subset(nfullright, !is.na(nfullright$normalized_wordfreq_r))
cor(nfullright$normalized_norvig_wordfreq_r, nfullright$normalized_wordfreq_r)  

head(nfull$normalized_norvig_wordfreq_l)
head(nfull$normalized_wordfreq_l)
nrow(subset(nfull, is.na(nfull$normalized_norvig_wordfreq_l)))

cor(aug$log_wordFreq_l, aug$thisSaccDuration_l)
nrow(subset(aug, is.na(aug$log_wordFreq_l)))

aug$preFixation_duration_l <- aug$startTime_l - aug$preSaccEndTime_l
head(aug$preFixation_duration_l)
aug$postFixation_duration_l <- aug$nextSaccStartTime_l - aug$endTime_l
head(aug$postFixation_duration_l)
hist(aug$postFixation_duration_l)
range(aug$postFixation_duration_l)
aug$preSaccDuration_l <- aug$preSaccEndTime_l - aug$preSaccStartTime_l
head(aug$preSaccDuration_l)
hist(aug$preSaccDuration_l)
cor(aug$preSaccDuration_l, aug$wordFreq_l)
cor(aug$preSaccDuration_l, aug$log_wordFreq_l)
hist(aug$log_wordFreq_l)