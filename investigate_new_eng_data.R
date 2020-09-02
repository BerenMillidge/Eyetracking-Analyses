#Exploratory investigation of new dataset

getwd()

fname <- "/home/beren/work/phd/eyetracking/Tokens_datafile.csv"
d <- read.csv(fname, sep="\t")
colnames(d)
head(d)
nrow(d)
range(unique(d$Trial.Number))

fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/f_prepared_English.csv"
d <- read.csv(fname, sep=",")
colnames(d)

fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/f_prepared_EnglishDyslexia.csv"
d <- read.csv(fname, sep=",")
colnames(d)
head(d)

unique(d$gender)
unique(d$language)
range(d$lateralityQuotient)
unique(d$glassesOrContacts)
unique(d$HandledBy)
unique(d$handedness)
range(d$handednessValue)


range(d$preSaccStartTime_l)
range(d$preSaccStartX_l)
range(d$nextSaccStartTime_l)
range(d$nextSaccStartX_l)
head(d$preSaccEndX_l)
tail(d$nextSaccEndX_l)
head(d$startTime_l)
head(d$preSaccStartTime_l)
head(d$nextSaccStartTime_l)
head(d$endTime_l)
head(d$x_l_travel)
ls <- d[1:2,]
ls$preSaccDiff <- ls$preSaccEndX_l - ls$preSaccStartX_l
ls$endSaccDiff <- ls$nextSaccEndX_l - ls$nextSaccStartX_l
colnames(ls)
head(ls$preSaccDiff)
head(ls$x_l_travel)
ls$saccDiff <- ls$nextSaccStartX_l - ls$preSaccEndX_l
head(ls$saccDiff)
head(ls$meanX_l)
head(ls$meanX_l_corrected)
ls$myMean <- (ls$preSaccEndX_l + ls$nextSaccStartX_l) / 2
head(ls$myMean)
head(ls$preSaccEndX_l)
head(ls$nextSaccStartX_l)
head(d$disparity)
head(ls$X)
d$thisSaccStartX_l <- d$preSaccEndX_l
d$thisSaccEndX_l <- d$nextSaccStartX_l
d$thisSaccStartX_r <- d$preSaccEndX_r
d$thisSaccEndX_r <- d$nextSaccEndX_r
d$thisSaccDuration_l <- d$endTime_l - d$startTime_l
d$thisSaccDuration_r <- d$endTime_r - d$startTime_r

d$total_travel_distance_l <- d$nextSaccEndX_l - d$preSaccStartTime_l
d$total_travel_distance_r <- d$nextSaccEndX_r - d$preSaccEndX_r
d$total_duration_l <- d$nextSaccEndTime_l - d$preSaccStartTime_l
d$total_duration_r <- d$nextSaccEndTime_r - d$preSaccStartTime_r

d$thisSaccSpeed_l <- d$x_l_travel / d$thisSaccDuration_l
d$thisSaccSpeed_r <- d$x_r_travel / d$thisSaccDuration_r


range(d$startTimeDisparity)
d$startTimeDisparity[1:100]
d$endTimeDisparity[1:100]

d$ScreenSide<- ifelse(d$thisSaccStartX_l<= 341 | d$thisSaccStartX_r<= 341,"Left",
                         ifelse(d$thisSaccStartX_l> 341& d$thisSaccStartX_l< 682 | d$thisSaccStartX_r> 341& d$thisSaccStartX_r< 682,"Middle",
                                ifelse(d$thisSaccStartX_l>= 682 | d$thisSaccStartX_r>= 682,"Right",NA)))

head(d$ScreenSide)
d$startTimeDiff <- d$startTime_r - d$startTime_l
d$startTimeDiff[1:100]
d$startTimeDisparity[1:100]

enddisp <- d$endTime_r - d$endTime_l
d$endTimeDisparity[1:100]
enddisp[1:100]
data$ST

d["STtype"]<- ifelse(as.numeric(d$startTimeDisparity)>0,"RW",
                        ifelse(as.numeric(d$startTimeDisparity)<0,"LW","Syn"))
d["ETtype"]<- ifelse(as.numeric(d$endTimeDisparity)>0,"RW",
                        ifelse(as.numeric(d$endTimeDisparity)<0,"LW","Syn"))

head(d$STtype)
head(d$ETtype)

d["Type"]<- ifelse(d$STtype=="Syn"&d$ETtype=="Syn",d$Type<- "Syn",
                      ifelse(d$STtype=="RW"&d$ETtype=="RW",d$Type<- "T8",
                             ifelse(d$STtype=="RW"&d$ETtype=="LW",d$Type<- "T7",
                                    ifelse(d$STtype=="LW"&d$ETtype=="LW",d$Type<-"T4",
                                           ifelse(d$STtype=="LW"&d$ETtype=="RW",d$Type<- "T5",
                                                  ifelse(d$STtype=="Syn"&d$ETtype=="LW",d$Type<- "T1",
                                                         ifelse(d$STtype=="Syn"&d$ETtype=="RW",d$Type<- "T2",
                                                                ifelse(d$STtype=="LW"&d$ETtype=="Syn",d$Type<- "T3",
                                                                       ifelse(d$STtype=="RW"&d$ETtype=="Syn",d$Type<- "T6",NA
                                                                       )))))))))
head(d$Type)
colnames(d)

# okay, let's recreate the divergence plot
start: syn, end: syn -> syn
start: Syn, end: L -> T1
start: Syn, end: R -> T2
start: L, end: Syn -> T3
start: L, End: L -> T4
start: L, End: R -> T5
start: R, end: Syn -> T6
start: R, end: L -> T7
start: R, end: R -> T8


# okay.. got the augmented dataframe now let's save it.
savename <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English.csv"
write.csv(d, savename)

# let's create the typology hexplots
d.syn <- subset(d, d$Type == "Syn")
d.T1 <- subset(d, d$Type == "T1")
d.T2 <- subset(d, d$Type == "T2")
d.T3 <- subset(d, d$Type == "T3")
d.T4 <- subset(d, d$Type == "T4")
d.T5 <- subset(d, d$Type == "T5")
d.T6 <- subset(d, d$Type == "T6")
d.T7 <- subset(d, d$Type == "T7")
d.T8 <- subset(d, d$Type == "T8")

hex <- ggplot(d.syn, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T1, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T2, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T3, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T4, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T5, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T6, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T7, aes(meanX_l, meanY_l))
hex + geom_hex()

hex <- ggplot(d.T8, aes(meanX_l, meanY_l))
hex + geom_hex()


# just looking at some of the other vriables
head(d$lateralityQuotient)
range(d$lateralityQuotient) 
range(d$handednessValue)
range(unique(d$handednessValue))
d$handednessValue[1:100] 
d$lateralityQuotient[1:100]
d$dominanceCheck[1:100] 
d$handedness[1:100] 
lefties <- subset(d, d$handedness == "L")
righties <- subset(d, d$handedness == "R")
nrow(lefties) / nrow(d)
nrow(righties) / nrow(d)
unique(d$handedness)
# so 82% righties is a fairly reasonable.
unique(d$HandledBy)
monica <- subset(d, d$HandledBy=="Monica")
clare <- subset(d, d$HandledBy=="Clare")
nrow(monica)
nrow(clare)
mean(monica$meanP_l)
mean(clare$meanP_l)
mean(monica$meanP_r)
mean(clare$meanP_r)
unique(d$isGoodPair)
unique(d$calID_l)
unique(d$calID_r)
unique(d$displayOn)
unique(d$oneID)

nrow(subset(d, d$isGoodPair == "True"))
nrow(subset(d, d$isGoodPair == "False"))
aug_fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English_norvig.csv"
aug <- read.csv(aug_fname, sep=',')
colnames(aug)
aug$normalized_norvig_wordfreq_l <- aug$norvig_wordFreq_l / sum(na.omit(aug$norvig_wordFreq_l))
aug$normalized_norvig_wordfreq_r <- aug$norvig_wordFreq_r / sum(na.omit(aug$norvig_wordFreq_r))
aug$normalized_norvig_wordfreq_l

aug$normalized_wordFreq_l <- aug$wordFreq_l / sum(aug$wordFreq_l)
aug$normalized_wordFreq_r <- aug$wordFreq_r / sum(aug$wordFreq_r)

# now for the log frequencies
aug$log_wordFreq_l <- log(aug$normalized_wordFreq_l)
head(aug$log_wordFreq_l)
aug$log_wordFreq_r <- log(aug$normalized_wordFreq_r)
aug$log_norvig_wordFreq_l <- log(aug$normalized_norvig_wordfreq_l)
aug$log_norvig_wordFreq_r <- log(aug$normalized_norvig_wordfreq_r)

colnames(aug)
# now write to csv
write.csv(aug, "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English_2.csv")


# test the bigram data
bigram_fname <- "/home/beren/work/phd/eyetracking/test_bigrams.csv"
bigram <- read.csv(bigram_fname, sep=",")
head(bigram)
colnames(bigram)
bigram$bigram_frequencies_l[1:100]
nrow(subset(bigram, is.na(bigram$bigram_frequencies_l))) /nrow(bigram)
bigram$word_l[1:100] 
