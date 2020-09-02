


#effects of screen side on pupil sizes
#left side:
#  left pupil: mean: 1180.09, std: 370.8977
#right pupil: mean: 1169.921, std: 463.9138
#middle:
#  left pupil: mean: 1148.941, std: 359.1006
#right pupil: mean: 1179.610, std: 451.4678
#right:
##  left pupil: mean: 1105.671, std: 341.7702
#right pupil: mean: 1183.172, std: 433.778

library(dplyr)
library(ggplot2)


fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English_2.csv"
d <- read.csv(fname, sep=",")
colnames(d)

mean(d$meanP_l) # 1151.299
mean(d$meanP_r)# 1176.588
sd(d$meanP_l) #  361.1135
sd(d$meanP_r) # 452.0393

mean(d$pupil_disparity) # 25.289
sd(d$pupil_disparity) # 240.3743


cor(d.withhandedness$pupil_disparity, d.withhandedness$handednessValue) # -0.11623.


lefteyes <- subset(d, d$eyePreferred == "L")
righteyes <-subset(d, d$eyePreferred == "R")

mean(d$meanP_l) # 1151.299
mean(d$meanP_r)# 1176.588


lefteyes <- subset(d, d$eyePreferred == "L")
righteyes <-subset(d, d$eyePreferred == "R")
lefties <- subset(d, d$handedness == "L")
righties <- subset(d, d$handedness == "R")

mean(lefteyes$meanP_l) # 1195.546
mean(lefteyes$meanP_r) # 1293.163
mean(righteyes$meanP_l) # 1149.953
mean(righteyes$meanP_r) # 1137.801

sd(lefteyes$meanP_l) # 421.7412
sd(lefteyes$meanP_r) # 609.9321
sd(righteyes$meanP_l) # 337.0896
sd(righteyes$meanP_r) # 346.5546
t.test(lefteyes$meanP_l, lefteyes$meanP_r) # t = -27.757, p = 0
t.test(righteyes$meanP_l, righteyes$meanP_r) # t = 7.044, p = 1.865e-12 #



mean(lefties$meanP_l) # 865.17
mean(lefties$meanP_r) # 937.8086
mean(righties$meanP_l) # 1173.073
mean(righties$meanP_r) # 1187.183
unique(d$gender)
male <- subset(d, d$gender == "M")
female <- subset(d, d$gender == "F")
nrow(male) / nrow(d)
nrow(female) / nrow(d)
mean(male$meanP_l) # 1125.218
mean(male$meanP_r) # 1100.882
mean(female$meanP_l) # 1182.052
mean(female$meanP_r) # 1258.473

nogender <- subset(d, d$gender == "N")
nrow(nogender)
mean(nogender$meanP_l) # 1112.829
sd(nogender$meanP_l) # 131.5803
mean(nogender$meanP_r) # 1069.193
sd(nogender$meanP_r) # 139.4148


nrow(subset(lefties, lefties$glassesOrContacts != "N")) / nrow(lefties) # 0.326243
nrow(subset(righties, righties$glassesOrContacts != "N")) / nrow(righties) # 0.2881965


nrow(subset(lefties, lefties$glassesOrContacts == "contacts")) / nrow(lefties) # 0.326243
nrow(subset(righties, righties$glassesOrContacts == "contacts")) / nrow(righties) # 0.2881965

unique(d$HandledBy)
nrow(subset(lefties, lefties$HandledBy == "Clare")) / nrow(lefties)#0.7012
nrow(subset(righties, righties$HandledBy == "Clare")) / nrow(righties) #0.5777907
leftclare <- subset(lefties, lefties$HandledBy == "Clare")
leftmonica <- subset(lefties, lefties$HandledBy == "Monica")
mean(leftclare$meanP_l) #738.9853 
mean(leftmonica$meanP_l) #  1161.471
unique(leftclare$subjectID)
mean(leftclare$normalized_pupil_disparity) # 102.211
mean(leftmonica$normalized_pupil_disparity)#-81
rightclare <- subset(righties, righties$HandledBy == "Clare")
rightmonica <- subset(righties, righties$HandledBy == "Monica")
mean(rightclare$meanP_l)  # 1197.025
mean(rightmonica$meanP_l) #1140.294
mean(rightclare$normalized_pupil_disparity) # 30.8749
mean(rightmonica$normalized_pupil_disparity) # -68.729
clare <-subset(d, d$HandledBy == "Clare")
monica <- subset(d, d$HandledBy == "Monica")
mean(clare$meanP_l) # 1163.177
mean(clare$meanP_r) # 1221.549
mean(monica$meanP_l) # 1141.777
mean(monica$meanP_r) # 1097.423... 
mean(clare$normalized_pupil_disparity)# 33.08
mean(monica$normalized_pupil_disparity) #-69.644
nrow(clare)
nrow(monica)


unique(d$eyePreferred)
clare.lefteyed <- subset(clare, clare$eyePreferred == "L")
clare.righteyed <- subset(clare, clare$eyePreferred == "R")
clare.eithereyed <- subset(clare, clare$earPrephered == "Either")
monica.lefteyed <- subset(monica, monica$eyePreferred == "L")
monica.righteyed <- subset(monica, monica$eyePreferred == "R")
monica.eithereyed <- subset(monica, monica$eyePreferred == "Either")
nrow(clare.lefteyed) / nrow(clare) #0.3421
nrow(clare.righteyed) / nrow(clare) # 0.4782882
nrow(clare.eithereyed) / nrow(clare) # 0.160669
nrow(monica.lefteyed) / nrow(monica) # 0.2591226
nrow(monica.righteyed) / nrow(monica) # 0.6594718
nrow(monica.eithereyed) / nrow(monica) # 0.0814
mean(clare.lefteyed$meanP_l) # 1195.195
mean(clare.lefteyed$meanP_r) # 1382.763
mean(clare.righteyed$meanP_l) # 1172.695
mean(clare.righteyed$meanP_r) # 1172.995 
mean(clare.eithereyed$meanP_l) # 1248.607
mean(clare.eithereyed$meanP_r) # 1224.863
sd(clare$meanP_l) # 397.8723
sd(clare$meanP_r) # 516.4014 

mean(monica.lefteyed$meanP_l) # 1196.279
mean(monica.lefteyed$meanP_r) # 1106.001 
mean(monica.righteyed$meanP_l) # 1123.936
mean(monica.righteyed$meanP_r) # 1097.537
mean(monica.eithereyed$meanP_l) # 1112.829
mean(monica.eithereyed$meanP_r) # 1069.193

sd(monica$meanP_l) # 308.0755
sd(monica$meanP_r) # 333.7801 

nrow(lefties)
head(lefties$han)
nrow(righties)
mean(lefties$normalized_pupil_disparity)  # 47.24514
mean(righties$normalized_pupil_disparity) #-11.17917 
lefties.lefteyed <-  subset(lefties, lefties$eyePreferred =="L")
lefties.righteyed <-subset(lefties, lefties$eyePreferred == "R")
righties.lefteyed <-  subset(righties,  righties$eyePreferred =="L")
righties.righteyed <-subset(righties, righties$eyePreferred == "R")


nrow(lefties.righteyed)#sono1lefteyedrighties1/
mean(lefties.lefteyed$normalized_pupil_disparity)#47.24
mean(lefties.righteyed$normalized_pupil_disparity)#an
mean(righties.lefteyed$normalized_pupil_disparity)# 82.715
mean(righties.righteyed$normalized_pupil_disparity)#-37.4429
#itdoesappearthateverythigisfie there...toehoest...sothat'sgood1

hist(lefties$meanP_l)
hist(righties$meanP_l)

unique(d$glassesOrContacts)

cor(d$meanP_l, d$meanX_l)
nrow(lefteyes)

lefteyes.left <- subset(lefteyes, lefteyes$ScreenSide == "Left")
lefteyes.middle <- subset(lefteyes, lefteyes$ScreenSide == "Middle")
lefteyes.right <- subset(lefteyes, lefteyes$ScreenSide == "Right")

nrow(lefteyes.left)

mean(lefteyes.left$meanP_l) # 1229.584
mean(lefteyes.left$meanP_r) # 1299.439

mean(lefteyes.middle$meanP_l) # 1188.975
mean(lefteyes.middle$meanP_r) # 1293.738

mean(lefteyes.right$meanP_l) #1149.414
mean(lefteyes.right$meanP_r) # 1281.874

mean(lefteyes.left$normalized_pupil_disparity) # 44.45694
mean(lefteyes.middle$normalized_pupil_disparity) # 79.47328
mean(lefteyes.right$normalized_pupil_disparity) # 107.1704

boxplot(lefteyes$meanP_l ~ lefteyes$ScreenSide)

righteyes.left <- subset(righteyes, righteyes$ScreenSide == "Left")
righteyes.middle <- subset(righteyes, righteyes$ScreenSide == "Middle")
righteyes.right <- subset(righteyes, righteyes$ScreenSide == "Right")

mean(righteyes.left$meanP_l) # 1177.838
mean(righteyes.left$meanP_r) # 1129.806

mean(righteyes.middle$meanP_l) # 1147.429
mean(righteyes.middle$meanP_r) # 1140.009

mean(righteyes.right$meanP_l) #1104.09
mean(righteyes.right$meanP_r) # 1148.383

mean(righteyes.left$normalized_pupil_disparity) # -73.32
mean(righteyes.middle$normalized_pupil_disparity) # -33.00996
mean(righteyes.right$normalized_pupil_disparity) # 19.00364

d$crossed_start <-d$preSaccEndX_l > d$preSaccEndX_r
head(d$crossed_start) 
d$crossed_end  <- d$nextSaccStartX_l  > d$nextSaccStartX_r
head(d$crossed_end)

cor(as.numeric(d$crossed_end), as.numeric(d$crossed_start))# 0.623444

d$pupil_disparity <- d$meanP_r -  d$meanP_l
d$normalized_pupil_disparity <- d$pupil_disparity - mean(d$pupil_disparity)
head(d$normalized_pupil_disparity)

mean(lefteyes$normalized_pupil_disparity) # 72.32689
mean(righteyes$normalized_pupil_disparity) # -37.44249
sd(lefteyes$normalized_pupil_disparity) #299.716
sd(righteyes$normalized_pupil_disparity)  # 208.4954

mean(d$pupil_disparity)

unique(d$onSameWord)
d.sameword <- subset(d, d$onSameWord == "True")
d.diffword <- subset(d, d$onSameWord == "False")
nrow(d.sameword)
nrow(d.diffword)

lefteyes <- subset(monica, monica$eyePreferred=="L")
righteyes <- subset(monica, monica$eyePreferred  ==  "R")

lefteyes.sameword <- subset(lefteyes, lefteyes$onSameWord == "True")
lefteyes.diffword <- subset(lefteyes, lefteyes$onSameWord == "False")
righteyes.sameword <- subset(righteyes, righteyes$onSameWord == "True")
righteyes.diffword <- subset(righteyes, righteyes$onSameWord == "False")

lefteyes.crossed <- subset(lefteyes,lefteyes$crossed == "TRUE")
lefteyes.uncrossed <- subset(lefteyes, lefteyes$crossed == "FALSE")
righteyes.crossed <- subset(righteyes, righteyes$crossed == "TRUE")
righteyes.uncrossed <- subset(righteyes, righteyes$crossed == "FALSE")

d$crossed <- d$crossed_start==TRUE & d$crossed_end==TRUE

d.crossed <- subset(d, d$crossed == "TRUE")
d.uncrossed <- subset(d, d$crossed == "FALSE")
mean(d.crossed$normalized_pupil_disparity) 9.2287
mean(d.uncrossed$normalized_pupil_disparity) -94.761

d.sameword <- subset(d, d$onSameWord == "True")
d.diffword <- subset(d, d$onSameWord == "False")
nrow(d.sameword) / nrow(d) #62%
nrow(d.diffword) / nrow(d) # 38%
mean(d.sameword$normalized_pupil_disparity) # -0.81122
mean(d.diffword$normalized_pupil_disparity) # 1.2984
t.test(d.sameword$normalized_pupil_disparity,d.diffword$normalized_pupil_disparity)

unique(d$crossed)
lefteyes.sameword.crossed <- subset(lefteyes.sameword, lefteyes.sameword$crossed == "TRUE")
lefteyes.sameword.uncrossed <- subset(lefteyes.sameword, lefteyes.sameword$crossed == "FALSE")
lefteyes.diffword.crossed <- subset(lefteyes.diffword, lefteyes.diffword$crossed == "TRUE")
lefteyes.diffword.uncrossed <- subset(lefteyes.diffword, lefteyes.diffword$crossed == "FALSE")

righteyes.sameword.crossed <- subset(righteyes.sameword, righteyes.sameword$crossed == "TRUE")
righteyes.sameword.uncrossed <- subset(righteyes.sameword, righteyes.sameword$crossed == "FALSE")
righteyes.diffword.crossed <- subset(righteyes.diffword, righteyes.diffword$crossed == "TRUE")
righteyes.diffword.uncrossed <- subset(righteyes.diffword, righteyes.diffword$crossed == "FALSE")


mean(lefteyes.sameword$normalized_pupil_disparity) # 54.755 # CLARE: 157.927  #MONICA  -127.7558
mean(lefteyes.diffword$normalized_pupil_disparity) # 104.0896 # CLARE: 168.9977 # MONICA -86.6952
mean(righteyes.sameword$normalized_pupil_disparity) # -45.10659 CLARE; -38.91 # MONICA -52.88941
mean(righteyes.diffword$normalized_pupil_disparity) # - 26.80266 # CLARE; -3.523  # MONICA  - 50.20786

mean(lefteyes.crossed$normalized_pupil_disparity) # 96.56137 # CLARE 171.8559 #MONICA -87.43307
mean(lefteyes.uncrossed$normalized_pupil_disparity) # -125.5098 # CLARE  24.922 #MONICA #  -227.4628
mean(righteyes.crossed$normalized_pupil_disparity) # -29.10152 # CLARE -14.70858 #MONICA  # - 45.02503 
mean(righteyes.uncrossed$normalized_pupil_disparity) # -162.8277 # CLARE  -148.1511 # MONICA  #-191.2054


mean(lefteyes.sameword.crossed$normalized_pupil_disparity) # 86.6538 #CLARE  172.6939 #MONICA -95.3895
mean(lefteyes.sameword.uncrossed$normalized_pupil_disparity) # -120.4459 # # CLARE  23.54634 #MONICA -220.4436
mean(lefteyes.diffword.crossed$normalized_pupil_disparity) # 112.1478 # CLARE  170.6745 #MONICA -70.855395
mean(lefteyes.diffword.uncrossed$normalized_pupil_disparity) # -175.9084 #  # CLARE 41.18071  #MONICA- 290.4416

mean(righteyes.sameword.crossed$normalized_pupil_disparity) # -32.21104 # CLARE -24.21937 #MONICA -41.77594
mean(righteyes.sameword.uncrossed$normalized_pupil_disparity) # -162.31 # CLARE -148.2291 #MONICA -190.6965
mean(righteyes.diffword.crossed$normalized_pupil_disparity) # -25.167 # CLARE   -1.609403 #MONNICA  -48.77131
mean(righteyes.diffword.uncrossed$normalized_pupil_disparity) # -167.8389 #CLARE -147.0687  #MONICA -196.0086

cor(clare$normalized_pupil_disparity[1:50000], monica$normalized_pupil_disparity[1:50000])

mean(subset(clare, clare$handedness=="L")$meanP_l)

unique(d$gender)
male <- subset(d, d$gender == "M")
female <-subset(d, d$gender == "F")

male.lefteyed <- subset(male, male$eyePreferred == "L")
male.righteyed <- subset(male, male$eyePreferred == "R")
male.lefteyed.sameword <- subset(male.lefteyed, male.lefteyed$onSameWord=="True")
male.lefteyed.diffword <-subset(male.lefteyed, male.lefteyed$onSameWord == "False")
male.righteyed.sameword <- subset(male.righteyed, male.righteyed$onSameWord=="True")
male.righteyed.diffword <-subset(male.righteyed, male.righteyed$onSameWord == "False")

male.lefteyed.sameword.crossed <- subset(male.lefteyed.sameword, male.lefteyed.sameword$crossed=="TRUE")
male.lefteyed.sameword.uncrossed <- subset(male.lefteyed.sameword, male.lefteyed.sameword$crossed=="FALSE")
male.lefteyed.diffword.crossed <- subset(male.lefteyed.diffword, male.lefteyed.diffword$crossed=="TRUE")
male.lefteyed.diffword.uncrossed <- subset(male.lefteyed.diffword, male.lefteyed.diffword$crossed=="FALSE")

male.righteyed.sameword.crossed <- subset(male.righteyed.sameword, male.righteyed.sameword$crossed=="TRUE")
male.righteyed.sameword.uncrossed <- subset(male.righteyed.sameword, male.righteyed.sameword$crossed=="FALSE")
male.righteyed.diffword.crossed <- subset(male.righteyed.diffword, male.righteyed.diffword$crossed=="TRUE")
male.righteyed.diffword.uncrossed <- subset(male.righteyed.diffword, male.righteyed.diffword$crossed=="FALSE")

female.lefteyed <- subset(female, female$eyePreferred == "L")
female.righteyed <- subset(female, female$eyePreferred == "R")
female.lefteyed.sameword <- subset(female.lefteyed, female.lefteyed$onSameWord=="True")
female.lefteyed.diffword <-subset(female.lefteyed, female.lefteyed$onSameWord == "False")
female.righteyed.sameword <- subset(female.righteyed, female.righteyed$onSameWord=="True")
female.righteyed.diffword <-subset(female.righteyed, female.righteyed$onSameWord == "False")

female.lefteyed.sameword.crossed <- subset(female.lefteyed.sameword, female.lefteyed.sameword$crossed=="TRUE")
female.lefteyed.sameword.uncrossed <- subset(female.lefteyed.sameword, female.lefteyed.sameword$crossed=="FALSE")
female.lefteyed.diffword.crossed <- subset(female.lefteyed.diffword, female.lefteyed.diffword$crossed=="TRUE")
female.lefteyed.diffword.uncrossed <- subset(female.lefteyed.diffword, male.lefteyed.diffword$crossed=="FALSE")

female.righteyed.sameword.crossed <- subset(female.righteyed.sameword, female.righteyed.sameword$crossed=="TRUE")
female.righteyed.sameword.uncrossed <- subset(female.righteyed.sameword, female.righteyed.sameword$crossed=="FALSE")
female.righteyed.diffword.crossed <- subset(female.righteyed.diffword, female.righteyed.diffword$crossed=="TRUE")
female.righteyed.diffword.uncrossed <- subset(female.righteyed.diffword, female.righteyed.diffword$crossed=="FALSE")

# okay... and now for all the means!!!
mean(male.lefteyed$normalized_pupil_disparity)# -81.1171
mean(male.righteyed$normalized_pupil_disparity)#  -53.7878

mean(male.lefteyed.sameword$normalized_pupil_disparity) #-89,61487
mean(male.lefteyed.diffword$normalized_pupil_disparity) #-58.95017
mean(male.righteyed.sameword$normalized_pupil_disparity)  #-58.7157
mean(male.righteyed.diffword$normalized_pupil_disparity)  #-46.85224


mean(male.lefteyed.sameword.crossed$normalized_pupil_disparity) #-70.54495
mean(male.lefteyed.sameword.uncrossed$normalized_pupil_disparity) #-151.5635
mean(male.lefteyed.diffword.crossed$normalized_pupil_disparity) #-49.75736
mean(male.lefteyed.diffword.uncrossed$normalized_pupil_disparity) #-192.5842

mean(male.righteyed.sameword.crossed$normalized_pupil_disparity) #-43.16257
mean(male.righteyed.sameword.uncrossed$normalized_pupil_disparity) #-201.5924
mean(male.righteyed.diffword.crossed$normalized_pupil_disparity) #-44.48891
mean(male.righteyed.diffword.uncrossed$normalized_pupil_disparity) #-215.6547

#for  femalemeans
mean(female.lefteyed$normalized_pupil_disparity)# 204.5129
mean(female.righteyed$normalized_pupil_disparity)#  -16.35058

mean(female.lefteyed.sameword$normalized_pupil_disparity) #210.9151
mean(female.lefteyed.diffword$normalized_pupil_disparity) #195.8257
mean(female.righteyed.sameword$normalized_pupil_disparity)  #-20.9905
mean(female.righteyed.diffword$normalized_pupil_disparity)  #-10.37022


mean(female.lefteyed.sameword.crossed$normalized_pupil_disparity) #225.8597
mean(female.lefteyed.sameword.uncrossed$normalized_pupil_disparity) #-0.4694871
mean(female.lefteyed.diffword.crossed$normalized_pupil_disparity) #198.0261
mean(female.lefteyed.diffword.uncrossed$normalized_pupil_disparity) #276.3622

mean(female.righteyed.sameword.crossed$normalized_pupil_disparity) #-9.222345
mean(female.righteyed.sameword.uncrossed$normalized_pupil_disparity) #-139.1628
mean(female.righteyed.diffword.crossed$normalized_pupil_disparity) #-9.263749
mean(female.righteyed.diffword.uncrossed$normalized_pupil_disparity) #-135.1393

nrow(subset(male, male$HandledBy=="Clare"))/ nrow(male) #58.9% male
nrow(subset(male, male$HandledBy == "Monica"))/ nrow(male) # 41.0% male...
mean(male$normalized_pupil_disparity) # -49.62613
mean(female$normalized_pupil_disparity) # 51.13118

unique(clare$gender)
mean(subset(clare, clare$gender=="M")$normalized_pupil_disparity) # 40.41462
mean(subset(clare, clare$gender=="F")$normalized_pupil_disparity) # 69.77678
mean(subset(monica, monica$gender=="M")$normalized_pupil_disparity) # -179.133 
mean(subset(monica, monica$gender=="F")$normalized_pupil_disparity) # 19.71386

cor(d$normalized_pupil_disparity, d$thisSaccDuration_l)
cor(d$normalized_pupil_disparity, d$thisSaccDuration_r)
cor(lefteyes$normalized_pupil_disparity, lefteyes$thisSaccDuration_l)
cor(lefteyes$normalized_pupil_disparity, lefteyes$thisSaccDuration_r)
cor(lefteyes.crossed$normalized_pupil_disparity, lefteyes.crossed$thisSaccDuration_l)
cor(lefteyes.crossed$normalized_pupil_disparity, lefteyes.crossed$wordFreq_l)

unique(d$alreadyPassed_l)
unique(d$alreadyPassed_r)
mean(subset(d,d$alreadyPassed_l=="True")$normalized_pupil_disparity)
mean(subset(d,d$alreadyPassed_l=="False")$normalized_pupil_disparity)

nrow(subset(d, d$crossed == "TRUE")) / nrow(d)# 91%of fixations are crossed,
nrow(lefteyes) / nrow(d) #0.2987
nrow(righteyes) / nrow(d) #0.5277
es.sameword.crossed$normalized_pupil_disparity, righteyes.diffword.crossed$normalized_pupil_disparity)# is significant t = 4.603, p = 4.17e-6

#  let'stest the differences ni different languages
arab_fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/f_prepared_Arabic_noWordFrequency.csv"
spanish_fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/f_paired_Spanish_EyelinkEvent_notAligned.csv"
hebrew_fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/f_paired_Hebrew_EyelinkEvent_notAligned.csv"

# let's inspect the other language data
lang <- read.csv(arab_fname, sep=',')
colnames(lang)
d$crossed_start <-d$preSaccEndX_l > d$preSaccEndX_r
head(d$crossed_start) 
d$crossed_end  <- d$nextSaccStartX_l  > d$nextSaccStartX_r

lang$crossed_start <- lang$preSaccEndX_l > lang$preSaccEndX_r
lang$crossed_end <- lang$nextSaccStartX_l > lang$nextSaccEndX_r
lang$crossed <- lang$crossed_start & lang$crossed_end 
nrow(subset(lang, lang$crossed== "TRUE")) / nrow(lang)
lang <- read.csv(hebrew_fname, sep=',')
colnames(lang)


cor(d$wordLength_l, d$meanP_l) # a 0.01 correlation is nonexistent
cor(d$wordLength_r, d$meanP_r) 
colnames(d)
d$crossed <- d$crossed_start == TRUE & d$crossed_end == TRUE
head(d$crossed)
d.crossed <- subset(d, d$crossed == TRUE)
d.uncrossed <- subset(d, d$crossed == FALSE)

hex.crossed_l <- ggplot(d.crossed, aes(meanX_l, meanY_l))
hex.crossed_l + geom_hex()
hex.crossed_r <- ggplot(d.crossed, aes(meanX_r, meanY_r))
hex.crossed_r + geom_hex()

hex.uncrossed_r <- ggplot(d.uncrossed, aes(meanX_r, meanY_r))
hex.uncrossed_r + geom_hex()
d$binocular_disparity_start <- d$preSaccEndX_r - d$preSaccEndX_l
mean(d$binocular_disparity_start)
hist(d$binocular_disparity_start)
# so that is the mean of the starting binocular disparity
d$binocular_disparity_end <- d$nextSaccStartX_r - d$nextSaccStartX_l
mean(d$binocular_disparity_end)
hist(d$binocular_disparity_end)

cor(d$normalized_pupil_disparity, d$binocular_disparity_start) #-0.02831
# huh... no appreciable correlation!
cor(d$normalized_pupil_disparity, d$binocular_disparity_end) # 0.016665
# so tiny tiny correlations!
cor(d.crossed$normalized_pupil_disparity, d.crossed$binocular_disparity_start) # 0.04025995
cor(d.uncrossed$normalized_pupil_disparity, d.uncrossed$binocular_disparity_start) #-0.09607696


mean(d.crossed$thisSaccDuration_l)
mean(d.crossed$thisSaccDuration_r)
mean(d.uncrossed$thisSaccDuration_l)
mean(d.uncrossed$thisSaccDuration_r)

mean(d.crossed$wordFreq_l)
mean(d.uncrossed$wordFreq_l)

d$regression_l <- d$x_l_travel < 0
head(d$regression_l)
d$regression_r <- d$x_r_travel < 0
d$regression <- d$regression_l == TRUE & d$regression_r == TRUE
d.regression <- subset(d, d$regression == TRUE)
nrow(d.regression) / nrow(d)
hex.regression <- ggplot(d.regression, aes(meanX_l, meanY_l))
hex.regression + geom_hex()

d.regression.crossed <- subset(d.regression, d.regression$crossed == TRUE)
nrow(d.regression.crossed) / nrow(d.regression)
nrow(d.crossed)  / nrow(d)

mean(d.crossed$meanP_l)
mean(d.uncrossed$meanP_l)
mean(d.crossed$meanP_r)
mean(d.uncrossed$meanP_r)


d.middle <- subset(d, d$ScreenSide == "Middle")

dys <- read.csv("/home/beren/work/phd/eyetracking/data/data/Jun_data/f_prepared_EnglishDyslexia.csv")
colnames(dys)
nrow(dys)
unique(dys$subjectID)
