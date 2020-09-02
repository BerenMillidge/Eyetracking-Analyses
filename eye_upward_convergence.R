library(dplyr)


d <- read.csv("~/work/phd/eyetracking/data/data/Jun_data/augmented_English_4_word_info.csv")
colnames(d)

mean(d$preSaccEndY_l - d$nextSaccStartY_l)
mean(d$preSaccEndY_r - d$nextSaccStartY_r)
d$pre_binocular_disparity_x <- d$preSaccEndX_l - d$preSaccEndX_r
d$post_binocular_dispary_x <- d$nextSaccStartX_l  - d$nextSaccStartX_r

d$pre_binocular_disparity_y <- d$preSaccEndY_l - d$preSaccEndY_r
d$post_binocular_dispary_y <- d$nextSaccStartY_l  - d$nextSaccStartY_r
unique(d$wordLength_l)
unique(d$wordLength_r)

dat <- subset(d, d$wordLength_l == d$wordLength_r)
cor(dat$wordLength_l, dat$pre_binocular_disparity_x)
cor(dat$wordLength_l, dat$post_binocular_dispary_x)

plot(dat$wordLength_l, dat$pre_binocular_disparity_x)
dat.pre_crossed <- subset(dat, dat$preSaccEndX_l > dat$preSaccEndX_r)
nrow(dat.pre_crossed)
cor(dat.pre_crossed$wordLength_l, dat.pre_crossed$pre_binocular_disparity_x)
dat.post_crossed <- subset(dat, dat$nextSaccStartX_l > dat$nextSaccStartX_r)
nrow(dat.post_crossed)
cor(dat.post_crossed$wordLength_l, dat.post_crossed$pre_binocular_disparity_x)


dat.pre_uncrossed <- subset(dat, dat$preSaccEndX_l <= dat$preSaccEndX_r)
nrow(dat.pre_uncrossed)
cor(dat.pre_uncrossed$wordLength_l, dat.pre_uncrossed$pre_binocular_disparity_x)
# so there is a correlationi nthe size of the disparity in the length of the word!#
mean(dat.pre_uncrossed$wordLength_l)
mean(dat.pre_crossed$wordLength_l)

