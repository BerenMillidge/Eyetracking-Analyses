# A quick script to look at the word information at the character level using the bigrams
d_fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English_4_word_info.csv"
d <- read.csv(d_fname, sep=',')
colnames(d)

d$word_information_gain_l[1:100]
mean(d$word_information_gain_l)
hist(d$word_information_gain_l)
sd(d$word_information_gain_l)
d <- subset(d, d$word_information_gain_l < 10 & d$word_information_gain_r < 10)
nrow(d)
mean(d$word_information_gain_l)
hist(d$word_information_gain_l)
range(d$word_information_gain_l)

d$word_information_l[1:100]
hist(d$word_information_l)

d<- subset(d, d$word_information_l < 10 & d$word_information_r < 10)
hist(d$word_information_l)
d$wordLength_l[1:100]
cor(d$wordLength_l, d$word_information_l)
cor(d$wordFreq_l, d$word_information_l)
cor(d$meanP_l, d$word_information_l)
cor(d$thisSaccDuration_l, d$word_information_l)