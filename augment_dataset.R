library(dplyr)
library(ggplot2)

fname <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/f_prepared_EnglishDyslexia.csv"
d <- read.csv(fname, sep=",")
colnames(d)

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

d$ScreenSide<- ifelse(d$thisSaccStartX_l<= 341 | d$thisSaccStartX_r<= 341,"Left",
                      ifelse(d$thisSaccStartX_l> 341& d$thisSaccStartX_l< 682 | d$thisSaccStartX_r> 341& d$thisSaccStartX_r< 682,"Middle",
                             ifelse(d$thisSaccStartX_l>= 682 | d$thisSaccStartX_r>= 682,"Right",NA)))


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


d$pupil_disparity <- d$meanP_r -  d$meanP_l
d$normalized_pupil_disparity <- d$pupil_disparity - mean(d$pupil_disparity)


d$crossed_start <-d$preSaccEndX_l > d$preSaccEndX_r
head(d$crossed_start) 
d$crossed_end  <- d$nextSaccStartX_l  > d$nextSaccStartX_r
head(d$crossed_end)
d$crossed <- d$crossed_start==TRUE & d$crossed_end==TRUE

d$crossed_type <- ifelse(d$crossed_start == "TRUE" & d$crossed_end == "TRUE", d$crossed_type <- "Crossed",
                         ifelse(d$crossed_start == "TRUE" & d$crossed_end == "FALSE", d$crossed_type <- "C_UC",
                                ifelse(d$crossed_start=="FALSE" & d$crossed_end == "TRUE", d$crossed_type <- "UC_C",
                                       ifelse(d$crossed_start == "FALSE" & d$crossed_end == "FALSE", d$crossed_type <- "Uncrossed", NA))))

savename <- "/home/beren/work/phd/eyetracking/data/data/Jun_data/augmented_English.csv"
write.csv(d, savename)