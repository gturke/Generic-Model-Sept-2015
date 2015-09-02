#Pulling Survey Sample Script
#sink(file = "refresh_sampling_sink.txt", append=TRUE,type = c("output","message"), split=FALSE) #CHANGE TO TRUE LATER
getwd()
setwd(<directory name here>)
getwd()

#Read in file:
df_name <- read.csv("file_name.csv")
head(df_name)

#Read in sampling frame:
sampling_frame <- read.csv("sampling_frame_sept2015.csv")
head(sampling_frame)

#Create segment variable for df_name
#Age, Race, Sex
table(df_name$racename)
table(df_name$sex)
xtabs(~df_name$racename+df_name$sex, data=df_name)

#test.1$race_group[test.1$race_group=="Hispanic"] <- "Other"

#Recode race names
df_name$race2[df_name$racename=="Caucasian"] <- "White"
df_name$race2[df_name$racename=="African-American"] <- "Black"
df_name$race2[df_name$racename!="Caucasian" & df_name$racename!="African-American"]<-"Other"

xtabs(~df_name$race2+df_name$racename,data=df_name)

#Recode age into age groups
summary(df_name$age)
df_name$age_group[df_name$age>=18 & df_name$age<=34] <- "18-34" #18-34
df_name$age_group[df_name$age>=35 & df_name$age<=49] <- "35-49" #18-34
df_name$age_group[df_name$age>=50 & df_name$age<=64] <- "50-64" #18-34
df_name$age_group[df_name$age>=65 & df_name$age<=125] <- "65+" #18-34

summary(df_name$age_group)
xtabs(~df_name$age+df_name$age_group, data=df_name)

#Set Segments
df_name$segment[df_name$sex=="M" & df_name$race2=="White" & df_name$age_group=="18-34"] <-0
df_name$segment[df_name$sex=="M" & df_name$race2=="White" & df_name$age_group=="35-49"] <-1
df_name$segment[df_name$sex=="M" & df_name$race2=="White" & df_name$age_group=="50-64"] <-2
df_name$segment[df_name$sex=="M" & df_name$race2=="White" & df_name$age_group=="65+"] <-3
df_name$segment[df_name$sex=="F" & df_name$race2=="White" & df_name$age_group=="18-34"] <-4
df_name$segment[df_name$sex=="F" & df_name$race2=="White" & df_name$age_group=="35-49"] <-5
df_name$segment[df_name$sex=="F" & df_name$race2=="White" & df_name$age_group=="50-64"] <-6
df_name$segment[df_name$sex=="F" & df_name$race2=="White" & df_name$age_group=="65+"] <-7
df_name$segment[df_name$sex=="M" & df_name$race2=="Black" & df_name$age_group=="18-34"] <-8
df_name$segment[df_name$sex=="M" & df_name$race2=="Black" & df_name$age_group=="35-49"] <-9
df_name$segment[df_name$sex=="M" & df_name$race2=="Black" & df_name$age_group=="50-64"] <-10
df_name$segment[df_name$sex=="M" & df_name$race2=="Black" & df_name$age_group=="65+"] <-11
df_name$segment[df_name$sex=="F" & df_name$race2=="Black" & df_name$age_group=="18-34"] <-12
df_name$segment[df_name$sex=="F" & df_name$race2=="Black" & df_name$age_group=="35-49"] <-13
df_name$segment[df_name$sex=="F" & df_name$race2=="Black" & df_name$age_group=="50-64"] <-14
df_name$segment[df_name$sex=="F" & df_name$race2=="Black" & df_name$age_group=="65+"] <-15
df_name$segment[df_name$sex=="M" & df_name$race2=="Other" & df_name$age_group=="18-34"] <-16
df_name$segment[df_name$sex=="M" & df_name$race2=="Other" & df_name$age_group=="35-49"] <-17
df_name$segment[df_name$sex=="M" & df_name$race2=="Other" & df_name$age_group=="50-64"] <-18
df_name$segment[df_name$sex=="M" & df_name$race2=="Other" & df_name$age_group=="65+"] <-19
df_name$segment[df_name$sex=="F" & df_name$race2=="Other" & df_name$age_group=="18-34"] <-20
df_name$segment[df_name$sex=="F" & df_name$race2=="Other" & df_name$age_group=="35-49"] <-21
df_name$segment[df_name$sex=="F" & df_name$race2=="Other" & df_name$age_group=="50-64"] <-22
df_name$segment[df_name$sex=="F" & df_name$race2=="Other" & df_name$age_group=="65+"] <-23

#Keep only observations with a phone number and segment
colnames(df_name)
#df_name <- subset(df_name, sex!="U")
df_name <- subset(df_name,is.na(segment)==0)
df_name <- subset(df_name,((is.na(homephone)+is.na(cellphone))<=1))

#test_sample1 <- (subset(df_name, segment==1))[sample(nrow(subset(df_name, segment==1)),50),]

sample0 <- (subset(df_name,segment==0))[sample(nrow(subset(df_name, segment==0 )),20989),]
sample1 <- (subset(df_name,segment==1))[sample(nrow(subset(df_name, segment==1 )),11227),]
sample2 <- (subset(df_name,segment==2))[sample(nrow(subset(df_name, segment==2 )),8475),]
sample3 <- (subset(df_name,segment==3))[sample(nrow(subset(df_name, segment==3 )),3544),]
sample4 <- (subset(df_name,segment==4))[sample(nrow(subset(df_name, segment==4 )),23276),]
sample5 <- (subset(df_name,segment==5))[sample(nrow(subset(df_name, segment==5 )),11562),]
sample6 <- (subset(df_name,segment==6))[sample(nrow(subset(df_name, segment==6 )),6942),]
sample7 <- (subset(df_name,segment==7))[sample(nrow(subset(df_name, segment==7 )),3912),]
sample8 <- (subset(df_name,segment==8))[sample(nrow(subset(df_name, segment==8 )),2337),]
sample9 <- (subset(df_name,segment==9))[sample(nrow(subset(df_name, segment==9 )),1184),]
sample10 <- (subset(df_name,segment==10))[sample(nrow(subset(df_name, segment==10 )),708),]
sample11 <- (subset(df_name,segment==11))[sample(nrow(subset(df_name, segment==11 )),285),]
sample12 <- (subset(df_name,segment==12))[sample(nrow(subset(df_name, segment==12 )),2638),]
sample13 <- (subset(df_name,segment==13))[sample(nrow(subset(df_name, segment==13 )),1136),]
sample14 <- (subset(df_name,segment==14))[sample(nrow(subset(df_name, segment==14 )),609),]
sample15 <- (subset(df_name,segment==15))[sample(nrow(subset(df_name, segment==15 )),298),]
sample16 <- (subset(df_name,segment==16))[sample(nrow(subset(df_name, segment==16 )),1577),]
sample17 <- (subset(df_name,segment==17))[sample(nrow(subset(df_name, segment==17 )),943),]
sample18 <- (subset(df_name,segment==18))[sample(nrow(subset(df_name, segment==18 )),554),]
sample19 <- (subset(df_name,segment==19))[sample(nrow(subset(df_name, segment==19 )),234),]
sample20 <- (subset(df_name,segment==20))[sample(nrow(subset(df_name, segment==20 )),2131),]
sample21 <- (subset(df_name,segment==21))[sample(nrow(subset(df_name, segment==21 )),766),]
sample22 <- (subset(df_name,segment==22))[sample(nrow(subset(df_name, segment==22 )),433),]
sample23 <- (subset(df_name,segment==23))[sample(nrow(subset(df_name, segment==23 )),241),]

sample_file = rbind(sample1
               , sample2
               , sample3
               , sample4
               , sample5
               , sample6
               , sample7
               , sample8
               , sample9
               , sample10
               , sample11
               , sample12
               , sample13
               , sample14
               , sample15
               , sample16
               , sample17
               , sample18
               , sample19
               , sample20
               , sample21
               , sample22
               , sample23
)

###
