#Re-sample additional Af-Ams

getwd()

#Read in OH VF:
ex_vf <- read.csv("<filepath>.csv")

setwd("<filepath>")

head(ex_vf)

#Drop from file:
#Non-African-Americans

#Create segment variable for ex_vf
#Age, Race, Sex
table(ex_vf$racename)
table(ex_vf$sex)
xtabs(~ex_vf$racename+ex_vf$sex, data=ex_vf)

#ex_vf <- subset(ex_vf,is.na(segment)==0)
frame <- subset(ex_vf,ex_vf$racename=="African-American") #662,788
table(ex_vf$racename) #662,788 #Names still here
table(frame$racename)

#Keep only observations with a phone number and segment
colnames(frame)
table(frame$sex)
frame <-subset(frame, sex!="U") #Drop obs where gender is unknown
frame <-subset(frame,((is.na(homephone)+is.na(cellphone)<=1))) #Drop obs where cell phone and home phone are missing
frame <-subset(frame,(is.na(age)==0)) #Keep obs with age not missing

View(frame) #still here

#drop obs from first round of calling!
#skip drop here

#unscored <- merge(x=unscored,y=scored_means_df,by="segment", all.x=TRUE)
#frame <- merge(x=frame,y=bump,by="vanid",all.x=TRUE) #Names are missing here! -- this is where the drop happens!!!

#table(is.na(frame$X))
#frame <- subset(frame,(is.na(frame$X)==TRUE))

#NEED TO CODE SEGMENT!
#Recode age into age groups
summary(frame$age)

frame$age_group[frame$age>=18 & frame$age<=34] <- "18-34" #18-34
frame$age_group[frame$age>=35 & frame$age<=49] <- "35-49" #18-34
frame$age_group[frame$age>=50 & frame$age<=64] <- "50-64" #18-34
frame$age_group[frame$age>=65 & frame$age<=125] <- "65+" #18-34

summary(frame$age_group)
xtabs(~frame$age+frame$age_group, data=frame)

#Set Segments
frame$segment[frame$sex=="M" & frame$age_group=="18-34"] <-8
frame$segment[frame$sex=="M" & frame$age_group=="35-49"] <-9
frame$segment[frame$sex=="M" & frame$age_group=="50-64"] <-10
frame$segment[frame$sex=="M" & frame$age_group=="65+"] <-11
frame$segment[frame$sex=="F" & frame$age_group=="18-34"] <-12
frame$segment[frame$sex=="F" & frame$age_group=="35-49"] <-13
frame$segment[frame$sex=="F" & frame$age_group=="50-64"] <-14
frame$segment[frame$sex=="F" & frame$age_group=="65+"] <-15

#frame <- subset(frame,is.na(segment)==0) #drop any obs not assigned to a frame due to missing data #not necessary

sample1 <-read.csv("/Users/graceturke/Dropbox (270 Data)/270 Data All/270_Signed/Responsible Ohio/RO Model Refresh - Sept 2015/Sampling/RO_Sept15Refresh/OH_Sept2015_refresh_call_list_20150902_v3.csv")
colnames(sample1)
bump <- sample1[,c(1,8)]

frame <-merge (x=frame,y=bump,by="vanid",all.x=TRUE)
frame <- subset(frame,(is.na(frame$X)==TRUE))
frame <- frame[,-c(17)]
#Sample data!

#1384
#1004
#609
#390
#1392
#927
#298
#245

nrow(frame)

#Shuffle before sampling
set.seed(862518965) 

frame <- frame[sample(1:nrow(frame), nrow(frame), replace=F),]
nrow(frame)

sample8 <- (subset(frame,segment==8))[sample(nrow(subset(frame, segment==8 )),1384),]
sample9 <- (subset(frame,segment==9))[sample(nrow(subset(frame, segment==9 )),1004),]
sample10 <- (subset(frame,segment==10))[sample(nrow(subset(frame, segment==10 )),609),]
sample11 <- (subset(frame,segment==11))[sample(nrow(subset(frame, segment==11 )),390),]
sample12 <- (subset(frame,segment==12))[sample(nrow(subset(frame, segment==12 )),1392),]
sample13 <- (subset(frame,segment==13))[sample(nrow(subset(frame, segment==13 )),927),]
sample14 <- (subset(frame,segment==14))[sample(nrow(subset(frame, segment==14 )),298),]
sample15 <- (subset(frame,segment==15))[sample(nrow(subset(frame, segment==15 )),245),]

afam_sample2 <- rbind(sample8
                      , sample9
                      , sample10
                      , sample11
                      , sample12
                      , sample13
                      , sample14
                      , sample15
)

write.csv(afam_sample2,"afam_sample2.csv")

