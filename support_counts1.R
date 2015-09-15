#Create scores for unscored records and calculate support projection

#rm(list = ls()) #Clear Environment
#load("~/.RData")

#Keep only scores dataframe #and oh1?

unscored <- read.csv("ToBeScored20150909.csv")
head(unscored)
colnames(unscored)

unscored <-unscored[,c(2,4:7,13,15)]

nrow(scores)
nrow(unscored)

table(unscored$RaceName)
table(unscored$Sex)

#Recode race names
unscored$race2 <- "Other"
unscored$race2[unscored$RaceName=="Caucasian"] <- "White"
unscored$race2[unscored$RaceName=="African-American"] <- "Black"
unscored$race2[unscored$RaceName=="Hispanic"] <- "Hispanic"

xtabs(~unscored$race2+unscored$RaceName,data=unscored)

#Recode age into age groups
summary(unscored$Age)
unscored$age_group[unscored$Age>=18 & unscored$Age<=34] <- "18-34" #18-34
unscored$age_group[unscored$Age>=35 & unscored$Age<=49] <- "35-49" #18-34
unscored$age_group[unscored$Age>=50 & unscored$Age<=64] <- "50-64" #18-34
unscored$age_group[unscored$Age>=65 & unscored$Age<=125] <- "65+" #18-34

summary(unscored$age_group)
xtabs(~unscored$Age+unscored$age_group, data=unscored)

###To Do###
#Code segments for:
#   -scored
#   -unscored
#Calculate segment averages by scored data
#Apply segment averages to unscored data
#Merge to one file
#Calculate Projection
#Create segment

#Create segements for scored data:
scores$age_group[scores$age>=18 & scores$age<=34] <- "18-34" #18-34
scores$age_group[scores$age>=35 & scores$age<=49] <- "35-49" #18-34
scores$age_group[scores$age>=50 & scores$age<=64] <- "50-64" #18-34
scores$age_group[scores$age>=65 & scores$age<=125] <- "65+" #18-34

#Challenge -- projection combines Hispanic with Others, think it would be better to calc separately for average scores?

scores$segment[scores$sex=="M" & scores$race_group=="Caucasian" & scores$age_group=="18-34"] <-0
scores$segment[scores$sex=="M" & scores$race_group=="Caucasian" & scores$age_group=="35-49"] <-1
scores$segment[scores$sex=="M" & scores$race_group=="Caucasian" & scores$age_group=="50-64"] <-2
scores$segment[scores$sex=="M" & scores$race_group=="Caucasian" & scores$age_group=="65+"] <-3
scores$segment[scores$sex=="F" & scores$race_group=="Caucasian" & scores$age_group=="18-34"] <-4
scores$segment[scores$sex=="F" & scores$race_group=="Caucasian" & scores$age_group=="35-49"] <-5
scores$segment[scores$sex=="F" & scores$race_group=="Caucasian" & scores$age_group=="50-64"] <-6
scores$segment[scores$sex=="F" & scores$race_group=="Caucasian" & scores$age_group=="65+"] <-7
scores$segment[scores$sex=="M" & scores$race_group=="African-American" & scores$age_group=="18-34"] <-8
scores$segment[scores$sex=="M" & scores$race_group=="African-American" & scores$age_group=="35-49"] <-9
scores$segment[scores$sex=="M" & scores$race_group=="African-American" & scores$age_group=="50-64"] <-10
scores$segment[scores$sex=="M" & scores$race_group=="African-American" & scores$age_group=="65+"] <-11
scores$segment[scores$sex=="F" & scores$race_group=="African-American" & scores$age_group=="18-34"] <-12
scores$segment[scores$sex=="F" & scores$race_group=="African-American" & scores$age_group=="35-49"] <-13
scores$segment[scores$sex=="F" & scores$race_group=="African-American" & scores$age_group=="50-64"] <-14
scores$segment[scores$sex=="F" & scores$race_group=="African-American" & scores$age_group=="65+"] <-15
scores$segment[scores$sex=="M" & scores$race_group=="Other" & scores$age_group=="18-34"] <-16
scores$segment[scores$sex=="M" & scores$race_group=="Other" & scores$age_group=="35-49"] <-17
scores$segment[scores$sex=="M" & scores$race_group=="Other" & scores$age_group=="50-64"] <-18
scores$segment[scores$sex=="M" & scores$race_group=="Other" & scores$age_group=="65+"] <-19
scores$segment[scores$sex=="F" & scores$race_group=="Other" & scores$age_group=="18-34"] <-20
scores$segment[scores$sex=="F" & scores$race_group=="Other" & scores$age_group=="35-49"] <-21
scores$segment[scores$sex=="F" & scores$race_group=="Other" & scores$age_group=="50-64"] <-22
scores$segment[scores$sex=="F" & scores$race_group=="Other" & scores$age_group=="65+"] <-23
scores$segment[scores$sex=="M" & scores$race_group=="Hispanic" & scores$age_group=="18-34"] <-24
scores$segment[scores$sex=="M" & scores$race_group=="Hispanic" & scores$age_group=="35-49"] <-25
scores$segment[scores$sex=="M" & scores$race_group=="Hispanic" & scores$age_group=="50-64"] <-26
scores$segment[scores$sex=="M" & scores$race_group=="Hispanic" & scores$age_group=="65+"] <-27
scores$segment[scores$sex=="F" & scores$race_group=="Hispanic" & scores$age_group=="18-34"] <-28
scores$segment[scores$sex=="F" & scores$race_group=="Hispanic" & scores$age_group=="35-49"] <-29
scores$segment[scores$sex=="F" & scores$race_group=="Hispanic" & scores$age_group=="50-64"] <-30
scores$segment[scores$sex=="F" & scores$race_group=="Hispanic" & scores$age_group=="65+"] <-31

table(scores$segment)
xtabs(~scores$race_group+scores$segment,data=scores)

unscored$segment[unscored$Sex=="M" & unscored$race2=="White" & unscored$age_group=="18-34"] <-0
unscored$segment[unscored$Sex=="M" & unscored$race2=="White" & unscored$age_group=="35-49"] <-1
unscored$segment[unscored$Sex=="M" & unscored$race2=="White" & unscored$age_group=="50-64"] <-2
unscored$segment[unscored$Sex=="M" & unscored$race2=="White" & unscored$age_group=="65+"] <-3
unscored$segment[unscored$Sex=="F" & unscored$race2=="White" & unscored$age_group=="18-34"] <-4
unscored$segment[unscored$Sex=="F" & unscored$race2=="White" & unscored$age_group=="35-49"] <-5
unscored$segment[unscored$Sex=="F" & unscored$race2=="White" & unscored$age_group=="50-64"] <-6
unscored$segment[unscored$Sex=="F" & unscored$race2=="White" & unscored$age_group=="65+"] <-7
unscored$segment[unscored$Sex=="M" & unscored$race2=="Black" & unscored$age_group=="18-34"] <-8
unscored$segment[unscored$Sex=="M" & unscored$race2=="Black" & unscored$age_group=="35-49"] <-9
unscored$segment[unscored$Sex=="M" & unscored$race2=="Black" & unscored$age_group=="50-64"] <-10
unscored$segment[unscored$Sex=="M" & unscored$race2=="Black" & unscored$age_group=="65+"] <-11
unscored$segment[unscored$Sex=="F" & unscored$race2=="Black" & unscored$age_group=="18-34"] <-12
unscored$segment[unscored$Sex=="F" & unscored$race2=="Black" & unscored$age_group=="35-49"] <-13
unscored$segment[unscored$Sex=="F" & unscored$race2=="Black" & unscored$age_group=="50-64"] <-14
unscored$segment[unscored$Sex=="F" & unscored$race2=="Black" & unscored$age_group=="65+"] <-15
unscored$segment[unscored$Sex=="M" & unscored$race2=="Other" & unscored$age_group=="18-34"] <-16
unscored$segment[unscored$Sex=="M" & unscored$race2=="Other" & unscored$age_group=="35-49"] <-17
unscored$segment[unscored$Sex=="M" & unscored$race2=="Other" & unscored$age_group=="50-64"] <-18
unscored$segment[unscored$Sex=="M" & unscored$race2=="Other" & unscored$age_group=="65+"] <-19
unscored$segment[unscored$Sex=="F" & unscored$race2=="Other" & unscored$age_group=="18-34"] <-20
unscored$segment[unscored$Sex=="F" & unscored$race2=="Other" & unscored$age_group=="35-49"] <-21
unscored$segment[unscored$Sex=="F" & unscored$race2=="Other" & unscored$age_group=="50-64"] <-22
unscored$segment[unscored$Sex=="F" & unscored$race2=="Other" & unscored$age_group=="65+"] <-23
unscored$segment[unscored$Sex=="M" & unscored$race2=="Hispanic" & unscored$age_group=="18-34"] <-24
unscored$segment[unscored$Sex=="M" & unscored$race2=="Hispanic" & unscored$age_group=="35-49"] <-25
unscored$segment[unscored$Sex=="M" & unscored$race2=="Hispanic" & unscored$age_group=="50-64"] <-26
unscored$segment[unscored$Sex=="M" & unscored$race2=="Hispanic" & unscored$age_group=="65+"] <-27
unscored$segment[unscored$Sex=="F" & unscored$race2=="Hispanic" & unscored$age_group=="18-34"] <-28
unscored$segment[unscored$Sex=="F" & unscored$race2=="Hispanic" & unscored$age_group=="35-49"] <-29
unscored$segment[unscored$Sex=="F" & unscored$race2=="Hispanic" & unscored$age_group=="50-64"] <-30
unscored$segment[unscored$Sex=="F" & unscored$race2=="Hispanic" & unscored$age_group=="65+"] <-31

table(unscored$segment)
xtabs(~unscored$race2+unscored$segment,data=unscored)

#Not scoring records with unknown gender


#
##
###
####

#For Scored, create average support and average turnout score by segment
#sapply(scores, class)
tapply(scores$support_sept15, scores$segment, mean) #Get average support score by segment

#Create a dataframe of average support score by segment
scored_means <-tapply(scores$support_sept15, scores$segment, mean) #Creates array of means
y_name <- "scored_means"
x <-c(0:31)
x_name <- "segment"
scored_means_df <- data.frame(x,scored_means)
names(scored_means_df) <- c(x_name,y_name)
print(scored_means_df)

#Create a dataframe of average turnout score by segment
#2015 turnout score data not in dataframe :(

unscored <- merge(x=unscored,y=scored_means_df,by="segment", all.x=TRUE)

#Read turnout scores into environment
turnout15 <-read.csv("2015_turnout_scores.csv")
colnames(turnout15)

#Repeat average/projection process for turnout scores
#First, join turnout scores to scores dataframe by vanid

turnout15 <- turnout15[,-c(1)]
scores <- merge(scores,turnout15,"vanid")


#
##
###
####

#Create a dataframe of average TURNOUT score by segment
turnout_means <-tapply(scores$turnout_score_mean, scores$segment, mean) #Creates array of means #array b
#b_name <- "turnout_means"
a <-c(0:31)
#a_name <- "segment"
turnout_means_df <- data.frame(a,turnout_means)
names(turnout_means_df) <- c("segment","turnout_means")
print(scored_means_df)

#Create a dataframe of average turnout score by segment
#2015 turnout score data not in dataframe :(

unscored <- merge(x=unscored,y=turnout_means_df,by="segment", all.x=TRUE)

#write.csv(scores,"ResponsibleOH_supportscores_20150910.csv")
write.csv(unscored[,c(2,11,12)],"Unscored_averages_20150911.csv")

#
##
###
####

#Recalculate turnout & support by segment, this time combining Hispanic + Other
table(scores$race_group) #Or just average?
scored_means
xtabs(~scores$race_group+scores$segment,data=scores)

scores$race2 <-scores$race_group
scores$race2[scores$race_group=="Hispanic"]<-"Other"
xtabs(~scores$race_group+scores$race2,data=scores)

#Use race2 to recalc projection!
#Need to recreate segments first with Hispanic + Other Collapsed

#
##
###
####

scores$segment2[scores$sex=="M" & scores$race2=="Caucasian" & scores$age_group=="18-34"] <-0
scores$segment2[scores$sex=="M" & scores$race2=="Caucasian" & scores$age_group=="35-49"] <-1
scores$segment2[scores$sex=="M" & scores$race2=="Caucasian" & scores$age_group=="50-64"] <-2
scores$segment2[scores$sex=="M" & scores$race2=="Caucasian" & scores$age_group=="65+"] <-3
scores$segment2[scores$sex=="F" & scores$race2=="Caucasian" & scores$age_group=="18-34"] <-4
scores$segment2[scores$sex=="F" & scores$race2=="Caucasian" & scores$age_group=="35-49"] <-5
scores$segment2[scores$sex=="F" & scores$race2=="Caucasian" & scores$age_group=="50-64"] <-6
scores$segment2[scores$sex=="F" & scores$race2=="Caucasian" & scores$age_group=="65+"] <-7
scores$segment2[scores$sex=="M" & scores$race2=="African-American" & scores$age_group=="18-34"] <-8
scores$segment2[scores$sex=="M" & scores$race2=="African-American" & scores$age_group=="35-49"] <-9
scores$segment2[scores$sex=="M" & scores$race2=="African-American" & scores$age_group=="50-64"] <-10
scores$segment2[scores$sex=="M" & scores$race2=="African-American" & scores$age_group=="65+"] <-11
scores$segment2[scores$sex=="F" & scores$race2=="African-American" & scores$age_group=="18-34"] <-12
scores$segment2[scores$sex=="F" & scores$race2=="African-American" & scores$age_group=="35-49"] <-13
scores$segment2[scores$sex=="F" & scores$race2=="African-American" & scores$age_group=="50-64"] <-14
scores$segment2[scores$sex=="F" & scores$race2=="African-American" & scores$age_group=="65+"] <-15
scores$segment2[scores$sex=="M" & scores$race2=="Other" & scores$age_group=="18-34"] <-16
scores$segment2[scores$sex=="M" & scores$race2=="Other" & scores$age_group=="35-49"] <-17
scores$segment2[scores$sex=="M" & scores$race2=="Other" & scores$age_group=="50-64"] <-18
scores$segment2[scores$sex=="M" & scores$race2=="Other" & scores$age_group=="65+"] <-19
scores$segment2[scores$sex=="F" & scores$race2=="Other" & scores$age_group=="18-34"] <-20
scores$segment2[scores$sex=="F" & scores$race2=="Other" & scores$age_group=="35-49"] <-21
scores$segment2[scores$sex=="F" & scores$race2=="Other" & scores$age_group=="50-64"] <-22
scores$segment2[scores$sex=="F" & scores$race2=="Other" & scores$age_group=="65+"] <-23

xtabs(~scores$segment2+scores$race2,data=scores)
xtabs(~scores$segment2+scores$sex,data=scores)
xtabs(~scores$segment2+scores$age_group,data=scores)

#New Projection
#Create a dataframe of average support score by segment2

scored_means2 <-tapply(scores$support_sept15, scores$segment2, mean) #Creates array of means
y2_name <- "scored_means2"
x2 <-c(0:23) #Creates array from 0 to 23
x2_name <- "segment2"
scored_means_df2 <- data.frame(x2,scored_means2) #Combine arrays into data frame
names(scored_means_df2) <- c(x2_name,y2_name)
print(scored_means_df2)

write.csv(scored_means_df2, "Projection_avg_support_scores.csv")
