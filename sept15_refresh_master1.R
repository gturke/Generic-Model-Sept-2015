#R Code for --SAMPLE-- September 2015 Model Refresh
#09/09/2015

#Import libraries needed for modelling:
library(rJava)
library(RStudioAMI) #Use to read csvs more efficiently
#library(Amelia) #files already multiply imputed
library(MASS)
library(stringr)
library(parallel)
library(grDevices)
library(grid)

#Must install packages before adding, only do once:
#install.packages("binomTools")
#install.packages("leaps")
#install.packages("gains")
#install.packages("pROC")
#install.packages("glmnet")
#install.packages("caret")
#install.packages("Hmisc")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("ggthemes")
#install.packages("glmulti")

library(glmulti)
library(binomTools)
library(leaps)
library(gains)
library(pROC)
library(glmnet)
library(caret)
library(Hmisc)
library(ggplot2)
library(ggthemes)
library(scales)

setwd("/home/rstudio")
getwd()

#Read data and clean for use########################################################################
#Read in imputed files:
#ex_vf <- read.csv("skinny_exvf.csv")
#head(ex_vf)
ex5 <- read.csv("consolidated_mi_5.csv") #1.1 GB -- too big for this instance?? - NOPE :D
ex4 <- read.csv("consolidated_mi_4.csv")
ex3 <- read.csv("consolidated_mi_3.csv")
ex2 <- read.csv("consolidated_mi_2.csv")
ex1 <- read.csv("consolidated_mi_1.csv")

#Read validated responses (already validated in python)
survey <- read.csv("validated_responses.csv")

#Next steps:
#Subsets for survey models
#Variable evaluation
#Test & train models
#Finalize models
#Compile and export scores

set.seed(862518965) 

colnames(ex1)
colnames(survey)

sapply(ex1,class)
sapply(survey,class)

survey <-survey[,c(3,11)]

ex1 <-ex1[,-c(1,2,4,24)]
ex2 <-ex2[,-c(1,2,4,24)]
ex3 <-ex3[,-c(1,2,4,24)]
ex4 <-ex4[,-c(1,2,4,24)]
ex5 <-ex5[,-c(1,2,4,24)]

#Reclassify variables as needed:
ex1$vb_household_income_range <- as.character(ex1$vb_household_income_range)
ex1$vb_household_income_range[ex1$vb_household_income_range=="A"] <- 1
ex1$vb_household_income_range[ex1$vb_household_income_range=="B"] <- 2
ex1$vb_household_income_range[ex1$vb_household_income_range=="C"] <- 3
ex1$vb_household_income_range[ex1$vb_household_income_range=="D"] <- 4
ex1$vb_household_income_range[ex1$vb_household_income_range=="E"] <- 5
ex1$vb_household_income_range[ex1$vb_household_income_range=="F"] <- 6
ex1$vb_household_income_range[ex1$vb_household_income_range=="G"] <- 7
ex1$vb_household_income_range[ex1$vb_household_income_range=="H"] <- 8
ex1$vb_household_income_range[ex1$vb_household_income_range=="I"] <- 9
ex1$vb_household_income_range[ex1$vb_household_income_range=="J"] <- 10
ex1$vb_household_income_range[ex1$vb_household_income_range=="K"] <- 11
ex1$vb_household_income_range[ex1$vb_household_income_range=="L"] <- 12
ex1$vb_household_income_range[ex1$vb_household_income_range=="M"] <- 13
ex1$vb_household_income_range[ex1$vb_household_income_range=="N"] <- 14
ex1$vb_household_income_range <- as.numeric(ex1$vb_household_income_range)

ex1$race2 <-ex1$race_group
#xtabs(~ex_vf$racename+ex_vf$sex, data=ex_vf)
xtabs(~ex1$race_group+ex1$race2, data=ex1)

ex1$race2[ex1$race_group=="Hispanic"] <-"Other"
ex1$race2 <- as.factor(as.character(ex1$race2))
xtabs(~ex1$race_group+ex1$race2, data=ex1)

##

ex2$vb_household_income_range <- as.character(ex2$vb_household_income_range)
ex2$vb_household_income_range[ex2$vb_household_income_range=="A"] <- 1
ex2$vb_household_income_range[ex2$vb_household_income_range=="B"] <- 2
ex2$vb_household_income_range[ex2$vb_household_income_range=="C"] <- 3
ex2$vb_household_income_range[ex2$vb_household_income_range=="D"] <- 4
ex2$vb_household_income_range[ex2$vb_household_income_range=="E"] <- 5
ex2$vb_household_income_range[ex2$vb_household_income_range=="F"] <- 6
ex2$vb_household_income_range[ex2$vb_household_income_range=="G"] <- 7
ex2$vb_household_income_range[ex2$vb_household_income_range=="H"] <- 8
ex2$vb_household_income_range[ex2$vb_household_income_range=="I"] <- 9
ex2$vb_household_income_range[ex2$vb_household_income_range=="J"] <- 10
ex2$vb_household_income_range[ex2$vb_household_income_range=="K"] <- 11
ex2$vb_household_income_range[ex2$vb_household_income_range=="L"] <- 12
ex2$vb_household_income_range[ex2$vb_household_income_range=="M"] <- 13
ex2$vb_household_income_range[ex2$vb_household_income_range=="N"] <- 14
ex2$vb_household_income_range <- as.numeric(ex2$vb_household_income_range)

ex2$race2 <-ex2$race_group
#xtabs(~ex_vf$racename+ex_vf$sex, data=ex_vf)
xtabs(~ex2$race_group+ex2$race2, data=ex2)

ex2$race2[ex2$race_group=="Hispanic"] <-"Other"
ex2$race2 <- as.factor(as.character(ex2$race2))
xtabs(~ex2$race_group+ex2$race2, data=ex2)

##

ex3$vb_household_income_range <- as.character(ex3$vb_household_income_range)
ex3$vb_household_income_range[ex3$vb_household_income_range=="A"] <- 1
ex3$vb_household_income_range[ex3$vb_household_income_range=="B"] <- 2
ex3$vb_household_income_range[ex3$vb_household_income_range=="C"] <- 3
ex3$vb_household_income_range[ex3$vb_household_income_range=="D"] <- 4
ex3$vb_household_income_range[ex3$vb_household_income_range=="E"] <- 5
ex3$vb_household_income_range[ex3$vb_household_income_range=="F"] <- 6
ex3$vb_household_income_range[ex3$vb_household_income_range=="G"] <- 7
ex3$vb_household_income_range[ex3$vb_household_income_range=="H"] <- 8
ex3$vb_household_income_range[ex3$vb_household_income_range=="I"] <- 9
ex3$vb_household_income_range[ex3$vb_household_income_range=="J"] <- 10
ex3$vb_household_income_range[ex3$vb_household_income_range=="K"] <- 11
ex3$vb_household_income_range[ex3$vb_household_income_range=="L"] <- 12
ex3$vb_household_income_range[ex3$vb_household_income_range=="M"] <- 13
ex3$vb_household_income_range[ex3$vb_household_income_range=="N"] <- 14
ex3$vb_household_income_range <- as.numeric(ex3$vb_household_income_range)

ex3$race2 <-ex3$race_group
#xtabs(~ex_vf$racename+ex_vf$sex, data=ex_vf)
xtabs(~ex3$race_group+ex3$race2, data=ex3)

ex3$race2[ex3$race_group=="Hispanic"] <-"Other"
ex3$race2 <- as.factor(as.character(ex3$race2))
xtabs(~ex3$race_group+ex3$race2, data=ex3)

##

ex4$vb_household_income_range <- as.character(ex4$vb_household_income_range)
ex4$vb_household_income_range[ex4$vb_household_income_range=="A"] <- 1
ex4$vb_household_income_range[ex4$vb_household_income_range=="B"] <- 2
ex4$vb_household_income_range[ex4$vb_household_income_range=="C"] <- 3
ex4$vb_household_income_range[ex4$vb_household_income_range=="D"] <- 4
ex4$vb_household_income_range[ex4$vb_household_income_range=="E"] <- 5
ex4$vb_household_income_range[ex4$vb_household_income_range=="F"] <- 6
ex4$vb_household_income_range[ex4$vb_household_income_range=="G"] <- 7
ex4$vb_household_income_range[ex4$vb_household_income_range=="H"] <- 8
ex4$vb_household_income_range[ex4$vb_household_income_range=="I"] <- 9
ex4$vb_household_income_range[ex4$vb_household_income_range=="J"] <- 10
ex4$vb_household_income_range[ex4$vb_household_income_range=="K"] <- 11
ex4$vb_household_income_range[ex4$vb_household_income_range=="L"] <- 12
ex4$vb_household_income_range[ex4$vb_household_income_range=="M"] <- 13
ex4$vb_household_income_range[ex4$vb_household_income_range=="N"] <- 14
ex4$vb_household_income_range <- as.numeric(ex4$vb_household_income_range)

ex4$race2 <-ex4$race_group
#xtabs(~ex_vf$racename+ex_vf$sex, data=ex_vf)
xtabs(~ex4$race_group+ex4$race2, data=ex4)

ex4$race2[ex4$race_group=="Hispanic"] <-"Other"
ex4$race2 <- as.factor(as.character(ex4$race2))
xtabs(~ex4$race_group+ex4$race2, data=ex4)

##

ex5$vb_household_income_range <- as.character(ex5$vb_household_income_range)
ex5$vb_household_income_range[ex5$vb_household_income_range=="A"] <- 1
ex5$vb_household_income_range[ex5$vb_household_income_range=="B"] <- 2
ex5$vb_household_income_range[ex5$vb_household_income_range=="C"] <- 3
ex5$vb_household_income_range[ex5$vb_household_income_range=="D"] <- 4
ex5$vb_household_income_range[ex5$vb_household_income_range=="E"] <- 5
ex5$vb_household_income_range[ex5$vb_household_income_range=="F"] <- 6
ex5$vb_household_income_range[ex5$vb_household_income_range=="G"] <- 7
ex5$vb_household_income_range[ex5$vb_household_income_range=="H"] <- 8
ex5$vb_household_income_range[ex5$vb_household_income_range=="I"] <- 9
ex5$vb_household_income_range[ex5$vb_household_income_range=="J"] <- 10
ex5$vb_household_income_range[ex5$vb_household_income_range=="K"] <- 11
ex5$vb_household_income_range[ex5$vb_household_income_range=="L"] <- 12
ex5$vb_household_income_range[ex5$vb_household_income_range=="M"] <- 13
ex5$vb_household_income_range[ex5$vb_household_income_range=="N"] <- 14
ex5$vb_household_income_range <- as.numeric(ex5$vb_household_income_range)

ex5$race2 <-ex5$race_group
#xtabs(~ex_vf$racename+ex_vf$sex, data=ex_vf)
xtabs(~ex5$race_group+ex5$race2, data=ex5)

ex5$race2[ex5$race_group=="Hispanic"] <-"Other"
ex5$race2 <- as.factor(as.character(ex5$race2))
xtabs(~ex5$race_group+ex5$race2, data=ex5)

#Merge to survey data, create training & testing subsets########################################################################

#Inner join survey data to analytics data
#responses.1 <- merge(mi.file.1,completes,"vanid")
responses1 <- merge(ex1,survey,"vanid")
responses2 <- merge(ex2,survey,"vanid")
responses3 <- merge(ex3,survey,"vanid")
responses4 <- merge(ex4,survey,"vanid")
responses5 <- merge(ex5,survey,"vanid")

#Randomly sort sets before assigning to training and testing sets
responses1 <- responses1[sample(1:nrow(responses1), nrow(responses1), replace = F),]
responses2 <- responses2[sample(1:nrow(responses2), nrow(responses2), replace = F),]
responses3 <- responses3[sample(1:nrow(responses3), nrow(responses3), replace = F),]
responses4 <- responses4[sample(1:nrow(responses4), nrow(responses4), replace = F),]
responses5 <- responses5[sample(1:nrow(responses5), nrow(responses5), replace = F),]

#training.1 <- responses.1[1:(nrow(responses.1)*.8),]
#testing.1 <- responses.1[((nrow(responses.1)*.8)+1):nrow(responses.1),]

#Subsets for survey models
#Assign 80% of responses to training set and 20% to testing set
#training 1 = rows 1 to n*8
training1 <- responses1[1:(nrow(responses1)*.8),]
training2 <- responses2[1:(nrow(responses2)*.8),]
training3 <- responses3[1:(nrow(responses3)*.8),]
training4 <- responses4[1:(nrow(responses4)*.8),]
training5 <- responses5[1:(nrow(responses5)*.8),]

#testing = rows n*8 + 1 to n
testing1 <- responses1[((nrow(responses1)*.8)+1):nrow(responses1),]
testing2 <- responses2[((nrow(responses2)*.8)+1):nrow(responses2),]
testing3 <- responses3[((nrow(responses3)*.8)+1):nrow(responses3),]
testing4 <- responses4[((nrow(responses4)*.8)+1):nrow(responses4),]
testing5 <- responses5[((nrow(responses5)*.8)+1):nrow(responses5),]

#Variable evaluation########################################################################
test.1 <- ex1
test.1 <- merge(test.1,survey,"vanid") #Merge in survey data to test data set

#Run a simple GLM regression model to evaluate variables for inclusion in glmulti eval
var_eval_model <-glm(
  support ~
    registered + clarity_tnt_2014 + age + race_group + sex + claritynatparty3 + 
    vb_voterbase_marital_status + gsyn_synth_hh_average_age + 
    gsyn_synth_hh_sum_rep_primary_votes_cast_count + 
    ib_ibx_hh_8621_travel_and_entertainment_card_holder + ib_ibx_ind_8640_number_of_sources + 
    ib_ibx_hh_8652_generations_in_household + ib_ibx_hh_8628_number_of_adults + 
    ib_ibx_hh_8629_household_size + ib_ibx_hh_9153_standard_specialty_furniture_buyers + 
    ib_ibx_hh_9153_standard_retail_main_street_retail + apt_yes + vb_household_income_range + 
    tb_family_composition_cd,
  data = test.1,
  family=binomial(logit)
)

var_eval_step <-stepAIC(var_eval_model)
summary(var_eval_model)
summary(var_eval_step)

#Re-run basic GLM with recoded race variable
var_eval_model2 <-glm(
  support ~
    registered + clarity_tnt_2014 + age + race2 + sex + claritynatparty3 + 
    vb_voterbase_marital_status + gsyn_synth_hh_average_age + 
    gsyn_synth_hh_sum_rep_primary_votes_cast_count + 
    ib_ibx_hh_8621_travel_and_entertainment_card_holder + ib_ibx_ind_8640_number_of_sources + 
    ib_ibx_hh_8652_generations_in_household + ib_ibx_hh_8628_number_of_adults + 
    ib_ibx_hh_8629_household_size + ib_ibx_hh_9153_standard_specialty_furniture_buyers + 
    ib_ibx_hh_9153_standard_retail_main_street_retail + apt_yes + vb_household_income_range + 
    tb_family_composition_cd,
  data = test.1,
  family=binomial(logit)
)

var_eval_step2 <-stepAIC(var_eval_model2)
summary(var_eval_model2)
summary(var_eval_step2)

##Check for interactions with glmulti()
#GLMULTI 1
var_eval_model_multi <- glmulti(
  support ~
    age + 
    sex + 
    claritynatparty3 +
    vb_voterbase_marital_status +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count + 
    ib_ibx_hh_8628_number_of_adults + 
    ib_ibx_hh_8629_household_size +
    ib_ibx_hh_8628_number_of_adults + 
    ib_ibx_hh_8621_travel_and_entertainment_card_holder +
    race_group + 
    registered,
  data = test.1,
  level=2,
  method="g",
  fitfunction="glm",
  family=binomial,
  report=T,
  plotty=F,
  crit="aicc",
  confsetsize=1) 

summary(var_eval_model_multi@objects[[1]])

#GLMULTI2
var_eval_model_multi2 <- glmulti(
  support ~
    age +
    sex +
    claritynatparty3 +
    vb_voterbase_marital_status +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8621_travel_and_entertainment_card_holder +
    ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8628_number_of_adults +
    ib_ibx_hh_8629_household_size +
    race2 +
    registered +
    vb_household_income_range,
  data = test.1,
  level=2,
  method="g",
  fitfunction="glm",
  family=binomial,
  report=T,
  plotty=F,
  crit="aicc",
  confsetsize=1) 

summary(var_eval_model_multi2@objects[[1]])

#Test revised GLM model with interaction variables

#Test revised GLM
revised_model <- glm(
  support ~
    vb_voterbase_marital_status +
    race2 +
    age +
    ib_ibx_hh_8629_household_size +
    vb_household_income_range +
    vb_voterbase_marital_status:race2 +
    race2:age +
    claritynatparty3:registered +
    ib_ibx_hh_8629_household_size:claritynatparty3 +
    age:ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8629_household_size:sex +
    vb_household_income_range:registered +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count:registered +
    ib_ibx_hh_8628_number_of_adults:gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8629_household_size:gsyn_synth_hh_sum_rep_primary_votes_cast_count,
  data = test.1,
  family=binomial(logit)
)

revised_model_step <-stepAIC(revised_model)
summary(revised_model)
summary(revised_model_step)

###Revision 2
#Nix vb_household_income_range and range * registered
revised_model2 <- glm(
  support ~
    vb_voterbase_marital_status +
    race2 +
    age +
    ib_ibx_hh_8629_household_size +
    vb_voterbase_marital_status:race2 +
    race2:age +
    claritynatparty3:registered +
    ib_ibx_hh_8629_household_size:claritynatparty3 +
    age:ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8629_household_size:sex +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count:registered +
    ib_ibx_hh_8628_number_of_adults:gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8629_household_size:gsyn_synth_hh_sum_rep_primary_votes_cast_count,
  data = test.1,
  family=binomial(logit)
)

revised_model_step2 <-stepAIC(revised_model2)
summary(revised_model2)
summary(revised_model_step2) #step = revised_model2

#Use revised_model2

#Test & train models########################################################################
summary (revised_model2)
Rsq(revised_model2)

roc(training1$support,predict(revised_model2,training1,type="response"))$auc
roc(testing1$support,(predict(revised_model2,testing1,type="response")))$auc

gains(training1$support,predict(revised_model2,training1,type="response"),groups=10)
gains(testing1$support,predict(revised_model2,testing1,type="response"),groups=10)

aggregate(predict(revised_model2,training1,type="response") ~ training1$support,FUN=mean)
aggregate(predict(revised_model2,testing1,type="response") ~ testing1$support, FUN=mean)

#Calculation: (average survey response where predicted support score is GREATER THAT mean score) - 
#               (average survey response where predicted support score is LESS THAN mean score)
mean(testing1$support[predict(revised_model2,testing1,type="response")>mean(predict(revised_model2,testing1,type="response"))]) -
  mean(testing1$support[predict(revised_model2,testing1,type="response")<mean(predict(revised_model2,testing1, type="response"))])

########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################

#Finalize models########################################################################

support_sept15.1 <- glm(
  support ~
    vb_voterbase_marital_status +
    race2 +
    age +
    ib_ibx_hh_8629_household_size +
    vb_voterbase_marital_status:race2 +
    race2:age +
    claritynatparty3:registered +
    ib_ibx_hh_8629_household_size:claritynatparty3 +
    age:ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8629_household_size:sex +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count:registered +
    ib_ibx_hh_8628_number_of_adults:gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8629_household_size:gsyn_synth_hh_sum_rep_primary_votes_cast_count,
  data = training1,
  family=binomial(logit)
)

##

support_sept15.2 <- glm(
  support ~
    vb_voterbase_marital_status +
    race2 +
    age +
    ib_ibx_hh_8629_household_size +
    vb_voterbase_marital_status:race2 +
    race2:age +
    claritynatparty3:registered +
    ib_ibx_hh_8629_household_size:claritynatparty3 +
    age:ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8629_household_size:sex +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count:registered +
    ib_ibx_hh_8628_number_of_adults:gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8629_household_size:gsyn_synth_hh_sum_rep_primary_votes_cast_count,
  data = training2,
  family=binomial(logit)
)


##

support_sept15.3 <- glm(
  support ~
    vb_voterbase_marital_status +
    race2 +
    age +
    ib_ibx_hh_8629_household_size +
    vb_voterbase_marital_status:race2 +
    race2:age +
    claritynatparty3:registered +
    ib_ibx_hh_8629_household_size:claritynatparty3 +
    age:ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8629_household_size:sex +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count:registered +
    ib_ibx_hh_8628_number_of_adults:gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8629_household_size:gsyn_synth_hh_sum_rep_primary_votes_cast_count,
  data = training3,
  family=binomial(logit)
)

##

support_sept15.4 <- glm(
  support ~
    vb_voterbase_marital_status +
    race2 +
    age +
    ib_ibx_hh_8629_household_size +
    vb_voterbase_marital_status:race2 +
    race2:age +
    claritynatparty3:registered +
    ib_ibx_hh_8629_household_size:claritynatparty3 +
    age:ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8629_household_size:sex +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count:registered +
    ib_ibx_hh_8628_number_of_adults:gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8629_household_size:gsyn_synth_hh_sum_rep_primary_votes_cast_count,
  data = training4,
  family=binomial(logit)
)

##

support_sept15.5 <- glm(
  support ~
    vb_voterbase_marital_status +
    race2 +
    age +
    ib_ibx_hh_8629_household_size +
    vb_voterbase_marital_status:race2 +
    race2:age +
    claritynatparty3:registered +
    ib_ibx_hh_8629_household_size:claritynatparty3 +
    age:ib_ibx_ind_8640_number_of_sources +
    ib_ibx_hh_8629_household_size:sex +
    gsyn_synth_hh_sum_rep_primary_votes_cast_count:registered +
    ib_ibx_hh_8628_number_of_adults:gsyn_synth_hh_sum_rep_primary_votes_cast_count +
    ib_ibx_hh_8629_household_size:gsyn_synth_hh_sum_rep_primary_votes_cast_count,
  data = training5,
  family=binomial(logit)
)

#COMPARE TRAINING AND TESTING SETS######################################

#compare training & testing set

##Set 1
roc(training1$support,predict(support_sept15.1,training1,type="response"))$auc
roc(testing1$support,(predict(support_sept15.1,testing1,type="response")))$auc

gains(training1$support,predict(support_sept15.1,training1,type="response"),groups=10)
gains(testing1$support,predict(support_sept15.1,testing1,type="response"),groups=10)

aggregate(predict(support_sept15.1,training1,type="response") ~ training1$support,FUN=mean)
aggregate(predict(support_sept15.1,testing1,type="response") ~ testing1$support, FUN=mean)

#Calculation: (average survey response where predicted support score is GREATER THAT mean score) - 
#               (average survey response where predicted support score is LESS THAN mean score)
mean(testing1$support[predict(support_sept15.1,testing1,type="response")>mean(predict(support_sept15.1,testing1,type="response"))]) -
  mean(testing1$support[predict(support_sept15.1,testing1,type="response")<mean(predict(support_sept15.1,testing1, type="response"))])

##Set 2
roc(training2$support,predict(support_sept15.2,training2,type="response"))$auc
roc(testing2$support,(predict(support_sept15.2,testing2,type="response")))$auc

gains(training2$support,predict(support_sept15.2,training2,type="response"),groups=10)
gains(testing2$support,predict(support_sept15.2,testing2,type="response"),groups=10)

aggregate(predict(support_sept15.2,training2,type="response") ~ training2$support,FUN=mean)
aggregate(predict(support_sept15.2,testing2,type="response") ~ testing2$support, FUN=mean)

#Calculation: (average survey response where predicted support score is GREATER THAT mean score) - 
#               (average survey response where predicted support score is LESS THAN mean score)
mean(testing2$support[predict(support_sept15.2,testing2,type="response")>mean(predict(support_sept15.2,testing2,type="response"))]) -
  mean(testing2$support[predict(support_sept15.2,testing2,type="response")<mean(predict(support_sept15.2,testing2, type="response"))])

##Set 3
roc(training3$support,predict(support_sept15.3,training3,type="response"))$auc
roc(testing3$support,(predict(support_sept15.3,testing3,type="response")))$auc

gains(training3$support,predict(support_sept15.3,training3,type="response"),groups=10)
gains(testing3$support,predict(support_sept15.3,testing3,type="response"),groups=10)

aggregate(predict(support_sept15.3,training3,type="response") ~ training3$support,FUN=mean)
aggregate(predict(support_sept15.3,testing3,type="response") ~ testing3$support, FUN=mean)

#Calculation: (average survey response where predicted support score is GREATER THAT mean score) - 
#               (average survey response where predicted support score is LESS THAN mean score)
mean(testing3$support[predict(support_sept15.3,testing3,type="response")>mean(predict(support_sept15.3,testing3,type="response"))]) -
  mean(testing3$support[predict(support_sept15.3,testing3,type="response")<mean(predict(support_sept15.3,testing3, type="response"))])

##Set 4

roc(training4$support,predict(support_sept15.4,training4,type="response"))$auc
roc(testing4$support,(predict(support_sept15.4,testing4,type="response")))$auc

gains(training4$support,predict(support_sept15.4,training4,type="response"),groups=10)
gains(testing4$support,predict(support_sept15.4,testing4,type="response"),groups=10)

aggregate(predict(support_sept15.4,training4,type="response") ~ training4$support,FUN=mean)
aggregate(predict(support_sept15.4,testing4,type="response") ~ testing4$support, FUN=mean)

#Calculation: (average survey response where predicted support score is GREATER THAT mean score) - 
#               (average survey response where predicted support score is LESS THAN mean score)
mean(testing4$support[predict(support_sept15.4,testing4,type="response")>mean(predict(support_sept15.4,testing4,type="response"))]) -
  mean(testing4$support[predict(support_sept15.4,testing4,type="response")<mean(predict(support_sept15.4,testing4, type="response"))])

##Set 5
roc(training5$support,predict(support_sept15.5,training5,type="response"))$auc
roc(testing5$support,(predict(support_sept15.5,testing5,type="response")))$auc

gains(training5$support,predict(support_sept15.5,training5,type="response"),groups=10)
gains(testing5$support,predict(support_sept15.5,testing5,type="response"),groups=10)

aggregate(predict(support_sept15.5,training5,type="response") ~ training5$support,FUN=mean)
aggregate(predict(support_sept15.5,testing5,type="response") ~ testing5$support, FUN=mean)

#Calculation: (average survey response where predicted support score is GREATER THAT mean score) - 
#               (average survey response where predicted support score is LESS THAN mean score)
mean(testing5$support[predict(support_sept15.5,testing5,type="response")>mean(predict(support_sept15.5,testing5,type="response"))]) -
  mean(testing5$support[predict(support_sept15.5,testing5,type="response")<mean(predict(support_sept15.5,testing5, type="response"))])

#Score dataframes########################################################################

ex1$support_sept15.1 <- predict(support_sept15.1,ex1,type="response")
ex2$support_sept15.2 <- predict(support_sept15.2,ex2,type="response")
ex3$support_sept15.3 <- predict(support_sept15.3,ex3,type="response")
ex4$support_sept15.4 <- predict(support_sept15.4,ex4,type="response")
ex5$support_sept15.5 <- predict(support_sept15.5,ex5,type="response")

#Compile and export scores########################################################################

#Create dataframe of variable only for export called "scores"

scores <- ex1[,c(1,2,4,5,6)] #vanid, reg status, age, race, sex
scores <- merge(scores,ex1[,c(1,22)],"vanid")
scores <- merge(scores,ex2[,c(1,22)],"vanid")
scores <- merge(scores,ex3[,c(1,22)],"vanid")
scores <- merge(scores,ex4[,c(1,22)],"vanid")
scores <- merge(scores,ex5[,c(1,22)],"vanid")

colnames(scores)

#Average 5 scores into 1!
#rowMeans(x, na.rm = FALSE, dims = 1)
#RM<-rowMeans(DF[,2:4])
scores$support_sept15 <- rowMeans(scores[,6:10])
scores <- scores[,-c(6:10)] #Export this file, save dataframe!

write.csv(scores,"--SAMPLE--_supportscores_20150910.csv")
save.image(".RData")
