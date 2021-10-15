#Descriptive statistics
###Libraries
library(modeest)
library(tidyverse)
library(dplyr)
library(psych)
library(lme4)
library(Reshape2)
library(ggplot2)
library(ggpubr)

setwd("/Users/agnesdetert/Desktop/Agnes/")
Final_Dataset <- read.csv("~/Desktop/Agnes/Final_Data.csv",sep=",", dec=",", header=TRUE)
save(Final_Dataset,  file="Final_Dataset.RData")#save dataset in the format of a file#

#clean up the dataset and delete some unnecessary variables from questionnaires
colnames(Final_Dataset)

Final_Dataset[,c("ypi1","ypi2","ypi3","ypi4","ypi5", "ypi6","ypi7","ypi8","ypi9",
                 "ypi10", "ypi11", "ypi12","ypi13","ypi14","ypi15","ypi16","ypi17","ypi18", 
                 "ypi19", "ypi20", "ypi21", "ypi22","ypi23", "ypi24","ypi25","ypi26","ypi27", 
                 "ypi28", "ypi29", "ypi30","ypi31", "ypi32","ypi33","ypi34","ypi35","ypi36",
                 "ypi37","ypi38","ypi39","ypi40","ypi41","ypi42","ypi43", "ypi44", "ypi45","ypi46",
                 "ypi47","ypi48","ypi49","ypi50")] <- list(NULL)

Final_Dataset[,c("ctq1",                           
                 "ctq2",                            "ctq3",                           
                 "ctq4",                            "ctq5",                           
                 "ctq6",                            "ctq7",                           
                 "ctq8",                            "ctq9" ,                          
                 "ctq10",                           "ctq11",                          
                 "ctq12",                           "ctq13",                          
                 "ctq14",                           "ctq15",                          
                 "ctq16",                           "ctq17",                          
                 "ctq18",                           "ctq19",                          
                 "ctq20",                           "ctq21",                          
                 "ctq22",                           "ctq23",                          
                 "ctq24",                           "ctq25",                          
                 "ctq26",                           "ctq27",                          
                 "ctq28")] <- list(NULL)


Final_Dataset[,c("dr1b",                            "dr1c",                           
                 "dr1d",                            "dr1e",                           
                 "dr1f",                            "dr2" ,                           
                 "dr2a",                            "dr2b",                           
                 "dr2c",                            "dr4" ,                           
                 "dr5",                             "dr6a",                           
                 "dr6b",                            "dr6c",                           
                 "dr6d",                            "dr6e",                           
                 "dr7a",                            "dr7b",                           
                 "dr7c",                            "dr7d",                           
                 "dr7e",                            "dr8" ,                           
                 "dr9",                             "dr10",                           
                 "dr11a",                           "dr11b",                          
                 "dr11c",                           "dr11d")] <- list(NULL)                          

colnames(Final_Dataset)

Final_Dataset[,c("gender.x","gender.y" )] <- list(NULL)
Final_Dataset[,c("rctyn","instno","expwp5" )] <- list(NULL)
Final_Dataset[,c("group.1", "yob.y", "gender.y", "yob.x")] <- list(NULL)

Final_Dataset[,c("icuca1",                         
                 "icuca2",                          "icuca3",                         
                 "icuca4",                          "icuca5",                         
                 "icuca6",                          "icuca7",                         
                 "icuca8",                          "icuca9",                         
                 "icuca10",                         "icuca11",                        
                 "icuca12",                         "icuca13",                        
                 "icuca14",                         "icuca15",                        
                 "icuca16",                         "icuca17",                        
                 "icuca18",                         "icuca19",                        
                 "icuca20",                         "icuca21",                       
                 "icuca22",                         "icuca23",                        
                 "icuca24")] <- list(NULL) 

Final_Dataset[,c("ctq1",                           
                 "ctq2",                            "ctq3",                           
                 "ctq4",                            "ctq5",                           
                 "ctq6",                            "ctq7",                           
                 "ctq8",                            "ctq9",                           
                 "ctq10",                           "ctq11",                          
                 "ctq12",                           "ctq13",                          
                 "ctq14",                           "ctq15",                          
                 "ctq16",                           "ctq17",                          
                 "ctq18",                           "ctq19",                          
                 "ctq20",                           "ctq21",                          
                 "ctq22",                           "ctq23",                          
                 "ctq24",                           "ctq25",                          
                 "ctq26",                           "ctq27",                          
                 "ctq28")] <- list(NULL) 

Final_Dataset[,c("cbcl_rawscore_1",                
                 "cbcl_rawscore_2",              
                 "cbcl_rawscore_3",                
                 "cbcl_rawscore_4",              
                 "cbcl_rawscore_5",                
                 "cbcl_rawscore_6",              
                 "cbcl_rawscore_7",                
                 "cbcl_rawscore_8",                
                 "cbcl_rawscore_9",                
                 "cbcl_rawscore_10",               
                 "cbcl_rawscore_11")] <- list(NULL) 



Final_Dataset[,c("compl.y")] <- list(NULL) 

Final_Dataset[,c("yob.x","yob.y")] <- list(NULL) 

#Create factors for the df 
Final_Dataset$fCD_acute<- factor(Final_Dataset$CD_acute, labels=c("NO_CD","CD")) #Faktor generieren
Final_Dataset$fADHD_acute<- factor(Final_Dataset$ADHD_acute, labels=c("NO_ADHD","ADHD")) #Faktor generieren
Final_Dataset$fDep_acute<- factor(Final_Dataset$Dep_acute, labels=c("NO_Dep","Dep")) #Faktor generieren
Final_Dataset$fgender <- as.factor(Final_Dataset$gender)
levels(Final_Dataset$fgender)[levels(Final_Dataset$fgender) == 1] <- "female"
levels(Final_Dataset$fgender)[levels(Final_Dataset$fgender) == 2] <- "male"
levels(Final_Dataset$fgroup)[levels(Final_Dataset$fgroup) == 1] <- "Case"
levels(Final_Dataset$fgroup)[levels(Final_Dataset$fgroup) == 2] <- "Control"
Final_Dataset$fgroup<- factor(Final_Dataset$group, labels=c("Case","Control"))
Final_Dataset$fgroup<- factor(Final_Dataset$group, labels=c("Female","Male")) #Faktor generieren

#scale the relevant variables: HitRate_all, False Alarm all
Final_Dataset$HitRate_all_scaled <- scale(Final_Dataset$HitRate_all)
Final_Dataset$False_alarm_scaled <- scale(Final_Dataset$FARate_all)
Final_Dataset$RT_reward_hit_mean_scaled <- scale(Final_Dataset$RT_reward_hit_mean)
Final_Dataset$RT_avoidance_errors_mean_scaled <- scale(Final_Dataset$RT_avoidance_errors_mean)
Final_Dataset$reap_sum_imp_scaled <- scale(Final_Dataset$reap_sum_imp)
Final_Dataset$suppr_sum_imp_scaled <- scale(Final_Dataset$suppr_sum_imp)
Final_Dataset$Avoidance_errorSum_error_rate_scaled <- scale(Final_Dataset$Avoidance_errorSum_error_rate)

### explore the data
View(Final_Dataset) # 
names(Final_Dataset)# 
colnames(Final_Dataset) # 
rownames(Final_Dataset) #
str(Final_Dataset)

#summary statistics of data
summary(Final_Dataset)
describe(Final_Dataset)

### Classic functions
# Numeric Variables
mean(Final_Dataset$age, na.rm=TRUE) # Mean
sum(Final_Dataset$age, na.rm=TRUE) # Sum
median(Final_Dataset$age, na.rm=TRUE) # Median
sd(Final_Dataset$age, na.rm=TRUE) # SD
var(Final_Dataset$age, na.rm=TRUE) # Variance
range(Final_Dataset$age, na.rm=TRUE) #Range
min(Final_Dataset$age, na.rm=TRUE) #Minimum
max(Final_Dataset$age, na.rm=TRUE) #Maximum
quantile(Final_Dataset$age,na.rm=TRUE) #Quantile (Standard: 25% P.)
quantile(Final_Dataset$age, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)

se <- sd(Final_Dataset$age, na.rm=TRUE)/sqrt(length(Final_Dataset$age)) #Standard-Fehler
m <- mean(Final_Dataset$age, na.rm=TRUE) #Mean
m + 1.96*se #upper 95%-Konfidenzintervall
m - 1.96*se #lower 95%-Konfidenzintervall

#general summary of age
summary(Final_Dataset$age)

age <- c(Final_Dataset$age); class(age)
prop_table_age_only<- table(Final_Dataset$age)
prop_table_age_only
prop.table(prop_table_age_only) 

#visualization in form of a histogram
hist(Final_Dataset$age)
plot_age <- gghistogram(Final_Dataset[!is.na(Final_Dataset$age),], x= "age", size = .5, stat="count", color = "age", fill = "age", na.rm = T)
plot_age

#descriptive statistics; IQ
# Numeric Variables
mean(Final_Dataset$iq_e_total, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$iq_e_total, na.rm=TRUE) # Summe
median(Final_Dataset$iq_e_total, na.rm=TRUE) # Median
sd(Final_Dataset$iq_e_total, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$iq_e_total, na.rm=TRUE) # Varianz
range(Final_Dataset$iq_e_total, na.rm=TRUE) #Range
min(Final_Dataset$iq_e_total, na.rm=TRUE) #Minimum
max(Final_Dataset$iq_e_total, na.rm=TRUE) #Maximum
quantile(Final_Dataset$iq_e_total,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$iq_e_total, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; SES
# Numeric Variables
mean(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Summe
median(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Median
sd(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Varianz
range(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) #Range
min(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) #Minimum
max(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) #Maximum
quantile(Final_Dataset$SESfeb2019withincountry,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; False Alarms all
# Numeric Variables
mean(Final_Dataset$FARate_all, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$FARate_all, na.rm=TRUE) # Summe
median(Final_Dataset$FARate_all, na.rm=TRUE) # Median
sd(Final_Dataset$FARate_all, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$FARate_all, na.rm=TRUE) # Varianz
range(Final_Dataset$FARate_all, na.rm=TRUE) #Range
min(Final_Dataset$FARate_all, na.rm=TRUE) #Minimum
max(Final_Dataset$FARate_all, na.rm=TRUE) #Maximum
quantile(Final_Dataset$FARate_all,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$FARate_all, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; Hit Rate all
# Numeric Variables
mean(Final_Dataset$HitRate_all, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$HitRate_all, na.rm=TRUE) # Summe
median(Final_Dataset$HitRate_all, na.rm=TRUE) # Median
sd(Final_Dataset$HitRate_all, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$HitRate_all, na.rm=TRUE) # Varianz
range(Final_Dataset$HitRate_all, na.rm=TRUE) #Range
min(Final_Dataset$HitRate_all, na.rm=TRUE) #Minimum
max(Final_Dataset$HitRate_all, na.rm=TRUE) #Maximum
quantile(Final_Dataset$HitRate_all,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$HitRate_all, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; Avoidance_error_Sum_error_rate
# Numeric Variables
mean(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Summe
median(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Median
sd(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Varianz
range(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) #Range
min(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) #Minimum
max(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) #Maximum
quantile(Final_Dataset$Avoidance_error_Sum_error_rate,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; RT_reward_hit_mean
# Numeric Variables
mean(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Summe
median(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Median
sd(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Varianz
range(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) #Range
min(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) #Minimum
max(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) #Maximum
quantile(Final_Dataset$RT_reward_hit_mean,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; RT_avoidance_errors_mean
# Numeric Variables
mean(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Mean
sum(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Sum
median(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Median
sd(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Standard Deviation
var(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Varianz
range(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) #Range
min(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) #Minimum
max(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) #Maximum
quantile(Final_Dataset$RT_avoidance_errors_mean,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)

#descriptive genders
gendersum <- summary(Final_Dataset$fgender)
gendersum

prop_table_age_gender <- table(Final_Dataset$age, Final_Dataset$fgender) 
prop.table(prop_table_age_gender) 

prop_table_gender <- table(Final_Dataset$fgender)
prop.table(prop_table_gender) 

describe(Final_Dataset$gender)
Final_Dataset$fgender <- as.factor(Final_Dataset$gender)
summary(Final_Dataset$fgender)
Final_Dataset[is.na(Final_Dataset)]<-NA

table(Final_Dataset$fgender)


#visualization of genders in form of a histogram 
plot_gender <- gghistogram(Final_Dataset[!is.na(Final_Dataset$fgender),], x= "fgender", size = .5, stat="count", color = "fgender", fill = "fgender", na.rm = T)
plot_gender
#again, you need to get rid of the NAs here
plot_gender <- gghistogram(Final_Dataset[!is.na(Final_Dataset$fgender),], x= "fgender", size = .5, stat="density", color = "fgender", fill = "fgender", na.rm = T)
plot_gender


#descriptives of groups; cases & controls
groupsum <- summary(Final_Dataset$fgroup)
groupsum

Final_Dataset$fgroup <- as.factor(Final_Dataset$group)
summary(Final_Dataset$fgroup)

save(Final_Dataset,  file="Final_Dataset.RData")

table(Final_Dataset$fgender, Final_Dataset$trauma_standardmean)
summary(Final_Dataset$n_icu)

#how many percent of participants are cases vs controls?
CD_sum <- table(Final_Dataset$fgroup)
prop.table(CD_sum)

#visualization of groups
plot_group_1 <- gghistogram(Final_Dataset[!is.na(Final_Dataset$fgroup),], x= "fgroup", size = .5, stat="count", color = "fgroup", fill = "fgroup", na.rm = T)
plot_group_2 <- gghistogram(Final_Dataset[!is.na(Final_Dataset$fgroup),], x= "fgroup", size = .5, stat="density", color = "fgroup", fill = "fgroup", na.rm = T)
plot_group_1
plot_group_2

#percentage of groups
fgroup_sum <- table(Final_Dataset$fgroup)
fgroup_sum
prop.table(fgroup_sum)

prop_table_fgroup_gender <- table(Final_Dataset$fgroup, Final_Dataset$fgender) 
prop.table(prop_table_fgroup_gender) 

#Save dataset
save(Final_Dataset,  file="Final_Dataset.RData")                  

#visualization of groups CD
plot_CD <- gghistogram(Final_Dataset[!is.na(Final_Dataset$fCD_acute),], x= "fCD_acute", size = .5, stat="count", color = "fCD_acute", fill = "fCD_acute")
plot_CD 

#visualization of groups ADHD
plot_ADHD <- gghistogram(Final_Dataset[!is.na(Final_Dataset$fADHD_acute),], x= "fADHD_acute", size = .5, stat="count", color = "fADHD_acute", fill = "fADHD_acute")
plot_ADHD

#looking at comorbidities
#both CD and ADHD together
ADHD_CD_table <- table(Final_Dataset$fADHD_acute, Final_Dataset$fCD_acute)
ADHD_CD_table
prop.table(ADHD_CD_table) 

#both CD and Depression together
sum(Final_Dataset$Dep_acute)
Dep_CD_table <- table(Final_Dataset$fDep_acute, Final_Dataset$fCD_acute)
Dep_CD_table
prop.table(Dep_CD_table) 

### Classic functions
# Numeric Variables
mean(Final_Dataset$iq_e_total, na.rm=TRUE) # Mean
median(Final_Dataset$iq_e_total, na.rm=TRUE) # Median
sd(Final_Dataset$iq_e_total, na.rm=TRUE) # SD
var(Final_Dataset$iq_e_total, na.rm=TRUE) # Variance
range(Final_Dataset$iq_e_total, na.rm=TRUE) #Range
min(Final_Dataset$iq_e_total, na.rm=TRUE) #Minimum
max(Final_Dataset$iq_e_total, na.rm=TRUE) #Maximum
quantile(Final_Dataset$iq_e_total,na.rm=TRUE) #Quantile (Standard: 25% P.)
quantile(Final_Dataset$iq_e_total, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)

se <- sd(Final_Dataset$iq_e_total, na.rm=TRUE)/sqrt(length(Final_Dataset$iq_e_total)) #Standard-Fehler
m <- mean(Final_Dataset$iq_e_total, na.rm=TRUE) #Mean
m + 1.96*se #upper 95%-Konfidenzintervall
m - 1.96*se #lower 95%-Konfidenzinterval


#Number of participants
sum(!is.na(Final_Dataset$ADHD_acute) & !is.na(Final_Dataset$trauma_standardmean) & !is.na(Final_Dataset$age))

sum(!is.na(Final_Dataset$ADHD_acute) & !is.na(Final_Dataset$trauma_standardmean) & !is.na(Final_Dataset$age))

sum(!is.na(Final_Dataset$reap_sum_imp) & !is.na(Final_Dataset$trauma_standardmean))

sum(!is.na(Final_Dataset$reap_sum_imp & !is.na(Final_Dataset)))
sum(!is.na(Final_Dataset$suppr_sum_imp))
sum(!is.na(Final_Dataset$reap_sum_imp))


sum(!is.na(Final_Dataset$suppr_sum_imp) & !is.na(Final_Dataset$trauma_standardmean) & !is.na(Final_Dataset$age))


summary(Final_Dataset$age[!is.na(Final_Dataset$reap_sum_imp) & !is.na(Final_Dataset$standardmean_ELA) & (dataset_final$fgroup == "Case") & !is.na(dataset_final$age)])
  
#Anzahl männlich weiblich (für Faktoren):
table(Final_Dataset$fgroup[(Final_Dataset$fgroup == "Case") & !is.na(Final_Dataset$PCA_Cumulative)])
table(Final_Dataset$fgroup[(Final_Dataset$fgroup == "Case") & !is.na(Final_Dataset$PCA_Neglect)])
table(Final_Dataset$fgroup[(Final_Dataset$fgroup == "Case") & !is.na(Final_Dataset$PCA_Abuse)])


table(Final_Dataset$fgroup[(Final_Dataset$fgroup == "Case") & !is.na(Final_Dataset$reap_sum_imp)])
table(Final_Dataset$fgroup[(Final_Dataset$fgroup == "Case") & !is.na(Final_Dataset$suppr_sum_imp)])

table(Final_Dataset$fgroup[(Final_Dataset$fgroup == "Control") & !is.na(Final_Dataset$suppr_sum_imp)])
table(Final_Dataset$fgroup[(Final_Dataset$fgroup == "Control") & !is.na(Final_Dataset$suppr_sum_imp)])


# statistical tests
chisq.test(t2) #Chi-Quadrat Test (nominale Variablen)
t.test(Final_Dataset$CD_acute ~ Final_Dataset$fgender) #T-Test (Unterschiede zwischen 2 Gruppen)
cor.test(Final_Dataset$age, Final_Dataset$CD_acute) #Korrelation (Zusammenhang zwischen 2 Variablen)

cor.test(Final_Dataset$iq_e_total, Final_Dataset$CD_acute)

#descriptive statistics; IQ
# Numeric Variables
mean(Final_Dataset$iq_e_total, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$iq_e_total, na.rm=TRUE) # Summe
median(Final_Dataset$iq_e_total, na.rm=TRUE) # Median
sd(Final_Dataset$iq_e_total, na.rm=TRUE) # Standard deviation
var(Final_Dataset$iq_e_total, na.rm=TRUE) # Variance
range(Final_Dataset$iq_e_total, na.rm=TRUE) #Range
min(Final_Dataset$iq_e_total, na.rm=TRUE) #Minimum
max(Final_Dataset$iq_e_total, na.rm=TRUE) #Maximum
quantile(Final_Dataset$iq_e_total,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$iq_e_total, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; SES
# Numeric Variables
mean(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Summe
median(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Median
sd(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) # Varianz
range(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) #Range
min(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) #Minimum
max(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE) #Maximum
quantile(Final_Dataset$SESfeb2019withincountry,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$SESfeb2019withincountry, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; False Alarms all
# Numeric Variables
mean(Final_Dataset$FARate_all, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$FARate_all, na.rm=TRUE) # Summe
median(Final_Dataset$FARate_all, na.rm=TRUE) # Median
sd(Final_Dataset$FARate_all, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$FARate_all, na.rm=TRUE) # Varianz
range(Final_Dataset$FARate_all, na.rm=TRUE) #Range
min(Final_Dataset$FARate_all, na.rm=TRUE) #Minimum
max(Final_Dataset$FARate_all, na.rm=TRUE) #Maximum
quantile(Final_Dataset$FARate_all,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$FARate_all, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; Hit Rate all
# Numeric Variables
mean(Final_Dataset$HitRate_all, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$HitRate_all, na.rm=TRUE) # Summe
median(Final_Dataset$HitRate_all, na.rm=TRUE) # Median
sd(Final_Dataset$HitRate_all, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$HitRate_all, na.rm=TRUE) # Varianz
range(Final_Dataset$HitRate_all, na.rm=TRUE) #Range
min(Final_Dataset$HitRate_all, na.rm=TRUE) #Minimum
max(Final_Dataset$HitRate_all, na.rm=TRUE) #Maximum
quantile(Final_Dataset$HitRate_all,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$HitRate_all, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; Avoidance_error_Sum_error_rate
# Numeric Variables
mean(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Summe
median(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Median
sd(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) # Varianz
range(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) #Range
min(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) #Minimum
max(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE) #Maximum
quantile(Final_Dataset$Avoidance_error_Sum_error_rate,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$Avoidance_error_Sum_error_rate, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; RT_reward_hit_mean
# Numeric Variables
mean(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Summe
median(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Median
sd(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) # Varianz
range(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) #Range
min(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) #Minimum
max(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE) #Maximum
quantile(Final_Dataset$RT_reward_hit_mean,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$RT_reward_hit_mean, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#descriptive statistics; RT_avoidance_errors_mean
# Numeric Variables
mean(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Mittelwert
sum(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Summe
median(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Median
sd(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Standardabweichung
var(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) # Varianz
range(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) #Range
min(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) #Minimum
max(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE) #Maximum
quantile(Final_Dataset$RT_avoidance_errors_mean,na.rm=TRUE) #Quantile (Standard: 25% Perzentile)
quantile(Final_Dataset$RT_avoidance_errors_mean, na.rm=TRUE, probs = c(0.1, 0.5, 0.8)) #Quantile (Custom)


#Overview of ERQ
table(FINALdata$miss_erq)
sum(FINALdata$miss_erq, na.rm =TRUE)

#cognitive reappraisal
table(FINALdata$reap_sum_imp)
sum(FINALdata$reap_sum_imp, na.rm =TRUE)


table(FINALdata$miss_all_erq)
sum(FINALdata$miss_all_erq, na.rm =TRUE)

#ICU
table(Final_Dataset$total_sum_imp)
sum(Final_Dataset$total_sum_imp, na.rm =TRUE)

table(FINALdata$CD_prev)
table(FINALdata$CD_life)

table(Final_dataset$CD_acute)
table(Final_dataset$ADHD_acute)
table(Final_dataset$Dep_acute)


table(Final_dataset$fCD_acute, Final_dataset$fADHD_acute)

ggplot(Final_dataset, aes(x=fCD_acute, y=fADHD_acute)) + geom_point()

sum(!is.na(Final_Dataset$suppr_sum_imp) & (Final_Dataset$fgroup == "Control"))
sum(!is.na(Final_Dataset$reap_sum_imp) & (Final_Dataset$fgroup == "Control"))

sum(!is.na(Final_Dataset$PCA_Abuse) & (Final_Dataset$fgroup == "Case"))
sum(!is.na(Final_Dataset$PCA_Abuse) & (Final_Dataset$fgroup == "Control"))

sum(!is.na(Final_Dataset$trauma_standardmean) & (Final_Dataset$fgroup == "Case"))
sum(!is.na(Final_Dataset$trauma_standardmean) & (Final_Dataset$fgroup == "Control"))

sum(!is.na(Final_Dataset$HitRate_cognControl) & (Final_Dataset$fgroup == "Case"))
sum(!is.na(Final_Dataset$HitRate_cognControl) & (Final_Dataset$fgroup == "Control"))

sum(!is.na(Final_Dataset$AvoidErrorsSum_allBlocks) & (Final_Dataset$fgroup == "Case"))
sum(!is.na(Final_Dataset$AvoidErrorsSum_allBlocks) & (Final_Dataset$fgroup == "Control"))


#We will check whether there are differences between groups and /or between genders in some of the variables that we know might have an impact
#on aggresive behaviours and maltreatment, for example IQ and SES
#to do that, we can use different tests: 

# == 1 <- "Case"
# == 2 <- "Control"
#this would be using a standard t-test for IQ between the groups
g_1 = Final_Dataset[Final_Dataset$group ==1,]
g_2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(g_1$iq_e_total, g_2$iq_e_total)


#testing for age 
age_g1 = Final_Dataset[Final_Dataset$group ==1,]
age_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(age_g1$age, age_g2$age)

#testing for SES
ses_g1 = Final_Dataset[Final_Dataset$group ==1,]
ses_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(ses_g1$SESfeb2019withincountry, ses_g2$SESfeb2019withincountry)

#testing for CU Traits
cu_g1 = Final_Dataset[Final_Dataset$group ==1,]
cu_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(cu_g1$total_sum_imp, cu_g2$total_sum_imp)

#testing for ADHD
adhd_g1 = Final_Dataset[Final_Dataset$group ==1,]
adhd_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(adhd_g1$ADHD_acute, adhd_g2$ADHD_acute)

#testing for Depression
dep_g1 = Final_Dataset[Final_Dataset$group ==1,]
dep_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(dep_g1$Dep_acute, dep_g2$Dep_acute)

#testing for Gender
gen_g1 = Final_Dataset[Final_Dataset$group ==1,]
gen_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(gen_g1$gender, gen_g2$gender)

#testing for False alarm rates
FA_g1 = Final_Dataset[Final_Dataset$group ==1,]
FA_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(FA_g1$FARate_all, FA_g2$FARate_all)

#testing for Hit Rate All
HIT_g1 = Final_Dataset[Final_Dataset$group ==1,]
HIT_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(HIT_g1$HitRate_all, HIT_g2$HitRate_all)

#testing for sum of avoidance errors 
SUM_g1 = Final_Dataset[Final_Dataset$group ==1,]
SUM_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(SUM_g1$Avoidance_error_Sum_error_rate, SUM_g2$Avoidance_error_Sum_error_rate)

#testing for MRT Reward Hits
MRT_REWARD_g1 = Final_Dataset[Final_Dataset$group ==1,]
MRT_REWARD_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(MRT_REWARD_g1 $RT_reward_hit_mean, MRT_REWARD_g2$RT_reward_hit_mean)

#testing for MRT avoidance_errors 
AVOID_g1 = Final_Dataset[Final_Dataset$group ==1,]
AVOID_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(AVOID_g1$RT_avoidance_errors_mean, AVOID_g2$RT_avoidance_errors_mean)


#testing for expressive suppression
SUPPR_g1 = Final_Dataset[Final_Dataset$group ==1,]
SUPPR_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(SUPPR_g1 $suppr_sum_imp, SUPPR_g2$suppr_sum_imp)

#testing for cognitive reappraisal
REAP_g1 = Final_Dataset[Final_Dataset$group ==1,]
REAP_g2 = Final_Dataset[Final_Dataset$group ==2,]
t.test(REAP_g1$reap_sum_imp, REAP_g2$reap_sum_imp)

#getting all the standard deviations divided per cases
sd(Final_Dataset$age[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$SESfeb2019withincountry[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$total_sum_imp[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$ADHD_acute[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$Dep_acute[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$FARate_all[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$HitRate_all[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$Avoidance_errorSum_error_rate[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$RT_reward_hit_mean[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$RT_avoidance_errors_mean[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$iq_e_total[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$suppr_sum_imp[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
sd(Final_Dataset$reap_sum_imp[Final_Dataset$fgroup == "Case"], na.rm =TRUE)

#getting all the standard deviations divided per controls
sd(Final_Dataset$age[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$SESfeb2019withincountry[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$total_sum_imp[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$ADHD_acute[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$Dep_acute[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$FARate_all[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$HitRate_all[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$Avoidance_errorSum_error_rate[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$RT_reward_hit_mean[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$RT_avoidance_errors_mean[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$iq_e_total[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$suppr_sum_imp[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
sd(Final_Dataset$reap_sum_imp[Final_Dataset$fgroup == "Control"], na.rm =TRUE)


#getting the means
mean(Final_Dataset$Avoidance_errorSum_error_rate[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
mean(Final_Dataset$Avoidance_errorSum_error_rate[Final_Dataset$fgroup == "Control"], na.rm =TRUE)
mean(Final_Dataset$total_sum_imp[Final_Dataset$fgroup == "Case"], na.rm =TRUE)
mean(Final_Dataset$total_sum_imp[Final_Dataset$fgroup == "Control"], na.rm =TRUE)

colnames(Final_Dataset) <- make.unique(names(Final_Dataset))

########creating boxplots for the different variables
#false alarms all
boxplot_False_Alarms_female_male <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$False_alarm_scaled),], "fgroup","False_alarm_scaled", color = "fgender")
boxplot_False_Alarms_female_male

boxplot_False_Alarms_by_group <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$False_alarm_scaled),], "fgroup","False_alarm_scaled", color = "fgroup",xlab = "Groups", ylab = "False Alarms")
boxplot_False_Alarms_by_group

#hit rate all
boxplot_HitRate_all_scaled_by_group <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$HitRate_all_scaled),], "fgroup","HitRate_all_scaled", color = "fgroup")
boxplot_HitRate_all_scaled_by_group

#sum of avoidance errors
boxplot_Avoidance_errorSum_error_rate_scaled_by_group <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$Avoidance_errorSum_error_rate_scaled),], "fgroup","Avoidance_errorSum_error_rate_scaled", color = "fgender", xlab ="Groups", ylab = "Sum of Avoidance Errors Rate")
boxplot_Avoidance_errorSum_error_rate_scaled_by_group

boxplot_RT_reward_hit_mean_scaled_by_group <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$RT_reward_hit_mean_scaled),], "fgroup","RT_reward_hit_mean_scaled", color = "fgender", xlab ="Groups", ylab = "Mean Reaction Time Reward Hits")
boxplot_RT_reward_hit_mean_scaled_by_group

boxplot_RT_avoidance_errors_mean_scaled_by_group <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$RT_avoidance_errors_mean_scaled),], "fgroup","RT_avoidance_errors_mean_scaled", color = "fgender", xlab ="Groups", ylab = "Mean Reaction Time Avoidance Errors")
boxplot_RT_avoidance_errors_mean_scaled_by_group


boxplot_iq_e_total_by_group <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$iq_e_total),], "fgroup","iq_e_total", color = "fgroup")
boxplot_iq_e_total_by_group


#regression lines for each group,create a plot! 
boxplot_cumulative_per_group<- ggboxplot(Final_Dataset[!is.na(Final_Dataset$suppr_sum_imp_scaled),], "fgroup","trauma_standardmean", color = "fgender", xlab = "Groups", ylab = "Cumulative ELA")
boxplot_cumulative_per_group

boxplot_dimensional_abuse_per_group<- ggboxplot(Final_Dataset[!is.na(Final_Dataset$suppr_sum_imp_scaled),], "fgroup","PCA_Abuse", color = "fgender", xlab = "Groups", ylab = "Dimension Abuse")
boxplot_dimensional_abuse_per_group

boxplot_dimensional_neglect_per_group<- ggboxplot(Final_Dataset[!is.na(Final_Dataset$suppr_sum_imp_scaled),], "fgroup","PCA_Neglect", color = "fgender", xlab = "Groups", ylab = "Dimension Neglect")
boxplot_dimensional_neglect_per_group

boxplot_suppr_female_male <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$suppr_sum_imp_scaled),], "fgroup","suppr_sum_imp_scaled", color = "fgender",xlab = "Groups", ylab = "Expressive Suppression")
boxplot_suppr_female_male

boxplot_reap_female_male <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$reap_sum_imp_scaled),], "fgroup","reap_sum_imp_scaled", color = "fgender",xlab = "Groups", ylab = "Cognitive Reappraisal")
boxplot_reap_female_male


