library(psych)
library(car)
library(QuantPsyc)
library(lmtest)
library(jtools)
library(pwr)
library(ggplot2)

#Dimensional models of Passive Avoidance Task and ANOVA
#RT_reward_hit_mean
RT_reward_hit_mean_scaled_dimensional <-lm(RT_reward_hit_mean_scaled ~ PCA_Abuse*fgroup  + PCA_Neglect*fgroup, Final_Dataset)

### Multicollinearity: 
vif(RT_reward_hit_mean_scaled_dimensional)

### Linearity
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~ PCA_Abuse, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset_no_inf, "PCA_Abuse","RT_reward_hit_mean_scaled", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset_no_inf, "PCA_Neglect","RT_reward_hit_mean_scaled", color = "fgroup")

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(RT_reward_hit_mean_scaled_dimensional)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(RT_reward_hit_mean_scaled_dimensional))) #looks normally distributed
qqnorm(resid(RT_reward_hit_mean_scaled_dimensional)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(RT_reward_hit_mean_scaled_dimensional))) #Standardized Residuals should be <|3| or < |2|

###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(RT_reward_hit_mean_scaled_dimensional) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(RT_reward_hit_mean_scaled_dimensional)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(RT_reward_hit_mean_scaled_dimensional)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf)  

#Rerun analysis without the potentially influencial cases
Dimensional_model_RT_REWARD_AVO_no_inf <- lm(RT_reward_hit_mean_scaled~ fgroup * PCA_Abuse + fgroup * 	PCA_Neglect, data=Final_Dataset[no_inf,])
summary(Dimensional_model_RT_REWARD_AVO_no_inf)


### Linearity
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~ PCA_Abuse, Final_Dataset[no_inf,]) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "PCA_Neglect","RT_reward_hit_mean_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","RT_reward_hit_mean_scaled", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~ PCA_Neglect, Final_Dataset[no_inf,]) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "PCA_Neglect","RT_reward_hit_mean_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","RT_reward_hit_mean_scaled", color = "fgroup")

Final_Model_Dimensional_RT_reward_hit_mean_scaled <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry + ADHD_acute + age , data = Final_Dataset)


#Covariates
##------------------------------------------
# Testing the covariates
#AGE
RT_reward_hit_mean_scaled_dimensional_and_AGE <-lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age, Final_Dataset)
summary(RT_reward_hit_mean_scaled_dimensional_and_AGE)

anova(RT_reward_hit_mean_scaled_dimensional, RT_reward_hit_mean_scaled_dimensional_and_AGE ) #Significant!! (p=0.005123 **)

#IQ (Database; IQ_IMPUTED) is the total score
RT_reward_hit_mean_scaled_dimensional_and_IQ_ <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
#in order to have a comparable number of individuals, I have to subset the initial model by the number of individuals that have been included in the model when I add the covariate

RT_reward_hit_mean_scaled_dimensional_subset <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
#Significant!! (p=3.025e-05 ***)

summary(RT_reward_hit_mean_scaled_dimensional_IQ)

anova(RT_reward_hit_mean_scaled_dimensional_subset, RT_reward_hit_mean_scaled_dimensional_IQ) ##Significant!! (p=3.025e-05 ***)

#ADHD
RT_reward_hit_mean_scaled_dimensional_and_ADHD<- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + ADHD_acute, Final_Dataset[!is.na(Final_Dataset$ADHD_acute),]) 
summary(RT_reward_hit_mean_scaled_dimensional_and_ADHD)

anova(RT_reward_hit_mean_scaled_dimensional, RT_reward_hit_mean_scaled_dimensional_and_ADHD) #NOT significant!!

#Depression
RT_reward_hit_mean_scaled_dimensional_and_DEPRESSION <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + Dep_acute, Final_Dataset)
summary(RT_reward_hit_mean_scaled_dimensional_and_DEPRESSION)

anova(RT_reward_hit_mean_scaled_dimensional, RT_reward_hit_mean_scaled_dimensional_and_DEPRESSION) #Significant!! (p=0.01849 *)

#SESfeb2019withincountry
RT_reward_hit_mean_scaled_dimensional_and_SES <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry,Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])

RT_reward_hit_mean_scaled_dimensional_SES_subset <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),]) 
summary(RT_reward_hit_mean_scaled_dimensional_and_SES)

anova(RT_reward_hit_mean_scaled_dimensional_SES_subset, RT_reward_hit_mean_scaled_dimensional_and_SES) #NOT significant!!

#Gender
RT_reward_hit_mean_scaled_dimensional_and_GENDER <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + gender,Final_Dataset[!is.na(Final_Dataset$gender),])
summary(RT_reward_hit_mean_scaled_dimensional_and_GENDER)

anova(RT_reward_hit_mean_scaled_dimensional, RT_reward_hit_mean_scaled_dimensional_and_GENDER) #NOT significant!

#CU traits (ICU /YPI) 
RT_reward_hit_mean_scaled_dimensional_and_CU_Traits <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(RT_reward_hit_mean_scaled_dimensional_and_CU_Traits)

anova(RT_reward_hit_mean_scaled_dimensional, RT_reward_hit_mean_scaled_dimensional_and_CU_Traits) #NOT significant!!

#-------------------------------------------------------------------------------
#Final Model
Final_Model_Dimensional_RT_reward_hit_mean_scaled <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + Dep_acute + iq_e_total +age, data = Final_Dataset)


##--------------------------------------------------------
#Model of cumulative trauma
#RT_reward_hit_mean across all participants
Cumulative_RT_REWARD_AVO_standardmean_of_trauma <-lm(RT_reward_hit_mean_scaled ~ trauma_standardmean * fgroup, Final_Dataset)

### Multicollinearity: 
vif(Cumulative_RT_REWARD_AVO_standardmean_of_trauma) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled ~ trauma_standardmean, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled ~ trauma_standardmean +I(trauma_standardmean^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled ~ trauma_standardmean+I(trauma_standardmean^2)+I(trauma_standardmean^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) #no model significant

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(Cumulative_RT_REWARD_AVO_standardmean_of_trauma)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(Cumulative_RT_REWARD_AVO_standardmean_of_trauma))) #looks normally distributed
qqnorm(resid(Cumulative_RT_REWARD_AVO_standardmean_of_trauma)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(Cumulative_RT_REWARD_AVO_standardmean_of_trauma))) #Standardized Residuals should be <|3| or < |2|

###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(Cumulative_RT_REWARD_AVO_standardmean_of_trauma) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(Cumulative_RT_REWARD_AVO_standardmean_of_trauma)) 
        
# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.
        
#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(Cumulative_RT_REWARD_AVO_standardmean_of_trauma)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
        no_inf <- names(which(inf==0)); length(no_inf)  
        
#Rerun analysis without the potentially influencial cases
        Cumulative_RT_REWARD_AVO_standardmean_of_trauma_no_inf <- lm(RT_reward_hit_mean_scaled ~ trauma_standardmean * fgroup, data=Final_Dataset[no_inf,])
        summary(model_False_alarm_all_dimensional_no_inf)
        
### Linearity
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled ~ trauma_standardmean, Final_Dataset[no_inf,]) #linear
fit_1<-lm(RT_reward_hit_mean_scaled ~ trauma_standardmean +I(trauma_standardmean^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled ~ trauma_standardmean+I(trauma_standardmean^2)+I(trauma_standardmean^3),Final_Dataset[no_inf,]) #cubic
anova(fit_0,fit_1,fit_2) #no model significant
        
ggscatter(Final_Dataset, "trauma_standardmean","RT_reward_hit_mean_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "trauma_standardmean","RT_reward_hit_mean_scaled", color = "fgroup")
        
#Covariates
##------------------------------------------
# Testing the covariates
#AGE
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_AGE <-lm(RT_reward_hit_mean_scaled ~ fgroup * trauma_standardmean + age, Final_Dataset)
anova(Cumulative_RT_REWARD_AVO_standardmean_of_trauma, Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_AGE ) #significant! (p=5.757e-05 ***)
        
### Linearity for Age
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled ~  age, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled ~  age +I(age^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled ~ age  +I(age^2)+I(age ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant
        
ggscatter(Final_Dataset, "age","False_alarm_all_scaled", color = "fgroup")
        
#IQ (Database; IQ_IMPUTED) is the total score
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_IQ <- lm(RT_reward_hit_mean_scaled  ~ fgroup * trauma_standardmean + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_IQ_subset <- lm(RT_reward_hit_mean_scaled  ~ fgroup * trauma_standardmean, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
        
anova(Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_IQ_subset, Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_IQ ) #Significant!! (p=2.415e-05 ***)
        
### Linearity for IQ
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled ~  iq_e_total, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~  iq_e_total+I(iq_e_total^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled ~ iq_e_total  +I(iq_e_total^2)+I(iq_e_total ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant
        
ggscatter(Final_Dataset, "iq_e_total","RT_reward_hit_mean_scaled", color = "fgroup")
        
#ADHD
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_ADHD<- lm(RT_reward_hit_mean_scaled ~ fgroup * trauma_standardmean + ADHD_acute, Final_Dataset) 
        
anova(Cumulative_RT_REWARD_AVO_standardmean_of_trauma, Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_ADHD) #NOT significant!!
        
### Linearity for ADHD
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~  ADHD_acute, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled ~  ADHD_acute +I(ADHD_acute^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ ADHD_acute  +I(ADHD_acute^2)+I(ADHD_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant
        
ggscatter(Final_Dataset, "ADHD_acute","RT_reward_hit_mean_scaled", color = "fgroup")
        
#Depression
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_DEPRESSION <- lm(RT_reward_hit_mean_scaled   ~ fgroup * trauma_standardmean + Dep_acute, Final_Dataset)
        
anova(Cumulative_RT_REWARD_AVO_standardmean_of_trauma, Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_DEPRESSION) #NOT significant!
        
### Linearity for Depression
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~  Dep_acute, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~ Dep_acute +I(Dep_acute^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ Dep_acute  +I(Dep_acute^2)+I(Dep_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant
        
ggscatter(Final_Dataset, "Dep_acute","RT_reward_hit_mean_scaled", color = "fgroup")
        
#SESfeb2019withincountry
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_SES_subset <- lm(RT_reward_hit_mean_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_SES <- lm(RT_reward_hit_mean_scaled  ~ fgroup * trauma_standardmean + SESfeb2019withincountry,Final_Dataset)
        
anova(Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_SES_subset, Cumulative_RT_REWARD_AVO_standardmean_of_trauma_all_and_SES) #NOT significant!!
        
 ### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~  SESfeb2019withincountry, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~  SESfeb2019withincountry +I(SESfeb2019withincountry^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ SESfeb2019withincountry  +I(SESfeb2019withincountry^2)+I(SESfeb2019withincountry ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant
        
ggscatter(Final_Dataset, "SESfeb2019withincountry","RT_reward_hit_mean_scaled", color = "fgroup")
        
#Gender
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_GENDER <- lm(RT_reward_hit_mean_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset)
summary(Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_GENDER)

anova(Cumulative_RT_REWARD_AVO_standardmean_of_trauma, Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_GENDER) #NOT significant!
        
### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~  gender, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~  gender +I(gender^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~  gender  +I(gender^2)+I(gender ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant
        
#CU traits (ICU /YPI) 
Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_CU_Traits <- lm(RT_reward_hit_mean_scaled ~ fgroup * trauma_standardmean + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_CU_Traits)

anova(Cumulative_RT_REWARD_AVO_standardmean_of_trauma, Cumulative_RT_REWARD_AVO_standardmean_of_trauma_and_CU_Traits) #NOT significant!!
        
### Linearity for CU traits
# Linear association predictor - outcome
fit_0<-lm(RT_reward_hit_mean_scaled  ~  total_sum_imp, Final_Dataset) #linear
fit_1<-lm(RT_reward_hit_mean_scaled  ~  total_sum_imp +I(total_sum_imp^2),Final_Dataset) #quadratic
fit_2<-lm(RT_reward_hit_mean_scaled  ~ total_sum_imp  +I(total_sum_imp^2)+I(total_sum_imp ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant
        
ggscatter(Final_Dataset, "total_sum_imp","RT_reward_hit_mean_scaled", color = "fgroup")
Final_Model_Dimensional_RT_reward_hit_mean_scaled <- lm(RT_reward_hit_mean_scaled ~ fgroup * trauma_standardmean + SESfeb2019withincountry + ADHD_acute + age , data = Final_Dataset)

#Final models (cumulative and dimensional) are being tested with ANOVA
Final_Model_Cumulative_RT_reward_hit_mean_scaled <- lm(RT_reward_hit_mean_scaled ~  fgroup * trauma_standardmean  + age  + gender + iq_e_total + total_sum_imp + SESfeb2019withincountry, data = Final_Dataset)
Final_Model_Dimensional_RT_reward_hit_mean_scaled <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + gender + iq_e_total + total_sum_imp +SESfeb2019withincountry, data = Final_Dataset)
        
anova(Final_Model_Cumulative_RT_reward_hit_mean_scaled, Final_Model_Dimensional_RT_reward_hit_mean_scaled)
#no difference, the cumulative model explains this better, since it's less complex than the dimensional model

boxplot_RT_reward_hit_mean_scaled_female_male <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$RT_reward_hit_mean_scaled),], "fgroup","RT_reward_hit_mean_scaled", color = "fgender")
boxplot_RT_reward_hit_mean_scaled_female_male

#-----------------------------------------------------------------------------------------
#final models from above
#with all cases 
Final_Model_Dimensional_RT_reward_hit_mean_scaled  <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total + Dep_acute, data = Final_Dataset)
summary(Final_Model_Dimensional_RT_reward_hit_mean_scaled)

Final_Model_Cumulative_RT_reward_hit_mean_scaled <- lm(RT_reward_hit_mean_scaled ~  fgroup * trauma_standardmean  + age + iq_e_total + Dep_acute, data = Final_Dataset)
summary(Final_Model_Cumulative_RT_reward_hit_mean_scaled)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 

anova(Final_Model_Dimensional_RT_reward_hit_mean_scaled, Final_Model_Cumulative_RT_reward_hit_mean_scaled)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#without influential cases

#Beware of what is the comparison in the noInf cases for each model - here you need to pick the one that exclude more cases, and leave those out in both models 
inf_cum <- apply(ifelse(influence.measures(Final_Model_Cumulative_RT_reward_hit_mean_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_cum <- names(which(inf_cum>0)); length(inf_cum)
no_inf_cum <- names(which(inf_cum==0)); length(no_inf_cum) 

inf <- apply(ifelse(influence.measures(Final_Model_Dimensional_RT_reward_hit_mean_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_dim <- names(which(inf>0)); length(is_inf)
no_inf_dim <- names(which(inf==0)); length(no_inf) 

#you have more influential cases in the Dim model, that is you need to limit your analysis based on the most restrictive model (which in this case is the dimenssional, so we exlucde the 63 that are influential in the dimentional model)
length(is_inf_dim)
length(is_inf_cum)


#now we do the same without the influential cases
#Dimensional model without influential cases
Final_Model_Dimensional_HitRate_all_noInf<- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total + Dep_acute, data = Final_Dataset[no_inf,])
summary(Final_Model_Dimensional_HitRate_all_noInf)

#Cumulative model wihtout influential cases
Final_Model_Cumulative_HitRate_all_scaled_noInf<- lm(RT_reward_hit_mean_scaled ~  fgroup * trauma_standardmean  + age + iq_e_total + Dep_acute, data = Final_Dataset[no_inf,])
summary(Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_noInf)

anova(Final_Model_Dimensional_HitRate_all_noInf,Final_Model_Cumulative_HitRate_all_scaled_noInf)

