library(psych)
library(car)
library(QuantPsyc)
library(lmtest)
library(jtools)
library(pwr)
library(ggplot2)


#Models of dimensional trauma of GNG and ANOVA
#Hit rate all
model_HitRate_all_dimensional <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * 	PCA_Neglect, Final_Dataset)
summary(model_HitRate_all_dimensional)


### Multicollinearity: 
vif(model_HitRate_all_dimensional) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~ PCA_Abuse, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "PCA_Abuse","HitRate_all_scaled", color = "fgroup")

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(model_HitRate_all_dimensional)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(model_HitRate_all_dimensional))) #looks normally distributed
qqnorm(resid(model_HitRate_all_dimensional)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(model_HitRate_all_dimensional))) #Standardized Residuals should be <|3| or < |2|


###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(model_HitRate_all_dimensional) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(model_HitRate_all_dimensional)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(model_HitRate_all_dimensional)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf)  

#Rerun analysis without the potentially influencial cases
model_HitRate_all_dimensional_no_inf <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * 	PCA_Neglect, data=Final_Dataset[no_inf,])
summary(model_HitRate_all_dimensional_no_inf)

# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~ PCA_Abuse, Final_Dataset[no_inf,]) #linear
fit_1<-lm(HitRate_all_scaled ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(HitRate_all_scaled ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant
Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset_no_inf, "PCA_Abuse","HitRate_all_scaled", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "PCA_Neglect","HitRate_all_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","HitRate_all_scaled", color = "fgroup")


##------------------------------------------
# Testing the covariates
#AGE
model_HitRate_all_dimensional_and_AGE<- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age, Final_Dataset) 
summary(model_HitRate_all_dimensional_and_AGE)
anova(model_HitRate_all_dimensional, model_HitRate_all_dimensional_and_AGE) #model 2 significant! (p=0.003372 **)

#IQ (Database; IQ_IMPUTED) is the total score
model_HitRate_all_dimensional_and_IQ <- lm(HitRate_all_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
#in order to have a comparable number of individuals, I have to subset the initial model by the number of individuals that have been included in the model when I add the covariate
model_HitRate_all_dimensional_subset <- lm(HitRate_all_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
summary(model_HitRate_all_dimensional_and_IQ)

anova(model_HitRate_all_dimensional_subset, model_HitRate_all_dimensional_and_IQ) #model 2 significant!! (p=0.0009987 ***) #all good because the same number of participants have been included in both models

#ADHD
model_HitRate_all_dimensional_and_ADHD<- lm(HitRate_all_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + ADHD_acute, Final_Dataset) 
summary(model_HitRate_all_dimensional_and_ADHD)

anova(model_HitRate_all_dimensional, model_HitRate_all_dimensional_and_ADHD) #NOT significant!!

#Depression
model_HitRate_all_dimensional_and_DEPRESSION <- lm(HitRate_all_scaled~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + Dep_acute, Final_Dataset)
summary(model_HitRate_all_dimensional_and_DEPRESSION)

anova(model_HitRate_all_dimensional, model_HitRate_all_dimensional_and_DEPRESSION) #NOT significant!

#SESfeb2019withincountry
model_HitRate_all_dimensional_and_SES <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry, Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
summary(model_HitRate_all_dimensional_and_SES)

model_HitRate_all_dimensional_subset <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect, Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),]) 

anova(model_HitRate_all_dimensional_subset, model_HitRate_all_dimensional_and_SES) #NOT that significant!


#Gender
model_HitRate_all_dimensional_and_GENDER <- lm(HitRate_all_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + gender, Final_Dataset[!is.na(Final_Dataset$gender),])
summary(model_HitRate_all_dimensional_and_GENDER)

anova(model_HitRate_all_dimensional, model_HitRate_all_dimensional_and_GENDER) #significant!! (p=0.01204 *)

#CU traits (ICU /YPI) 
model_HitRate_all_dimensional_and_CU_Traits <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(model_HitRate_all_dimensional_and_CU_Traits)

anova(model_HitRate_all_dimensional,model_HitRate_all_dimensional_and_CU_Traits) #NOT significant!!

#------------------------------------------------------------------------------------------------------------
#Final model!
Final_Model_Dimensional_HitRate_all <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total + gender, data = Final_Dataset)
summary(Final_Model_Dimensional_HitRate_all)


##-------------------------------------------------------------------------------
#Hit rate all, cumulative model
cumulative_model_HitRate_all <- lm(HitRate_all_scaled ~ fgroup * trauma_standardmean, Final_Dataset)

### Multicollinearity: 
vif(cumulative_model_HitRate_all) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~ trauma_standardmean, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled ~ trauma_standardmean +I(trauma_standardmean^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled ~ trauma_standardmean+I(trauma_standardmean^2)+I(trauma_standardmean^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "HitRate_all_scaled","trauma_standardmean", color = "fgroup")


###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(cumulative_model_HitRate_all)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(cumulative_model_HitRate_all))) #looks normally distributed
qqnorm(resid(cumulative_model_HitRate_all)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(cumulative_model_HitRate_all))) #Standardized Residuals should be <|3| or < |2|


###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(cumulative_model_HitRate_all) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(cumulative_model_HitRate_all)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(cumulative_model_HitRate_all)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf)  

#Rerun analysis without the potentially influencial cases
cumulative_model_HitRate_all_no_inf <- lm(HitRate_all_scaled ~ fgroup * trauma_standardmean, data=Final_Dataset[no_inf,])
summary(cumulative_model_HitRate_all_no_inf)

# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~  trauma_standardmean, Final_Dataset[no_inf,]) #linear
fit_1<-lm(HitRate_all_scaled ~  trauma_standardmean +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(HitRate_all_scaled ~  trauma_standardmean +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant
Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset, "trauma_standardmean","HitRate_all_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "trauma_standardmean","HitRate_all_scaled", color = "fgroup")

#Covariates
##------------------------------------------
# Testing the covariates
#AGE
cumulative_model_HitRate_all_and_AGE <-lm(HitRate_all_scaled ~ fgroup * trauma_standardmean +age, Final_Dataset)
anova(cumulative_model_HitRate_all, cumulative_model_HitRate_all_and_AGE ) #significant! (p=5.757e-05 ***)

### Linearity for Age
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~  age, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled ~  age +I(age^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled ~ age  +I(age^2)+I(age ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "age","HitRate_all_scaled", color = "fgroup")

#IQ (Database; IQ_IMPUTED) is the total score
cumulative_model_HitRate_all_and_IQ<- lm(HitRate_all_scaled  ~ fgroup * trauma_standardmean + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
cumulative_model_HitRate_all_and_IQ_subset <- lm(HitRate_all_scaled  ~ fgroup * trauma_standardmean, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
summary()cumulative_model_HitRate_all_and_IQ

anova(cumulative_model_HitRate_all_and_IQ_subset, cumulative_model_HitRate_all_and_IQ ) #significant!!(p= 0.0005038 ***)

### Linearity for IQ
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~  iq_e_total, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled  ~  iq_e_total+I(iq_e_total^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled ~ iq_e_total  +I(iq_e_total^2)+I(iq_e_total ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "iq_e_total","HitRate_all_scaled", color = "fgroup")

#ADHD
cumulative_model_HitRate_all_and_ADHD<- lm(HitRate_all_scaled ~ fgroup * trauma_standardmean + ADHD_acute, Final_Dataset) 
summary(cumulative_model_HitRate_all_and_ADHD)

anova(cumulative_model_HitRate_all, cumulative_model_HitRate_all_and_ADHD) #NOT significant!!

### Linearity for ADHD
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled  ~  ADHD_acute, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled ~  ADHD_acute +I(ADHD_acute^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled  ~ ADHD_acute  +I(ADHD_acute^2)+I(ADHD_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "ADHD_acute","HitRate_all_scaled", color = "fgroup")

#Depression
cumulative_model_HitRate_all_and_DEPRESSION <- lm(HitRate_all_scaled   ~ fgroup * trauma_standardmean + Dep_acute, Final_Dataset)
summary(cumulative_model_HitRate_all_and_DEPRESSION)

anova(cumulative_model_HitRate_all, cumulative_model_HitRate_all_and_DEPRESSION) #NOT significant!

### Linearity for Depression
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled  ~  Dep_acute, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled  ~ Dep_acute +I(Dep_acute^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled  ~ Dep_acute  +I(Dep_acute^2)+I(Dep_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "Dep_acute","HitRate_all_scaled", color = "fgroup")

#SESfeb2019withincountry
cumulative_model_HitRate_all_and_SES <- lm(HitRate_all_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
cumulative_model_HitRate_all_subset<- lm(HitRate_all_scaled  ~ fgroup * trauma_standardmean,Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
summary(cumulative_model_HitRate_all_and_SES)
anova(cumulative_model_HitRate_all_subset, cumulative_model_HitRate_all_and_SES) #Significant!! (p=0.007384 **)

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled  ~  SESfeb2019withincountry, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled  ~  SESfeb2019withincountry +I(SESfeb2019withincountry^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled  ~ SESfeb2019withincountry  +I(SESfeb2019withincountry^2)+I(SESfeb2019withincountry ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "SESfeb2019withincountry","HitRate_all_scaled", color = "fgroup")
ggscatter(Final_Dataset, "HitRate_all_scaled","SESfeb2019withincountry", color = "fgroup")

#Gender
cumulative_model_HitRate_all_and_GENDER <- lm(HitRate_all_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset[!is.na(Final_Dataset$gender),])
summary(cumulative_model_HitRate_all_and_GENDER_subset)
anova(cumulative_model_HitRate_all, cumulative_model_HitRate_all_and_GENDER) #Significant!(p=0.01159 *)

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled  ~  gender, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled  ~  gender +I(gender^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled  ~  gender  +I(gender^2)+I(gender ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

#CU traits (ICU /YPI) 
cumulative_model_HitRate_all_and_CU_Traits <- lm(HitRate_all_scaled ~ fgroup * trauma_standardmean + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(cumulative_model_HitRate_all_and_CU_Traits)

anova(cumulative_model_HitRate_all, cumulative_model_HitRate_all_and_CU_Traits) #NOT significant!!

### Linearity for CU traits
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled  ~  total_sum_imp, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled  ~  total_sum_imp +I(total_sum_imp^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled  ~ total_sum_imp  +I(total_sum_imp^2)+I(total_sum_imp ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "HitRate_all_scaled","total_sum_imp", color = "fgroup")
##----------------------------------------
#
Final_Model_Cumulative_HitRate_all <- lm(HitRate_all_scaled ~  fgroup * trauma_standardmean  + age + iq_e_total + gender, data = Final_Dataset)

#Final models (cumulative and dimensional) are being tested with ANOVA
Final_Model_Dimensional_HitRate_all <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total + gender, data = Final_Dataset)
summary(Final_Model_Dimensional_HitRate_all)

Final_Model_Cumulative_HitRate_all <- lm(HitRate_all_scaled ~  fgroup * trauma_standardmean  + age + iq_e_total + gender, data = Final_Dataset)
summary(Final_Model_Cumulative_HitRate_all)

anova(Final_Model_Cumulative_HitRate_all, Final_Model_Dimensional_HitRate_all)
#no model is significant in this instance, so I would choose the cumulative model, since it's less complex



#-----------------------------------------------------------------------------------------
#final models from above
#with all cases 
Final_Model_Dimensional_HitRate_all <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total + gender + SESfeb2019withincountry, data = Final_Dataset)
summary(Final_Model_Dimensional_HitRate_all)

Final_Model_Cumulative_HitRate_all_scaled<- lm(HitRate_all_scaled ~  fgroup * trauma_standardmean  + age + iq_e_total + gender + SESfeb2019withincountry, data = Final_Dataset)
summary(Final_Model_Cumulative_HitRate_all_scaled)
#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 

anova(Final_Model_Dimensional_HitRate_all, Final_Model_Cumulative_HitRate_all_scaled)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#without influential cases

#Beware of what is the comparison in the noInf cases for each model - here you need to pick the one that exclude more cases, and leave those out in both models 
inf_cum <- apply(ifelse(influence.measures(Final_Model_Cumulative_HitRate_all_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_cum <- names(which(inf_cum>0)); length(inf_cum)
no_inf_cum <- names(which(inf_cum==0)); length(no_inf_cum) 

inf <- apply(ifelse(influence.measures(Final_Model_Dimensional_HitRate_all)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_dim <- names(which(inf>0)); length(is_inf)
no_inf_dim <- names(which(inf==0)); length(no_inf) 

#you have more influential cases in the Dim model, that is you need to limit your analysis based on the most restrictive model (which in this case is the dimenssional, so we exlucde the 63 that are influential in the dimentional model)
length(is_inf_dim)
length(is_inf_cum)


#now we do the same without the influential cases
#Dimensional model without influential cases
Final_Model_Dimensional_HitRate_all_noInf<- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total + gender + SESfeb2019withincountry, data = Final_Dataset[no_inf,])
summary(Final_Model_Dimensional_HitRate_all_noInf)

#Cumulative model wihtout influential cases
Final_Model_Cumulative_HitRate_all_scaled_noIf<- lm(HitRate_all_scaled ~  fgroup * trauma_standardmean  + age + iq_e_total + gender + SESfeb2019withincountry, data = Final_Dataset[no_inf,])
summary(Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_noInf)

anova(Final_Model_Dimensional_HitRate_all_noInf,Final_Model_Cumulative_HitRate_all_scaled_noIf)



#plots!
boxplot_HitRate_all_scaled_female_male <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$HitRate_all_scaled),], "fgroup","HitRate_all_scaled", color = "fgender")
boxplot_HitRate_all_scaled_female_male


