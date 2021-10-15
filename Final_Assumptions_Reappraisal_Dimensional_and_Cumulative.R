#load packages here
library(psych)
library(car)
library(QuantPsyc)
library(lmtest)
library(jtools)
library(pwr)
library(ggplot2)

#Reappraisal from ERQ and dimensional model of trauma
model_dimensional_model_reap <- lm(reap_sum_imp_scaled ~fgroup * PCA_Abuse + fgroup * PCA_Neglect, Final_Dataset)
summary(model_dimensional_model_reap)

### Multicollinearity: 
vif(model_dimensional_model_reap) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~ PCA_Abuse, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled  ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) # models 2 and 3 significant

#plot the different fits and see how they look 
ggscatter(Final_Dataset, "PCA_Abuse","reap_sum_imp_scaled", color = "fgroup")
##add line in the graph to show the different regression options in the plot


### Linearity
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(model_dimensional_model_reap)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(model_dimensional_model_reap))) #looks normally distributed
qqnorm(resid(model_dimensional_model_reap)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(model_dimensional_model_reap))) #Standardized Residuals should be <|3| or < |2|


###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(model_dimensional_model_reap) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(model_dimensional_model_reap)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled ~ PCA_Abuse, Final_Dataset[no_inf,]) #linear
fit_1<-lm(reap_sum_imp_scaled ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(reap_sum_imp_scaled ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant
Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset_no_inf, "PCA_Abuse","reap_sum_imp_scaled", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "PCA_Neglect","reap_sum_imp_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","reap_sum_imp_scaled", color = "fgroup")

##------------------------------------------
# Testing the covariates
#AGE
model_dimensional_model_reap_and_AGE<- lm(reap_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age, Final_Dataset) 
anova(model_dimensional_model_reap, model_dimensional_model_reap_and_AGE) #model 2 significant! (p=5.879e-05 ***)
##double-check please this was not sign for AC

#IQ (Database; IQ_IMPUTED) is the total score
model_dimensional_model_reap_and_IQ <- lm(reap_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
summary(model_dimensional_model_reap_and_IQ)
#in order to have a comparable number of individuals, I have to subset the initial model by the number of individuals that have been included in the model when I add the covariate
model_dimensional_model_reap_subset <- lm(reap_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 

anova(model_dimensional_model_reap_subset, model_dimensional_model_reap_and_IQ) #model 2 significant!! (p=0.0007773 ***) #all good because the same number of participants have been included in both models

#ADHD
model_dimensional_model_reap_and_ADHD<- lm(reap_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + ADHD_acute, Final_Dataset[!is.na(Final_Dataset$ADHD_acute),]) 
summary(model_dimensional_model_reap_and_ADHD)
anova(model_dimensional_model_reap, model_dimensional_model_reap_and_ADHD) #NOT significant!!

#Depression
model_dimensional_model_reap_and_DEPRESSION <- lm(reap_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + Dep_acute, Final_Dataset)
summary(model_dimensional_model_reap_and_DEPRESSION)
anova(model_dimensional_model_reap, model_dimensional_model_reap_and_DEPRESSION) #NOT significant!

#SESfeb2019withincountry
model_dimensional_model_reap_and_SES <- lm(reap_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry,Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
summary(model_dimensional_model_reap_and_SES)
model_dimensional_model_subset <- lm(reap_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),]) 
anova(model_dimensional_model_subset, model_dimensional_model_reap_and_SES) #NOT significant!

#Gender
model_dimensional_model_reap_and_GENDER <- lm(reap_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + gender,Final_Dataset[!is.na(Final_Dataset$gender),])
summary(model_dimensional_model_reap_and_GENDER)
anova(model_dimensional_model_reap, model_dimensional_model_reap_and_GENDER) # #NOT significant!


#CU traits (ICU /YPI) 
model_dimensional_model_reap_and_CU_Traits <- lm(reap_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(model_dimensional_model_reap_and_CU_Traits)
anova(model_dimensional_model_reap, model_dimensional_model_reap_and_CU_Traits) #NOT significant!!

#Final model
Final_Model_Dimensional_suppr_sum_imp_scaled <- lm(reap_sum_imp_scaled ~   fgroup * PCA_Abuse + fgroup * PCA_Neglect+ iq_e_total , data = Final_Dataset)
summary(Final_Model_Dimensional_suppr_sum_imp_scaled)

###REPEAT PROCESS WITH STANDARD MEAN MODEL , THEN CHECK FINAL DIM VS FINAL CUM 

##---------------------------------------------------------------------------------------
#Reappraisal and cumulative model of trauma                                                                    
reap_cumulative_model <-lm(reap_sum_imp_scaled ~ trauma_standardmean*fgroup, Final_Dataset[!is.na(Final_Dataset$reap_sum_imp_scaled),])
summary(reap_cumulative_model)

### Multicollinearity: 
vif(reap_cumulative_model) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~ trauma_standardmean, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled  ~ trauma_standardmean +I(trauma_standardmean^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled   ~ trauma_standardmean+I(trauma_standardmean^2)+I(trauma_standardmean^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "PCA_Abuse","reap_sum_imp_scaled", color = "fgroup")

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(reap_cumulative_model)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(reap_cumulative_model))) #looks normally distributed
qqnorm(resid(reap_cumulative_model)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(reap_cumulative_model))) #Standardized Residuals should be <|3| or < |2|


###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(reap_cumulative_model) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(reap_cumulative_model)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~ PCA_Abuse, Final_Dataset[no_inf,]) #linear
fit_1<-lm(reap_sum_imp_scaled  ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant
Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset_no_inf, "PCA_Abuse","reap_sum_imp_scaled ", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled ~ PCA_Neglect, Final_Dataset[no_inf,]) #linear
fit_1<-lm(reap_sum_imp_scaled  ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "PCA_Neglect","reap_sum_imp_scaled ", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","reap_sum_imp_scaled ", color = "fgroup")

#Covariates
##------------------------------------------
# Testing the covariates
#AGE
reap_cumulative_model_and_AGE <-lm(reap_sum_imp_scaled ~ fgroup * trauma_standardmean +age, Final_Dataset)
summary(reap_cumulative_model_and_AGE)
anova(reap_cumulative_model, reap_cumulative_model_and_AGE ) #NOT significant!!

### Linearity for Age
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled ~  age, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled ~  age +I(age^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled ~ age  +I(age^2)+I(age ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "age","False_alarm_all_scaled", color = "fgroup")


#IQ (Database; IQ_IMPUTED) is the total score
reap_cumulative_model_and_IQ <- lm(reap_sum_imp_scaled ~ fgroup * trauma_standardmean + iq_e_total, Final_Dataset) 
summary(reap_cumulative_model_and_IQ)
reap_cumulative_model_subset <- lm(reap_sum_imp_scaled~ fgroup * trauma_standardmean, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 

anova(reap_cumulative_model_and_IQ, reap_cumulative_model_subset) 

### Linearity for IQ
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled ~  iq_e_total, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled  ~  iq_e_total+I(iq_e_total^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled ~ iq_e_total  +I(iq_e_total^2)+I(iq_e_total ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "iq_e_total","False_alarm_all_scaled", color = "fgroup")

#ADHD
reap_cumulative_model_and_ADHD <- lm(reap_sum_imp_scaled  ~ fgroup * trauma_standardmean + ADHD_acute, Final_Dataset) 
summary(reap_cumulative_model_and_ADHD)
anova(reap_cumulative_model, reap_cumulative_model_and_ADHD) #NOT significant!!

### Linearity for ADHD
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~  ADHD_acute, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled ~  ADHD_acute +I(ADHD_acute^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ ADHD_acute  +I(ADHD_acute^2)+I(ADHD_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "age","reap_sum_imp_scaled", color = "fgroup")

#Depression
reap_cumulative_model_and_DEPRESSION <- lm(reap_sum_imp_scaled  ~ fgroup * trauma_standardmean + Dep_acute, Final_Dataset)
summary(reap_cumulative_model_and_DEPRESSION)
anova(reap_cumulative_model, reap_cumulative_model_and_DEPRESSION) #NOT significant!

### Linearity for Depression
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~  Dep_acute, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled  ~ Dep_acute +I(Dep_acute^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ Dep_acute  +I(Dep_acute^2)+I(Dep_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "age","reap_sum_imp_scaled", color = "fgroup")

#SESfeb2019withincountry
reap_cumulative_model_and_SES <- lm(reap_sum_imp_scaled  ~ fgroup * trauma_standardmean + SESfeb2019withincountry,Final_Dataset)
summary(reap_cumulative_model_and_SES)
reap_cumulative_model_subset <- lm(reap_sum_imp_scaled~ fgroup * trauma_standardmean, Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),]) 

anova(reap_cumulative_model_and_SES, reap_cumulative_model_subset) #NOT significant!!

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~  SESfeb2019withincountry, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled  ~  SESfeb2019withincountry +I(SESfeb2019withincountry^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ SESfeb2019withincountry  +I(SESfeb2019withincountry^2)+I(SESfeb2019withincountry ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "SESfeb2019withincountry","False_alarm_all_scaled", color = "fgroup")

#Gender
reap_cumulative_model_and_GENDER <- lm(reap_sum_imp_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset)
summary(reap_cumulative_model_and_GENDER)
anova(reap_cumulative_model, reap_cumulative_model_and_GENDER) #NOT significant!

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~  gender, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled  ~  gender +I(gender^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ gender  +I(gender^2)+I(gender ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

#CU traits (ICU /YPI) 

reap_cumulative_model_and_CU_Traits <- lm(reap_sum_imp_scaled ~ fgroup * trauma_standardmean + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(reap_cumulative_model_and_CU_Traits)
anova(reap_cumulative_model, reap_cumulative_model_and_CU_Traits) #NOT significant!!

### Linearity for CU traits
# Linear association predictor - outcome
fit_0<-lm(reap_sum_imp_scaled  ~  total_sum_imp, Final_Dataset) #linear
fit_1<-lm(reap_sum_imp_scaled  ~  total_sum_imp +I(total_sum_imp^2),Final_Dataset) #quadratic
fit_2<-lm(reap_sum_imp_scaled  ~ total_sum_imp  +I(total_sum_imp^2)+I(total_sum_imp ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #1 model significant

ggscatter(Final_Dataset, "total_sum_imp","reap_sum_imp_scaled", color = "fgroup")
##----------------------------------------

#final models from above
 
#with all cases 
Final_Model_Cumulative_reap_sum_imp_scaled <- lm(reap_sum_imp_scaled ~   fgroup*trauma_standardmean + iq_e_total, data = Final_Dataset)
summary(Final_Model_Cumulative_reap_sum_imp_scaled)

Final_Model_Dimensional_suppr_sum_imp_scaled <- lm(reap_sum_imp_scaled ~   fgroup * PCA_Abuse + fgroup * PCA_Neglect+ iq_e_total , data = Final_Dataset)
summary(Final_Model_Dimensional_suppr_sum_imp_scaled)

#comparison between the two models (cum/dim) for the relevant dependent variable (reap sum) variable with all cases 
#with all cases 

anova(Final_Model_Dimensional_suppr_sum_imp_scaled,Final_Model_Cumulative_reap_sum_imp_scaled)


#comparison between the two models (cum/dim) for the relevant dependent variable (reap sum) variable with all cases 
#witout influential cases


#Beware of what is the comparison in the noInf cases for each model - here you need to pick the one that exclude more cases, and leave those out in both models 
inf_cum <- apply(ifelse(influence.measures(Final_Model_Cum_suppr_sum_imp_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_cum <- names(which(inf_cum>0)); length(inf_cum)
no_inf_cum <- names(which(inf_cum==0)); length(no_inf_cum) 

inf <- apply(ifelse(influence.measures(Final_Model_Dimensional_suppr_sum_imp_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf) 

#you have more influential cases in the Dim model, that is you need to limit your analysis based on themost restrictive model (which in this case is the dimenssional, so we exlucde the 63 that are influential in the dimentional model)
length(is_inf)
length(is_inf_cum)


#now we do the same without the influential cases
#Dimensional model wihtout influential cases 

Final_Model_Dimensional_suppr_sum_imp_scaled_noInf <- lm(reap_sum_imp_scaled ~   fgroup * PCA_Abuse + fgroup * PCA_Neglect+ iq_e_total , data = Final_Dataset[no_inf,])
summary(Final_Model_Dimensional_suppr_sum_imp_scaled_noInf)
#Cumulative model wihtout influential cases
Final_Model_Cum_suppr_sum_imp_scaled_noInf <- lm(reap_sum_imp_scaled ~   fgroup*trauma_standardmean + iq_e_total , data = Final_Dataset[no_inf,])
summary(Final_Model_Cum_suppr_sum_imp_scaled_noInf)


anova(Final_Model_Dimensional_suppr_sum_imp_scaled_noInf,Final_Model_Cum_suppr_sum_imp_scaled_noInf)
##I would certainly discuss why the influential models may take away part of the effect in this case. 