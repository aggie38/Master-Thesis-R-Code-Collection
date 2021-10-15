library(psych)
library(car)
library(QuantPsyc)
library(lmtest)
library(jtools)
library(pwr)
library(ggplot2)

#Suppressive expression from ERQ and dimensional model of trauma
model_dimensional_model_suppr <- lm(suppr_sum_imp_scaled ~fgroup * PCA_Abuse + fgroup * PCA_Neglect, Final_Dataset)

### Multicollinearity: 
vif(model_dimensional_model_suppr) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled  ~ PCA_Abuse, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled  ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled  ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) # model 3 very significant
ggscatter(Final_Dataset, "PCA_Abuse","suppr_sum_imp_scaled", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "PCA_Neglect","suppr_sum_imp_scaled", color = "fgroup")

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(model_dimensional_model_suppr)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(model_dimensional_model_suppr))) #looks normally distributed
qqnorm(resid(model_dimensional_model_suppr)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(model_dimensional_model_suppr))) #Standardized Residuals should be <|3| or < |2|

###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(model_dimensional_model_suppr) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(model_dimensional_model_suppr)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(model_dimensional_model_suppr)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf)  

#Rerun analysis without the potentially influencial cases
model_dimensional_suppr_sum_imp_scaled_no_inf <- lm(suppr_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * 	PCA_Neglect, data=Final_Dataset[no_inf,])
summary(model_dimensional_suppr_sum_imp_scaled_no_inf)

# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled ~ PCA_Abuse, Final_Dataset[no_inf,]) #linear
fit_1<-lm(suppr_sum_imp_scaled ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(suppr_sum_imp_scaled ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant

Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset_no_inf, "PCA_Abuse","suppr_sum_imp_scaled", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "PCA_Neglect","suppr_sum_imp_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","suppr_sum_imp_scaled", color = "fgroup")

##------------------------------------------
# Testing the covariates
#AGE
model_dimensional_model_suppr_and_AGE <- lm(suppr_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age, Final_Dataset) 
anova(model_dimensional_model_suppr, model_dimensional_model_suppr_and_AGE) #model 2 significant! (p=5.879e-05 ***)

#IQ (Database; IQ_IMPUTED) is the total score
model_dimensional_model_suppr_and_IQ <- lm(suppr_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
#in order to have a comparable number of individuals, I have to subset the initial model by the number of individuals that have been included in the model when I add the covariate
model_dimensional_model_suppr_subset <- lm(suppr_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 

#anova(model_Avoidance_errorSum_error_rate_scaled_dimensional,model_Avoidance_errorSum_error_rate_scaled_dimensional_and_IQ) #ERROR because I dont hav teh same number of individuals

anova(model_dimensional_model_suppr_dimensional_subset, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_IQ) #model 2 significant!! (p=0.0007773 ***) #all good because the same number of participants have been included in both models

#ADHD
model_dimensional_model_suppr_and_ADHD<- lm(suppr_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + ADHD_acute, Final_Dataset[!is.na(Final_Dataset$ADHD_acute),]) 

anova(model_dimensional_model_suppr, model_dimensional_model_suppr_and_ADHD) #NOT significant!!

#Depression
model_dimensional_model_suppr_and_DEPRESSION <- lm(suppr_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + Dep_acute, Final_Dataset)

anova(model_dimensional_model_suppr, model_dimensional_model_suppr_and_DEPRESSION) #NOT significant!

#SESfeb2019withincountry
model_dimensional_model_suppr_and_SES <- lm(suppr_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry,Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])

model_dimensional_model_suppr_SES_subset <- lm(suppr_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),]) 

#anova(model_Avoidance_errorSum_error_rate_scaled_dimensional, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_SESfeb2019withincountry) #ERROR

anova(model_dimensional_model_suppr_SES_subset, model_dimensional_model_suppr_and_SES) #NOT significant!


#Gender
model_dimensional_model_suppr_and_GENDER <- lm(suppr_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + gender, Final_Dataset[!is.na(Final_Dataset$gender),])

anova(model_dimensional_model_suppr, model_dimensional_model_suppr_and_GENDER) #significant!! (p=5.925e-07 ***)

#CU traits (ICU /YPI) 

model_dimensional_model_suppr_and_CU_Traits <- lm(suppr_sum_imp_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])

anova(model_dimensional_model_suppr, model_dimensional_model_suppr_and_CU_Traits) #NOT significant!!

#Final model
Final_Model_Dimensional_suppr_sum_imp_scaled <- lm(suppr_sum_imp_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect  + gender + age + iq_e_total, data = Final_Dataset)


##-----------------------------------------------------

#Suppressive expression and cumulative model of trauma
suppr_cumulative <-lm(suppr_sum_imp_scaled  ~ trauma_standardmean* fgroup, Final_Dataset[ !is.na(Final_Dataset$suppr_sum_imp_scaled),])

### Multicollinearity: 
vif(suppr_cumulative) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled ~ trauma_standardmean, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled ~ trauma_standardmean +I(trauma_standardmean^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled   ~ trauma_standardmean+I(trauma_standardmean^2)+I(trauma_standardmean^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) # model 2 significant

ggscatter(Final_Dataset, "PCA_Abuse","suppr_sum_imp_scaled", color = "fgroup")

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(suppr_cumulative)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(suppr_cumulative))) #looks normally distributed
qqnorm(resid(suppr_cumulative)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(suppr_cumulative))) #Standardized Residuals should be <|3| or < |2|


###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(suppr_cumulative) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(suppr_cumulative)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(suppr_cumulative)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf)  

#Rerun analysis without the potentially influencial cases
model_HitRate_all_dimensional_no_inf <- lm(suppr_sum_imp_scaled ~ fgroup * PCA_Abuse + fgroup * 	PCA_Neglect, data=Final_Dataset[no_inf,])
summary(model_HitRate_all_dimensional_no_inf)

# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled~ PCA_Abuse, Final_Dataset[no_inf,]) #linear
fit_1<-lm(suppr_sum_imp_scaled ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(suppr_sum_imp_scaled ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant
Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset_no_inf, "PCA_Abuse","suppr_sum_imp_scaled", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(HitRate_all_scaled ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(HitRate_all_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(HitRate_all_scaled ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "PCA_Neglect","suppr_sum_imp_scaled", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","suppr_sum_imp_scaled", color = "fgroup")

#Covariates
##------------------------------------------
# Testing the covariates
#AGE
suppr_cumulative_and_AGE <-lm(suppr_sum_imp_scaled ~ fgroup * trauma_standardmean + age, Final_Dataset)
anova(suppr_cumulative, suppr_cumulative_and_AGE ) #NOT significant!!

### Linearity for Age
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled ~  age, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled ~  age +I(age^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled ~ age  +I(age^2)+I(age ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "age","suppr_sum_imp_scaled", color = "fgroup")


#IQ (Database; IQ_IMPUTED) is the total score
suppr_cumulative_and_IQ_subset <- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
suppr_cumulative_and_IQ <- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean + iq_e_total, Final_Dataset) 

anova(suppr_cumulative_and_IQ_subset, suppr_cumulative_and_IQ ) #NOT significant!

### Linearity for IQ
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled ~  iq_e_total, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled  ~  iq_e_total+I(iq_e_total^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled ~ iq_e_total  +I(iq_e_total^2)+I(iq_e_total ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "iq_e_total","suppr_sum_imp_scaled", color = "fgroup")

#ADHD
suppr_cumulative_and_ADHD<- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean + ADHD_acute, Final_Dataset) 

anova(suppr_cumulative, suppr_cumulative_and_ADHD) #NOT significant!!

### Linearity for ADHD
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled  ~  ADHD_acute, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled ~  ADHD_acute +I(ADHD_acute^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled  ~ ADHD_acute  +I(ADHD_acute^2)+I(ADHD_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "age","suppr_sum_imp_scaled", color = "fgroup")

#Depression
suppr_cumulative_and_DEPRESSION <- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean + Dep_acute, Final_Dataset)

anova(suppr_cumulative, suppr_cumulative_and_DEPRESSION) #NOT significant!

### Linearity for Depression
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled  ~  Dep_acute, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled  ~ Dep_acute +I(Dep_acute^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled  ~ Dep_acute  +I(Dep_acute^2)+I(Dep_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "age","False_alarm_all_scaled", color = "fgroup")

#SESfeb2019withincountry
suppr_cumulative_and_SES <- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean+ SESfeb2019withincountry,Final_Dataset)
suppr_cumulative_and_SES_subset <- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean+ SESfeb2019withincountry,Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
anova(suppr_cumulative_and_SES_subset, suppr_cumulative_and_SES) #NOT significant!!

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled  ~  SESfeb2019withincountry, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled  ~  SESfeb2019withincountry +I(SESfeb2019withincountry^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled  ~ SESfeb2019withincountry  +I(SESfeb2019withincountry^2)+I(SESfeb2019withincountry ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "SESfeb2019withincountry","False_alarm_all_scaled", color = "fgroup")

#Gender
suppr_cumulative_and_GENDER_subset <- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset[!is.na(Final_Dataset$gender),])
suppr_cumulative_and_GENDER <- lm(suppr_sum_imp_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset)
anova(suppr_cumulative_and_GENDER_subset, suppr_cumulative_and_GENDER) #NOT significant!

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled  ~  gender, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled  ~  gender +I(gender^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled  ~ gender  +I(gender^2)+I(gender ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

#CU traits (ICU /YPI) 

suppr_cumulative_and_CU_Traits <- lm(suppr_sum_imp_scaled ~ fgroup * trauma_standardmean + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])

anova(suppr_cumulative, suppr_cumulative_and_CU_Traits) #Significant!! (p= 8.936e-05 ***)

### Linearity for CU traits
# Linear association predictor - outcome
fit_0<-lm(suppr_sum_imp_scaled  ~  total_sum_imp, Final_Dataset) #linear
fit_1<-lm(suppr_sum_imp_scaled  ~  total_sum_imp +I(total_sum_imp^2),Final_Dataset) #quadratic
fit_2<-lm(suppr_sum_imp_scaled  ~ total_sum_imp  +I(total_sum_imp^2)+I(total_sum_imp ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #1 model significant

ggscatter(Final_Dataset, "total_sum_imp","suppr_sum_imp_scaled", color = "fgroup")


#-----------------------------------------------------------------------------------------
#final models from above
#with all cases 

Final_Model_Cumulative_suppr_sum_imp_scaled <- lm(suppr_sum_imp_scaled ~ fgroup *trauma_standardmean + age  + gender + total_sum_imp, data = Final_Dataset)
summary(Final_Model_Cumulative_suppr_sum_imp_scaled)

Final_Model_Dimensional_suppr_sum_imp_scaled <- lm(suppr_sum_imp_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect  + gender + age + total_sum_imp, data = Final_Dataset)
summary(Final_Model_Dimensional_suppr_sum_imp_scaled )

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 

anova(Final_Model_Dimensional_suppr_sum_imp_scaled, Final_Model_Cumulative_suppr_sum_imp_scaled)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#without influential cases

#Beware of what is the comparison in the noInf cases for each model - here you need to pick the one that exclude more cases, and leave those out in both models 
inf_cum <- apply(ifelse(influence.measures(Final_Model_Cumulative_suppr_sum_imp_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_cum <- names(which(inf_cum>0)); length(inf_cum)
no_inf_cum <- names(which(inf_cum==0)); length(no_inf_cum) 

inf <- apply(ifelse(influence.measures(Final_Model_Dimensional_suppr_sum_imp_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_dim <- names(which(inf>0)); length(is_inf)
no_inf_dim <- names(which(inf==0)); length(no_inf) 

#you have more influential cases in the Dim model, that is you need to limit your analysis based on the most restrictive model (which in this case is the dimenssional, so we exlucde the 63 that are influential in the dimentional model)
length(is_inf_dim)
length(is_inf_cum)


#now we do the same without the influential cases
#Dimensional model without influential cases 
Final_Model_Dimensional_suppr_sum_imp_scaled_noInf <- lm(suppr_sum_imp_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + gender + total_sum_imp, data = Final_Dataset[no_inf,])
summary(Final_Model_Dimensional_suppr_sum_imp_scaled_noInf)

#Cumulative model wihtout influential cases
Final_Model_Cumulative_suppr_sum_imp_scaled_noInf <- lm(suppr_sum_imp_scaled ~   fgroup * trauma_standardmean + age + gender + total_sum_imp, data = Final_Dataset[no_inf,])
summary(Final_Model_Cumulative_suppr_sum_imp_scaled)

anova(Final_Model_Dimensional_suppr_sum_imp_scaled_noInf,Final_Model_Cumulative_suppr_sum_imp_scaled_noInf)


