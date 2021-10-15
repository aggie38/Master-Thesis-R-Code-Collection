library(psych)
library(car)
library(QuantPsyc)
library(lmtest)
library(jtools)
library(pwr)
library(ggplot2)

#Dimensional model
#Avoidance_errorSum_error_rate
model_Avoidance_errorSum_error_rate_scaled_dimensional <-lm(Avoidance_errorSum_error_rate_scaled ~ PCA_Abuse*fgroup  + PCA_Neglect*fgroup, Final_Dataset)

### Multicollinearity: 
vif(model_Avoidance_errorSum_error_rate_scaled_dimensional) #Variance Inflation Factor: should be < 10); if not => mean centering


### Linearity
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Abuse, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) #no model significant

### Linearity
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled ~ PCA_Neglect, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #model 3 significant

ggscatter(Final_Dataset, "PCA_Abuse","HitRate_all_scaled", color = "fgroup")

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(model_Avoidance_errorSum_error_rate_scaled_dimensional )) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(model_Avoidance_errorSum_error_rate_scaled_dimensional ))) #looks normally distributed
qqnorm(resid(model_Avoidance_errorSum_error_rate_scaled_dimensional)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(model_Avoidance_errorSum_error_rate_scaled_dimensional ))) #Standardized Residuals should be <|3| or < |2|


###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(model_Avoidance_errorSum_error_rate_scaled_dimensional ) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(model_Avoidance_errorSum_error_rate_scaled_dimensional )) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(model_Avoidance_errorSum_error_rate_scaled_dimensional )$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf)  

#Rerun analysis without the potentially influencial cases
model_Avoidance_errorSum_error_rate_scaled_dimensional_no_inf <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * PCA_Abuse + fgroup * 	PCA_Neglect, data=Final_Dataset[no_inf,])
summary(model_HitRate_all_dimensional_no_inf)

# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Abuse, Final_Dataset[no_inf,]) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Abuse +I(PCA_Abuse^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Abuse +I(PCA_Abuse^2)+I(PCA_Abuse ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant
Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset_no_inf, "PCA_Abuse","Avoidance_errorSum_error_rate_scaled ", color = "fgroup")

### Linearity
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Neglect, Final_Dataset[no_inf,]) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled ~ PCA_Neglect +I(PCA_Neglect^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ PCA_Neglect +I(PCA_Neglect^2)+I(PCA_Neglect ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant

ggscatter(Final_Dataset, "PCA_Neglect","Avoidance_errorSum_error_rate_scaled ", color = "fgroup")
ggscatter(Final_Dataset_no_inf, "PCA_Neglect","Avoidance_errorSum_error_rate_scaled ", color = "fgroup")

##------------------------------------------
# Testing the covariates
#AGE
model_Avoidance_errorSum_error_rate_scaled_dimensional_and_AGE<- lm(Avoidance_errorSum_error_rate_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + age, Final_Dataset) 
anova(model_Avoidance_errorSum_error_rate_scaled_dimensional, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_AGE) #significant! (p=5.879e-05 ***)

#IQ (Database; IQ_IMPUTED) is the total score
model_Avoidance_errorSum_error_rate_scaled_dimensional_and_IQ <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
#in order to have a comparable number of individuals, I have to subset the initial model by the number of individuals that have been included in the model when I add the covariate
model_Avoidance_errorSum_error_rate_scaled_dimensional_subset <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 

anova(model_Avoidance_errorSum_error_rate_scaled_dimensional_subset, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_IQ) #significant!! (p=0.0007773 ***) #all good because the same number of participants have been included in both models

#ADHD
model_Avoidance_errorSum_error_rate_scaled_dimensional_and_ADHD<- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + ADHD_acute, Final_Dataset[!is.na(Final_Dataset$ADHD_acute),]) 
summary(model_Avoidance_errorSum_error_rate_scaled_dimensional_and_ADHD)
anova(model_Avoidance_errorSum_error_rate_scaled_dimensional, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_ADHD) #NOT significant!!

#Depression
model_Avoidance_errorSum_error_rate_scaled_dimensional_and_DEPRESSION <- lm(Avoidance_errorSum_error_rate_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + Dep_acute, Final_Dataset)
summary(model_Avoidance_errorSum_error_rate_scaled_dimensional_and_DEPRESSION)
anova(model_Avoidance_errorSum_error_rate_scaled_dimensional, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_DEPRESSION) #NOT significant!

#SESfeb2019withincountry
model_Avoidance_errorSum_error_rate_scaled_dimensional_and_SESfeb2019withincountry <- lm(Avoidance_errorSum_error_rate_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry,Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
model_Avoidance_errorSum_error_rate_scaled_dimensional_SES_subset <- lm(Avoidance_errorSum_error_rate_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect , Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),]) 
summary(model_Avoidance_errorSum_error_rate_scaled_dimensional_and_SESfeb2019withincountry)
anova(model_Avoidance_errorSum_error_rate_scaled_dimensional_SES_subset, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_SESfeb2019withincountry) #NOT significant!


#Gender
model_Avoidance_errorSum_error_rate_scaled_dimensional_and_GENDER <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + gender,Final_Dataset[!is.na(Final_Dataset$gender),])
summary(model_Avoidance_errorSum_error_rate_scaled_dimensional_and_GENDER)

anova(model_Avoidance_errorSum_error_rate_scaled_dimensional, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_GENDER) #NOT significant!! 

#CU traits (ICU /YPI) 
model_Avoidance_errorSum_error_rate_scaled_dimensional_and_CU_Traits <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(model_Avoidance_errorSum_error_rate_scaled_dimensional_and_CU_Traits)
anova(model_Avoidance_errorSum_error_rate_scaled_dimensional, model_Avoidance_errorSum_error_rate_scaled_dimensional_and_CU_Traits) #NOT significant!!

#------------------------------------------------------------------------------------------------------
Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled <- lm(Avoidance_errorSum_error_rate_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect +  age + iq_e_total , data = Final_Dataset)

##--------------------------------------------
#Cumulative Model
#Avoidance_errorSum_error_rate (in percentage) across all participants
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma <-	lm(Avoidance_errorSum_error_rate_scaled ~ 	trauma_standardmean * fgroup, 	Final_Dataset)
### Multicollinearity: 
vif(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma) #Variance Inflation Factor: should be < 10); if not => mean centering

### Linearity
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled   ~ trauma_standardmean, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled   ~ trauma_standardmean +I(trauma_standardmean^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ trauma_standardmean+I(trauma_standardmean^2)+I(trauma_standardmean^3),Final_Dataset) #cubic
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "trauma_standardmean","Avoidance_errorSum_error_rate_scaled", color = "fgroup")

###Normal Distribution of Residuals
#Statistical Test
shapiro.test(resid(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma)) #Normality Test: should be non sign

#Residual Plots
par(mfrow=c(1,2))
hist(scale(resid(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma))) #looks normally distributed
qqnorm(resid(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma)) #QQ-Plot: Values should lie on straight line
summary(scale(resid(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma))) #Standardized Residuals should be <|3| or < |2|


###Outliers
#Statistical Analysis of Potential Outliers
outlierTest(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma) #all ok

###Influential Cases Statistics (Regression Leave-one-out Deletion Diagnostics)
#Statistical Analysis of Influencial Cases
summary(influence.measures(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma)) 

# Comment: 
# list of cases with at least one indicator suggesting them influencial. 
# An "*" indicates an influencial measure.

#Select only cases without any influencial measures
inf <- apply(ifelse(influence.measures(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma)$is.inf==TRUE, 1, 0), 1, sum)
is_inf <- names(which(inf>0)); length(is_inf)
no_inf <- names(which(inf==0)); length(no_inf)  

#Rerun analysis without the potentially influencial cases
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_no_inf <- lm(Avoidance_errorSum_error_rate_scaled  ~ trauma_standardmean * fgroup, data=Final_Dataset[no_inf,])
summary(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_no_inf)

# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~ trauma_standardmean, Final_Dataset[no_inf,]) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~ trauma_standardmean +I(trauma_standardmean^2),Final_Dataset[no_inf,]) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ trauma_standardmean +I(trauma_standardmean^2)+I(trauma_standardmean ^3),Final_Dataset[no_inf,])
anova(fit_0,fit_1,fit_2) #3 model significant
Final_Dataset_no_inf <- Final_Dataset[no_inf,]

ggscatter(Final_Dataset_no_inf, "trauma_standardmean,","Avoidance_errorSum_error_rate_scaled", color = "fgroup")


#Covariates
##------------------------------------------
# Testing the covariates
#AGE
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_AGE <-lm(Avoidance_errorSum_error_rate_scaled ~ fgroup * trauma_standardmean + age, Final_Dataset)
anova(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma, Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_AGE ) #significant! (p=5.757e-05 ***)

### Linearity for Age
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled ~  age, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled ~  age +I(age^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled ~ age  +I(age^2)+I(age ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "age","Avoidance_errorSum_error_rate_scaled", color = "fgroup")


#IQ (Database; IQ_IMPUTED) is the total score
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_IQ_subset <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * trauma_standardmean + iq_e_total, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_subset <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * trauma_standardmean, Final_Dataset[!is.na(Final_Dataset$iq_e_total),]) 

anova(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_subset, Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_IQ ) #Significant!(p=0.0007864 ***)

### Linearity for IQ
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled ~  iq_e_total, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~  iq_e_total+I(iq_e_total^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled ~ iq_e_total  +I(iq_e_total^2)+I(iq_e_total ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "iq_e_total","Avoidance_errorSum_error_rate_scaled", color = "fgroup")

#ADHD
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_ADHD<- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * trauma_standardmean + ADHD_acute, Final_Dataset) 
anova(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma, Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_ADHD) #NOT significant!!

### Linearity for ADHD
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~  ADHD_acute, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled ~  ADHD_acute +I(ADHD_acute^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ ADHD_acute  +I(ADHD_acute^2)+I(ADHD_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "ADHD_acute","Avoidance_errorSum_error_rate_scaled", color = "fgroup")

#Depression
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_DEPRESSION <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * trauma_standardmean + Dep_acute, Final_Dataset)

anova(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma, Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_DEPRESSION) #NOT significant!

### Linearity for Depression
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~  Dep_acute, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~ Dep_acute +I(Dep_acute^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ Dep_acute  +I(Dep_acute^2)+I(Dep_acute ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

ggscatter(Final_Dataset, "age","False_alarm_all_scaled", color = "fgroup")

#SESfeb2019withincountry
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_SES_subset <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * trauma_standardmean + SESfeb2019withincountry, Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_SES <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * trauma_standardmean, Final_Dataset[!is.na(Final_Dataset$SESfeb2019withincountry),])
summary(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_SES_subset)

anova(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_SES_subset, Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_SES) #NOT significant!!

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~  SESfeb2019withincountry, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~  SESfeb2019withincountry +I(SESfeb2019withincountry^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ SESfeb2019withincountry  +I(SESfeb2019withincountry^2)+I(SESfeb2019withincountry ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #2 model significant

ggscatter(Final_Dataset, "SESfeb2019withincountry","False_alarm_all_scaled", color = "fgroup")

#Gender
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_GENDER <- lm(Avoidance_errorSum_error_rate_scaled  ~ fgroup * trauma_standardmean + gender, Final_Dataset)
summary(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_GENDER)

anova(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma, Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_GENDER) #NOT significant!

### Linearity for SES
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~  gender, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~  gender +I(gender^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ gender  +I(gender^2)+I(gender ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #no model significant

#CU traits (ICU /YPI) 
Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_CU_Traits <- lm(Avoidance_errorSum_error_rate_scaled ~ fgroup * trauma_standardmean + total_sum_imp, Final_Dataset[!is.na(Final_Dataset$total_sum_imp),])
summary(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_CU_Traits)

anova(Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma, Avoidance_errorSum_error_rate_scaled_standardmean_of_trauma_and_CU_Traits) #NOT significant!!

### Linearity for CU traits
# Linear association predictor - outcome
fit_0<-lm(Avoidance_errorSum_error_rate_scaled  ~  total_sum_imp, Final_Dataset) #linear
fit_1<-lm(Avoidance_errorSum_error_rate_scaled  ~  total_sum_imp +I(total_sum_imp^2),Final_Dataset) #quadratic
fit_2<-lm(Avoidance_errorSum_error_rate_scaled  ~ total_sum_imp  +I(total_sum_imp^2)+I(total_sum_imp ^3),Final_Dataset)
anova(fit_0,fit_1,fit_2) #1 model significant

ggscatter(Final_Dataset, "total_sum_imp","Avoidance_errorSum_error_rate_scaled", color = "fgroup")
##----------------------------------------
Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled <- lm(Avoidance_errorSum_error_rate_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total, data = Final_Dataset)
summary(Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled)

#-----------------------------------------------------------------------------------------
#final models from above
#with all cases 
Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled <- lm(Avoidance_errorSum_error_rate_scaled ~  fgroup * trauma_standardmean  + age + iq_e_total, data = Final_Dataset)
summary(Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled)

Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled <- lm(Avoidance_errorSum_error_rate_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect +  age + iq_e_total , data = Final_Dataset)
summary(Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 
anova(Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled, Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#without influential cases

#Beware of what is the comparison in the noInf cases for each model - here you need to pick the one that exclude more cases, and leave those out in both models 
inf_cum <- apply(ifelse(influence.measures(Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_cum <- names(which(inf_cum>0)); length(inf_cum)
no_inf_cum <- names(which(inf_cum==0)); length(no_inf_cum) 

inf <- apply(ifelse(influence.measures(Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled)$is.inf==TRUE, 1, 0), 1, sum)
is_inf_dim <- names(which(inf>0)); length(is_inf)
no_inf_dim <- names(which(inf==0)); length(no_inf) 

#you have more influential cases in the Dim model, that is you need to limit your analysis based on the most restrictive model (which in this case is the dimenssional, so we exlucde the 63 that are influential in the dimentional model)
length(is_inf_dim)
length(is_inf_cum)


#now we do the same without the influential cases
#Dimensional model without influential cases 
Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled_noInf <- lm(Avoidance_errorSum_error_rate_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect + age + iq_e_total, data = Final_Dataset[no_inf,])
summary(Final_Model_Dimensional_model_False_alarm_all_noInf)

#Cumulative model without influential cases
Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_noInf <- lm(Avoidance_errorSum_error_rate_scaled ~   fgroup * trauma_standardmean + age + iq_e_total, data = Final_Dataset[no_inf,])
summary(Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_noInf)

anova(Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled_noInf,Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_noInf)



#boxplot_Avoidance_errorSum_error_rate_scaled_female_male <- ggboxplot(Final_Dataset[!is.na(Final_Dataset$Avoidance_errorSum_error_rate_scaled),], "fgroup","Avoidance_errorSum_error_rate_scaled", color = "fgender")
#boxplot_Avoidance_errorSum_error_rate_scaled_female_male

