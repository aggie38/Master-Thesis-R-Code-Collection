
Final_Dataset$HitRate_all_scaled <- scale(Final_Dataset$HitRate_all)
Final_Dataset$False_alarm_scaled <- scale(Final_Dataset$FARate_all)

# gender+ ADHD + SES +IQ + age

#FALSE ALARMS
Final_Model_Dimensional_model_False_alarm_all_final <- lm(False_alarm_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect  + gender + ADHD_acute + SESfeb2019withincountry + iq_e_total + age , data = Final_Dataset)
summary(Final_Model_Dimensional_model_False_alarm_all_final)


Final_Model_Cumulative_model_False_alarm_all_final <- lm(False_alarm_scaled ~ fgroup *trauma_standardmean  + gender + ADHD_acute + SESfeb2019withincountry + iq_e_total + age , data = Final_Dataset)
summary(Final_Model_Cumulative_model_False_alarm_all_final)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 
anova(Final_Model_Dimensional_model_False_alarm_all_final,Final_Model_Cumulative_model_False_alarm_all_final)

#HIT RATE ALL
Final_Model_Dimensional_HitRate_all_final <- lm(HitRate_all_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + gender + ADHD_acute + SESfeb2019withincountry + iq_e_total + age, data = Final_Dataset)
summary(Final_Model_Dimensional_HitRate_all_final)

Final_Model_Cumulative_HitRate_all_scaled_final<- lm(HitRate_all_scaled ~  fgroup * trauma_standardmean  + gender + ADHD_acute + SESfeb2019withincountry + iq_e_total + age, data = Final_Dataset)
summary(Final_Model_Cumulative_HitRate_all_scaled_final)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 
anova(Final_Model_Dimensional_HitRate_all_final,Final_Model_Cumulative_HitRate_all_scaled_final)

