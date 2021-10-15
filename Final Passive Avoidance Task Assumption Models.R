Final_Dataset$RT_reward_hit_mean_scaled <- scale(Final_Dataset$RT_reward_hit_mean)
Final_Dataset$RT_avoidance_errors_mean_scaled <- scale(Final_Dataset$RT_avoidance_errors_mean)
Final_Dataset$Avoidance_errorSum_error_rate_scaled <- scale(Final_Dataset$Avoidance_errorSum_error_rate)

#SESfeb2019withincountry + age + iq_e_total + Dep_acute + ADHD_acute
#AVOIDANCE TASK
#MRT Avoidance Errors 
Final_Model_Dimensional_RT_avoidance_errors_mean_scaled_final <- lm(RT_avoidance_errors_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry + age + iq_e_total + Dep_acute + ADHD_acute, data = Final_Dataset)
summary(Final_Model_Dimensional_RT_avoidance_errors_mean_scaled_final)

Final_Model_Cumulative__RT_avoidance_errors_mean_scaled_final <- lm(RT_avoidance_errors_mean_scaled ~  fgroup * trauma_standardmean +SESfeb2019withincountry + age + iq_e_total + Dep_acute + ADHD_acute , data = Final_Dataset)
summary(Final_Model_Cumulative__RT_avoidance_errors_mean_scaled_final)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 

anova(Final_Model_Dimensional_RT_avoidance_errors_mean_scaled_final, Final_Model_Cumulative__RT_avoidance_errors_mean_scaled_final)

#MRT Reward Hits 
Final_Model_Dimensional_RT_reward_hit_mean_scaled_final  <- lm(RT_reward_hit_mean_scaled ~ fgroup * PCA_Abuse + fgroup * PCA_Neglect +SESfeb2019withincountry + age + iq_e_total + Dep_acute + ADHD_acute, data = Final_Dataset)
summary(Final_Model_Dimensional_RT_reward_hit_mean_scaled_final)

Final_Model_Cumulative_RT_reward_hit_mean_scaled_final <- lm(RT_reward_hit_mean_scaled ~  fgroup * trauma_standardmean  + SESfeb2019withincountry + age + iq_e_total + Dep_acute + ADHD_acute, data = Final_Dataset)
summary(Final_Model_Cumulative_RT_reward_hit_mean_scaled_final)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 

anova(Final_Model_Dimensional_RT_reward_hit_mean_scaled_final, Final_Model_Cumulative_RT_reward_hit_mean_scaled_final)

#Sum of Avoidance Errors 
Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled_final <- lm(Avoidance_errorSum_error_rate_scaled ~  fgroup * PCA_Abuse + fgroup * PCA_Neglect + SESfeb2019withincountry + age + iq_e_total + Dep_acute + ADHD_acute, data = Final_Dataset)
summary(Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled_final)

Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_final <- lm(Avoidance_errorSum_error_rate_scaled ~  fgroup * trauma_standardmean +  SESfeb2019withincountry + age + iq_e_total + Dep_acute + ADHD_acute, data = Final_Dataset)
summary(Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_final)

#comparison between the two models (cum/dim) for the relevant dependent variable (False alarm) variable with all cases 
#with all cases 
anova(Final_Model_Dimensional_Avoidance_errorSum_error_rate_scaled_final, Final_Model_Cumulative_Avoidance_errorSum_error_rate_scaled_final)
