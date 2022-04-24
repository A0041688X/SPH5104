library(survival)
library(survey)
library(tidyverse)
library(ggplot2)
library(survminer)

match_data = read.csv(file = "psm_data_V2.csv")
original_data = read.csv(file = "original_data.csv")

match_data$administered_haloperidol = as.factor(match_data$administered_haloperidol)
match_data$administered_haloperidol = relevel (match_data$administered_haloperidol, "No Haloperidol")
summary(match_data$administered_haloperidol)

original_data$administered_haloperidol = as.factor(original_data$administered_haloperidol)
original_data$administered_haloperidol = relevel (original_data$administered_haloperidol, "No Haloperidol")
summary(original_data$administered_haloperidol)

### matched data ####
match_data = 
  match_data %>%
  mutate(death = ifelse(dod %in% "NULL", 0, 1),
         death_date = ifelse(death ==1, as.character(dod), NA),
         death_date = as.Date(death_date),
         icu_28day = as.Date(icu_in_date) + 28, 
         discharged_within_28days = ifelse(as.Date(discharge_date) < icu_28day, 1,0),
         death_within_icu_28days_numeric = ifelse(death_within_icu28days %in% "Yes", 1, 0))

#calculate survival time: if discharged before 28 days, survival time = discharge - icu admission
match_data$surv_time = ifelse (match_data$discharged_within_28days == 1, 
                               as.Date(match_data$discharge_date) - as.Date(match_data$icu_in_date),
                               as.Date(match_data$icu_28day) - as.Date(match_data$icu_in_date))

#survival analysis
match_data$survival_time = Surv(match_data$surv_time,
                                match_data$death_within_icu_28days_numeric)

ipw_svydesign = svydesign(ids = ~ subject_id, weights = ~ iptw, data = match_data)



univariate_ipw_cox = svycoxph(survival_time~administered_haloperidol, design = ipw_svydesign)
summary(univariate_ipw_cox)
cox.zph(univariate_ipw_cox) 
res1_hr = exp(cbind(HR = coef(univariate_ipw_cox), confint(univariate_ipw_cox)))


multivariate_ipw_cox = svycoxph(survival_time~administered_haloperidol  + admission_age + gender +bmi_imputed+bmi_impute_yn + ethnicity_edited +charlson_comorbidity_index + apsiii + psyc_disorder + sub_abuse   +first_care_unit_edited , design = ipw_svydesign)
summary(multivariate_ipw_cox)
cox.zph(multivariate_ipw_cox) 
res2_hr = exp(cbind(HR = coef(multivariate_ipw_cox), confint(multivariate_ipw_cox)))[1,]



#logistic regreesion
glm_svydesign = svydesign(ids = match_data$subject_id, weights = match_data$iptw, data = match_data, family = binomial)

univariate_ipw_log = svyglm(death_within_icu_28days_numeric ~ administered_haloperidol, design = glm_svydesign)
summary(univariate_ipw_log)
res1_log = exp(cbind(OR = coef(univariate_ipw_log), confint(univariate_ipw_log))[2,])

multivariate_ipw_log = svyglm(death_within_icu_28days_numeric ~ administered_haloperidol  + admission_age + gender +bmi_imputed+bmi_impute_yn + 
                                ethnicity_edited +charlson_comorbidity_index + apsiii + psyc_disorder + sub_abuse  +first_care_unit_edited ,
                              design= glm_svydesign)
summary(multivariate_ipw_log)
res2_log = exp(cbind(OR = coef(multivariate_ipw_log), confint(multivariate_ipw_log)))[2,]

### original data ####


original_data = 
  original_data %>%
  mutate(death = ifelse(dod %in% "NULL", 0, 1),
         death_date = ifelse(death ==1, as.character(dod), NA),
         death_date = as.Date(death_date),
         icu_28day = as.Date(icu_in_date) + 28, 
         discharged_within_28days = ifelse(as.Date(discharge_date) < icu_28day, 1,0),
         death_within_icu_28days_numeric = ifelse(death_within_icu28days %in% "Yes", 1, 0))

#calculate survival time: if discharged before 28 days, survival time = discharge - icu admission
original_data$surv_time = ifelse (original_data$discharged_within_28days == 1, 
                                  as.Date(original_data$discharge_date) - as.Date(original_data$icu_in_date),
                                  as.Date(original_data$icu_28day) - as.Date(original_data$icu_in_date))

#survival analysis
original_data$survival_time = Surv(original_data$surv_time,
                                   original_data$death_within_icu_28days_numeric)


#survival analysis
univariate_original_cox = coxph(survival_time~administered_haloperidol, strata(administered_haloperidol), data = original_data)
summary(univariate_original_cox)
cox.zph(univariate_original_cox) 
res3_hr = exp(cbind(HR = coef(univariate_original_cox), confint(univariate_original_cox)))


multivariate_original_cox = coxph(survival_time~administered_haloperidol  + admission_age + gender +bmi_imputed+bmi_impute_yn + ethnicity_edited +charlson_comorbidity_index + apsiii + psyc_disorder + sub_abuse  +first_care_unit_edited , 
                                  data = original_data)
summary(multivariate_original_cox)
cox.zph(multivariate_original_cox) 
res4_hr = exp(cbind(HR = coef(multivariate_original_cox), confint(multivariate_original_cox)))[1,]


#logistic regression
univariate_original_log = glm(death_within_icu_28days_numeric ~ administered_haloperidol  , family=binomial, data= original_data)
summary(univariate_original_log)
res3_log = exp(cbind(OR = coef(univariate_original_log), confint(univariate_original_log))[2,])

multivariate_original_log = glm(death_within_icu_28days_numeric ~ administered_haloperidol  + admission_age + gender +bmi_imputed+bmi_impute_yn + ethnicity_edited +charlson_comorbidity_index + apsiii + psyc_disorder + sub_abuse  +first_care_unit_edited , family=binomial, data= original_data)
summary(multivariate_original_log)
res4_log = exp(cbind(OR = coef(multivariate_original_log), confint(multivariate_original_log)))[2,]



# summary
hr_results = round(rbind(res1_hr, res2_hr, res3_hr, res4_hr),3)
or_results = round(rbind(res1_log, res2_log, res3_log, res4_log), 3)




##### survival curve #####
#for matched data
survivalplot =  survfit(formula = survival_time ~ administered_haloperidol, data = match_data)
par(bty = "o", 
    las = 0, 
    mar = c(4.5,4.5,3.5,3.5)+0.1, # b l t r
    mfrow=c(1,1))  

ggsurvplot(survivalplot, 
           conf.int=TRUE,
           pval=TRUE, 
           risk.table=TRUE, 
           legend.labs=c("Not Administered Haloperidol", "Administered Haloperidol"), 
           legend.title="",
           xlab="Time in days",
           #title="Kaplan-Meier Curve",
           risk.table.height=.3,
           tables.y.text = FALSE,
           risk.table.y.text = FALSE,
           break.x.by=4)

survivalplot_original =  survfit(formula = survival_time ~ administered_haloperidol, data = original_data)
ggsurvplot(survivalplot_original,
           conf.int=TRUE,
           pval=TRUE,
           risk.table=TRUE,
           legend.labs=c("Not Administered Haloperidol", "Administered Haloperidol"),
           legend.title="",
           xlab="Time in days",
           #title="Kaplan-Meier Curve",
           risk.table.height=.3,
           tables.y.text = FALSE,
           risk.table.y.text = FALSE,
           break.x.by=4)

#### subgroup anaysis ####
match_data$severe_apsiii = ifelse(match_data$apsiii > 50, 1, 0)
severity <- as.factor(match_data$severe_apsiii)
summary(severity)

ipw_svydesign_severe = svydesign(ids = ~ subject_id, weights = ~ iptw, data =match_data[match_data$severe_apsiii == 1, ] )
ipw_svydesign_notsevere = svydesign(ids = ~ subject_id, weights = ~ iptw, data =match_data[match_data$severe_apsiii == 0, ] )

#Univairate analysis
univariate_ipw_cox_severe = svycoxph(survival_time~administered_haloperidol, design = ipw_svydesign_severe)
summary(univariate_ipw_cox_severe)
cox.zph(univariate_ipw_cox_severe) 
subres1 = exp(cbind(HR = coef(univariate_ipw_cox_severe), confint(univariate_ipw_cox_severe)))


univariate_ipw_cox_notsevere = svycoxph(survival_time~administered_haloperidol, design = ipw_svydesign_notsevere)
summary(univariate_ipw_cox_notsevere)
cox.zph(univariate_ipw_cox_notsevere) 
subres2 = exp(cbind(HR = coef(univariate_ipw_cox_notsevere), confint(univariate_ipw_cox_notsevere)))

#multivairate analysis
multivariate_ipw_cox_severe = svycoxph(survival_time~administered_haloperidol  + admission_age + gender +bmi_imputed+bmi_impute_yn + ethnicity_edited +charlson_comorbidity_index  + psyc_disorder + sub_abuse  +first_care_unit_edited , 
                                   design = ipw_svydesign_severe)
summary(multivariate_ipw_cox_severe)
cox.zph(multivariate_ipw_cox_severe) 
subres3 = exp(cbind(HR = coef(multivariate_ipw_cox_severe), confint(multivariate_ipw_cox_severe)))[1,]

multivariate_ipw_cox_notsevere = svycoxph(survival_time~administered_haloperidol  + admission_age + gender +bmi_imputed+bmi_impute_yn + ethnicity_edited +charlson_comorbidity_index  + psyc_disorder + sub_abuse  +first_care_unit_edited , 
                                       design = ipw_svydesign_notsevere)
summary(multivariate_ipw_cox_notsevere)
cox.zph(multivariate_ipw_cox_notsevere) 
subres4 = exp(cbind(HR = coef(multivariate_ipw_cox_notsevere), confint(multivariate_ipw_cox_notsevere)))[1,]


sub_results = round(rbind(subres1, subres2, subres3, subres4), 3)


