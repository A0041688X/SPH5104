library(tidyverse)
library(table1)
library(MatchIt)
library(lubridate)
library(data.table)
library(labelled)
library(Hmisc)
library(glmtoolbox)

`%nin%` = Negate(`%in%`)

data = read.csv("raw_data.csv")
icu_stay = fread("mimic-iv-1.0/icu/icustays.csv.gz")

data_merge = 
  left_join(data, icu_stay, by = c("subject_id", "hadm_id", "stay_id"), all.x = TRUE, all.y = FALSE)


# format date and time variables
data_format = 
  data_merge %>%
  mutate(admit_date =  as.Date(admittime),
         admit_time = substring(admittime, 12,19), 
         discharge_date = as.Date(dischtime), 
         discharge_time = substring(dischtime, 12,19),
    
          icu_in_date = as.Date(icu_intime), 
         icu_in_time = substring(icu_intime, 12,19), 
         icu_out_date = as.Date(icu_outtime), 
         icu_out_time = substring(icu_outtime, 12,19), 
         
         vent_start_date = as.Date(ven_starttime), 
         vent_start_time = substring(ven_starttime, 12,19), 
         vent_end_date = as.Date(ven_endtime), 
         vent_end_time = substring(ven_endtime, 12,19), 
         
         administered_haloperidol = ifelse(drug %nin% "NULL", 1, 0),
         haloperidol_give_date = ifelse(pharm_starttime %nin% "NULL", substring(pharm_starttime,1,10), NA), 
         haloperidol_give_time = ifelse(pharm_starttime %nin% "NULL", substring(pharm_starttime,12,19), NA), 
         haloperidol_stop_date = ifelse(pharm_stoptime %nin% "NULL", substring(pharm_stoptime,1,10), NA), 
         
         # some patients were only given at one time point
         haloperidol_stop_date = ifelse(administered_haloperidol == 1 & is.na(haloperidol_stop_date), 
                                        haloperidol_give_date, 
                                        haloperidol_stop_date),
         
         haloperidol_stop_time = ifelse(pharm_starttime %nin% "NULL", substring(pharm_starttime,12,19), NA), 
         
         # given haloperidol before or after ventilation 
         haloperidol_after_ventilation = ifelse(administered_haloperidol ==1, 
                                                 haloperidol_give_date>=vent_end_date & haloperidol_give_time >= vent_end_time, 
                                                0),
         haloperidol_before_ventilation = ifelse(administered_haloperidol ==1, 
                                                 haloperidol_stop_date<=vent_start_date & haloperidol_stop_time <= vent_start_time, 
                                                 0),
         haloperidol_during_ventilation = ifelse(administered_haloperidol ==1, 
                                                 haloperidol_after_ventilation ==0 & haloperidol_before_ventilation ==0, 
                                                 0), 
         
         haloperidol_after_icu = ifelse(administered_haloperidol ==1, 
                                        haloperidol_give_date > icu_out_date & haloperidol_give_time >= icu_out_time, 
                                        0),
         
         # death within 28 days of ICU admission
         icu_28day = as.Date(icu_in_date) + 28,
         death = ifelse(dod %in% "NULL", 0, 1),
         death_date = ifelse(death ==1, as.character(dod), NA), 
         death_within_icu28days = ifelse(death == 1 & (as.Date(death_date) <= icu_28day), 1, 0)
  )

dim(data_format) # 39580

table(data_format$death_within_icu28days)
table(data_format$haloperidol_after_icu)

# take only first ICU stay for each patient
first_icu =
  data_format %>%
  group_by(subject_id) %>%
  arrange(subject_id, icu_in_date) %>%
  slice(1) %>%
  ungroup()

# take only those who were administered haloperidol during ICU

table(first_icu$haloperidol_after_icu) # 145

first_icu_haloperidol_in_icu = 
  first_icu %>%
  filter(haloperidol_after_icu == 0)

# check exclusion criteria
table(first_icu_haloperidol_in_icu$ventilation_status)
dim(first_icu_haloperidol_in_icu) # 16640 

# select all variables of interest
all_vars = c("subject_id", 
             "admission_age", "gender", "ethnicity", "height", "weight", 
             "psyc_disorder", "sub_abuse", "charlson_comorbidity_index", "apsiii", "los_icu",
             "admit_date", "admit_time", "discharge_date", "discharge_time", "death_within_icu28days", 
             "hospital_expire_flag", "dod", "icu_in_date", "icu_in_time", "icu_out_date", "icu_out_time", "vent_start_date", "vent_end_date", "vent_end_time", "haloperidol_give_date", "haloperidol_give_time", "haloperidol_stop_date", "haloperidol_stop_time", 
             "administered_haloperidol",  "haloperidol_after_icu", "haloperidol_after_ventilation", "haloperidol_before_ventilation" , "haloperidol_during_ventilation","first_careunit")

# imputing missing and outlier BMI
data_touse = 
  first_icu_haloperidol_in_icu %>%
  select(all_vars) %>%
  mutate(height_edited = ifelse(height %in% "NULL", NA, as.numeric(as.character(height))), 
         weight_edited = ifelse(height %in% "NULL", NA, as.numeric(as.character(weight))),
         
         # check outlier for non-imputed BMI
         bmi_noimpute = weight_edited/(height_edited/100)^2,
         
         # get z-score for bmi
         bmi_noimpute_zscore = ifelse(!is.na(bmi_noimpute), ((bmi_noimpute - mean(bmi_noimpute, na.rm = TRUE))/sd(bmi_noimpute, na.rm = TRUE )), 0),
         bmi_noimpute_zscore = as.numeric(bmi_noimpute_zscore),
         
         # to impute outlier BMI those missing
         bmi_impute_yn_1 =  ifelse(is.na(bmi_noimpute), 1,0),
         bmi_impute_yn_2 = ifelse(abs(bmi_noimpute_zscore) > 3, 1, 0), 
         bmi_impute_yn = ifelse(bmi_impute_yn_1 == 1 | bmi_impute_yn_2 == 1, 1,0 ),

         bmi_female = ifelse(gender %in% "F" & bmi_impute_yn == 0 ,bmi_noimpute , NA),
         bmi_male = ifelse(gender %in% "M" & bmi_impute_yn == 0 ,bmi_noimpute , NA),
         
         median_bmi_female = median(bmi_female, na.rm = TRUE), 
         median_bmi_male = median(bmi_male, na.rm = TRUE), 
         
         bmi_imputed = ifelse(bmi_impute_yn == 0, bmi_noimpute, NA),
         bmi_imputed = ifelse(gender %in% "F" & bmi_impute_yn == 1 , median_bmi_female, bmi_imputed),
         bmi_imputed = ifelse(gender %in% "M" & bmi_impute_yn == 1 , median_bmi_male, bmi_imputed),

         ethnicity_edited = str_to_title(ethnicity),
         ethnicity_edited = ifelse(ethnicity_edited %in% "Unable To Obtain", "Unknown",  ethnicity_edited),
         ethnicity_edited = ifelse(ethnicity_edited %in% c("American Indian/Alaska Native", "Black/African American"), "Black/African/Indian/Alaska Native American", ethnicity_edited),
         
         first_care_unit_edited = first_careunit, 
         first_care_unit_edited = ifelse(first_care_unit_edited %in% c("Neuro Intermediate", "Neuro Stepdown", "Neuro Surgical Intensive Care Unit (Neuro SICU)"), "Neurology", first_care_unit_edited), 
         first_care_unit_edited = ifelse(first_care_unit_edited %in% c("Cardiac Vascular Intensive Care Unit (CVICU)", "Coronary Care Unit (CCU)"), "Cardiology", first_care_unit_edited),
         first_care_unit_edited = ifelse(first_care_unit_edited %in% c("Trauma SICU (TSICU)"), "Trauma", first_care_unit_edited), 
         first_care_unit_edited = ifelse(first_care_unit_edited %in% c("Medical Intensive Care Unit (MICU)"), "Medical", first_care_unit_edited), 
         first_care_unit_edited = ifelse(first_care_unit_edited %in% c("Medical/Surgical Intensive Care Unit (MICU/SICU)"), "Medical/Surgical", first_care_unit_edited),
         first_care_unit_edited = ifelse(first_care_unit_edited %in% c("Surgical Intensive Care Unit (SICU)"), "Surgical", first_care_unit_edited ),
)


# variables to be used in the study
all_vars_v2 = c("subject_id", 
             "admission_age", "gender", "ethnicity","ethnicity_edited", "height", "weight", "bmi_impute_yn", "bmi_imputed", 
             "psyc_disorder", "sub_abuse", "charlson_comorbidity_index", "apsiii", "los_icu",
             "admit_date", "admit_time", "discharge_date", "discharge_time", "death_within_icu28days",
             "hospital_expire_flag", "dod", "icu_in_date", "icu_in_time", "icu_out_date", "icu_out_time", "vent_start_date", "vent_end_date", "vent_end_time", "haloperidol_give_date", "haloperidol_give_time", "haloperidol_stop_date", "haloperidol_stop_time", 
             "administered_haloperidol",  "haloperidol_after_icu", "haloperidol_after_ventilation", "haloperidol_before_ventilation" , "haloperidol_during_ventilation", "first_careunit",  "first_care_unit_edited")

data_touse_v2 = 
  data_touse %>%
  select(all_vars_v2)

# convert to numeric
data_touse_v2[,c("admission_age",  "charlson_comorbidity_index", "apsiii", "los_icu" )] = sapply(data_touse[c("admission_age",  "charlson_comorbidity_index", "apsiii", "los_icu" )], as.numeric)


######### Descriptive statistic (Original Cohort) ########

data_touse_v2$administered_haloperidol = 
  factor(data_touse_v2$administered_haloperidol, 
         levels=c(0,1),
         labels=c("No Haloperidol", 
                  "Haloperidol"))

data_touse_v2$gender = 
  factor(data_touse_v2$gender, 
         levels=c("M","F"),
         labels=c("Male", 
                  "Female"))

data_touse_v2$psyc_disorder = 
  factor(data_touse_v2$psyc_disorder, 
         levels=c(1,0),
         labels=c("Yes", 
                  "No"))

data_touse_v2$sub_abuse = 
  factor(data_touse_v2$sub_abuse, 
         levels=c(1,0),
         labels=c("Yes", 
                  "No"))


data_touse_v2$death_within_icu28days = 
  factor(data_touse_v2$death_within_icu28days,
         levels = c(1,0), 
         labels = c("Yes", 
                    "No")) 

data_touse_v2$severe_apsiii = factor(ifelse(data_touse_v2$apsiii > 50, 1, 0),
                                     levels = c(1,0), 
                                     labels = c("Yes", 
                                                "No"))


label(data_touse_v2$admission_age) = "Age"
label(data_touse_v2$gender) = "Gender"
label(data_touse_v2$ethnicity_edited) = "Ethnicity"
label(data_touse_v2$bmi_imputed) = "Body Mass Index (BMI)"
label(data_touse_v2$death_within_icu28days) = "28 Days Mortality"
label(data_touse_v2$charlson_comorbidity_index) = "Charlson Comorbidity Index (CCI)"
label(data_touse_v2$psyc_disorder) = "Psychiatric Disorders"
label(data_touse_v2$sub_abuse) = "Substance Abuse Disorders"
label(data_touse_v2$los_icu) = "Length of ICU stay"
label(data_touse_v2$apsiii) = "APS III Score"
label(data_touse_v2$severe_apsiii) = "APS III Severity"
label(data_touse_v2$first_care_unit_edited) = "First ICU Care Unit"

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


# original
table1(~ admission_age + gender + ethnicity_edited + bmi_imputed  +
         charlson_comorbidity_index + apsiii + severe_apsiii+ psyc_disorder + sub_abuse + los_icu + death_within_icu28days + first_care_unit_edited | administered_haloperidol, data = data_touse_v2, overall=F, extra.col=list(`P-value`=pvalue))

# check missing data
original_missing =
  data_touse %>%
  select(allvarstable) %>%
  summarise_all(funs(sum(is.na(.)) / length(.) * 100))


write.csv(data_touse_v2, "original_data.csv")

###### propensity score matching #####

# estimating propensity score
m_ps = glm(administered_haloperidol ~ as.factor(gender) + admission_age  + as.factor(ethnicity_edited) + apsiii + charlson_comorbidity_index + psyc_disorder + sub_abuse + as.factor(first_care_unit_edited),
           family = binomial(), data = data_touse_v2)

summary(m_ps)

data_touse_v2$psvalue = predict(m_ps, type = "response")

# using nearest neighbour
ps_match = matchit(administered_haloperidol ~ as.factor(gender) + admission_age  + as.factor(ethnicity_edited) + apsiii + charlson_comorbidity_index + psyc_disorder + sub_abuse + as.factor(first_care_unit_edited),
                   method = "nearest", data = data_touse_v2, replace = FALSE)

# using caliper with width 0.2
ps_match_2 = matchit(administered_haloperidol ~ as.factor(gender) + admission_age  + as.factor(ethnicity_edited) + apsiii + charlson_comorbidity_index + psyc_disorder + sub_abuse + as.factor(first_care_unit_edited),
                  caliper = 0.2 , data = data_touse_v2)

# using caliper with width 0.1
ps_match_3 = matchit(administered_haloperidol ~ as.factor(gender) + admission_age  + as.factor(ethnicity_edited) + apsiii + charlson_comorbidity_index + psyc_disorder + sub_abuse + as.factor(first_care_unit_edited),
                     caliper = 0.2 , data = data_touse_v2)


summary(ps_match)
summary(ps_match_2)
summary(ps_match_3)

# all three are very very similar

match_data = match.data(ps_match)
match_data$iptw = ifelse(match_data$administered_haloperidol %in% "Haloperidol", 1/match_data$distance, 1/(1-match_data$distance))
plot(ps_match, type = "hist")

write.csv(match_data ,"psm_data_V2.csv")

######### Descriptive statistic (Matched Cohort) ########

pvalue_paired <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a paired 2-sample t-test
    p <- t.test(y ~ g, paired = TRUE)$p.value
  } else {
    p <- NA
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ admission_age + gender + ethnicity_edited + bmi_imputed +  
         charlson_comorbidity_index + apsiii + severe_apsiii + psyc_disorder + sub_abuse + los_icu + death_within_icu28days +first_care_unit_edited | administered_haloperidol, data = match_data, overall=F,  extra.col=list(`P-value`=pvalue_paired))

# compute paired tests 

contvars = c("admission_age", "bmi_imputed", "charlson_comorbidity_index", "apsiii", "los_icu")

y_trt = match_data[which(match_data$administered_haloperidol == "Haloperidol"),]
y_cont = match_data[which(match_data$administered_haloperidol == "No Haloperidol"),]

# paired mcnmar/mh
mcnemar.test(table(y_trt$gender, y_cont$gender))
coin::mh_test(table(y_trt$ethnicity_edited, y_cont$ethnicity_edited))
mcnemar.test(table(y_trt$psyc_disorder, y_cont$psyc_disorder))
mcnemar.test(table(y_trt$sub_abuse, y_cont$sub_abuse))
mcnemar.test(table(y_trt$severe_apsiii, y_cont$severe_apsiii))
mcnemar.test(table(y_trt$death_within_icu28days, y_cont$death_within_icu28days))
coin::mh_test(table(y_trt$first_care_unit_edited, y_cont$first_care_unit_edited))


