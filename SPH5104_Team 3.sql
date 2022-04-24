SELECT DISTINCT icu.*,
ven.starttime AS ven_starttime,
ven.endtime AS ven_endtime,
ven.ventilation_status,
p1.pharmacy_id, p1.starttime as pharm_starttime,
p1.stoptime as pharm_stoptime,
p1.drug_type,p1.drug,p1.gsn,p1.prod_strength,p1.form_rx,
p1.dose_val_rx,p1.dose_unit_rx,p1.form_val_disp,p1.form_unit_disp,
p1.doses_per_24_hrs, p1.route,
CASE WHEN dia.subject_id IN (SELECT DISTINCT dia1.subject_id
					FROM mimic_hosp.diagnoses_icd dia1
					WHERE dia1.icd_code IN (
	'29500','29501','29502','29503','29504','29510','29511',
	'29512','29513','29514','29520','29521','29522','29523',
	'29524','29530','29531','29532','29533','29534','29540',
	'29541','29542','29543','29544','29560','29561','29562',
	'29563','29564','29570','29571','29572','29573','29574',
	'29580','29581','29582','29583','29584','29590','29591',
	'29592','29593','29594','29610','29611','29612','29613',
	'29614','29631','29632','29633','29634','29640','29641',
	'29642','29643','29644','29650','29651','29652','29653',
	'29654','29660','29661','29662','29663','29664','2967',
	'29680','29681','29682','29689','2971','2980','2981','2984',
	'2989','30002','3003','3071','30750','30751','30981','311',
	'F20','F200','F201','F202','F203','F205','F208','F2081','F2089',
	'F209','F21','F25','F250','F251','F258','F259','F31','F310','F311',
	'F3110','F3111','F3112','F3113','F312','F313','F3130','F3131','F3132',
	'F314','F315','F316','F3160','F3161','F3162','F3163','F3164','F3171',
	'F3173','F3175','F3177','F318','F3181','F3189','F319','F329','F33','F330',
	'F331','F332','F333','F3341','F338','F339','F411','F42','F429','F43','F431',
	'F4310','F4311','F4312','F50','F500','F5000','F5001','F5002','F502','F840',
	'F842','F843','F845','F848','F849','F88','F89','F90','F900','F901','F902','F908','F909'))
	THEN 1 ELSE 0 END AS psyc_disorder,
CASE WHEN dia.subject_id IN (SELECT DISTINCT dia2.subject_id
					FROM mimic_hosp.diagnoses_icd dia2
					WHERE dia2.icd_code IN (
	'2911','2912','2913','2914','2915','29181','29182','29189',
'2919','30300','30301','30302','30500','30501','30502','2910',
'30390','30391','30392','4255','53530','53531','5710','7903',
'F10','G312','G621','G721','I426','K292','K2920','K2921',
'K704','K7040','K7041','K70','K700','K701','K7010','K7011',
'K702','K703','K7030','K7031','K709','K852','K8520','K8521',
'K8522','K860','R780','T51','T518','T518X','T518X2','T518X2A',
'T518X2D','T518X2S','T518X3','T518X3A','T518X3D','T518X3S','T518X4','T518X4A',
'T518X4D','T518X4S','T519','T5192','T5192XA','T5192XD','T5192XS','T5193',
'T5193XA','T5193XD','T5193XS','T5194','T5194XA','T5194XD','T5194XS',
'Y90','Z714','Z7141'))
	THEN 1 ELSE 0 END AS sub_abuse,
weight.weight,
height.height,
aps.apsiii, aps.apsiii_prob, aps.hr_score, 
aps.mbp_score, aps.temp_score, aps.resp_rate_score, 
aps.pao2_aado2_score, aps.hematocrit_score, aps.wbc_score, 
aps.creatinine_score, aps.uo_score, aps.bun_score, aps.sodium_score, 
aps.albumin_score, aps.bilirubin_score, aps.glucose_score, aps.acidbase_score, 
aps.gcs_score,cha.charlson_comorbidity_index,
cha.age_score, cha.myocardial_infarct, cha.congestive_heart_failure, 
cha.peripheral_vascular_disease, cha.cerebrovascular_disease, 
cha.dementia, cha.chronic_pulmonary_disease, 
cha.rheumatic_disease, cha.peptic_ulcer_disease, cha.mild_liver_disease, 
cha.diabetes_without_cc, cha.diabetes_with_cc, 
cha.paraplegia, cha.renal_disease, cha.malignant_cancer, cha.severe_liver_disease, 
cha.metastatic_solid_tumor, cha.aids
FROM mimic_derived.icustay_detail icu
INNER JOIN mimic_derived.ventilation ven
ON icu.stay_id = ven.stay_id
AND icu.first_icu_stay = 'true'
AND ven.starttime >=icu.icu_intime
AND ven.starttime <=icu.icu_outtime
AND LOWER (ven.ventilation_status) like 'invasive%'
AND icu.subject_id NOT IN (
	SELECT DISTINCT dia3.subject_id FROM mimic_hosp.diagnoses_icd dia3
	INNER JOIN mimic_hosp.d_icd_diagnoses icd
	ON dia3.icd_version = icd.icd_version
	AND dia3.icd_code = icd.icd_code
	WHERE LOWER(icd.long_title) LIKE '%dementia%'
	OR LOWER(icd.long_title) LIKE '%parkinson%'
	OR Lower(icd.long_title) like '%arrhythmia%'
	OR Lower(icd.long_title) like '%pregnan%'
	OR Lower(icd.long_title) like '%lactat%'
	OR LOWER(icd.long_title) like 'coma%')
LEFT join mimic_hosp.prescriptions p1
ON p1.hadm_id = icu.hadm_id
AND LOWER(p1.drug) like '%haloperidol%'
AND p1.hadm_id NOT IN (
	SELECT DISTINCT p2.hadm_id FROM mimic_hosp.prescriptions p2
	INNER JOIN mimic_derived.icustay_detail icu2
	ON p2.hadm_id = icu2.hadm_id
	AND LOWER(p2.drug) like '%haloperidol%'
	AND p2.starttime >= icu2.admittime
	AND p2.starttime < icu2.icu_intime)
LEFT join mimic_hosp.diagnoses_icd dia
ON dia.subject_id = icu.subject_id
LEFT JOIN mimic_derived.first_day_weight weight
ON weight.stay_id = icu.stay_id
LEFT JOIN mimic_derived.first_day_height height
ON height.stay_id = icu.stay_id
LEFT join mimic_derived.apsiii aps
ON aps.hadm_id = icu.hadm_id
LEFT join mimic_derived.charlson cha
ON cha.hadm_id = icu.hadm_id;
