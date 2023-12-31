#=========================================================================
# Purpose: Main R file for Preparing/Cleaning Data
# Author: Jacob Jameson 
#=========================================================================
rm(list = ls()) 

library(tidyverse)
library(stringr)
library(lfe)
library(lubridate)

path <- "~/Sue Goldie Dropbox/Jacob Jameson/Batch vs sequential testing/Data/"
data <- read.csv(paste0(path, 'deidentified_FINAL.csv'))

#=========================================================================
# Clean Vars -------------------------------------------------------------
#=========================================================================

# Identify columns with *_REL suffix
rel_cols <- grep("_REL$", names(data), value = TRUE)

# Apply the transformation to each *_REL column
for (col in rel_cols) {
  data <- data %>%
    separate(col, c(paste0(col, "_hours"), paste0(col, "_minutes")), sep = ":") %>%
    mutate(!!col := as.numeric(get(paste0(col, "_hours"))) * 60 + 
             as.numeric(get(paste0(col, "_minutes"))))
}

data <- data %>%
  select(-matches("_hours$|_minutes$"))


data$PATIENT_RACE <- str_to_lower(data$PATIENT_RACE)

data <- data %>%
  mutate(race = case_when(
    grepl('black', PATIENT_RACE, fixed = TRUE) ~ "black",
    grepl('african', PATIENT_RACE, fixed = TRUE) ~ "black",
    grepl('asian', PATIENT_RACE, fixed = TRUE) ~ "asian",
    grepl('pacific islander', PATIENT_RACE, fixed = TRUE) ~ "asian",
    grepl('native', PATIENT_RACE, fixed = TRUE)~ "native",
    grepl('samoan', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('guamanian or chamorro', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('white', PATIENT_RACE, fixed = TRUE) ~ "white",
    grepl('unknown', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('choose not to disclose', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('unable to provide', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    grepl('other', PATIENT_RACE, fixed = TRUE) ~ "other",
    grepl('', PATIENT_RACE, fixed = TRUE) ~ "unknown",
    TRUE ~ PATIENT_RACE))

data$ARRIVAL_AGE_DI <- ifelse(
  data$ARRIVAL_AGE_DI == '85+', '85', data$ARRIVAL_AGE_DI
)
data$ARRIVAL_AGE_DI <- as.numeric(data$ARRIVAL_AGE_DI)

#=========================================================================
# Clean Chief Complaint --------------------------------------------------
#=========================================================================

complaints <- list(
  "Abdominal Complaints" = 
    c('Abdominal Cramping', 'Abdominal Distention', 'Dyspepsia',
      'Abdominal Pain', 'Ascites', 'Hernia', 
      'Abdominal Aortic Aneurysm', 'Abdominal Injury', "Pancreatitis",
      'Umbilical Hernia'),
  'Abnormal Test Results' = 
    c('Abnormal Lab', 'Abnormal Potassium', 'Abnormal Calcium', 
      'ECG Changes', 'Abnormal ECG', 'Abnormal Test Result', 
      'Blood Infection', 'Acute Renal Failure', 'Hypocalcemia',
      'Chronic Renal Failure', 'Pulmonary Embolism', 'Abnormal X-ray', 
      'Hypoglycemic Unawareness', 'Elevated Blood Pressure', 
      'Abnormal Sodium', 'Hyperglycemia', 'Hyponatremia', 
      'Platelet Disorders', 'Anemia', 'Hypoglycemia', 'Hypertension', 
      'Hypotension', 'Abnormal Chest Imaging', 'Abnormal Oximetry', 
      'Abnormal Stress Test', 'Blood Sugar Problem',
      'Hypocalcemia', 'Hyponatremia'),
  'Allergic Reaction' = 
    c('Allergic Reaction', 'Anaphylaxis'),
  'Back or Flank Pain' = 
    c('Back Pain', 'Back Problem', 'Flank Pain', 
      'Sciatica', 'Back Injury', 'Disc Disorder'),
  'Breast Complaints' = 
    c('Breast Mass', 'Breast Pain', 'Breast Problem', 'Breast Discharge',
      'Breast Cancer', 'Breast Discharge', 'Breast Inflammation'),
  'Cardiac Arrhythmias' = 
    c('Atrial Fibrillation', 'Atrial Flutter', 'Cardiac Valve Problem',
      'Bradycardia', 'Irregular Heart Beat', 'Palpitations', 'POTS', 
      'Ventricular Tachycardia','Rapid Heart Rate', 'Heart Problem', 
      'Cardiac Arrest', 'Congestive Heart Failure', 'Circulatory Problem',  
      "Transient Ischemic Attack", 'Ventricular Tachycardia'),
  'Chest Pain' = 
    c('Chest Injury', 'Chest Pain', 'Chest Wall Pain', 'Angina',
      'Collarbone Injury', 'Rib Injury', 'Heart Pain'),
  'Dizziness/Lightheadedness/Syncope' = 
    c('Dizziness', 'Near Syncope', 'Syncope', 'Vertigo', 'Spells',
      'Hypotension', 'Paroxysmal Positional Vertigo', 
      'Paroxysmal Positional Vertig'),
  'Ear Complaints' = 
    c('Cerumen Impaction', 'Ear Drainage', 'Ear Fullness',
      'Ear Laceration', 'Ear Problem', 'Earache',
      'Hearing Problem', 'Tinnitus', 'Ear Injury', 'Hearing Loss',
      'Nasal Trauma'),
  'Epistaxis' = 
    c('Epistaxis', 'Epistaxis (Nose Bleed)', 'Nose Problem'),
  'Exposures, Bites, and Envenomations' =
    c('Animal Bite', 'Body Fluid Exposure', 'Chemical Exposure', 
      'Poisoning', 'Exposure to STD', 'Insect Bite', 'Smoke Inhalation', 
      'Radiation', 'Snake Bite', 'Toxic Inhalation'),
  'Extremity Complaints' = 
    c('Ankle Injury', 'Ankle Pain', 'Arm Injury', 'Arm Pain', 
      'Cold Extremity', 'Arm Swelling', 'Arthritis', 'Elbow Injury', 
      'Elbow Pain', 'Pseudogout', 'Extremity Pain', 'Extremity Weakness', 
      'Finger Injury', 'Hip Injury', 'Extremity Weakness', 
      'Finger Injury', 'Finger Pain', 'Dislocation',
      'Foot Infection', 'Foot Injury', 'Foot Numbness', 'Foot Pain',
      'Foot Swelling', 'Foot Ulcer', 'Foot Wound Check', 'Hand Injury',
      'Hand Pain', 'Hip Pain', 'Joint Pain', 'Joint Swelling', 
      'Knee Injury', 'Knee Pain', 'Knee Problem', 'Leg Injury', 
      'Leg Pain', 'Leg Swelling', 'Lower Extremity Issue', 
      'Pain in Limb', 'Shoulder Injury', 'Shoulder Pain', 'Toe Injury', 
      'Upper Extremity Issue', 'Wrist Injury',
      'Wrist Pain', 'Hand Problem', 'Leg Cramps', 'Arm Problem',
      'Foot Problem', 'Pain In Limb', 'Toe Pain', 'Hand Burn',
      'Foot Burn', 'Neck Injury', 'Leg Problem', 'Deep Vein Thrombosis',
      "Varicose Veins",  "Ingrown Toenail", 'Hip Injury', 'Wound Care', 
      'Venous Thromboembolic Diseas'),
  'Eye Complaints' = 
    c('Blurred Vision', 'Decreased Visual Acuity', 'Diplopia', 
      'Detached Retina', 'Eye Drainage', 'Eye Exposure', 'Eye Pain', 
      'Eye Problem', 'Eye Swelling', 'Eye Trauma', 'Foreign Body Eye',
      'Flashes/Light', 'Loss of Vision', 'Red Eye', 'Visual Field Change', 
      'Eyelid Problem', 'Itchy Eye', 'Eye Exam', 'Burning Eyes', 
      'Eye Twitching', "Eyelid/brow Lift Evaluation",
      'Strabismus', 'Glaucoma',  "Spots/Floaters"),
  'Falls, Motor Vehicle Crashes, Assaults, and Trauma' =
    c('Assault Victim', 'Concussion', 'Facial Injury', 'Fall', 
      'Nasal Trauma', 'Head Injury', 'Head Laceration', 
      'Motor Vehicle Crash', 'Puncture Wound', 
      'Sexual Assault', 'Trauma', 'Domestic Violence', 'Gun Shot Wound',
      'Work Related Injury', 'Motorcycle Crash', 'Injury', 
      'Bicycle Accident', 'Near Drowning', 'Lip Laceration'),
  'Fatigue and Weakness' =
    c('Difficulty Walking', 'Fatigue', 'Gait Problem', 
      'Weakness-Generalized',
      'Chronic Fatique', 'Weakness - Generalized'),
  'Fevers, Sweats or Chills' =
    c('Chills', 'Diaphoresis', 'Fever', 'Night Sweats', 'Diaphoretic', 
      'Diapohresis', 'Hoarseness', 'Laryngitis'),
  'Foreign Body' =
    c('Food Bolus', 'Foreign Body', 'Foreign Body in Ear',
      'Foreign Body in Skin', 'Foreign Body in Vagina',
      'Swallowed Foreign Body', 'Foreign Body in Nose', 
      'Foreign Body', 'FB eye', 'Foreign Body in Rectum'),
  'Gastrointestinal Issues' =
    c('Anal Fissure', 'Black or Bloody Stool', 'Constipation', 'GERD', 
      'Anal Fistula', 'Diarrhea', 'Dysphagia', 'Fecal Impaction',
      'Fistula Follow Up', 'GIbleeding', 'GI Problem', 'Hemorrhoids', 
      'Morning Sickness', 'Nausea', 'Ostomy Care', 'Rectal Bleeding',
      'Rectal Pain', 'Vomiting', 'Vomiting Blood', 
      'Vomiting During Pregnancy',
      'GI Bleeding', 'Fecal Incontinence',
      'Bloated', 'Hematochezia', 'Urine Leakage', 'Heartburn',
      'Rectal Discharge', 'Urolithiasis', "Ulcerative Colitis",
      "Irritable Bowel Syndrome", "Rectal Prolapse","Fistula Evaluation",
      "Rectal Problems", 'Perianal Abscess', 'Fisula Evaluation', 
      'Stoma Dysfunction'),
  'Genital Complaints' =
    c('Groin Burn', 'Groin Pain', 'Groin Swelling', 'Inguinal Hernia', 
      'Menstrual Problem', 'Pelvic Pain', 'Penis Pain', 'Priapism', 
      'Testicle Pain', 'Menorrhagia',
      'Vaginal Bleed', 'Vaginal Bleeding', 'Vaginal Itching', 
      "Bartholin's Cyst", 'Genital Warts', 'Groin Injury', 
      'Vaginal Bleeding - Pregnant', 
      'Vag Bleed Pregnant', 'Female Genital Issue', 'Penis Injury',
      'Vaginal Discharge', 'Vaginal Pain', 'Erectile Dysfunction',
      'Vaginal Prolapse', 'Urethral Stricture', 'Penile Discharge',
      'Menorrhagia', "Gynecologic Exam", "Menstrual Problem", 
      "Vaginitis/Bacterial Vaginosis",
      'Ovarian Cyst', 'Vaginitis/Bacterial Vaginosi'),
  'Medical Device or Treatment Issue' =
    c('Cast Problem', 'Device Check', 'Dressing Change', 'Feeding Tube', 
      'AICD Problem', 'Insulin Pump Visit', 'Gastrostomy Tube Change', 
      'Medication Reaction', 'Shunt', 'Appliance Removal', 'Tube Problem', 
      'Urinary Catheter Change', 'Vascular Access Problem', 
      'Enteral Nutrition Evaluation', 'Device Malfunction', 
      'Pacemaker Problem', 
      'Removal / Exchange Catheter', 'Drain Removal', 
      'Outpatient Infusion', 
      'Treatment', 'Heart Assist Device', 'Stoma Dysfunction', 
      'Tracheostomy Tube Change',
      'Ureteral Stent Exchange'),
  'Medication Request' =
    c('Immunizations', 'Infusion/Injection Administration', 
      'IV Medication',  'Infusion/ Injection Administ', 'Med Refill',
      'Medication Visit', 
      'Pain Management', 'Blood Product Administration', 'Labs Only', 
      'Tetanus (Td & Tdap)', 'Wound Care'),
  'Neurological Issue' =
    c('Altered Mental Status', 'Cognitive Concerns', 'Facial Droop',
      'Pre Syncope', 'Focal Weakness', 'Headache', 'Memory Loss', 
      'Migraine', 'Dementia', 'Dysphasia',
      'Neuro Problem', 'Numbness', 'Paralysis', 'Seizures',
      'Slurred Speech', 'Spasms', 'Stroke Like Symptoms', 'Tingling',
      'Tremors', 'Trigeminal Neuralgia', 'Unable to Speak', 
      'Seizure Disorder', 'Insomnia', "Parkinson's Disease",
      'Loss of Consciousness', 'Neuropathy', 'Ataxia', 'Unable to speak',
      'Peripheral Neuropathy',
      'Stroke', 'Cerebrovascular Accident', 'Speech Problem', 
      'Acute Neurological Problem',
      'Flashes, Light', 'Unresponsive', "Multiple Sclerosis", 
      "Parkinson's Disease",
      "Febrile Seizure", 'Paresthesia', 'Peripheral Neuropathy', 
      'Hydrocephalus',
      'Spasticity', 'Neuroendocrine Tumor'),
  'Other' =
    c('Dehydration', 'Fisula Evaluation', 'Follow-Up', 'Illness', 
      'Letter for School/Work', 'Aneurysm',
      'Lung Eval', 'Error', 'Mass', 'Oral Swelling', 'Other', 
      'Advice Only', 'Deformity', 'Electric Shock',
      'Personal Problem', 'Shaking', 'Swelling', 'Swollen Glands', 
      'Adenopathy', 'Adrenal Problem',
      'Thrombophilia', 'Weight Gain', 'Weight Loss', 'Hiccups', '', 
      'Chemo Related Symptoms', 'Hot Flashes',
      'Follow-up', 'Non Healing Wound', '(Other)', 'Mouth Injury', 
      'Xerostomia', 'Prostate Check',
      'Suture / Staple Removal', 'Wellness', 'Voice Changes', 
      'Vital Sign Check', 'Coagulation Disorder',
      'Cold Exposure', 'Consult', 'Dental Problem', 
      'Tetanus (Td & Tdap)', "Infusion/ Injection Administ",
      "Tracheostomy Tube Change", 'Medical Information', 
      'Neutropenic Fever', 'Infection',
      'Leukemia',"Heat Exposure", "Poor Appetite", 'Gingivitis',
      "Pre-op Exam",
      'gingivitis', "Loss of appetite", "Failure To Thrive", 
      'Referral', 'Lymphoma',
      "Hot Flashes", 'Neutropenia', 'Radiation', 'Ingestion', 
      "TB Test", 'Fussy',
      'Lupus', 'Toxic Inhalation', 'Lung Screening', 
      'Leakage/Loss of Fluid',
      'Liver Eval', 'Hepatic Cancer', 'Lung Mass', 
      'Venous Thromboembolic Disease',
      'Insulin Pump Visit', 'Preventive Visit', 
      'Avulsion', 'Peripheral Edema',
      'Hypoglycemic Unawareness', 'Immobility', 
      'Giant Cell Arteritis', 'Polydipsia',
      'Platelet Disorders', 'Post-procedure', 'Lung Follow-up', 
      'Poisoning',
      'Injections', 'POTS', "Insulin Reaction", 
      'Liver Transplant', 'Labs Only'),
  'Other Pain' =
    c('Dental Pain', 'Facial Pain', 'Generalized Body Aches', 
      'Myalgia', 'Dental Injury',
      'Jaw Pain', 'Muscle Pain', 'Neck Pain', 'Pain', 
      'Sickle Cell Pain Crisis', 'Paresthesia',
      'Torticollis', 'Chronic Pain', 'Cancer Pain', 
      'Incisional Pain', 'Bone Pain',
      'Tailbone Pain', 'Gout', "Muscle pain/Weakness", 'Pseudogout'),
  'Post-Op Issue' =
    c('Post-Op', 'Post-Procedure', 'Post-Op Problem', 'Post-op',
      'Post-Op Issue', 'Wound Dehiscence', 'Post-op Problems',
      "Post-op Problem"),
  'Psychiatric Complaints' =
    c('Anxiety', 'Auditory Hallucinations', 'Depression', 'Panic Attack',
      'Homicidal', 'PTSD (Post-Traumatic Stress', 'Delusional', 'Fussy',
      'Paranoia', 'Suicide Attempt', 'Hallucinations', 'Manic Behavior',
      'Eating Disorder', 'Suicidal', 'Agitation', 'Psychiatric Evaluation',
      'Aggressive Behavior', 'Mental Health Problem', 
      'Inappropriate Words'),
  'Shortness of Breath' =
    c('Airway Obstruction', 'Aspiration', 'Pain With Breathing', 
      'Near Drowning',
      'Respiratory Distress', 'Shortness of Breath', 'Wheezing',
      'Increased Work Of Breathing',
      'Difficulty Breathing', 'Choking', "Oxygen Dependence",
      'Hyperventilating', 'Orthopnea'),
  'Skin Complaints' =
    c('Abrasion','Abscess', 'Bleeding/Bruising', 'Blister',
      'Angioedema', 'Lip Laceration',
      'Burn', 'Cellulitis', 'Cyst', 'Drainage from Incision', 
      'Disturb of Skin Sens',
      'Edema', 'Extremity Laceration', 'Facial Burn', 'Cyanosis',
      'Impetigo', 'Facial Laceration', 'Facial Swelling',
      'Finger Laceration', 'Leg Rash',
      'Herpes Zoster', 'Hives', 'Itching', 'Jaundice', 
      'Diabetic Ulcer', 'Diabetic Wound',
      'Laceration', 'Mouth Lesions', 'Non-Healing Wound', 'Rash', 
      'Recurrent Skin Infections', 'Skin Problem', 'Sore', 'Scabies',
      'Suture/Staple Removal', 'Wound Check', 'Wound Infection',
      'Lesion', 'Skin Check', 'Minor Skin Infection', 'Skin Ulcer',
      'Skin Discoloration', 'Sunburn', "Head Lice", 'Scabies',
      "Fungal Infection", "Leg Rash", 'Impetigo'),
  'Substance Abuse Issues' =
    c('Alcohol Intoxication', 'Alcohol Problem', 'Withdrawal',
      'Drug Overdose', 'Drug / Alcohol Dependency', 'Addiction Problem',
      'Addiction Assessment', 'Delirium Tremens (DTS)'),
  'Upper Respiratory Symptoms' =
    c('Congestion', 'Cough', 'Coughing Up Blood', 
      'Flu Symptoms', 'Enlarged Tonsils', 'Peritonsillar Abscess',
      'Nasal Congestion', 'Sinus Symptoms', 'Sinusitis',
      'Sore Throat', 'Hoarseness',
      'Throat Problem', 'Upper Respiratory Infection', 
      'Influenza', 'Laryngitis',
      'Respiratory Arrest', 'Pneumonia', 'Pleural Effusion', 'Asthma',
      'Croup', 'URI', 'Peritonsillar Abscess'),
  'Pregnancy Related' = 
    c("Pregnancy Problem", "Miscarriage", "Contractions", 
      'Ectopic Pregnancy',
      'Laboring',"Possible Pregnancy", "Pregnancy Related"), 
  'Renal' = 
    c('Av Fistula', 'Kidney Transplant', 'Elevated Serum Creatinine', 
      'End-Stage Liver Disease', "Hemodialysis Access", 'Nephritis',
      'Ureteral Stent Exchange'),
  'Urinary Complaints' =
    c('Bladder Problem', 'Blood in Urine', 'Cystitis',
      'Difficulty Urinating',
      'Dysuria', 'Gross Hematuria', 'Painful Urination',
      'Urinary Frequency', 'Urinary Symptom',
      'Urinary Incontinence', 'Urinary Problem', 
      'Urinary Retention', 'Slowing Urinary Stream',
      'Urinary Tract Infection', 'Urinary Urgency', 
      'Voiding Dysfunction',"Hesitancy Urinary")
)


for (i in seq(1,length(complaints))){
  name <- names(complaints[i])
  complaint <- complaints[[i]]
  
  data$CHIEF_COMPLAINT <- ifelse(
    data$CHIEF_COMPLAINT %in% complaint, name, data$CHIEF_COMPLAINT
  )
}

data <- data %>%
  mutate(complaint_esi  = paste(ESI, CHIEF_COMPLAINT),
         complaint_esi = factor(complaint_esi))

#=========================================================================
# Categorize Vital Signs -------------------------------------------------
#=========================================================================

data$tachycardic <- ifelse(
  is.na(data$TRIAGE_PULSE) == F & data$TRIAGE_PULSE > 100, 1, 0
)
data$tachypneic <- ifelse(
  is.na(data$TRIAGE_RR)  == F  & data$TRIAGE_RR > 20, 1, 0
)
data$febrile <- ifelse(
  is.na(data$TRIAGE_TEMP)  == F  & data$TRIAGE_TEMP > 38, 1, 0
)
data$hypotensive <- ifelse(
  is.na(data$TRIAGE_SBP)  == F  & data$TRIAGE_SBP < 90, 1, 0
)

#=========================================================================
# Create Time FE ---------------------------------------------------------
#=========================================================================

data <- data %>%
  mutate(rel_minutes_arrival = ARRIVAL_DTTM_REL,
         rel_minutes_depart = rel_minutes_arrival + ED_LOS)

# Calculate the number of patients in the hospital at the time of each patient's arrival
data$patients_in_hospital <- sapply(data$rel_minutes_arrival, function(arrival_time) {
  sum(data$rel_minutes_arrival <= arrival_time & data$rel_minutes_depart > arrival_time) - 1
})

# Define arbitrary start date
start_date <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")

# Convert relative minutes to datetime
data$datetime <- start_date + minutes(data$rel_minutes_arrival)

# Extract hour of day, day of week, and month of year
data$hour_of_day <- hour(data$datetime)
data$day_of_week <- weekdays(data$datetime)
data$month_of_year <- month(data$datetime, label = TRUE)
data$dayofweekt <- paste(data$day_of_week, data$hour_of_day)

#=========================================================================
# Create Final Dataset ---------------------------------------------------
#=========================================================================

data$ln_ED_LOS <- log(data$ED_LOS)

final <- data %>% 
  mutate(RTN_72_HR = ifelse(RTN_72_HR == 'Y', 1, 0),
         RTN_72_HR_ADMIT = ifelse(RTN_72_HR_ADMIT == 'Y', 1, 0)) 

final$age_groups <- cut(final$ARRIVAL_AGE_DI, 
                        c(-Inf, 20, 45, 65, Inf),
                        labels = c("<20", "20-45", "45-65", "65+"))

final$admit = ifelse(final$ED_DISPOSITION == 'Admit', 1, 0)
final$discharge = ifelse(final$ED_DISPOSITION == 'Discharge', 1, 0)
final$observation = ifelse(final$ED_DISPOSITION == 'Observation', 1, 0)

final <- final %>%
  mutate(dispo = case_when(
    admit == 1 ~ 'admit',
    discharge == 1 ~ 'discharge',
    observation == 1 ~ 'observeration',
    TRUE ~ 'other')) 

# Limit dataset to only physicians that had more than 520 encounters
provider_counts <- table(final$ED_PROVIDER)
providers_less_than_500 <- names(provider_counts[provider_counts < 520])
final <- final[!(final$ED_PROVIDER %in% providers_less_than_500), ]
final$complaint_esi <- paste(final$CHIEF_COMPLAINT, final$ESI)

rm(list = setdiff(ls(), "final"))

final$LAB_PERFORMED <- ifelse(final$LAB_PERF == 'Y', 1, 0)

#=========================================================================
##########################################################################
#=========================================================================
# IV Construction --------------------------------------------------------
#=========================================================================
##########################################################################
# Step 1: leave-out residualize at the ED encounter level
## conditional on shift-level variation, random assignment
## residual from regression represents physician testing inclincation
final$residual_lab <- resid(
  felm(LAB_PERFORMED ~ 0 | dayofweekt + month_of_year + complaint_esi, data=final))

# Step 2: get batch tendency for each provider
final <- final %>%
  group_by(ED_PROVIDER) %>%
  mutate(Sum_Resid=sum(residual_lab, na.rm=T),
         test.inclination = (Sum_Resid - residual_lab) / (n() - 1)) %>%
  ungroup()

write.csv(final, 'outputs/data/all_clean.csv')

# Limit to prevalent complaints only
final <- final %>%
  group_by(CHIEF_COMPLAINT) %>%
  filter(n() > 1000) %>%
  ungroup()

write.csv(final, 'outputs/data/final.csv')
#=========================================================================