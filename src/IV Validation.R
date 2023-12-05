#=========================================================================
# Purpose: Validate IV + Main Results
# Author: Jacob Jameson 
#=========================================================================

library(lfe)
library(stargazer)
library(texreg)
library(xtable)
library(tidyverse)

data <- read_csv('outputs/data/final.csv')

##########################################################################
#=========================================================================
# Establishing IV Validity -----------------------------------------------
#=========================================================================
## Relevance
## Exclusion
## Monotonicity
#=========================================================================
##########################################################################

##########################################################################
#=========================================================================
# Relevance --------------------------------------------------------------
## First Stage results
#=========================================================================
##########################################################################

# Shift-level FE
first_stage_final <- felm(
  LAB_PERFORMED ~ test.inclination | 
    dayofweekt + month_of_year |0| ED_PROVIDER, 
  data = data)

# Shift-level + complaint FE
first_stage_final_d <- felm(
  LAB_PERFORMED ~ test.inclination | 
    dayofweekt + month_of_year + age_groups + 
    complaint_esi | 0 | ED_PROVIDER,
  data = data)

# Shift-level + complaint + individual FE
first_stage_final_d_i <- felm(
  LAB_PERFORMED ~ test.inclination | 
    dayofweekt + month_of_year + age_groups + 
    complaint_esi + GENDER + race | 0 | ED_PROVIDER,
  data = data)

# Save the results to a .txt file
sink("outputs/tables/First Stage.txt")

screenreg(list(first_stage_final,
               first_stage_final_d,
               first_stage_final_d_i), 
          include.fstatistic = T)

stargazer(list(first_stage_final,
               first_stage_final_d,
               first_stage_final_d_i),
          type = "text", header = FALSE, 
          title = "First Stage", style = 'QJE')

sink()


##########################################################################
#=========================================================================
# Reduced-Form Results ---------------------------------------------------
#=========================================================================
##########################################################################

model.admit <- felm(
  admit ~ test.inclination + patients_in_hospital | 
    dayofweekt + month_of_year + complaint_esi |0| ED_PROVIDER, 
  data = data)

model.72 <- felm(
  RTN_72_HR ~  test.inclination + patients_in_hospital| 
    dayofweekt + month_of_year + complaint_esi |0| ED_PROVIDER, 
  data = data)


#=========================================================================
# All coefficients are scaled by the difference in tendency/inclination
# between the ninetieth and tenth percentile physicians for 
# interpretability. 
#=========================================================================

percentile_10.t <- quantile(data$test.inclination, probs = 0.10)
percentile_90.t <- quantile(data$test.inclination, probs = 0.90)

coeffecient.scale.t <- percentile_90.t - percentile_10.t

# Save the results to a .txt file
sink("outputs/tables/Reduced Form.txt")

stargazer(list(model.admit, 
               model.72), type = "text", 
          header = FALSE, title = "Reduced Form", style = 'QJE')


# Function for scaling the coefficients in regressions for 
# interpretability
scale_coefficients <- function(model, scale_factors) {
  
  scaled_model <- model
  
  scaled_model$coefficients[1, "Estimate"] <-
    model$coefficients[1, "Estimate"] * scale_factors[1]
  
  scaled_model$coefficients[1, "Cluster s.e."] <- 
    model$coefficients[1, "Cluster s.e."] * scale_factors[1]
  
  return(scaled_model)
}

scale_factors <- c(coeffecient.scale.t)

# Scaled regression results
summary(model.admit)$call
scale_coefficients(
  summary(model.admit), 
  scale_factors)$coefficients[,c('Estimate', 'Cluster s.e.')]

summary(model.72)$call
scale_coefficients(
  summary(model.72), 
  scale_factors)$coefficients[,c('Estimate', 'Cluster s.e.')]

sink()

##########################################################################
#=========================================================================
# Exclusion --------------------------------------------------------------
## Placebo Check
#=========================================================================
##########################################################################

data <- read_csv('outputs/data/all_clean.csv')

placebo.complaints <- data %>%
  group_by(CHIEF_COMPLAINT) %>%
  summarise(mean.LAB_PERFORMED = mean(LAB_PERFORMED), n=n()) %>%
  filter(mean.LAB_PERFORMED <= 0.30)

placebo.complaints <- placebo.complaints$CHIEF_COMPLAINT

placebo <- data %>%
  filter(CHIEF_COMPLAINT %in% placebo.complaints)

# Save the results to a .txt file
sink("outputs/tables/Placebo Check.txt")

#=========================================================================
# Time controls only
#=========================================================================

placebo.admit <- felm(
  admit ~ test.inclination | 
    dayofweekt + month_of_year |0| ED_PROVIDER, 
  data = placebo)

placebo.72 <- felm(
  RTN_72_HR ~ test.inclination | 
    dayofweekt + month_of_year |0| ED_PROVIDER, 
  data = placebo)


stargazer(list(placebo.admit,
               placebo.72),
          type = "text", header = FALSE, 
          title = "Placebo Check-- no controls", style = 'QJE')

#=========================================================================
# Time controls + complaint severity
#=========================================================================

placebo.LOS <- felm(
  admit ~  test.inclination | 
    dayofweekt + month_of_year + complaint_esi |0| ED_PROVIDER, 
  data = placebo)

placebo.72 <- felm(
  RTN_72_HR ~ test.inclination | 
    dayofweekt + month_of_year + complaint_esi |0| ED_PROVIDER, 
  data = placebo)

stargazer(list(placebo.admit,
               placebo.72),
          type = "text", header = FALSE, 
          title = "Placebo Check-- no controls", style = 'QJE')


sink()


##########################################################################
#=========================================================================
# Monotonicity -----------------------------------------------------------
## first stage should be weakly positive for all subsamples 
## (Dobbie, Goldin, and Yang 2018)

## instrument constructed by leaving out a particular subsample
## has predictive power over that same left-out subsample 
## (Bhuller et al. 2020).
#=========================================================================
##########################################################################

data <- read_csv('outputs/data/final.csv')

complaints <- unique(data$CHIEF_COMPLAINT)

model_results <- list()
# Iterate through each complaint and run the regression
for(complaint in complaints) {
  
  data_subset <- data %>% filter(CHIEF_COMPLAINT == complaint)
  model <- felm(
    LAB_PERFORMED ~ test.inclination | 
      dayofweekt + month_of_year + age_groups + ESI | 0 | ED_PROVIDER,
    data = data_subset
  )
  model_results[[complaint]] <- model
}

# Save the results to a .txt file
sink("outputs/tables/Monotonicity.txt")

stargazer(model_results[1:5], type = "text", 
          title = "Regression Results by Complaint",
          column.labels = complaints[1:5],
          style = 'QJE')

stargazer(model_results[5:10], type = "text", 
          title = "Regression Results by Complaint",
          column.labels = complaints[5:10],
          style = 'QJE')

stargazer(model_results[10:16], type = "text", 
          title = "Regression Results by Complaint",
          column.labels = complaints[10:16],
          style = 'QJE')


sink()

##########################################################################
#=========================================================================
# IV Results ------------------------------------------------------------
#=========================================================================
##########################################################################

# Main Results
model.admit <- felm(
  admit ~ 0 | dayofweekt + month_of_year + complaint_esi | 
    (LAB_PERFORMED ~ test.inclination)| ED_PROVIDER, 
  data = data)

model.IV.72 <- felm(
  RTN_72_HR ~ 0 | dayofweekt + month_of_year + complaint_esi | 
    (LAB_PERFORMED ~ test.inclination)| ED_PROVIDER, 
  data = data)


# Save the results to a .txt file
sink("outputs/tables/2SLS Results.txt")

stargazer(list(model.admit,
               model.IV.72), type = "text", 
          covariate.labels = c('Lab Testing'),
          dep.var.labels = c('Admission', '72 Hour Return'),
          header = F, 
          title = "2SLS Results: Admmission and 72-Hour", 
          style = 'QJE')

stargazer(list(model.admit,
               model.IV.72), type = "latex", 
          covariate.labels = c('Lab Testing'),
          dep.var.labels = c('Admission', '72 Hour Return'),
          header = F, 
          title = "2SLS Results: Admission and 72-Hour", 
          style = 'QJE')

sink()
