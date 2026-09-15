################ 
# Code for linear and logistic weighted regression
################

library(survey) 
library(broom)     
library(dplyr)   
library(emmeans)     

### Survey design - preparing accurate type of data frame

df_survey <- svydesign(id=~SDMVPSU, weights=~WTMEC2YR, strata=~SDMVSTRA,
                             nest=TRUE, survey.lonely.psu = "adjust",
                             data=df_model)

# Filtering participants based on the inclusion/exclusion criteria
df_analysis = subset(df_survey, inclusion>0)
df_analysis = subset(df_analysis, age>=20)
df_analysis = subset(df_analysis, (pregnancy!=1 | is.na(pregnancy)))
df_analysis = subset(df_analysis, !is.na(PHQ9_score))
df_analysis = subset(df_analysis, !is.na(cluster))
df_analysis = subset(df_analysis, complete.cases(df_analysis$variables[important_columns]))


##### Weighted (adjusted) linear regression - results table ##### 

# Outcome variables (PHQ-9 summary score and separate items)
outcomes <- c("PHQ9_score", "DPQ010", "DPQ020", "DPQ030", "DPQ040", 
              "DPQ050", "DPQ060", "DPQ070", "DPQ080", "DPQ090")

# Function to fit model and extract results
extract_cluster_effects <- function(outcome) {
  
# Create formula dynamically
formula <- as.formula(paste0(outcome, " ~ age + sex + race + education + marital_status_alone + ",
                               "recent_tobacco + children_below6 + children_below17+ general_health_condition + bmi + alcohol_use + ",
                               "work_last_week + cluster"))
  
# Fit the model
model <- svyglm(formula, design = n_analysis, family = gaussian())
  
# Extract coefficients
coef_df <- tidy(model)
  
# Get 95% confidence intervals using confint()
ci_df <- confint(model)
  
# Add CIs to tidy output
coef_df$conf.low <- ci_df[, 1]
coef_df$conf.high <- ci_df[, 2]
  
# Filter for cluster effects and add outcome name
  coef_df %>%
    filter(term %in% c("clusterDysregulated Late Starters", "clusterRegular Late Starters")) %>%
    mutate(outcome = outcome) %>%
    select(outcome, term, estimate, conf.low, conf.high)
}

# Run for all outcomes and combine
cluster_effects <- bind_rows(lapply(outcomes, extract_cluster_effects))

# View the result
print(cluster_effects)


##### Weighted (adjusted) logistic regression ##### 


model_logistic = svyglm(PHQ9_depression ~ 
                    age + sex + race + education +
                    children_below6 + children_below17 + marital_status_alone +
                    recent_tobacco + general_health_condition + bmi +
                    alcohol_use + work_last_week + cluster,
                  design = df_analysis,
                  family=quasibinomial)

summary(model_logistic)
anova(model_logistic)
exp(cbind(OR = coef(model_logistic), confint(model_logistic)))

emm_logistic = emmeans(model_logistic, revpairwise ~ cluster, rg.limit=300000, type='response')
emm_logistic

confint(emm_logistic)

plot(emm_logistic)
