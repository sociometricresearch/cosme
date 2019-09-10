library(lavaan.survey)
library(ggplot2)
library(dplyr)
library(essurvey)
library(sqpr)

# And measurementfree
library(measurementfree)


# Choose your selected variables
selected_vars <- c("ppltrst",
                   "trstprl",
                   "stflife",
                   "stfeco")

set_email("cimentadaj@gmail.com")
# Download the ESS data and clear missing values
ess4es_complete <- import_country("Spain", 4)[c("idno", selected_vars)]

# Exclude idno (only used in weights)
ess4es <- ess4es_complete[complete.cases(ess4es_complete[, -1]), selected_vars]

# Download SQP data
sqp_login("asqme", "asqme")

quality <-
  get_sqp(
    study = "ESS Round 4",
    question_name = selected_vars,
    country = "ES",
    lang = "spa"
  )

# Same order 
quality <- quality[match(selected_vars, quality$question), ]

# Exploratory correlation matrix (in order of the columns in data frame):
original_corr <- cor(ess4es, use = "complete.obs", method = "pearson")
# here
corrected_corr <- me_correlate(data = ess4es, diag_adj = quality$quality)

# subtract the cmv from the observed correlation
corrected_corr <-
  corrected_corr %>% 
  me_cmv_cor(me_data = quality, stfeco, stflife) %>%
  me_cmv_cor(me_data = quality, ppltrst, trstprl)

corrected_corr <- cov2cor(as.matrix(corrected_corr[, -1]))

model <- "ppltrst ~ stflife + trstprl + stfeco"

# Model based on original correlation matrix
fit <-
  sem(model,
      sample.cov = original_corr,
      sample.nobs = nrow(ess4es)) 

# Model based on corrected correlation matrix 
fit_corrected <-
  sem(model,
      sample.cov = corrected_corr,
      sample.nobs = nrow(ess4es)) 


weight_vars <- c("idno", "psu", "stratify", "prob")
weights_spain <- import_sddf_country("Spain", 4)[weight_vars]
svy_ess4es <- merge(ess4es_complete, weights_spain, by = "idno")

survey_design <-
  svydesign(
    ids = ~ psu + idno,
    strata = ~ stratify,
    weights = ~ prob,
    data = svy_ess4es
  )

options(survey.lonely.psu="adjust")
# Model based on corrected correlation matrix but weighted
fit_corrected_svy <- lavaan.survey(fit_corrected, survey_design)

############################# plot comparing both models ######################
###############################################################################

coef_table <-
  list(fit, fit_corrected, fit_corrected_svy) %>%
  lapply(parameterestimates) %>%
  lapply(function(.x) .x[.x$lhs == "ppltrst" & .x$op != "~1", ]) %>%
  lapply(function(.x) .x[, c("rhs", "est", "ci.lower", "ci.upper")]) %>%
  do.call(rbind, .)

coef_table$model <- rep(c("original", "corrected", "corrected_svy"),
                        each = length(selected_vars))

coef_table %>%
  ggplot(aes(rhs, est, colour = model)) +
  geom_linerange(aes(ymin = ci.lower, ymax = ci.upper), position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(x = "Predictors", y = "Estimated coefficients") +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw()
