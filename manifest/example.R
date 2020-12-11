## ------------------------------------------------------------------------
library(lavaan.survey)
library(ggplot2)
library(dplyr)
library(essurvey)
library(sqpr)

# And cosme
library(cosme)


# Choose your selected variables
selected_vars <- c("ppltrst",
                   "polintr",
                   "stflife",
                   "stfeco")

# Download the ESS data and clear missing values
ess7es_complete <- import_country("Spain", 7)[c("idno", selected_vars)]

# Exclude idno (only used in weights)
ess7es <- ess7es_complete[complete.cases(ess7es_complete[, -1]), selected_vars]

# Download SQP data
sqp_login()
quality <-
  get_sqp(
    study = "ESS Round 7",
    question_name = selected_vars,
    country = "ES",
    lang = "spa"
  )

## ------------------------------------------------------------------------

# Same order 
quality <- quality[match(selected_vars, quality$question), ]

## ------------------------------------------------------------------------
# Exploratory correlation matrix (in order of the columns in data frame):
original_corr <- cor(ess7es, use = "complete.obs", method = "pearson")

## ------------------------------------------------------------------------

corrected_corr <- me_correlate(x = ess7es, diag_adj = quality$quality)

## ------------------------------------------------------------------------

#subtract the cmv from the observed correlation
corrected_corr <-
  me_cmv_cor(x = corrected_corr,
             me_data = quality,
             stfeco, stflife)

## ------------------------------------------------------------------------
corrected_corr <- cov2cor(as.matrix(corrected_corr[, -1]))

## ------------------------------------------------------------------------
model <- "ppltrst ~ stflife + polintr + stfeco"

# Model based on original correlation matrix
fit <-
  sem(model,
      sample.cov=original_corr,
      sample.nobs= nrow(ess7es)) 

# Model based on corrected correlation matrix 
fit_corrected <-
  sem(model,
      sample.cov=corrected_corr,
      sample.nobs= nrow(ess7es)) 


weight_vars <- c("idno", "psu", "stratify", "prob")
weights_spain <- import_sddf_country("Spain", 7)[weight_vars]
svy_ess7es <- merge(ess7es_complete, weights_spain, by = "idno")

survey_design <-
  svydesign(
    ids = ~ psu + idno,
    strata = ~ stratify,
    weights = ~ prob,
    data = svy_ess7es
  )

options(survey.lonely.psu="adjust")
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
  theme_bw()
