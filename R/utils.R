# Check me data and throw erros if it's not in correct format.
# If it is in the correct format attach 'me' class if it doesn't
# have it.

# Why? Because using any of the me_ funs with tidyverse verbs
# (or any other function whatsoever), drops the 'me' class. These
# functions will check whether the data is in right format and assign
# the class accordingly. It basically makes sure the data is good for
# later processing.
me_reconstruct <- function(me_data, variables_check = me_env$me_columns) {

  # If me_data is not in the correct format, throw an error
  check_me_data(me_data, variables_check)

  # If it has a correct format, then simply add the me class if
  # it doesn't have it
  if (!inherits(me_data, "me")) class(me_data) <- c(class(me_data), "me")
  me_data
}

# This should ONLY be used when you want to check an existing me
# df
check_me_data <- function(me_data, available_vars) {
  # Check me_env$me_columns variables exists

  metrics_available <- all(available_vars %in% names(me_data))

  if (!metrics_available) {
    stop("Variables ", paste0(available_vars, collapse = ", "),
         " must be available in `me_data`",
         call. = FALSE)
  }

  for (i in me_data[available_vars]) col_checker(i)
}

col_checker <- function(x) {
  if (all(is.na(x))) return(TRUE)

  is_numeric <- is.numeric(x)
  is_perc <- all(x >= 0 & x <= 1, na.rm = TRUE)
  if (!is_numeric | !is_perc) {
    stop(paste0(me_env$me_columns, collapse = ", "),
         " must be numeric columns with values between/including 0 and 1 in `me_data`",
         call. = FALSE)
  }
  invisible(TRUE)
}

columns_present <- function(corr_data, me_data, var_names) {
  sum_corr <- corr_data[[1]] %in% var_names
  sum_me <- me_data[[1]] %in% var_names

  vars_corr <- var_names %in% corr_data[[1]]
  vars_me <- var_names %in% me_data[[1]]

  if (sum(sum_corr) != length(var_names)) {
    stop("At least one variable not present in `x`: ",
         paste0(var_names[!vars_corr], collapse = ", "),
         call. = FALSE)
  }

  if ((sum(sum_me) != length(var_names))) {
    stop("At least one variable not present in `me_data`: ",
         paste0(var_names[!vars_me], collapse = ", "),
         call. = FALSE)
  }

}


me_env <- new.env()
me_env$study_variables <- c("id", "name")
me_env$question_variables <- c("id",
                               "study_id",
                               "short_name",
                               "country_iso",
                               "language_iso")

me_env$me_columns <- c("reliability", "validity", "quality")

me_env$short_estimate_variables <-
  paste0(
    "prediction.",
    me_env$me_columns
  )

me_env$all_estimate_variables <- c("question",
                                    "reliability",
                                    "validity",
                                    "quality",
                                    "question_id",
                                    "id",
                                    "created",
                                    "routing_id",
                                    "authorized",
                                    "complete",
                                    "user_id",
                                    "error",
                                    "errorMessage",
                                    "reliabilityCoefficient",
                                    "validityCoefficient",
                                    "methodEffectCoefficient",
                                    "qualityCoefficient",
                                    "reliabilityCoefficientInterquartileRange",
                                    "validityCoefficientInterquartileRange",
                                    "qualityCoefficientInterquartileRange",
                                    "reliabilityCoefficientStdError",
                                    "validityCoefficientStdError",
                                    "qualityCoefficientStdError"
                                    )
