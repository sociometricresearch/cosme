#' Calculate quality of sum score of selected variables
#'
#' \code{me_sscore} takes a data frame with quality estimates of class
#' \code{me} and estimates the quality of a sum score for
#' selected variables in \code{...}. Use \code{me_sscore_} if you're
#' interested in programming with \code{me_sscore}
#'
#' @param me_data a data frame of class \code{me} containing
#' quality estimates from the variables specified in \code{...}.
#' 
#' @param data a data frame which contains data for the variables specified
#' in \code{...}.
#' 
#' @param new_name a bare unquoted name or a string specifying the name
#' of the new sum score.
#' 
#' @param ... bare unquoted names or separate strings specifying
#' the variable names from which to estimate quality of their sum score.
#' They all must be present in \code{me_data} and \code{df}. At minimum,
#' it must be two or more variable names.
#'
#' @param wt a non-NA numeric vector of the same length as the variables
#' specified in \code{...}. This will be used as weights in calculating the
#' sum scores of all variable. Be default, all variables are given the same
#' weight.
#'
#' @param drop a logical stating whether to drop the questions that compose
#' the sum score (specified in \code{...}) If \code{FALSE} it retains the
#' original questions and the composite score.
#'
#' @param vars_names character vector specifying the variable names from
#' which to estimate quality of their sum score. They all must be present
#' in \code{me_data} and \code{df}. At minimum, it must be two or more
#' variable names.
#'
#' @return a \code{\link[tibble]{tibble}} similar to \code{me_data} but
#' with a new row containing the quality of a sum score with the name
#' specified in \code{new_name}.
#'
#' @export
#'
#' @examples
#'
#' # Toy data
#'
#' library(tibble)
#' me_df <-
#' tibble(question = paste0("V", 1:5),
#'        quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'        reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
#'        validity = c(0.8, 0.1, 0.6, 0.7, 0.8))
#'
#'
#' me_df <- structure(me_df, class = c(class(me_df), "me"))
#'
#' sample_data <-
#'  as_tibble(
#'  setNames(
#'   replicate(5, rbinom(1000, 5, 0.6), simplify = FALSE),
#'  paste0("V", 1:5))
#'  )
#'
#'
#' me_sscore(
#' me_data = me_df,
#' data = sample_data,
#' new_name = new_sumscore,
#' V3, V4
#' )
#'
#' me_sscore(
#' me_data = me_df,
#' data = sample_data,
#' new_name = new_sumscore,
#' "V1", "V2"
#' )
#'
#'
me_sscore  <- function(me_data, data, new_name, ..., wt = NULL, drop = TRUE) {
  e_dots <- eval(substitute(alist(...)))
  f_dots <- lapply(e_dots, function(x) {
    if (is.name(x)) as.character(x) else eval(x)
  })
  
  vars_names <- unique(unlist(f_dots))
  if (is.null(vars_names)) vars_names <- character()

  new_name <- unique(as.character(substitute(new_name)))

  me_sscore_(me_data, data, new_name, vars_names, wt, drop)
}
#' @rdname me_sscore
#' @export
me_sscore_ <- function(me_data, data, new_name, vars_names, wt = NULL, drop = TRUE) {

  # Check me data has correct class and formats
  me_data <- me_reconstruct(me_data)

  summary_name <- new_name

  # Check all variables present in data
  vars_not_matched <- !vars_names %in% names(data)
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in `data`: ",
         paste0(vars_names[vars_not_matched], collapse = ", "),
         call. = FALSE)
  }

  # Check all variables present in me_data
  vars_not_matched <- !vars_names %in% me_data[[1]]
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in `me_data`: ",
         paste0(vars_names[vars_not_matched], collapse = ", "),
         call. = FALSE)
  }

  the_vars <- data[vars_names]

  # Check all variables are numeric and there are at least two columns in the data data
  if (!all(vapply(the_vars, is.numeric, FUN.VALUE = logical(1)))) {
    stop(paste0(vars_names, collapse = ", "), " must be numeric variables in `data`")
  }

  if (ncol(the_vars) < 2) stop("`data` must have at least two columns")

  # Select the rows with only the selected variales
  # for the sumscore
  rows_to_pick <- me_data[[1]] %in% vars_names
  me_scores <- me_data[rows_to_pick, me_env$me_columns]

  if (anyNA(me_scores)) {
    stop("`me_data` must have non-missing values at variable/s: ",
         paste0(me_env$me_columns, collapse = ", "))
  }

  new_estimate <-
    columns_me("quality", estimate_sscore(me_scores, the_vars, wt = wt))

  additional_rows <- generic_me(summary_name, new_estimate)

  # Bind the unselected questions with the new sumscore

  if (!drop) {
    rows_to_pick <- rep(TRUE, length(rows_to_pick))
  } else {
    rows_to_pick <- !rows_to_pick
  }
  combined_matrix <- dplyr::bind_rows(me_data[rows_to_pick, ], additional_rows)
  correct_order <- c("question", me_env$me_columns)
  new_order <- combined_matrix[c(correct_order, setdiff(names(combined_matrix), correct_order))]

  final_data <- me_reconstruct(new_order)
  final_data
}

# This is not supposed to be used in isolation.
# Rather with measurement quality as a wrapper
# because it checks all of the arguments are in
# the correct format, etc..
estimate_sscore <- function(me_data, the_data, wt) {

  if (is.null(wt)) wt <- rep(1, length(the_data))

  is_numeric <- is.numeric(wt)
  is_na <- anyNA(wt)
  correct_length <- length(wt) == ncol(the_data)

  if (!is_numeric | is_na | !correct_length) {
    stop("`wt` must be a non-NA numeric vector with the same length as the number of variables")
  }

  reliability <- grep("^r", me_env$me_columns, value = TRUE)
  validity <- grep("^v", me_env$me_columns, value = TRUE)
  quality <- grep("^q", me_env$me_columns, value = TRUE)

  qy2 <- me_data[[quality]]

  # By squaring this you actually get the reliability
  # coefficient.
  ry <- sqrt(me_data[[reliability]])
  vy <- sqrt(me_data[[validity]])

  # Method effect
  method_e <- sqrt(1 - vy^2)

  std_data <- vapply(the_data, stats::sd, na.rm = TRUE, FUN.VALUE = numeric(1))

  # This is the 'quality coefficient'
  # for the observed variable i. (1-qi2)var(yi)
  q_coef <- qcoef_observed(qy2, std_data)

  # Here you create
  # all combinations
  comb <- utils::combn(seq_along(the_data), 2, simplify = FALSE)

  # This the multiplication of all variable combinations
  # using ri * mi * mj * rj * si * sj
  # It's better not to use this in isolation but call
  # estimate_sscore as a whole.
  cov_e <- cov_both(comb, ry, method_e)

  weights_by_qcoef <- sum(wt^2 * q_coef)

  # you need to calculate the product of a combination
  # of the weights by the covariance of errors.
  intm <- sum(combn_multiplication(comb, wt, cov_e)) * 2

  var_ecs <- weights_by_qcoef + intm
  var_composite <- stats::var(rowSums(the_data, na.rm = TRUE))

  1 - (var_ecs / var_composite)
}

qcoef_observed <- function(quality, std_data) {
  qcoef_formula <- function(i) (1 - quality[i]) * std_data[i]^2
  vapply(seq_along(quality), qcoef_formula, FUN.VALUE = numeric(1))
}

combn_multiplication <- function(comb, wt, cov_e) {

  intm <- vapply(seq_along(comb), function(i) {
    # both_combn is the combination of variables like 1:2, 2:3 and c(3, 1)
    # below I grab both ends
    separ_first <- comb[[i]][1]
    separ_second <- comb[[i]][2]

    # and the multiply the weigghts with the cov_e
    # so for example wt[1] * wt[2] * cov_e[1]
    # so for example wt[1] * wt[3] * cov_e[3]
    wt[separ_first] * wt[separ_second] * cov_e[i]

  }, FUN.VALUE = numeric(1))
}

# For an explanation of this see combn_multiplication
cov_both <- function(combinations, r_coef, method_e) {

  # This formula is not complicated. It's simply the product of
  # the standard deviation of the data, the r_coef and the
  # method effect between all combination of questions.
  cov_formula <- function(one, two, r_coef, method_e) {
    (r_coef[one] * method_e[one]) * (r_coef[two] * method_e[two])
  }

  # Here I apply the formula to all combinations. combinations
  # must be a list where each slot is of length 2 with a pair
  # combination. The whole list must contain all combinations
  result <- vapply(combinations, function(index) {
    index_one <- index[1]
    index_two <- index[2]
    result <- cov_formula(index_one, index_two, r_coef, method_e)
    result
  }, FUN.VALUE = numeric(1))

  result
}
