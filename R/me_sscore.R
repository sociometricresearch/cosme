#' Calculate quality of sum score of selected variables
#'
#' \code{me_sscore} takes a data frame with quality estimates of class
#' \code{me} and estimates the quality of a sum score for
#' selected variables in \code{...}. Use \code{me_sscore_} if you're
#' interested in programming with \code{me_sscore}
#'
#' @param me_data a data frame of class \code{me} containing
#' quality estimates from the variables specified in \code{...}. It is
#' expected that this measurement error data is not square rooted (reliability,
#' validity and quality) as it will be done with \code{me_sscore}.
#'
#' @param .data a data frame which contains data for the variables specified
#' in \code{...}.
#'
#' @param new_name a bare unquoted name or a string specifying the name
#' of the new sum score.
#'
#' @param ... bare unquoted names or separate strings specifying
#' the variable names from which to estimate quality of their sum score.
#' They all must be present in \code{me_data} and \code{.data}. At minimum,
#' it must be two or more variable names.
#'
#' @param wt a non-NA numeric vector of the same length as the variables
#' specified in \code{...}. This will be used as weights in calculating the
#' sum scores of all variable. Be default, all variables are given the same
#' weight.
#'
#' @param .drop a logical stating whether to drop the questions that compose
#' the sum score (these are the variables specified in \code{...}) If
#' \code{FALSE} it retains the original questions and the sum score.
#'
#' @param vars_names character vector specifying the variable names from
#' which to estimate the quality of the sum score. They all must be present
#' in \code{me_data} and \code{.data}. At minimum, it must be two or more
#' variable names.
#'
#' @return a \code{\link[tibble]{tibble}} similar to \code{me_data} but
#' with a new row containing the quality of a sum score.
#'
#' @export
#'
#' @examples
#' # Political trust example
#' data(ess7es)
#'
#' me_data <-
#'   data.frame(
#'     question = c("trstprl", "trstplt", "trstprt"),
#'     reliability = c(0.812, 0.852, 0.858),
#'     validity = c(0.959, 0.965, 0.956),
#'     quality = c(0.779, 0.822, 0.821)
#'   )
#'
#' selected_vars <- c("trstprl", "trstplt", "trstprt")
#' score <- me_sscore(me_data[me_data$question %in% selected_vars, ],
#'                    ess7es,
#'                    new_name = "s1",
#'                    trstprl, trstplt, trstprt)
#'
#' # Returns the quality and method effect of any given sum score
#' score
#'
#' # State services example
#' me_data <-
#'   data.frame(
#'     question = c("stfedu", "stfhlth"),
#'     reliability = c(0.757, 0.760),
#'     validity = c(0.838, 0.798),
#'     quality = c(0.635, 0.607)
#'   )
#'
#' score <- me_sscore(me_data,
#'                    ess7es,
#'                    new_name = "s2",
#'                    stfedu, stfhlth)
#'
#' # Returns the quality and method effect of any given sum score
#' score
#'
me_sscore  <- function(me_data, .data, new_name, ..., wt = NULL, .drop = TRUE) {
  e_dots <- eval(substitute(alist(...)))
  f_dots <- lapply(e_dots, function(x) {
    if (is.name(x)) as.character(x) else eval(x)
  })

  vars_names <- unique(unlist(f_dots))
  if (is.null(vars_names)) vars_names <- character()

  new_name <- unique(as.character(substitute(new_name)))

  me_sscore_(me_data, .data, new_name, vars_names, wt, .drop)
}

#' @rdname me_sscore
#' @export
me_sscore_ <- function(me_data, .data, new_name, vars_names, wt = NULL,
                       .drop = TRUE) {

  # Check me data has correct class and formats
  me_data <- as_me(me_data)

  # If method_eff is not present, we assume it DOESN'T come from
  # adapted_sscore and medesign and thus we treat as coming
  # from sqpr. This means we need to calculate the sqrt to get the
  # method_eff.
  if (!"method_eff" %in% names(me_data)) {
    # Get square root of validity, quality and reliability
    me_data[-1] <- lapply(me_data[-1], sqrt)
    # method effect
    me_data$method_eff <- with(me_data, reliability * sqrt(1 - validity^2))
  }

                                        # Check all variables present in .data
  vars_not_matched <- !vars_names %in% names(.data)
  if (any(vars_not_matched)) {
    stop("One or more variables are not present in `.data`: ",
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

  the_vars <- .data[vars_names]

                                        # Check all variables are numeric and there are at least two columns in the
                                        # .data
  if (!all(vapply(the_vars, is.numeric, FUN.VALUE = logical(1)))) {
    stop(paste0(vars_names, collapse = ", "), " must be numeric variables in `.data`") # nolintr
  }

  if (ncol(the_vars) < 2) stop("`.data` must have at least two columns")

                                        # Select the rows with only the selected variales
                                        # for the sumscore
  rows_to_pick <- me_data[[1]] %in% vars_names
  cols_sel <- c(me_env$me_question, me_env$me_columns, "method_eff")

  me_scores <- me_data[rows_to_pick, cols_sel]

  if (anyNA(me_scores)) {
    stop("`me_data` must have non-missing values at variable/s: ",
         paste0(me_env$me_columns, collapse = ", "))
  }

  .data <- scale_add_sscore(.data, new_name, paste0(vars_names, collapse = "+"))
  estimate_of_sscore <- estimate_sscore(me_scores,
                                        .data,
                                        new_name,
                                        wt = wt)

  new_estimate <- columns_me("quality", estimate_of_sscore$quality)
  new_estimate$method_eff <- estimate_of_sscore$method_eff

  additional_rows <- generic_me(new_name, new_estimate)

                                        # Bind the unselected questions with the new sumscore
  if (!.drop) {
    rows_to_pick <- rep(TRUE, length(rows_to_pick))
  } else {
    rows_to_pick <- !rows_to_pick
  }

                                        # The only purpose of as_tibble here is to remove the class `me`
                                        # so that bind_rows can work well. `as_me` converts it to me
                                        # in the end, so it doesn't matter.
  combined_matrix <- dplyr::bind_rows(dplyr::as_tibble(me_data[rows_to_pick, ]),
                                      dplyr::as_tibble(additional_rows))
  correct_order <- c("question", me_env$me_columns, "method_eff")
  col_ord <- c(correct_order, setdiff(names(combined_matrix), correct_order))
  new_order <- combined_matrix[col_ord]

  final_data <- as_me(new_order)
  final_data
}

                                        # This is not supposed to be used in isolation.
                                        # Rather with measurement quality as a wrapper
                                        # because it checks all of the arguments are in
                                        # the correct format, etc..
estimate_sscore <- function(me_scores, .data, new_name, wt) {
  sscore <- .data[[new_name]]

                                        # If there are two variables, the number of combinations are
                                        # is one (so just one weight value). If there are three variables
                                        # the number of combinations are three and we need three
                                        # weight values, and so on..
  num_combn <- utils::combn(seq_len(nrow(me_scores)), 2, simplify = FALSE)
  if (is.null(wt)) wt <- rep(1, length(num_combn))

  is_numeric <- is.numeric(wt)
  is_na <- anyNA(wt)
  correct_combinations <- length(wt) == length(num_combn)

  if (!is_numeric | is_na | !correct_combinations) {
    stop("`wt` must be a non-NA numeric vector with the same length as the number of combinations") #nolintr
  }

  sd_sscore <- stats::sd(.data[[new_name]], na.rm = TRUE)
  quality <- grep("^q", me_env$me_columns, value = TRUE)
  sum_res <- sum((wt / sd_sscore * me_scores[[quality]])^2)

  correlation_res <-
    matrix2tibble(
      stats::cor(.data[me_scores$question], use = "complete.obs")
    )

  cor_res <-
    me_cmv_cor_(
      correlation_res,
      me_scores,
      me_scores$question
    )

  corr_adapted_cmv <- cor_res[-1][lower.tri(cor_res[-1])]
  sum_adapted_corr <-
    sum(
      corr_adapted_cmv * wt / sd_sscore * wt / sd_sscore
    ) * 2

  final_qcoef <- sum_adapted_corr + sum_res

  validity <- grep("^v", me_env$me_columns, value = TRUE)
  reliability <- grep("^r", me_env$me_columns, value = TRUE)
  methodeff <- sqrt(1 - me_scores[[validity]]^2)
  final_meff <- sum(wt / sd_sscore * me_scores[[reliability]] * methodeff)^2

  if (sign(final_qcoef) == -1) {
    stop("Calculating the quality coefficient for a sum score resulted in a value not within 0 and 1. Please report the exact example the produced this error at https://github.com/sociometricresearch/measurementfree/issues")
  }

  list(
    quality = final_qcoef,
    method_eff = final_meff
  )
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
