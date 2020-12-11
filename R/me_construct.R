#' Construct a measurement error tibble manually
#'
#' \code{me_construct} is designed to create measurement error tibbles
#' by manually inserting new metrics such as quality or validity.
#'
#' @param question_name a character string or a bare unquoted name that will be
#' used as the question name
#'
#' @param metrics a list containing new measurement error metrics. Currently it
#' only supports quality, reliability and validity. Can also specify one of the
#' metrics and the remaining are set to \code{0} by default
#'
#' @param all_columns if \code{TRUE} will return all columns (quite a few) that are
#' supported by the \code{cosme} package. If \code{FALSE} (default) it
#' will return only columns \code{question}, \code{quality}, \code{reliability}
#' and \code{validity}. See the details section for the definition of all
#' columns.
#'
#' @return a \code{\link[tibble]{tibble}} of one row with the supplied metrics.
#' It also has class \code{me} for further manipulations within the
#' \code{cosme} package.
#'
#' @details
#'
#' \code{me_construct} returns a four column \code{\link[tibble]{tibble}} with
#' the question name and the estimates for \code{quality}, \code{reliability}
#' and \code{validity}. However, if \code{all_columns} is set to \code{TRUE}
#' the returned \code{\link[tibble]{tibble}} contains new columns. Below you can
#' find the description of of all columns:
#'
#' \itemize{
#' \item question: the literal name of the question in the questionnaire of the
#' study
#' \item question_id: the API internal ID of the question
#' \item id: this is the coding ID, that is, the coding of the authorized
#' prediction
#' \item created: Date of the API request
#' \item routing_id: Version of the coding scheme applied to get that prediction
#' \item authorized: Whether it is an 'authorized' prediction or not. Authorized
#'  predictions come from the SQP software directly. Unauthorized predictions
#'  come from other users which provide their own measurement error statistics.
#'
#' \item complete: Whether all fields of the coding are complete
#' \item user_id: The id of the user that crowd-sourced the prediction
#' \item error: Whether there was an error in making the prediction. For an
#' example, see \url{http://sqp.upf.edu/loadui/#questionPrediction/12552/42383}
#' \item errorMessage: The error message, if there was an error
#'
#' \item reliability: The strength between the true score factor and the
#' observed variable or 1 - proportion random error in the observed variance.
#' Computed as the squared of the reliability coefficient
#'
#' \item validity: The strength between the latent concept factor and the
#'  true score factor or 1 - proportion method error variance in the true
#'  score variance. Computed as the square of the validity coefficient
#'
#' \item quality: The strength between the latent concept factor and the
#'  observed variable or 1 - proportion of random and method error variance
#'  in the latent concept's variance. Computed as the product of reliability
#'  and validity.
#'
#' \item reliabilityCoefficient: The effect between the true score factor and
#'  the observed variable
#'
#' \item validityCoefficient: The effect between the latent concept factor and
#'  the true score factor
#'
#' \item methodEffectCoefficient: The effect between the method factor and the
#'  true score factor
#'
#' \item qualityCoefficient: It is computed as the square root of the quality
#' \item reliabilityCoefficientInterquartileRange: Interquartile range for the
#'  reliability coefficient
#' \item validityCoefficientInterquartileRange: Interquartile range for the
#'  validity coefficient
#' \item qualityCoefficientInterquartileRange: Interquartile range for the
#'  quality coefficient
#' \item reliabilityCoefficientStdError: Predicted standard error of the
#'  reliability coefficient
#' \item validityCoefficientStdError: Predicted standard error of the validity
#'  coefficient
#' \item qualityCoefficientStdError: Predicted standard error of the quality
#'  coefficient
#' }
#'
#'
#' \code{me_construct_} is useful if you're interested in programming
#' with \code{cosme} rather than using it interactively. If you want
#' to use \code{me_construct} inside a function, use the equivalent
#' \code{me_construct_} which uses standard evaluation.
#'
#' @export
#'
#' @examples
#'
#' me_construct(new_question, list(quality = 0.3))
#' me_construct(new_question, list(quality = 0.3, validity = 0.2))
#'
#' # Note that specifying a column which is not available in me data
#' # will throw an error
#'
#' \dontrun{
#' me_construct(new_question, list(random_col = 0.3, validity = 0.2))
#' }
#'
#' # Currently only quality, reliability and validity are allowed.
me_construct <- function(question_name, metrics, all_columns = FALSE) {
  question <- as.character(substitute(question_name))
  me_construct_(question, metrics, all_columns)
}

#' @rdname me_construct
#' @export
me_construct_ <- function(question_name, metrics, all_columns = FALSE) {

  question <- question_name

  if (length(question) > 1) stop("`question_name` must have only one question",
                                 call. = FALSE)

  is_list <- is.list(metrics)
  named <- !is.null(names(metrics))
  numeric <- is.numeric(unlist(metrics))

  if (!named | !numeric | !is_list) {
    stop("`metrics` must be a named numeric list",
         call. = FALSE)
  }

  if (length(names(metrics)) != length(unlist(metrics))) {
    stop("`metrics` must contain only one element per name",
         call. = FALSE)
  }

  me_metrics <-
    columns_me(names(metrics),
                unlist(metrics),
                all_columns = all_columns)

  generic_me(question, me_metrics, all_columns = all_columns)
}

# Specify columns that should be in the me data and
# replacements
# returns a named list with the replacements
# added
columns_me <- function(columns_to_fill, replacement, all_columns = FALSE) {

  me_cols <- if (all_columns) me_env$all_estimate_variables else me_env$me_columns

  if (!all(columns_to_fill %in% me_cols)) {
    stop("One or more of the specified `metrics` don't match column names. Should be: ", #nolintr
         paste0(me_env$me_columns, collapse = ", "),
         call. = FALSE)
  }

  # me_columns is a global variable defining
  # the columns that me needs to have
  num_cols <- length(me_cols)
  empty_cols <- stats::setNames(rep(list(0), num_cols), me_cols)

  # iterate through each column/replacement and fill
  # out the empty list
  for (some_cols in seq_along(columns_to_fill)) {
    chosen_col <- columns_to_fill[some_cols]
    empty_cols[[chosen_col]] <- replacement[some_cols]
  }

  filled_cols <- empty_cols
  filled_cols
}

# Create a tibble with the question name and
# the me matrics. Returns the tibble
generic_me <- function(question_name, me_metrics, all_columns = FALSE) {
  stopifnot(!is.null(names(me_metrics)), is.list(me_metrics))

  if (all_columns) {
    me_data <- tibble::as_tibble(me_metrics,
                                 .name_repair = "minimal")
    me_data$question <- question_name
  } else {
    me_data <- tibble::as_tibble(c(question = question_name, me_metrics),
                                 .name_repair = "minimal")
  }
  as_me(me_data)
}
