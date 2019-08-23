#' Manually bind new rows to an me data frame
#'
#' @param me_data a \code{data frame} or \code{tibble} of class \code{me}
#' @param question a character string that will be used as the question name
#' @param metrics a list containing new  metrics. Currently it only
#' supports 'quality', 'reliability' and 'validity'. Can also specify one of the metrics
#' and the remaining are set to NA by default
#'
#' @return \code{me_data} with a new row that contains the new question name
#' and the metrics as columns. Metrics must match existing names from SQP data
#' such as 'quality', 'reliability' and 'validity'.
#'
#' @export
#'
#' @seealso \code{\link{me_construct}} for the underlying workhorse.
#'
#' @examples
#'
#' # Toy dataset
#' library(tibble)
#'
#' me_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(NA, NA, 0.6, 0.7, 0.8))
#'
#' me_df <- structure(me_df, class = c(class(me_df), "me"))
#'
#' me_bind_metrics(me_df, new_question, list(quality = 0.7))
#'
#' me_bind_metrics(me_df, new_question, list(quality = 0.7, reliability = 0.2))
#'
#' # Specifying a wrong metric name results in error
#'
#'\dontrun{
#'
#' me_bind_metrics(me_df, new_question, list(wrong_metric = 0.7))
#' # Error: One or more of the specified `metrics` don't match the me column names
#'
#'}
#'
#' # Currently only quality, reliability and validity are allowed.
#'
me_bind_metrics <- function(me_data, question, metrics) {

  me_reconstruct(me_data)

  question_name <- as.character(substitute(question))

  binded_me <- dplyr::bind_rows(me_data, me_construct_(question_name, metrics))
  binded_me
}
