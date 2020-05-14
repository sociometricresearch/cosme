#' Manually bind new rows to an me data frame
#'
#' @param me_data a data frame of class \code{me} containing
#' quality estimates from the variables specified in \code{...}.
#' @param question_name a character string or a bare unquoted name that will be
#' used as the question name
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
#'
#'}
#'
#' # Currently only quality, reliability and validity are allowed.
#'
me_bind_metrics <- function(me_data, question_name, metrics) {

  as_me(me_data)

  new_question <- as.character(substitute(question_name))

  # The only purpose of as_tibble here is to remove the class `me`
  # so that bind_rows can work well. `as_me` converts it to me
  # in the end, so it doesn't matter.
  binded_me <- dplyr::bind_rows(dplyr::as_tibble(me_data),
                                dplyr::as_tibble(me_construct_(new_question, metrics)))

  binded_me <- as_me(binded_me)
  binded_me
}
