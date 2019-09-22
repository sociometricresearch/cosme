#' Adjust a correlation matrix for Common Method Variance (CMV)
#'
#' \code{me_cmv_cor} accepts an \code{medesign} object specified in
#' \code{\link{medesign}} and adjusts the correlation coefficients of
#' common method variables with the reliability and validity coefficients
#' from \code{me_data}. Optionally, you can supply the CMV coefficients
#' manually in the argument \code{cmv}.
#'
#' @param .medesign An \code{medesign} object given by \code{\link{medesign}}
#' @param cmv an optional numeric vector of the same length as the number of
#' common method variance specifications in \code{.mdesign}.This argument is
#' left available if the user has reasons to input their own CMV. By default,
#' it is set to NULL and it is calculated internally.
#'
#' @return The common-method-variance corrected correlation.
#'
#' @export
#'
#' @seealso \code{link{medesign}} and \code{\link{me_cmv_cov}} for the same
#' adjustment but for a covariance matrix.
#'
#' @examples
#'
#' set.seed(2131)
#' library(tibble)
#'
#' original_df <- as.data.frame(matrix(rnorm(100, sd = 50), nrow = 20))
#'
#' # Toy quality dataset
#' me_df <-
#'  tibble(question = paste0("V", 1:5),
#'  reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(0.79, 0.9, 0.6, 0.7, 0.8),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9))
#'
#' # Define mdesign object
#' m_obj <- medesign("~ V4 + V5", original_df, me_df)
#'
#' # Original correlation matrix
#' me_correlate(original_df)
#'
#' # Coefficient of correlation changes
#' # when adjusting for common method variance
#' me_cmv_cor(m_obj)
#'
#' # The V5*V4 from both the upper/lower triangles
#' # correlation matrix changed from -0.05 to -0.27
#'
me_cmv_cor <- function(.medesign, cmv = NULL) {

  if (!inherits(.medesign, "medesign")) {
    stop("`.medesign` should be a measurement error design object given by `medesign`") #nolintr
  }
  
  # cmv vector equals the same length as the number of cmv definitions
  if (!is.null(cmv)) {
    stopifnot(is.numeric(cmv))
    stopifnot(length(cmv) == length(unique(.medesign$parsed_model$lhs)))
  } else {
    # Create empty list to iterate over each cmv
    cmv <- rep(list(NULL), length(unique(.medesign$parsed_model$lhs)))
  }

  parsed_model <- .medesign$parsed_model
  cmv_df <- parsed_model[parsed_model$op == "~", ]

  cmv_groups <- split(cmv_df, cmv_df$lhs)
  list_cmv_vars <- lapply(cmv_groups, `[[`, "rhs")

  for (i in seq_along(list_cmv_vars)) {

    # If it's the first iteration, provide the original correlation
    # otherwise the looped corrected correlation
    res <- me_cmv_cor_(x = if (i == 1) .medesign$corr else res,
                       me_data = .medesign$me_data,
                       cmv_vars = list_cmv_vars[[i]],
                       cmv = cmv[[i]])
  }

  res
}

me_cmv_cor_ <- function(x, me_data, cmv_vars, cmv = NULL) {
  # Althought this check is carried in `medesign`,
  # I leave this one here again because it's specific towards
  # two columns. Doesn't make a difference, but I'd rather
  # err on the side of safety
  me_data <- as_me(me_data, c("reliability", "validity"))

  selected_rows <- me_data[[1]] %in% cmv_vars
  if (is.null(cmv)) cmv <- estimate_cmv(me_data[selected_rows, ])

  # This is the standardized cmv given that the me coefficients
  # are already standardized
  cmv <- prod(cmv)

  corrected_corr <- tibble::as_tibble(replace_matrix_cmv(x, cmv, cmv_vars),
                                      .name_repair = "minimal")

  # Turn the correlation back to a correlation for the diagonal
  # to be one
  corrected_corr[, -1] <- stats::cov2cor(as.matrix(corrected_corr[, -1]))
  corrected_corr
}




#' Estimate the Common Method Variance (CMV) coefficient of a set of variables
#'
#'
#' @param me_data a data frame or tibble of class \code{me}
#' which contains the desired variables from which to estimate the CMV.
#'
#' @return a numeric vector of length one with the estimated coefficient
#'
#' @seealso \code{\link{me_cmv_cor}} for automatically adjusting a correlation
#' matrix for the CMV, \code{\link{me_cmv_cov}} for automatically adjusting a
#' covariance matrix for the CMV.
#'
#' @examples
#' library(tibble)
#'
#' # Don't remove don't run: since this fun is not exported
#' # it throws an error. Keeping docs jsut for me here.
#' \dontrun{
#' me_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(0.5, 0.1, 0.6, 0.7, 0.8))
#'
#' estimate_cmv(me_df)
#' }
#'
estimate_cmv <- function(me_data) {
  me_cols <- c("reliability", "validity")
  check_me_na(me_data, me_cols)
  reliability_coef <- sqrt(me_data[[me_cols[1]]])
  validity_coef <- sqrt(1 - me_data[[me_cols[2]]])

  cmv <- prod(c(reliability_coef, validity_coef))
  cmv
}

# This function is the one doing the replacement of the upper
# and lower of the correlation matrix.
replace_matrix_cmv <- function(x, cmv, cmv_vars) {

  # In case the order of rows is shuffled,
  # recode the initial order, order the data frame
  # calculate everything and then reorder back
  # when return the x data frame
  order_rows <- x[[1]]
  order_columns <- names(x)

  new_order <- sort(x[[1]])
  x  <- x[match(new_order, x$rowname), c("rowname", new_order)]

  x_row_low <- sort(match(cmv_vars, x[[1]]))
  x_col_low <- sort(match(cmv_vars, names(x)))

  x <- as.data.frame(x)

  p <- x[x_row_low, x_col_low] # subset only the select variables
  p[lower.tri(p)] <- p[lower.tri(p)] - cmv # adjust the lower.tri
  p[upper.tri(p)] <- p[upper.tri(p)] - cmv # adjust the upper.tri
  x[x_row_low, x_col_low] <- p # replace in the original data.frame

  x[match(order_rows, x[[1]]), order_columns]
}


matrix2tibble <- function(x) {
  has_rowname_col <- "rowname" %in% names(x)

  # It has a column rowname and is a tibble
  # then it's porbbaly from me_correlate
  if (tibble::has_name(x, "rowname") && tibble::is_tibble(x)) {
    return(x)
  } else if (tibble::has_rownames(x) & !has_rowname_col) {
    # If it has rownames and doesn't have a row name column
    # turn into tibble with row name column
    return(tibble::as_tibble(x, rownames = "rowname", .name_repair = "minimal"))
  }

  x <- tibble::as_tibble(x)

  if (!tibble::has_rownames(x) & !has_rowname_col) {
    x <- tibble::add_column(x,
                            rowname = names(x),
                            .before = 1)
  }
  x
}
