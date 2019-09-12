#' Adjust a correlation matrix for Common Method Variance (CMV)
#'
#' \code{me_cmv_cor} accepts a correlation matrix, a correlation data frame or a correlation
#' tibble from \code{\link{me_correlate}}
#' and adjusts the coefficients of the variables specified
#' in  \code{...} with the reliability and validity coefficients from \code{me_data}.
#' All variables specified in \code{...} must be present in both \code{x}
#' and \code{me_data}. Optionally, you can supply the cmv coefficient in the
#' argument \code{cmv}. Use \code{me_cmv_cor_} if you're interested in programming
#' with \code{me_cmv_cor}.
#'
#' @param x a correlation matrix, a correlation data frame or a correlation
#'  \code{tibble} given by \code{\link{me_correlate}}.
#'
#' @param me_data a data frame of class \code{me} containing
#' quality estimates from the variables specified in \code{...}.
#'
#' @param ... two or more variables present in both \code{x} and \code{me_data}. Can
#' be both in bare unquoted names or as character strings.
#'
#' @param cmv an optional numeric vector of length 1 which contains the
#' CMV coefficient of the variables specified in \code{...}.
#' This argument is left available if the user has reasons to input their own CMV.
#' By default, it is set to NULL and it is calculated internally.
#'
#' @param cmv_vars two or more variables present in both \code{x} and \code{me_data}. Only
#' as a character vector.
#'
#' @return the same matrix supplied in \code{x} but as a tibble with
#' the correlation coefficients of the variables supplied in \code{...}
#' adjusted for their common method variance.
#' If \code{x}  had row names, they're moved as a column called
#' 'rowname'. If it doesnt have row names or a column named 'rowname' a new
#' column is created with the column names to mimic a correlation matrix.
#'
#' @export
#'
#' @seealso \code{\link{me_correlate}} for calculating correlation matrices
#' and \code{\link{estimate_cmv}} for calculating the CMV manually.
#'
#' @examples
#'
#' set.seed(2131)
#' library(tibble)
#'
#' original_df <- as.data.frame(matrix(rnorm(100, sd = 50), nrow = 20))
#' corr_tibble <- me_correlate(original_df, rnorm(5))
#'
#' # Toy dataset
#' me_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(NA, NA, 0.6, 0.7, 0.8))
#'
#' me_df <- structure(me_df, class = c(class(me_df), "me"))
#'
#' # Show that when y is not from me class, there's an error
#'
#' # Original correlation matrix with new diagonal
#' corr_tibble
#'
#' # Coefficient of correlation changes
#' # when adjusting for common method variance
#' me_cmv_cor(corr_tibble, me_df, V4, V5)
#'
#' # The V5*V4 from both the upper/lower triangles
#' # correlation matrix changed from -0.05 to -0.203
#'
me_cmv_cor <- function(x, me_data, ..., cmv = NULL) {
  e_dots <- eval(substitute(alist(...)))
  f_dots <- lapply(e_dots, function(x) {
    if (is.name(x)) as.character(x) else eval(x)
  })
  
  cmv_vars <- unique(unlist(f_dots))

  me_cmv_cor_(x, me_data, cmv_vars, cmv)
}
#' @rdname me_cmv_cor
#' @export
me_cmv_cor_ <- function(x, me_data, cmv_vars, cmv = NULL) {

  if (!(is.data.frame(x) | is.matrix(x))) {
    stop("`x` must be a correlation data frame or matrix")
  }

  if (length(cmv_vars) < 2) {
    stop("You need to supply at least two variables to calculate the Common Method Variance",
         call. = FALSE)
  }

  me_data <- me_reconstruct(me_data, c("reliability", "validity"))

  x <- matrix2tibble(x)

  # Check if all supplied variables are present in both
  # dfs
  columns_present(x, me_data, cmv_vars)

  selected_rows <- me_data[[1]] %in% cmv_vars
  if (is.null(cmv)) cmv <- estimate_cmv(me_data[selected_rows, ])

  # This is the standardized cmv given that the me coefficients
  # are already standardized
  cmv <- prod(cmv)

  corrected_corr <- tibble::as_tibble(replace_matrix_cmv(x, cmv, cmv_vars),
                                      .name_repair = "minimal")

  # Turn the correlation back to a correlation for he diagonal
  # to be one
  ## corrected_cor[, -1] <- stats::cov2cor(as.matrix(corrected_cor[, -1]))
  corrected_corr
}




#' Estimate the Common Method Variance (CMV) coefficient of a set of variables
#'
#'
#' @param me_data a data frame or tibble of class \code{me}
#' which contains the desired variables from which to estimate the CMV.
#'
#' @return a numeric vector of length one with the estimated coefficient
#' @export
#'
#' @seealso \code{\link{me_cmv_cor}} for automatically adjusting a correlation
#' matrix for the CMV, \code{\link{me_cmv_cov}} for automatically adjusting a
#' covariance matrix for the CMV.
#'
#' @examples
#' library(tibble)
#'
#' me_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(0.5, 0.1, 0.6, 0.7, 0.8))
#'
#' estimate_cmv(me_df)
#'
#' \dontrun{
#'
#' me_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(NA, 0.1, 0.6, 0.7, 0.8))
#'
#' estimate_cmv(me_df)
#' # Error in estimate_cmv(me_df) :
#' # me_data must have non-missing values at columns reliability and validity for all variables
#' }
#'
#'
estimate_cmv <- function(me_data) {
  me_cols <- c("reliability", "validity")
  me_reconstruct(me_data, me_cols)

  if (anyNA(me_data[me_cols])) {
    stop("`me_data` must have non-missing values at columns reliability and validity for all variables")
  }

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
