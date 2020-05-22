#' Adjust a correlation matrix for Common Method Variance (CMV)
#'
#' \code{me_cmv_cor} accepts an \code{medesign} object specified in
#' \code{\link{medesign}} and adjusts the correlation coefficients of
#' common method variables with the reliability and validity coefficients
#' from \code{me_data}. Optionally, you can supply the CMV coefficients
#' manually in the argument \code{cmv}.
#'
#' @param .medesign An \code{medesign} object given by \code{\link{medesign}}
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
me_cmv_cor <- function(.medesign) {
  cmv <- NULL

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

  # If it's the first iteration, provide the original correlation
  # otherwise the looped corrected correlation
  res <- .medesign$corr
  for (i in seq_along(list_cmv_vars)) {
    res <- me_cmv_cor_(x = res,
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
  method_effect <- sqrt(1 - me_data[[me_cols[2]]])

  m <- stats::setNames(reliability_coef * method_effect, me_data$question)

  all_combn <- utils::combn(seq_len(nrow(me_data)), 2, simplify = FALSE)

  cmv <- unlist(lapply(all_combn, function(i) {
    setNames(prod(m[i]), paste0(names(m[i]), collapse = "_"))
  }))
  
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

  cmv_combn <- strsplit(names(cmv), "_")

  x <- as.data.frame(x)

  # Subset only the select variables
  for (i in seq_along(cmv_combn)) {
    cmv_vars <- cmv_combn[[i]]
    x_row_low <- match(cmv_vars, x$rowname)
    x_col_low <- match(cmv_vars, names(x))
    p <- x[x_row_low, x_col_low, drop = FALSE]
    
    p[lower.tri(p)] <- p[lower.tri(p)] - cmv[i] # adjust the lower.tri
    p[upper.tri(p)] <- p[upper.tri(p)] - cmv[i] # adjust the upper.tri
    x[x_row_low, x_col_low] <- p # replace in the original data.frame
  }

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

  x <- tibble::as_tibble(x, .name_repair = "minimal")

  if (!tibble::has_rownames(x) & !has_rowname_col) {
    x <- tibble::add_column(x,
                            rowname = names(x),
                            .before = 1,
                            .name_repair = "minimal")
  }
  x
}
