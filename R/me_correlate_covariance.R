#' Calculate a correlation matrix with an adjusted diagonal
#'
#' \code{me_correlate} calculates a correlation matrix through
#' \code{\link[stats]{cor}} and multiplies the diagonal with the supplied
#' numeric vector. It's a wrapper around \code{\link[stats]{cor}} with slight
#' tweaks.
#'
#' @param x a matrix or data frame with numeric columns.
#'
#' @param diag_adj a numeric vector with length equal to the number of columns
#'   of \code{x} to be multiplied by the diagonal.  Alternatively, it can be of
#'   length 1 which will be repeated through the whole diagonal.  If the
#'   argument \code{wt} is used, then the length of \code{diag_adj} must be the
#'   same as \code{x} excluding the weight column. By default it multiplies by
#'   1, giving the same diagonal.
#'
#' @param wt the name of the column which contains the weights as bare unquoted
#'   names or as character vector or length 1. Note that when the weight
#'   argument is specified, the estimation is done using
#'   \code{\link[stats]{cov.wt}} instead of \code{\link[stats]{cor}} or
#'   \code{\link[stats]{cov}}. This means that the arguments \code{use} and
#'   \code{method} are ignored.
#' 
#' @param use an optional character string giving a method for computing
#'   covariances in the presence of missing values. This must be (an
#'   abbreviation of) one of the strings "everything", "all.obs",
#'   "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' @param method a character string indicating which correlation coefficient (or
#'   covariance) is to be computed. One of "pearson" (default), "kendall", or
#'   "spearman": can be abbreviated.
#'
#' @return a correlation \code{tibble} with variable names as a column and the
#'   diagonal multiplied by \code{diag_adj}
#'
#' @seealso \code{\link[stats]{cor}} for the workhorse behind the function and
#'   \code{\link[stats]{cov.wt}} for the function used for the weighting.
#'
#' @export
#'
#' @examples
#'
#' # New diagonal
#' new_diagonal <- rnorm(ncol(mtcars))
#'
#' me_correlate(mtcars)
#'
#' me_correlate(mtcars, new_diagonal)
#'
#' me_correlate(mtcars, new_diagonal, method = "kendall")
#'
#' diagonal_wout_weight <- rnorm(ncol(mtcars) - 1)
#' me_correlate(mtcars, diagonal_wout_weight, wt = mpg)
#' 
me_correlate <- function(x, diag_adj = 1, wt = NULL, use = "complete.obs", method = "pearson") {
  wt <- as.character(substitute(wt))
  wt <- if (length(wt) == 0) NULL else wt

  cor_cov_matrix(type = "cor",
                 x = x,
                 diag_adj = diag_adj,
                 use = use,
                 method = method,
                 wt = wt)
}

#' Calculate a covariance matrix with an adjusted diagonal
#'
#' \code{me_covariance} calculates a covariance matrix through
#' \code{\link[stats]{cov}} and multiplies the diagonal with the supplied
#' numeric vector. It's a wrapper around \code{\link[stats]{cov}} with slight
#' tweaks.
#'
#' @inheritParams me_correlate
#'
#' @return a covariance \code{tibble} with variable names as a column and
#' the diagonal multiplied by \code{diag_adj}
#'
#' @seealso \code{\link[stats]{cov}} for the workhorse behind the function and
#'   \code{\link[stats]{cov.wt}} for the function used for the weighting.
#'
#' @export
#'
#' 
#' @examples
#'
#' # New diagonal
#' new_diagonal <- rnorm(ncol(mtcars))
#'
#' me_covariance(mtcars)
#'
#' me_covariance(mtcars, new_diagonal)
#'
#' me_covariance(mtcars, new_diagonal, method = "kendall")
#'
#' diagonal_wout_weight <- rnorm(ncol(mtcars) - 1)
#' me_covariance(mtcars, diagonal_wout_weight, wt = mpg)
me_covariance <- function(x, diag_adj = 1, wt = NULL, use = "complete.obs", method = "pearson") {
  wt <- as.character(substitute(wt))
  wt <- if (length(wt) == 0) NULL else wt

  cor_cov_matrix(type = "cov",
                 x = x,
                 diag_adj = diag_adj,
                 use = use,
                 method = method,
                 wt = wt
                 )
}

cor_cov_matrix <- function(type, x, diag_adj, use, method, wt) {
  if (!is.numeric(diag_adj)) stop("`diag_adj` must be numeric")

  fun <- if (type == "cor") stats::cor else stats::cov

  if (is.null(wt)) {
    obj_matrix <- fun(x = x,
                      use = use,
                      method = method)
    
    diag_adj <- if (length(diag_adj) == 1) rep(diag_adj, ncol(obj_matrix)) else diag_adj
    
  } else {

    pos_wt <- which(names(x) %in% wt)
    if (length(pos_wt) == 0) stop(paste0("Column ", wt, " not present in data"))
    wout_wt_x <- x[, -pos_wt]
    cor_cov_res <- stats::cov.wt(wout_wt_x, wt = x[[wt]], cor = TRUE)
    obj_matrix <- if (type == "cor") cor_cov_res$cor else cor_cov_res$cov
    diag_adj <-
      if (length(diag_adj) == 1) rep(diag_adj, ncol(wout_wt_x)) else diag_adj
  }

  if (length(diag_adj) != ncol(obj_matrix)) {
    stop("`diag_adj` must be the same length as the number of columns in `x`.")
  }

  diag(obj_matrix) <- diag_adj * diag(obj_matrix)

  # If has no rows/column names, create them
  names_matrix <- dimnames(obj_matrix)
  if (!is.list(names_matrix) & length(names_matrix) != 3) {
    both_names <- paste0("V", seq_len(ncol(obj_matrix)))
    dimnames(obj_matrix) <- list(both_names, both_names)
  }

  formatted_matrix <- tibble::rownames_to_column(as.data.frame(obj_matrix))

  tibble::as_tibble(formatted_matrix, .name_repair = "minimal")
}
