#' Adjust a covariance matrix for Common Method Variance (CMV)
#'
#' \code{me_cmv_cov} accepts an \code{medesign} object specified in
#' \code{\link{medesign}} and adjusts the covariance coefficients of
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
#' @seealso \code{link{medesign}} and \code{\link{me_cmv_cor}} for the same
#' adjustment but for a covariance matrix.
#'
#' @examples
#'
#' \dontrun{
#' ## TODO - Fix when me_cmv_cov works well
#' set.seed(2131)
#' library(tibble)
#'
#' original_df <- as.data.frame(matrix(rnorm(100, sd = 50), nrow = 20))
#'
#' # Toy quality dataset
#' me_df <-
#'  tibble(question = paste0("V", 1:5),
#'  quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
#'  reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
#'  validity = c(0.79, 0.9, 0.6, 0.7, 0.8))
#'
#' # Define mdesign object
#' m_obj <- medesign("~ V4 + V5", original_df, me_df)
#'
#' # Original correlation matrix
#' me_covariance(original_df)
#'
#' # Coefficient of correlation changes
#' # when adjusting for common method variance
#' me_cmv_cov(m_obj)
#'
#' # The V5*V4 from both the upper/lower triangles
#' # covariance matrix changed from -181 to -634
#'}
me_cmv_cov <- function(.medesign) {
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

  res <- .medesign$covv
  for (i in seq_along(list_cmv_vars)) {
    res <- me_cmv_cov_(res,
                       me_data = .medesign$me_data,
                       cmv_vars = list_cmv_vars[[i]],
                       .data = .medesign$.data,
                       cmv = cmv[[i]])
  }

  res
}

me_cmv_cov_ <- function(x, me_data, cmv_vars, .data, cmv = NULL) {
  # Although this check is carried in `medesign`,
  # I leave this one here again because it's specific towards
  # two columns. Doesn't make a difference, but I'd rather
  # err on the side of safety
  me_data <- as_me(me_data, c("reliability", "validity"))
  selected_rows <- me_data[[1]] %in% cmv_vars

  if (is.null(cmv)) cmv <- estimate_cmv(me_data[selected_rows, ])

  if (!all(cmv_vars %in% names(.data))) {
    stop("Variables ",
         paste(cmv_vars[!cmv_vars %in% names(.data)], collapse = ", "),
         " are not preset in `data`")
  }

  # We multiply by the SD of the data because all me coefficients are
  # standardized. This way, we unstandardize them to get an unstandardized cmv
  names_cmv <- names(cmv)
  cmv <-
    prod(
      cmv,
      vapply(.data[cmv_vars],
             stats::sd,
             na.rm = TRUE,
             FUN.VALUE = numeric(1))
    )

  corrected_cov <- tibble::as_tibble(replace_matrix_cmv(x, cmv, cmv_vars),
                                     .name_repair = "minimal")
  corrected_cov
}

## TODO: implementation of CMV for covariance code.
## ess7es$zstfhlth <- scale(ess7es$stfhlth)
## ess7es$zstfedu <- scale(ess7es$stfedu)
## ess7es$serv <- rowSums(ess7es[c("zstfhlth", "zstfedu")])
## ess7es$zserv <- with(ess7es, 1 / sd(serv, na.rm = TRUE) * (zstfhlth + zstfedu))

## ess7es$ztrstprl <- scale(ess7es$trstprl)
## ess7es$ztrstplt <- scale(ess7es$trstplt)
## ess7es$ztrstprt <- scale(ess7es$trstprt)
## ess7es$poltrst <- rowSums(ess7es[c("ztrstplt", "ztrstprt", "ztrstprl")])
## ess7es$zpoltrst <- with(ess7es, 1 / sd(poltrst, na.rm = TRUE) * (ztrstprl + ztrstplt + ztrstprt))

## round(cor(ess7es[c("ppltrst", "polintr", "zserv", "zpoltrst")], use = "complete.obs"), 4)

## sqpr::sqp_login()
## sel_vars <- c("trstprl", "trstplt", "trstprt", "ppltrst", "polintr", "stfedu", "stfhlth")
## me_data <-
##   sqpr::get_sqp(
##     study = "ESS Round 7",
##     question_name = sel_vars,
##     country = "es",
##     lang = "spa"
##   )

## m1 <- "std(zpoltrst) = trstprl + trstplt + trstprt
##        std(zserv) = stfhlth + stfedu
##        ~ ppltrst + zpoltrst"

## mdes <-
##   medesign(
##     model_syntax = m1,
##     .data = ess7es[c("trstprl", "trstplt", "trstprt", "stfedu", "stfhlth", "ppltrst", "polintr")],
##     me_data = me_data
##   )

## res_cor <- me_cmv_cor(mdes)
## res_cov <- matrix2tibble(cov(.data[res_cor$rowname], use = "complete.obs"))

## me_data <- mdes$me_data[match(res_cov$rowname, mdes$me_data$question), ]
## diag(as.matrix(res_cov[-1])) * me_data$quality
## res_cov

