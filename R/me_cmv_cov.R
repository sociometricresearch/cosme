#' Adjust a covariance matrix for Common Method Variance (CMV) and Quality of Sum score
#'
#' \code{me_cmv_cov} accepts an \code{medesign} object specified in
#' \code{\link{medesign}} and adjusts the covariance coefficients of
#' common method variables with the reliability and validity coefficients
#' from \code{me_data} as well as the quality of sum scores.
#'
#' @param .medesign An \code{medesign} object given by \code{\link{medesign}}
#'
#' @return The common-method-variance and quality corrected covariance matrix.
#'
#' @export
#'
#' @seealso \code{link{medesign}} and \code{\link{me_cmv_cor}} for the same
#' adjustment but for a correlation matrix.
#'
#' @examples
#'
#' set.seed(2131)
#'
#' # measurement data extract from SQP for Spain, Round 7 in Spanish
#' me_data <-
#' data.frame(
#'   question = c("ppltrst", "polintr", "trstprl", "trstplt", "trstprt", "stfedu", "stfhlth"),
#'   reliability = c(0.737, 0.624, 0.812, 0.852, 0.858, 0.757, 0.76),
#'   validity = c(0.952, 0.964, 0.959, 0.965, 0.956, 0.838, 0.798),
#'   quality = c(0.702, 0.601, 0.779, 0.822, 0.821, 0.635, 0.607)
#' )
#'
#' # Define a measurement error model with two sum scores
#' # and sharing a common method variance between ppltrst
#' # and the zpoltrst sum score
#' m1 <- "std(zserv) = stfhlth + stfedu;
#'        std(zpoltrst) = trstprl + trstplt + trstprt;
#'        ~ ppltrst + zpoltrst"
#'
#' # Create measurement error design
#' mdes <-
#'   medesign(
#'     model_syntax = m1,
#'     .data = ess7es[me_data$question],
#'     me_data = me_data
#'   )
#'
#' mdes
#'
#' # Original covariance matrix
#' me_covariance(mdes$.data)
#'
#' # Coefficients of covariance changes
#' # when adjusting for common method variance and
#' # quality of sum scores
#' me_cmv_cov(mdes)
#'
#'
me_cmv_cov <- function(.medesign) {

  if (!inherits(.medesign, "medesign")) {
    stop("`.medesign` should be a measurement error design object given by `medesign`") #nolintr
  }

  res_cor <- me_cmv_cor(.medesign)
  row_order <- res_cor$rowname
  cov_diag <- stats::setNames(diag(as.matrix(.medesign$covv[-1])), .medesign$covv[[1]])
  sscore_name <- .medesign$parsed_model[.medesign$parsed_model$std, "lhs"]
  to_fill <- setdiff(names(cov_diag), sscore_name)
  new_diag <- diag(stats::cov(.medesign$.data, use = "complete.obs"))
  cov_diag[to_fill] <- new_diag[to_fill]
  me_data <- .medesign$me_data[.medesign$me_data$question %in% names(cov_diag), ]
  quality <- stats::setNames(me_data$quality, me_data$question)
  tmp_cor <- as.matrix(res_cor[-1])
  diag_order <- match(res_cor$rowname, names(cov_diag))
  quality_order <- match(res_cor$rowname, names(quality))
  diag(tmp_cor) <- cov_diag[diag_order] * quality[quality_order]

  tmp_cor <- as.data.frame(tmp_cor)
  tmp_cor$rowname <- res_cor$rowname
  tmp_cor <- tmp_cor[names(res_cor)]
  sd_cols <- sqrt(cov_diag[diag_order])

  intm_sd <- cbind(
    expand.grid(var1 = names(sd_cols), var2 = names(sd_cols)),
    expand.grid(sd1 = sd_cols, sd2 = sd_cols)
  )

  intm_sd$multiplication_sd <- intm_sd$sd1 * intm_sd$sd2
  intm_sd$sd1 <- NULL
  intm_sd$sd2 <- NULL
  intm_sd[intm_sd$var1 == intm_sd$var2, "multiplication_sd"] <- 1

  x_long <- stats::reshape(tmp_cor,
                           idvar = "rowname",
                           times = setdiff(names(tmp_cor), "rowname"),
                           timevar = "rowname2",
                           varying = list(setdiff(names(tmp_cor), "rowname")),
                           v.names = "corr_coef",
                           direction = "long")

  rownames(x_long) <- NULL
  names(x_long)[1:2] <- c("var1", "var2")

  coef_sd_df <- merge(intm_sd, x_long, by = c("var1", "var2"))
  coef_sd_df$upd_corr <- with(coef_sd_df, multiplication_sd * corr_coef)
  coef_sd_df <- coef_sd_df[c("var1", "var2", "upd_corr")]

  x_wide <- stats::reshape(coef_sd_df,
                           idvar = "var1",
                           timevar = "var2",
                           direction = "wide")

  names(x_wide) <- gsub("upd_corr.", "", names(x_wide))
  names(x_wide)[1] <- "rowname"
  x_wide <- x_wide[match(row_order, x_wide$rowname), c("rowname", row_order)]
  x_wide <- tibble::as_tibble(x_wide)
  x_wide$rowname <- as.character(x_wide$rowname)
  x_wide
}
