#' Adjust a correlation matrix for Common Method Variance (CMV) and Quality of Sum score
#'
#' \code{me_cmv_cor} accepts an \code{medesign} object created by
#' \code{\link{medesign}} and adjusts the correlation coefficients of
#' common method variables with the reliability and validity coefficients
#' from \code{me_data}.
#'
#' @param .medesign An \code{medesign} object given by \code{\link{medesign}}
#'
#' @return The common-method-variance corrected correlation. It is a correlation
#' data frame but some correlation coefficients are adjusted for their shared
#' common method variance and the quality of sum scores.
#'
#' @export
#'
#' @seealso \code{link{medesign}} and \code{\link{me_cmv_cov}} for the same
#' adjustment but for a covariance matrix.
#'
#' @examples
#'
#' data(ess7es)
#'
#' .data <- ess7es[1:3]
#'
#' # Correct for measurement error for all variables
#' # and correct for the fact that these three variables
#' # share a common method
#' me_syntax <- "~~ .; ~ trstplt + trstprl + trstprt"
#'
#' # Data from sqp
#' me_data <-
#'   data.frame(
#'     question = c("trstprl", "trstplt", "trstprt"),
#'     reliability = c(0.812, 0.852, 0.858),
#'     validity = c(0.959, 0.965, 0.956),
#'     quality = c(0.779, 0.822, 0.821)
#'   )
#'
#' mdes <- medesign(me_syntax, .data, me_data)
#'
#' # Correlations adjusted for CMV
#' me_cmv_cor(mdes)
#'
#' # Original correlation
#' me_correlate(.data)
#'
#' # More elaborate with two pairs of variables
#' # sharing common methods
#'
#' # Each pair of variables share a common method.
#' me_syntax <-
#'  "~~ .
#'   ~ trstplt + trstprl + trstprt
#'   ~ stfedu + stfhlth"
#'
#' me_data2 <-
#'   data.frame(
#'     question = c("stfedu", "stfhlth"),
#'     reliability = c(0.870057469366248, 0.871779788708135),
#'     validity = c(0.915423399307664, 0.893308457365092),
#'     quality = c(0.796868872525461, 0.779102047231298)
#'   )
#'
#' me_data <- rbind(me_data, me_data2)
#'
#' .data <- ess7es[1:5]
#' mdes <- medesign(me_syntax, .data, me_data)
#'
#' # Correlations adjusted for CMV
#' me_cmv_cor(mdes)
#'
#' # Original correlations
#' me_correlate(.data)
#'
me_cmv_cor <- function(.medesign) {
  cmv <- NULL

  if (!inherits(.medesign, "medesign")) {
    stop("`.medesign` should be a measurement error design object given by `medesign`") #nolintr
  }

  # cmv vector equals the same length as the number of cmv definitions
  parsed_m <- .medesign$parsed_model
  number_cmvs <- unique(parsed_m[parsed_m$op == "~", "lhs"])
  if (!is.null(cmv)) {
    stopifnot(is.numeric(cmv))
    stopifnot(length(cmv) == length(number_cmvs))
  } else {
    # Create empty list to iterate over each cmv
    cmv <- rep(list(NULL), length(number_cmvs))
  }

  cmv_df <- parsed_m[parsed_m$op == "~", ]

  cmv_groups <- split(cmv_df, cmv_df$lhs)
  list_cmv_vars <- lapply(cmv_groups, `[[`, "rhs")

  if (length(list_cmv_vars) == 0) {
    names_cmv <- paste0(
      .medesign$me_data$question,
      .medesign$me_data$question,
      collapse = "_"
    )

    list_cmv_vars <- list(stats::setNames(.medesign$me_data$question, names_cmv))
    cmv <- list(stats::setNames(0, names_cmv))
  }

  # If it's the first iteration, provide the original correlation
  # otherwise the looped corrected correlation
  res <- .medesign$corr
  for (i in seq_along(list_cmv_vars)) {
    res <- me_cmv_cor_(
      x = res,
      me_data = .medesign$me_data,
      cmv_vars = list_cmv_vars[[i]],
      cmv = cmv[[i]]
    )
  }

  res
}

me_cmv_cor_ <- function(x, me_data, cmv_vars, cmv = NULL) {
  # Although this check is carried in `medesign`,
  # I leave this one here again because it's specific towards
  # two columns. Doesn't make a difference, but I'd rather
  # err on the side of safety
  me_data <- as_me(me_data, c("reliability", "validity"))

  selected_rows <- me_data[[1]] %in% cmv_vars
  if (is.null(cmv)) cmv <- estimate_cmv(me_data[selected_rows, ])

  corrected_corr <-
    tibble::as_tibble(
              replace_matrix_cmv(x, cmv, cmv_vars),
              .name_repair = "minimal"
            )

  # Turn the correlation back to a correlation for the diagonal to be one
  corrected_corr[, -1] <- stats::cov2cor(as.matrix(corrected_corr[, -1]))
  corrected_corr
}

estimate_cmv <- function(me_data) {

  me_cols <- c("reliability", "validity")
  check_me_na(me_data, me_cols)

  all_combn <- utils::combn(seq_len(nrow(me_data)), 2, simplify = FALSE)

  cmv <- unlist(lapply(all_combn, function(i) {
    m <- me_data[i, ]
    stats::setNames(prod(m$method_eff), paste0(m$question, collapse = "_"))
  }))

  cmv
}

# This function is the one doing the replacement of the upper
# and lower of the correlation matrix.
replace_matrix_cmv <- function(x, cmv, cmv_vars) {
  x <- as.data.frame(x)
  row_order <- x$rowname
  col_order <- names(x)
  x_long <- stats::reshape(x,
                           idvar = "rowname",
                           times = setdiff(names(x), "rowname"),
                           timevar = "rowname2",
                           varying = list(setdiff(names(x), "rowname")),
                           v.names = "values",
                           direction = "long")

  cmv_df <- data.frame(rowname = gsub("_.+$", "", names(cmv)),
                       rowname2 = gsub("^.+_", "", names(cmv)),
                       values2 = cmv)

  cmv_df2 <- cmv_df[c("rowname2", "rowname", "values2")]
  names(cmv_df2) <- c("rowname", "rowname2", "values2")
  cmv_df <- rbind(cmv_df, cmv_df2)

  merge_long <- merge(x_long, cmv_df, all.x = TRUE, sort = FALSE)
  merge_long$values2[is.na(merge_long$values2)] <- 0
  merge_long$values <- with(merge_long, values - values2)
  merge_long$values2 <- NULL

  x_wide <- stats::reshape(merge_long,
                           idvar = "rowname",
                           timevar = "rowname2",
                           direction = "wide")

  names(x_wide) <- gsub("values.", "", names(x_wide))
  x_wide[match(row_order, x_wide$rowname), col_order]
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

tibble2matrix <- function(x) {
  rowname_tibble <- x$rowname
  x$rowname <- NULL
  x <- as.matrix(x)
  row.names(x) <- rowname_tibble
  x
}
