#' @title Define your measurement error design
#'
#' @param model_syntax A description of the user-specified model as a character
#' string. The model is described using the \code{me} model syntax. See the
#' details and example section for on how to write this syntax.
#'
#' @param .data a data frame containing all variables defined in
#' \code{model_syntax}
#'
#' @param me_data A data frame of class \code{me}. Can be created manually
#' with \code{me_construct}. It is expected that this measurement error data is
#' not square rooted (reliability, validity and quality) as it will be done
#' with \code{me_sscore}.
#'
#' @param drop_sscore_vars Whether to remove variables that compose a sum score
#' from the final correlation. More specifically, if you define the sum score
#' \code{s1 = v1 + v2}, then \code{v1} and \code{v2} are excluded from the final
#' correlation. The default is to exclude these columns since you can get
#' correlation coefficients over one for these variables. Default set to
#' \code{TRUE}.
#'
#' @param ... arguments passed to \code{\link{me_sscore}}.
#'
#' @return an \code{me} design object which can be passed to other functions
#' of \code{measurementfree} such as \code{me_cmv_cor} and \code{me_cmv_cov}.
#'
#' @details
#'
#' The model syntax allows to define the relationship between variables
#' in terms of how to correct for their measurement error and whether
#' they share a common method in the question that was asked. To correct for
#' the measurement error of a set of variables you can write the model
#' like this:
#'
#' \preformatted{~~ stflife + stfwork}
#'
#' For most purposes, a user would just want to correct for measurement error
#' on all variables which have measurement error statistics in \code{me_data}
#' and is present on \code{.data}. For that, \code{~~} is interpreted as
#' a normal R formula so you can automatically apply the correction to all
#' variables like this:
#'
#' \preformatted{~~ .}
#'
#' Correcting for measurement error (that is, using `~~`) is a necessary
#' step for `medesign`. That is, it always needs to be specified as it is
#' the first step on which other corrections are built. \code{medesign}
#' will raise an error if no `~~` is specified.
#'
#' For correcting for common method variance, the syntax is virtually the same
#' but exchanging `~~` for `~`. For example, if the questions called
#' \code{stflife} and \code{stfwork} were both measured with a 1-10 likert
#' scale related to satisfaction, these two questions were measured with
#' the same method. The model syntax would specify them like:
#'
#' \preformatted{~ stflife + stfwork}
#'
#' These two questions should also be in \code{.data} and in \code{me_data},
#' otherwise no calculation can be performed. \code{.data} should be a data
#' frame with all variables specified in the \code{model_syntax} while
#' \code{me_data} should be a data frame with 4 columns: question, reliability,
#' validity an quality. The first column should contain the names of the
#' variables as character vector, while the other three should be numeric
#' vectors with their corresponding reliability, validity and quality
#' values.
#'
#' The model syntax also allows you to create 'new variables' and also
#' specify whether these new variables share a common method. For example,
#' we can add the variables /code{trstplt, trstprl, trstprt} together
#' and then specify that the new 'sum score' variable shares a common
#' method with the variables \code{stflife} and \code{stfwork}. We would
#' write it like this:
#'
#' \preformatted{
#' # Correct for measurement error of all variables
#' ~~ .
#'
#' # Creation of new variables
#' std(new_variable) = trstplt + trstprl + trstprt
#'
#' # Correct for common method variance
#' ~ new_variable + stflife + stfwork
#'}
#'
#' You can also combine these building blocks to specify a shared
#' common method between two new variables. For example:
#'
#' \preformatted{
#'
#' # Quality correction
#' ~~ .
#'
#' # Creation of new variables
#' std(new_variable1) = trstplt + trstprl + trstprt
#' std(new_variable2) = stflife + stfwork
#'
#' # Correction for common method
#' ~ new_variable1 + new_variable2
#'}
#'
#' Note that the operator \code{std()} just works to specify
#' the we want this new variable to be standardized.
#'
#' Currently, \code{medesign} replaces the non-NA quality of all variables
#' found in \code{me_data} and \code{.data} in the correlation/covariance
#' diagonal by default. This is then used by \code{me_cmv_cor} and
#' \code{me_cmv_cov} to correct for the quality in the diagonal.
#'
#' @author Jorge Cimentada and Wiebke Weber
#'
#' @export
#'
#' @examples
#'
#' data(ess7es)
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
#' medesign(me_syntax, ess7es, me_data)
#'
#' # Each pair of variables share a common method.
#' me_syntax <-
#'  "~~ .;
#'   ~ trstplt + trstprl + trstprt
#'   ~ stfedu + stfhlth"
#'
#' me_data2 <-
#'   data.frame(
#'     question = c("stfedu", "stfhlth"),
#'     reliability = c(0.757, 0.760),
#'     validity = c(0.838, 0.798),
#'     quality = c(0.635, 0.607)
#'   )
#'
#' me_data <- rbind(me_data, me_data2)
#'
#' medesign(me_syntax, ess7es, me_data)
#'
#' # We can also create new variables and use them
#' # as variables sharing a common method
#'
#' me_syntax <-
#'  "~~ .;
#'   std(new_sscore1) = trstplt + trstprl + trstprt;
#'   ~ new_sscore1 + stfedu + stfhlth"
#'
#' medesign(me_syntax, ess7es, me_data)
#'
#' # Alternatively, we can also combine two newly created variables
#' # as sharing a common method
#' me_syntax <-
#'  "std(new_sscore1) = trstplt + trstprl + trstprt;
#'   std(new_sscore2) = stfedu + stfhlth;
#'   ~~ .;
#'   ~ new_sscore1 + new_sscore2"
#'
#' medesign(me_syntax, ess7es, me_data)
#'
medesign <- function(model_syntax, .data, me_data, drop_sscore_vars = TRUE, ...) {

  me_data <- as_me(me_data)
  stopifnot(is.data.frame(.data), nrow(.data) > 0)
  parsed_model <- me_parse_model(model_syntax, me_data, .data)

  # If no quality has been specified, raise error
  if (!any(parsed_model$op == "~~")) {
    stop("You need to provide a quality specification with `~~`. To correct for the quality of all available variables write `~~ .`") #nolintr
  }

  split_model <- split(parsed_model, parsed_model$lhs)
  split_model <- Filter(function(x) all(x$op != "~~"), split_model)

  vars_used <- lapply(split_model, function(x) {
    all.vars(stats::as.formula(paste0("~", paste(x$rhs, collapse = "+"))))
  })

  # Check only for sscore variables because this is done first
  which_sscore <- vapply(split_model, function(x) all(x$op == "="),
                         FUN.VALUE = logical(1))

  .data <- as.data.frame(.data)
  for (i in vars_used[which_sscore]) check_data_vars(.data, i)
  for (i in vars_used[which_sscore]) check_me_vars(me_data, i)

  .data <- create_sscore(parsed_model, .data)

  # Get square root of validity and reliability
  col_trans <- c("validity", "reliability")
  me_data[col_trans] <- lapply(me_data[col_trans], sqrt)
  me_data$quality_coef <- sqrt(me_data[['quality']])

  # method effect
  me_data$method_eff <- with(me_data, reliability * sqrt(1 - validity^2))

  me_data_sscore <- adapted_sscore_quality(parsed_model = parsed_model,
                                           .data = .data,
                                           me_data = me_data,
                                           .drop = FALSE,
                                           ...)

  if (drop_sscore_vars) {
    sscore_vars_used <- unlist(vars_used[which_sscore])
    valid_vars <- setdiff(names(.data), sscore_vars_used)
    .data <- .data[valid_vars]
  }

  # Doing it AGAIN because we calculated the sum score quality
  # before
  complete_parse <- me_parse_model(model_syntax, me_data_sscore, .data)
  quality_vars <- complete_parse[complete_parse$op == "~~", "rhs"]
  cmv_vars <- complete_parse[complete_parse$op == "~", "rhs"]

  check_number_cmv(parsed_model)
  check_me_vars(me_data_sscore, unlist(vars_used))
  cmv_quality_present <- cmv_vars %in% quality_vars

  if (!all(cmv_quality_present)) {
    not_present <- cmv_vars[!cmv_quality_present]
    stop(
      "All variables defined as adjusting for CMV (`~`) need to be also included for the correction of measurement error in `~~`", #nolintr
      call. = FALSE
    )
  }

  quality_vars <- intersect(quality_vars, names(.data))

  check_me_na(
    me_data_sscore[me_data_sscore$question %in% quality_vars, ],
    me_cols = c("reliability", "validity")
  )

  check_data_vars(.data, unlist(vars_used[!which_sscore]))
  check_data_na(.data, unlist(vars_used[!which_sscore]))

  qual_cor <- stats::cor(.data, use = "complete.obs")
  qual_cov <- stats::cov(.data, use = "complete.obs")

  # Replace the diagonal with the non-na quality of all variables
  # in me_data_sscore (not the filtered one). This could change
  # to an explicit declaration of quality in model_syntax in the future
  me_data_sscore <- me_data_sscore[!is.na(me_data_sscore$quality), ]

  message(
    paste0("Correcting for measurement error in ", paste0(quality_vars, collapse = ", "), ". If you want to correct other variables, make sure they are both in `me_data` and `.data` and you specify their names in the model syntax (`~~`).") #nolintr
  )

  tmp_me <- me_data_sscore[me_data_sscore$question %in% quality_vars, ]
  pos_diag <- stats::na.omit(match(tmp_me$question, rownames(qual_cor)))
  diag(qual_cor)[pos_diag] <- tmp_me$quality

  ## sscores <- setdiff(tmp_me$question, me_data$question)
  sd_sscore <- vapply(.data[quality_vars], stats::sd, na.rm = TRUE, FUN.VALUE = numeric(1))
  sd_pos <- match(names(sd_sscore), tmp_me$question)
  diag(qual_cov)[pos_diag] <- tmp_me$quality[sd_pos] * (sd_sscore^2)

  structure(
    list(parsed_model = parsed_model,
         .data = .data,
         me_data = tmp_me,
         corr = matrix2tibble(qual_cor),
         covv = matrix2tibble(qual_cov)
         ),
    class = "medesign"
  )
}

##' @export
print.medesign <- function(x, ...) {
  sscore_df <- x$parsed_model[x$parsed_model$op == "=", ]
  split_sscore <- split(sscore_df, sscore_df$lhs)

  formatted_sscore <- vapply(split_sscore, function(x) {
    paste(unique(x$lhs), unique(x$op), paste(unique(x$rhs), collapse = " + "))
  }, FUN.VALUE = character(1)
  )

  cmv_df <- x$parsed_model[x$parsed_model$op == "~", ]
  split_cmv <- split(cmv_df, cmv_df$lhs)

  formatted_cmv <- vapply(split_cmv, function(x) {
    paste(unique(x$op), paste(unique(x$rhs), collapse = " + "))
  }, FUN.VALUE = character(1))

  formatted_quality <- paste0("~~ ", paste0(x$me_data$question, collapse = " + "))

  clean_model <- paste0("   ",
                        c(formatted_sscore, formatted_quality, formatted_cmv),
                        collapse = "\n")

  cat("<Measurement error design>",
      "Parsed model:",
      clean_model,
      sep = "\n")
}

create_sscore <- function(parsed_model, .data) {
  sscore_df <- parsed_model[parsed_model$op == "=", ]

  if (nrow(sscore_df) == 0) return(.data)

  separate_ss <- split(sscore_df, sscore_df$lhs)

  # Preserves same order because split doesn't preserve
  separate_ss <- separate_ss[unique(sscore_df$lhs)]

  for (x in separate_ss) {
    .data <- scale_add_sscore(.data, x$lhs, x$rhs)
  }

  .data
}

scale_add_sscore <- function(.data, new_name, vars_names) {
  ss_name <- unique(new_name)
  vars_names <- paste0("scale(", strsplit(vars_names, "\\+")[[1]], ")",
                       collapse = "+")

  sscore_for <-
    stats::as.formula(
      paste("~ I(", vars_names, ")")
    )

  available <- all.vars(sscore_for) %in% names(.data)

  if (sum(available) != length(available)) {
    stop(paste0("Variable(s) ",
                paste0(all.vars(sscore_for)[!available], collapse = ", "),
                " from sumscore",
                ss_name,
                " not available in `data`")
         )
  }

  created_ss <-
    stats::setNames(
      # Leave NA's as they are
      stats::model.frame(sscore_for, .data, na.action = NULL),
      ss_name
    )

  # This is because stats::model.frame returns the new variables with class
  # AsIs from the I() in the formula
  created_ss[ss_name] <- as.numeric(created_ss[[ss_name]])
  .data[[ss_name]] <- created_ss[[ss_name]]

  .data
}

adapted_sscore_quality <- function(parsed_model, .data, me_data, ...) {
  sscore_df <- parsed_model[parsed_model$op == "=", ]

  if (nrow(sscore_df) == 0) return(me_data)

  separate_ss <- split(sscore_df, sscore_df$lhs)

  # Preserves same order because split doesn't preserve
  separate_ss <- separate_ss[unique(sscore_df$lhs)]

  for (x in separate_ss) {
    me_data <-
      me_sscore_(me_data = me_data,
                 .data = .data,
                 new_name = x$lhs,
                 vars_names = all.vars(stats::as.formula(paste0("~", x$rhs))),
                 ...
                 )
  }

  me_data
}
