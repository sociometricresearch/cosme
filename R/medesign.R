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
#' @param ... arguments passed to \code{\link{me_sscore}}.
#'
#' @return an \code{me} design object which can be passed to other functions
#' of \code{measurementfree} such as \code{me_cmv_cor} and \code{me_cmv_cov}.
#'
#' @details
#'
#' The model syntax allows to define the relationship between variables
#' in terms of whether they share a common method in the question
#' that was asked. For example, if the questions called \code{stflife}
#' and \code{stfwork} were both measured with a 1-10 likert scale related
#' to satisfaction, these two questions were measured with the same method.
#' The model syntax would specify them like:
#'
#' \code{'~ stflife + stfwork'}.
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
#' \code{'std(new_variable) = trstplt + trstprl + trstprt;
#'        ~ new_variable + stflife + stfwork'}
#'
#' You can also combine these building blocks to specify a shared
#' common method between two new variables. For example:
#'
#' \code{'std(new_variable1) = trstplt + trstprl + trstprt;
#'        std(new_variable2) = stflife + stfwork;
#'        ~ new_variable1 + new_variable2'}
#'
#' Note that the small operator \code{std()} just works to specify
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
#' # These three variables share a common method
#' me_syntax <- "~ trstplt + trstprl + trstprt"
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
#'  "~ trstplt + trstprl + trstprt
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
#'  "std(new_sscore1) = trstplt + trstprl + trstprt;
#'   ~ new_sscore1 + stfedu + stfhlth"
#'
#' medesign(me_syntax, ess7es, me_data)
#'
#' # Alternatively, we can also combined two newly created variables
#' # as sharing a common method
#' me_syntax <-
#'  "std(new_sscore1) = trstplt + trstprl + trstprt;
#'   std(new_sscore2) = stfedu + stfhlth;
#'   ~ new_sscore1 + new_sscore2"
#'
#' medesign(me_syntax, ess7es, me_data)
#'
medesign <- function(model_syntax, .data, me_data, ...) {
  me_data <- as_me(me_data)

  stopifnot(is.data.frame(.data),
            nrow(.data) > 0)

  parsed_model <- me_parse_model(model_syntax)
  split_model <- split(parsed_model, parsed_model$lhs)

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

  # Get square root of validity, quality and reliability
  me_data[-1] <- lapply(me_data[-1], sqrt)

  # method effect
  me_data$method_eff <- with(me_data, reliability * sqrt(1 - validity^2))

  me_data_sscore <- adapted_sscore_quality(parsed_model = parsed_model,
                                           .data = .data,
                                           me_data = me_data,
                                           .drop = FALSE,
                                           ...)

  # Because variables used to create the quality
  # of the sumscore are now excluded from me_data
  # if we checked whether they're there, it would
  # raise an error. Here we exclude them
  ## vars_used <- unlist(vars_used[!which_sscore])

  # I checked the sscore vars above but all variables here again just
  # because I'm lazy
  check_number_cmv(parsed_model)
  check_me_vars(me_data_sscore, unlist(vars_used))

  ## me_data_filt <- me_data[match(vars_used, me_data$question), ]

  # Only check NA's on variables used
  non_sscore_vars <- setdiff(unlist(vars_used), names(vars_used))
  check_me_na(me_data_sscore[me_data_sscore$question %in% non_sscore_vars, ],
              me_cols = c("reliability", "validity"))

  check_data_vars(.data, unlist(vars_used))
  check_data_na(.data, unlist(vars_used))

  qual_cor <- stats::cor(.data, use = "complete.obs")
  qual_cov <- stats::cov(.data, use = "complete.obs")

  # Replace the diagonal with the non-na quality of all variables
  # in me_data_sscore (not the filtered one). This could change
  # to an explicit declaration of quality in model_syntax in the future
  me_data_sscore <- me_data_sscore[!is.na(me_data_sscore$quality), ]

  vars_match <- intersect(me_data_sscore$question, rownames(qual_cor))
  tmp_me <- me_data_sscore[me_data_sscore$question %in% vars_match, ]
  pos_diag <- stats::na.omit(match(tmp_me$question, rownames(qual_cor)))
  diag(qual_cor)[pos_diag] <- tmp_me$quality

  # The covariance quality needs to be unstandardized
  sscores <- setdiff(tmp_me$question, me_data$question)
  sd_sscore <- vapply(.data[sscores], stats::sd, na.rm = TRUE, FUN.VALUE = numeric(1))
  sd_pos <- match(names(sd_sscore), tmp_me$question)
  tmp_me$quality[sd_pos] <- tmp_me$quality[sd_pos] * (sd_sscore^2)

  diag(qual_cov)[pos_diag] <- tmp_me$quality

  structure(
    list(parsed_model = parsed_model,
         .data = .data,
         me_data = me_data_sscore,
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

  clean_model <- paste0("   ",
                        c(formatted_sscore, formatted_cmv),
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
