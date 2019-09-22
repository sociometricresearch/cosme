##' The model syntax allows to define the relationship between variables
##' in terms of whether they share a common method in the question
##' that was asked. For example, if the questions called \code{stflife}
##' and \code{stfwork} were both measured with a 1-10 likert scale related
##' to satisfaction, these two questions were measured with the same method.
##' The model syntax would specify them like \code{'~ stflife + stfwork'}.
##'
##' These two questions should also be in \code{.data} and in \code{me_data},
##' otherwise no calculation can be performed. \code{.data} should be a data
##' frame with all variables specified in the \code{model_syntax} while
##' \code{me_data} should be a dataframe with 4 columns: question, reliability,
##' validity an quality. The first column should contain the names of the
##' variables as character vector, while the other three should be numeric
##' vectors with their corresponding reliability, validity and quality
##' values.
##'
##' 
##' 
##' @title Define your measurement error design
##' @param model_syntax A description of the user-specified model as a character
##' string. The model is described using the me model syntax. See the details
##' and example section for on how to write this syntax.
##' 
##' @param .data a data frame containing all variables defined in
##' \code{model_syntax}
##' @param me_data A dataframe of class \code{me}. Can be created manually
##' with \code{me_construct}.
##' @return an \code{me} design object which can be passed to other functions
##' of measurementfree.
##' @author Jorge Cimentada
##' 
##' @export
##' 
##' @examples
##'
##' # These three variables share a common method
##' me_syntax <- "~ mpg + cyl + drat"
##'
##' # Fake data for the example
##' me_data <- data.frame(stringsAsFactors = FALSE,
##'                       question = c("mpg", "cyl", "drat"),
##'                       reliability = c(0.729, 0.815, 0.68),
##'                       validity = c(0.951, 0.944, 0.79),
##'                       quality = c(0.693, 0.77, 0.89)
##'                       )
##'
##' medesign(me_syntax, mtcars, me_data)
##'
##'
##' # Each pair of variables share a common method.
##' me_syntax <-
##' "# Let's assume these two variables share a common method
##'  ~ mpg + cyl
##'  # Let's assume these two variables as well
##'  ~ drat + disp"
##'
##' # Fake data for the example
##' me_data <- data.frame(stringsAsFactors = FALSE,
##'                       question = c("mpg", "cyl", "drat", "disp"),
##'                       reliability = c(0.729, 0.815, 0.68, 0.78),
##'                       validity = c(0.951, 0.944, 0.79, 0.97),
##'                       quality = c(0.693, 0.77, 0.89, 0.68)
##'                       )
##'
##' medesign(me_syntax, mtcars, me_data)
##' 

# TODO: Here I check whether the variables parsed by model_syntax
# are in the me_data data but when I extract the variables
# from the sscore, I cannot disentangle what the vars are
# from, let's say, weights.

# TODO: Currently you don't have sumscores implemented because if
# you create the the quality of the sumscore, reliability
# and validity have missing values and the original me code
# raises an error. I think it's because sscore are only
# used to adjust for quality, but why do I remember Wiebke
# doing CMV between sumscores and observed variables? Maybe
# I got confused.

# TODO: Populate the descript extensively on how the syntax
# is written and what is permitted. See model.syntax
# of lavaan for an example
medesign <- function(model_syntax, .data, me_data) {
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
  
  for (i in vars_used[which_sscore]) check_data_vars(.data, i)
  for (i in vars_used[which_sscore]) check_me_vars(me_data, i)

  .data <- create_sscore(parsed_model, .data)
  
  me_data <- adapted_sscore_quality(parsed_model,
                                    .data = .data,
                                    me_data = me_data)

  # Because variables used to create the quality
  # of the sumscore are now excluded from me_data
  # if we checked whether they're there, it would
  # raise an error. Here we exclude them
  vars_used <- unlist(vars_used[!which_sscore])

  # I checked he sscore vars above but I all variables here again
  # just because I'm lazy
  check_number_cmv(parsed_model)
  check_me_vars(me_data, vars_used)
  check_me_na(me_data, me_cols = c("reliability", "validity"))
  check_data_vars(.data, vars_used)
  check_data_na(.data, vars_used)

  me_data_filt <- me_data[match(vars_used, me_data$question), ]

  qual_cor <- stats::cor(.data, use = "complete.obs")
  qual_cov <- stats::cov(.data, use = "complete.obs")

  # Replce the diagonal with the quality of all variables
  # in me_data (not the filtered one). This could change
  # to an explicit declaration of quality in model_syntax in the future
  pos_diag <- match(me_data$question, rownames(qual_cor))
  diag(qual_cor)[pos_diag] <- me_data$quality
  diag(qual_cov)[pos_diag] <- me_data$quality

  structure(
    list(parsed_model = parsed_model,
         .data = .data,
         me_data = me_data_filt,
         corr = matrix2tibble(qual_cor),
         covv = matrix2tibble(qual_cov)),
    class = "medesign"
  )

}

print.medesign <- function(x) {
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

    ss_name <- unique(x$lhs)
    if (x$std) x$rhs <- paste0("scale(", x$rhs, ")")
    
    sscore_for <-
      stats::as.formula(
        paste("~ I(", x$rhs, ")")
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
    .data <- cbind(.data, created_ss)
  }
  .data
}

adapted_sscore_quality <- function(parsed_model, .data, me_data) {
  sscore_df <- parsed_model[parsed_model$op == "=", ]

  if (nrow(sscore_df) == 0) return(me_data)

  separate_ss <- split(sscore_df, sscore_df$lhs)

  # Preserves same order because split doesn't preserve
  separate_ss <- separate_ss[unique(sscore_df$lhs)]

  for (x in separate_ss) {
    me_data <-
      me_sscore_(me_data = me_data,
                 data = .data,
                 new_name = x$lhs,
                 vars_names = all.vars(stats::as.formula(paste0("~", x$rhs)))
                 )
  }
  
  me_data
}
