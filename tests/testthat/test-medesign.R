validate_medesign <- function(x) {
  expect_length(x, 5)
  expect_s3_class(x, "medesign")
  expect_named(x)
  expect_true(all(names(x) == c("parsed_model",
                                ".data",
                                "me_data",
                                "corr",
                                "covv")))

  expect_true(all(vapply(x, nrow, FUN.VALUE = numeric(1)) > 0))
  expect_true(all(vapply(x, is.data.frame, FUN.VALUE = logical(1))))
  
  expect_s3_class(x$me_data, "me")
}

quality <-
  structure(list(question = c("ppltrst", "polintr", "psppsgv",
                              "psppipl", "ptcpplt", "stflife", "stfeco", "stfedu", "stfhlth",
                              "trstprl", "trstplt", "trstprt"), reliability = c(0.729, 0.659,
                                                                                0.761, 0.757, 0.758, 0.716, 0.823, 0.729, 0.762, 0.815, 0.826,
                                                                                0.854), validity = c(0.951, 0.964, 0.933, 0.932, 0.932, 0.899,
                                                                                                     0.903, 0.827, 0.863, 0.944, 0.975, 0.898), quality = c(0.693,
                                                                                                                                                            0.636, 0.71, 0.705, 0.707, 0.644, 0.743, 0.602, 0.658, 0.77,
                                                                                                                                                            0.805, 0.767), r_coef = c(0.854, 0.812, 0.872, 0.87, 0.871, 0.846,
                                                                                                                                                                                      0.907, 0.854, 0.873, 0.903, 0.909, 0.924), v_coef = c(0.975,
                                                                                                                                                                                                                                            0.982, 0.966, 0.965, 0.965, 0.948, 0.95, 0.909, 0.929, 0.972,
                                                                                                                                                                                                                                            0.987, 0.948), q_coef = c(0.833, 0.797, 0.843, 0.84, 0.841, 0.803,
                                                                                                                                                                                                                                                                      0.862, 0.776, 0.811, 0.877, 0.897, 0.876)), .Names = c("question",
                                                                                                                                                                                                                                                                                                                             "reliability", "validity", "quality", "r_coef", "v_coef", "q_coef"
                                                                                                                                                                                                                                                                                                                             ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                        -12L))



test_that("medesign raises error when bad model_syntax", {
  # 1) The variables defined in the model are present in `me_data`.
  model_syntax <- "~~ .; ~ mpg + cyl + drat"
  .data <- mtcars
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl"),
                        reliability = c(0.729, 0.815),
                        validity = c(0.951, 0.944),
                        quality = c(0.693, 0.77)
                        )

  expect_error(
    medesign(model_syntax, .data, me_data),
    regexp = "Variable(s) drat must be available in `me_data`",
    fixed = TRUE
  )

  # 2) The variables defined in the model have no missing values in `me_data`.
  model_syntax <- "~~ .; ~ mpg + cyl + drat"
  .data <- mtcars
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "drat"),
                        reliability = c(0.729, 0.815, 0.68),
                        validity = c(0.951, 0.944, NA),
                        quality = c(0.693, 0.77, 0.89)
                        )

  expect_error(
    medesign(model_syntax, .data, me_data),
    regexp = "`me_data` must have non-missing values at columns reliability and validity for all variables", #nolintr
    fixed = TRUE
  )

  # 3) The variables defined in the model are present in `data`.
  model_syntax <- "~~ .; ~ mpg + cyl + whatever + sec"
  .data <- mtcars
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "whatever", "sec"),
                        reliability = c(0.729, 0.815, 0.68, 0.79),
                        validity = c(0.951, 0.944, 0.79, 0.67),
                        quality = c(0.693, 0.77, 0.89, 0.9)
                        )
  expect_error(
    medesign(model_syntax, .data, me_data),
    regexp = "Variable(s) whatever, sec not available in `.data`",
    fixed = TRUE
  )

  # 4) The variables defined in the model are not complete missing in `data`.
  model_syntax <- "~~ .; ~ mpg + cyl + whatever"
  .data <- mtcars
  .data$whatever <- NA
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "whatever"),
                        reliability = c(0.729, 0.815, 0.68),
                        validity = c(0.951, 0.944, 0.79),
                        quality = c(0.693, 0.77, 0.89)
                        )

  expect_error(
    medesign(model_syntax, .data, me_data),
    regexp = "Variable(s) whatever are all NA in `.data`. Estimates cannot be calculated using these variables", #nolintr
    fixed = TRUE
  )

  # 5) Defining CMV with one variable raises error.
  model_syntax <- "~~ .; ~ mpg + cyl + whatever; ~ drat"
  .data <- mtcars
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "whatever"),
                        reliability = c(0.729, 0.815, 0.68),
                        validity = c(0.951, 0.944, 0.79),
                        quality = c(0.693, 0.77, 0.89)
                        )

  expect_error(
    medesign(model_syntax, .data, me_data),
    regexp = "You need to supply at least two variables to calculate the Common Method Variance (CMV) in '~ drat'", #nolintr
    fixed = TRUE
  )
})


test_that("medesign checks format of .data", {
  # These three variables share a common method
  me_syntax <- "~~ .; ~ mpg + cyl + drat"
  # Fake data for the example
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "drat"),
                        reliability = c(0.729, 0.815, 0.68),
                        validity = c(0.951, 0.944, 0.79),
                        quality = c(0.693, 0.77, 0.89)
                        )
  .data <- mtcars[numeric(), ]

  expect_error(
    medesign(me_syntax, .data, me_data),
    regexp = "nrow(.data) > 0 is not TRUE",
    fixed = TRUE
  )

  .data <- as.matrix(mtcars)

  expect_error(
    medesign(me_syntax, .data, me_data),
    regexp = "is.data.frame(.data) is not TRUE",
    fixed = TRUE
  )

})

test_that("medesign returns expected format", {
  # These three variables share a common method
  me_syntax <- "~~ .; ~ mpg + cyl + drat"
  # Fake data for the example
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "drat"),
                        reliability = c(0.729, 0.815, 0.68),
                        validity = c(0.951, 0.944, 0.79),
                        quality = c(0.693, 0.77, 0.89)
                        )

  res <- medesign(me_syntax, mtcars, me_data)
  validate_medesign(res)
  expect_equivalent(nrow(res$parsed_model), 6)
  expect_equivalent(na.omit(unique(res$parsed_model$type)), "observed")

  # Make sure that the diag is replaced for the three observed variables
  replaced_diag <- diag(as.matrix(res$corr[-1]))
  expect_true(
    all(names(mtcars)[which(replaced_diag != 1)] == c("mpg", "cyl", "drat"))
  )

  # Compare that the replaced diagonals are different from the original
  # ones for the variables in me_data
  original_diag <- diag(as.matrix(me_covariance(mtcars)[-1]))
  replaced_diag <- diag(as.matrix(res$covv[-1]))
  are_different <- original_diag != replaced_diag

  expect_true(
    all(names(mtcars)[which(are_different)] == c("mpg", "cyl", "drat"))
  )
})

test_that("medesign ignores vars with NA in quality for diagonal", {
  # These three variables share a common method
  model_syntax <- "~~ mpg + cyl; ~ mpg + cyl"
  # Fake data for the example
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "drat"),
                        reliability = c(0.729, 0.815, NA),
                        validity = c(0.951, 0.944, NA),
                        quality = c(0.693, 0.77, NA)
                        )

  res <- medesign(model_syntax, mtcars, me_data)

  replaced_diag <- diag(as.matrix(res$corr[-1]))
  expect_true(
    all(names(mtcars)[which(replaced_diag != 1)] == c("mpg", "cyl"))
  )

  # Compare that the replaced diagonals are different from the original
  # ones for the variables in me_data
  original_diag <- diag(as.matrix(me_covariance(mtcars)[-1]))
  replaced_diag <- diag(as.matrix(res$covv[-1]))
  are_different <- original_diag != replaced_diag

  expect_true(
    all(names(mtcars)[which(are_different)] == c("mpg", "cyl"))
  )

})

test_that("medesign ignores quality with ALL NA in quality for diagonal", {
  # These three variables share a common method
  me_syntax <- "~~ mpg + cyl; ~ mpg + cyl"

  # Fake data for the example
  me_data <- data.frame(
    stringsAsFactors = FALSE,
    question = c("mpg", "cyl", "drat"),
    reliability = c(0.729, 0.815, NA),
    validity = c(0.951, 0.944, NA),
    quality = c(NA, NA, NA)
  )

  res <- medesign(me_syntax, mtcars, me_data)

  replaced_diag <- diag(as.matrix(res$corr[-1]))
  expect_true(
    all(names(mtcars)[which(replaced_diag != 1)] == character(0))
  )

  # Compare that the replaced diagonals are different from the original
  # ones for the variables in me_data
  original_diag <- diag(as.matrix(me_covariance(mtcars)[-1]))
  replaced_diag <- diag(as.matrix(res$covv[-1]))
  are_different <- original_diag != replaced_diag

  expect_true(
    all(names(mtcars)[which(are_different)] == character())
  )

})

test_that("medesign replaces diag for vars in me_data despite model_syntax", {
  # These three variables share a common method
  me_syntax <- "~~ .; ~ mpg + cyl"
  # Fake data for the example
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "drat"),
                        reliability = c(0.729, 0.815, 0.68),
                        validity = c(0.951, 0.944, 0.79),
                        quality = c(0.693, 0.77, 0.89)
                        )

  res <- medesign(me_syntax, mtcars, me_data)

  # Make sure that the diag is replaced for the three observed variables
  # in me_data even if the model_syntax only declares two
  replaced_diag <- diag(as.matrix(res$corr[-1]))
  expect_true(
    all(names(mtcars)[which(replaced_diag != 1)] == c("mpg", "cyl", "drat"))
  )

  # Compare that the replaced diagonals are different from the original
  # ones for the variables in me_data
  original_diag <- diag(as.matrix(me_covariance(mtcars)[-1]))
  replaced_diag <- diag(as.matrix(res$covv[-1]))
  are_different <- original_diag != replaced_diag

  expect_true(
    all(names(mtcars)[which(are_different)] == c("mpg", "cyl", "drat"))
  )
})

test_that("medesign creates sscore and keeps vars from me_data", {

  model_syntax <- "~~ .; sscore_one = ppltrst + trstprl; ~ stfeco + stflife"

  # Fake data for the example
  me_data <- quality

  set.seed(23141)
  fake_data <- as.data.frame(
    stats::setNames(
             lapply(1:4, function(x) rnorm(100)),
             c("ppltrst", "trstprl", "stfeco", "stflife")
           )
  )

  res <- medesign(model_syntax, fake_data, me_data)
  mod <- res$parsed_model
  sscore_names <- mod[mod$op == "=", 1]

  # sscore columns is created
  expect_true(
    sscore_names %in% names(res$.data)
  )

  # sscore columns is not empty
  expect_true(
    all(!is.na(res$.data[sscore_names]))
  )

  # sscore column is in corr/cov
  expect_true(all(sscore_names %in% names(res$corr)))
  expect_true(all(sscore_names %in% res$corr[["rowname"]]))
  expect_true(all(sscore_names %in% names(res$covv)))
  expect_true(all(sscore_names %in% res$covv[["rowname"]]))

  # create sscore exclude variables from me_data
  expect_false(
    all(c("ppltrst", "trstprl") %in% res$me_data$question)
  )
})


test_that("medesign creates two sscore and removes vars from me_data", {
  model_syntax <- "~~ .; sscore_one = ppltrst + trstprl; sscore_two = psppipl + psppsgv; ~ stfeco + stflife" #nolintr

  # Fake data for the example
  me_data <- quality

  set.seed(23141)
  fake_data <- as.data.frame(
    stats::setNames(lapply(1:6, function(x) rnorm(100)),
                    c("ppltrst", "trstprl", "psppipl", "psppsgv", "stfeco", "stflife"))
  )

  res <- medesign(model_syntax, fake_data, me_data)
  mod <- res$parsed_model
  sscore_names <- mod[mod$op == "=", 1]

  # sscore columns is created
  expect_true(
    all(sscore_names %in% names(res$.data))
  )

  # sscore columns is not empty
  expect_true(
    all(!is.na(res$.data[sscore_names]))
  )

  # sscore column is in corr/cov
  expect_true(all(sscore_names %in% names(res$corr)))
  expect_true(all(sscore_names %in% res$corr[["rowname"]]))
  expect_true(all(sscore_names %in% names(res$covv)))
  expect_true(all(sscore_names %in% res$covv[["rowname"]]))

  # create sscore exclude variables from me_data
  expect_false(
    all(c("ppltrst", "trstprl", "psppipl", "psppsgv") %in% res$me_data$question)
  )

})


test_that("medesign only checks for NA in me_data for used variables", {
  # These three variables share a common method
  me_syntax <- "~~ mpg + cyl; ~ mpg + cyl"
  # Fake data for the example
  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "drat"),
                        reliability = c(0.729, 0.815, NA),
                        validity = c(0.951, 0.944, NA),
                        quality = c(0.693, 0.77, 0.89)
                        )

  # Expect NO error
  # https://stackoverflow.com/questions/10826365/how-to-test-that-an-error-does-not-occur #nolintr
  expect_error(
    res <- medesign(me_syntax, mtcars, me_data),
    NA
  )

  me_data <- data.frame(stringsAsFactors = FALSE,
                        question = c("mpg", "cyl", "drat"),
                        reliability = c(0.729, NA, NA),
                        validity = c(0.951, 0.944, NA),
                        quality = c(0.693, 0.77, 0.89)
                        )

  # This should throw an error because there is an NA in
  # validity/reliability for a var used in the me_syntax
  expect_error(medesign(me_syntax, mtcars, me_data))
})

test_that("medesign checks for wrong format of me_data", {
  num_vars <- paste0(me_env$me_columns, collapse = ", ")
  all_vars <- paste0(c(me_env$me_question, me_env$me_columns), collapse = ", ")

  names(mtcars)[1:3] <- c("V1", "V2", "V3")

  me_df <-
    tibble::tibble(question = paste0("V", 1:3),
                   not_indf = c(0.2, 0.3, 0.5),
                   reliability = c(NA, 0.4, 0.5),
                   validity = c(NA, NA, 0.6))

  me_syntax <- "~~ .; ~ V1 + V2"
  expect_error(
    medesign(me_syntax, mtcars, me_df),
    paste0("Columns ",  all_vars, " must be available in `me_data`")
  )

  me_df <-
    tibble::tibble(question = paste0("V", 1:3),
                   not_indf = c(0.2, 0.3, 0.5),
                   reliability = c(NA, 0.4, 0.5),
                   validity = c(NA, NA, 0.6))

  expect_error(
    medesign(me_syntax, mtcars, me_df),
    paste0("Columns ",  all_vars, " must be available in `me_data`")
  )

  me_df <-
    tibble::tibble(question = c("V1", "V2", "V3"),
                   quality = c(0.2, 0.3, 0.5),
                   reliability = c(NA, 0.4, 0.5),
                   validity = c(NA, NA, 1.2))

  expect_error(
    medesign(me_syntax, mtcars, me_df),
    paste0(num_vars,
           " must be numeric columns with values between/including 0 and 1 in `me_data`" #nolintr
           )
  )

  me_df <-
    tibble::tibble(question = 1:3,
                   quality = c(0.2, 0.3, 0.5),
                   reliability = c(0.5, 0.4, 0.5),
                   validity = c(0.1, 0.2, 0.9))

  expect_error(
    medesign(me_syntax, mtcars, me_df),
    paste0("Column ",
           me_env$me_question,
           " must be a character vector containing the question names"
           )
  )

})

test_that("medesign calculates quality and method effect correctly for sumscore", {

  me_df <-
    data.frame(
      question = c("trstprl", "trstplt", "trstprt"),
      reliability = c(0.812, 0.852, 0.858),
      validity = c(0.959, 0.965, 0.956),
      quality = c(0.779, 0.822, 0.821)
    )

  data(ess7es)
  .data <- ess7es[c("trstprl", "trstplt", "trstprt")]
  me_data <- me_df
  model_syntax <- "~~ .; std(s1) = trstprl + trstplt + trstprt"
  medesign_tst <- medesign(model_syntax, .data, me_data)
  sscore_subset <- medesign_tst$me_data$question == "s1"
  res <- medesign_tst$me_data[sscore_subset, c("quality", "method_eff")]

  expect_equal(
    round(as.data.frame(res), 4),
    data.frame(quality = 0.8936, method_eff = 0.0422),
    tol = 0.000001
  )

})

test_that("medesign replaces quality in diagonal of cor and cov correctly", {

  me_data <-
    data.frame(
      question = c("trstprl", "trstplt", "trstprt"),
      reliability = c(0.812, 0.852, 0.858),
      validity = c(0.959, 0.965, 0.956),
      quality = c(0.779, 0.822, 0.821)
    )

  data(ess7es)
  .data <- ess7es[c("trstprl", "trstplt", "trstprt")]
  model_syntax <- "~~ .; std(s1) = trstprl + trstplt + trstprt"

  medesign_tst <-
    medesign(
      model_syntax,
      .data,
      me_data,
      drop_sscore_vars = FALSE
    )

  expect_equal(
    diag(as.matrix(medesign_tst$corr[-1])),
    medesign_tst$me_data$quality
  )

  expect_equal(
    diag(as.matrix(medesign_tst$covv[-1])),
    c(5.230898, 4.109667, 3.854053, 6.399893),
    tol = 0.00001
  )

})
