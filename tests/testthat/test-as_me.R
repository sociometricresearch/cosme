context("test-as_me.R")


test_that("as_me checks data format", {
  num_vars <- paste0(me_env$me_columns, collapse = ", ")
  all_vars <- paste0(c(me_env$me_question, me_env$me_columns), collapse = ", ")

  me_df <-
    tibble(question = paste0("V", 1:3),
           not_indf = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 0.6))

  expect_error(
    as_me(me_df),
    paste0("Columns ",  all_vars, " must be available in `me_data`")
  )

  me_df <-
    tibble(question = c("X1", "X2", "X3"),
           quality = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 1.2))

  expect_error(
    as_me(me_df),
    paste0(num_vars,
           " must be numeric columns with values between/including 0 and 1 in `me_data`" #nolintr
           )
  )

  me_df <-
    tibble(question = 1:3,
           quality = c(0.2, 0.3, 0.5),
           reliability = c(0.5, 0.4, 0.5),
           validity = c(0.1, 0.2, 0.9))

  expect_error(
    as_me(me_df),
    paste0("Column ",
           me_env$me_question,
           " must be a character vector containing the question names"
           )
  )
  
})

test_that("as_me assigns me class if everything is fine", {
  me_df <-
    tibble(question = paste0("V", 1:3),
           quality = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 0.6))

  expect_s3_class(as_me(me_df), "me")

})
