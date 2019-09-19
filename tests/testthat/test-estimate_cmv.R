context("test-estimate_cmv")

test_that("estimate_cmv throws back NA error in reliability and validity", {
  me_df <-
    tibble::tibble(question = paste0("V", 1:5),
                   quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
                   reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
                   validity = c(NA, 0.1, 0.6, 0.7, 0.8))

  expect_error(estimate_cmv(me_df),
               "`me_data` must have non-missing values at columns reliability and validity for all variables")


  me_df <-
    tibble::tibble(question = paste0("V", 1:5),
                   quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
                   reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
                   validity = rep(NA, 5))

  expect_error(estimate_cmv(me_df),
               "`me_data` must have non-missing values at columns reliability and validity for all variables")

  me_df <-
    tibble::tibble(question = paste0("V", 1:5),
                   quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
                   reliability = rep(NA, 5),
                   validity = rep(NA, 5))

  expect_error(estimate_cmv(me_df),
               "`me_data` must have non-missing values at columns reliability and validity for all variables")

  me_df <-
    tibble::tibble(question = paste0("V", 1:5),
                   quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
                   reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
                   random_name = c(NA, 0.1, 0.6, 0.7, 0.8))

  expect_error(estimate_cmv(me_df),
               "Columns question, reliability, validity must be available in `me_data`")
})

test_that("estimate_cmv returns correct output", {
  me_df <-
    tibble::tibble(question = paste0("V", 1:5),
                   quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
                   reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
                   validity = c(0.5, 0.1, 0.6, 0.7, 0.8))

  result <- estimate_cmv(me_df)
  expect_is(result, "numeric")
  expect_length(result, 1)
})
