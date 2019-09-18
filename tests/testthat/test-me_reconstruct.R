context("test-me_reconstruct.R")


test_that("me_reconstruct checks data format", {
  variables <- paste0(me_env$me_columns, collapse = ", ")

  me_df <-
    tibble(question = paste0("V", 1:3),
           not_indf = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 0.6))

  expect_error(
    me_reconstruct(me_df),
    paste0("Columns ",  variables," must be available in `me_data`")
  )

  me_df <-
    tibble(question = 1:3,
           quality = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 1.2))

  expect_error(
    me_reconstruct(me_df),
    paste0(variables,
           " must be numeric columns with values between/including 0 and 1 in `me_data`"
           )
  )
})

test_that("me_reconstruct assigns me class if everything is fine", {
  me_df <-
    tibble(question = paste0("V", 1:3),
           quality = c(0.2, 0.3, 0.5),
           reliability = c(NA, 0.4, 0.5),
           validity = c(NA, NA, 0.6))

  expect_s3_class(me_reconstruct(me_df), "me")

})

