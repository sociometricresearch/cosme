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

  # Political trust example
  me_df <-
    structure(list(question = c("trstplt", "trstprl", "trstprt"), 
                   reliability = c(0.852, 0.812, 0.858), validity = c(0.965, 
                                                                      0.959, 0.956), quality = c(0.822, 0.779, 0.821)), row.names = c(NA, 
                                                                                                                                      -3L), class = "data.frame")

  result <- round(estimate_cmv(me_df), 5)
  correct_res <- c(trstplt_trstprl = 0.03151,
                   trstplt_trstprt = 0.03355,
                   trstprl_trstprt = 0.03545)
  expect_is(result, "numeric")
  expect_length(result, nrow(me_df))
  expect_identical(result, correct_res)


  # State services example
  me_df <- structure(list(question = c("stfedu", "stfhlth"), reliability = c(0.757, 
                                                                             0.76), validity = c(0.838, 0.798), quality = c(0.635, 0.607)), row.names = c(NA, 
                                                                                                                                                          -2L), class = "data.frame")
  
  result <- round(estimate_cmv(me_df), 5)
  correct_res <- c(stfedu_stfhlth = 0.13721)
  expect_is(result, "numeric")
  expect_length(result, nrow(me_df) - 1)
  expect_identical(result, correct_res)
})
