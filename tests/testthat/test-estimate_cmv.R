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
    data.frame(
      question = c("trstprl", "trstplt", "trstprt"),
      reliability = c(0.901110426085505, 0.923038460737146, 0.926282894152753),
      validity = c(0.979285453787607, 0.982344135219425, 0.977752524926425),
      quality = c(0.882609766544649, 0.906642156531451, 0.906090503205943),
      method_eff = c(0.1824610, 0.1726847, 0.1942987)
    )

  result <- round(estimate_cmv(me_df), 5)
  correct_res <- c(trstprl_trstplt = 0.03151,
                   trstprl_trstprt = 0.03545,
                   trstplt_trstprt = 0.03355)

  expect_is(result, "numeric")
  expect_length(result, nrow(me_df))
  expect_identical(result, correct_res)

  # State services example
  me_df <-
    data.frame(
      question = c("stfedu", "stfhlth"),
      reliability = c(0.870057469366248, 0.871779788708135),
      validity = c(0.915423399307664, 0.893308457365092),
      quality = c(0.796868872525461, 0.779102047231298),
      method_eff = c(0.3501914, 0.3918163)
    )

  result <- round(estimate_cmv(me_df), 5)
  correct_res <- c(stfedu_stfhlth = 0.13721)
  expect_is(result, "numeric")
  expect_length(result, nrow(me_df) - 1)
  expect_identical(result, correct_res)
})
