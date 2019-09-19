context("test-me_sscore.R")

set.seed(231321)
library(tibble)
me_df <-
  tibble(question = paste0("V", 1:5),
         quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
         reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
         validity = c(0.2, 0.6, 0.6, 0.7, 0.8))


me_df <- structure(me_df, class = c(class(me_df), "me"))

sample_data <-
  as_tibble(
    stats::setNames(
      replicate(5, c(rbinom(1000, 5, 0.6), NA), simplify = FALSE),
      paste0("V", 1:5)),
    .name_repair = "minimal"
  )

test_that("me_sscore returns correct output", {
  result <-
    me_sscore(
      me_data = me_df,
      data = sample_data,
      new_name = new_sumscore,
      V3, V4
    )

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "me")

  expect_equal(nrow(result), 4)
  expect_true(ncol(result) >= 3)
  expect_is(result[[1]], "character")
})

test_that("me_sscore uses only unique variable names", {
  result <-
    me_sscore(
      me_data = me_df,
      data = sample_data,
      new_name = new_sumscore,
      V3, V4, V4
    )

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "me")

  expect_equal(nrow(result), 4)
  expect_true(ncol(result) >= 3)
  expect_is(result[[1]], "character")
})

test_that("me_sscore keeps variables that compose the sum score", {
  result <-
    me_sscore(
      me_data = me_df,
      data = sample_data,
      new_name = new_sumscore,
      V3, V4, V4,
      drop = FALSE
    )

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "me")

  expect_equal(nrow(result), nrow(me_df) + 1)
})


me_df <-
  tibble(question = paste0("V", 1:5),
         quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
         reliability = c(0.1, 0.4, 0.5, 0.5, 0.7),
         validity = c(0.4, 0.7, 0.6, 0.7, 0.8),
         random_var = NA_real_)


me_df <- structure(me_df, class = c(class(me_df), "me"))

sample_data <-
  as_tibble(
    stats::setNames(
      replicate(6, c(rbinom(1000, 5, 0.6), NA), simplify = FALSE),
      paste0("V", 1:6)),
    .name_repair = "minimal"
  )

test_that("me_sscore checks for arguments", {

  expect_error(
    me_sscore(
      me_data = me_df,
      data = sample_data,
      new_name = new_sumscore
    ),
    "`data` must have at least two columns"
  )

  tmp <- sample_data
  tmp$V5 <- as.character(tmp$V5)

  expect_error(
    me_sscore(
      me_data = me_df,
      data = tmp,
      new_name = new_sumscore,
      V1, V5),
    "V1, V5 must be numeric variables in `data`"
  )

  ## NOTEEEE##
  ## Check that all me_data are non-NA in all variables selected
})

test_that("me_sscore checks variables are in both dfs", {
  expect_error(
    me_sscore(
      me_data = me_df,
      data = sample_data,
      new_name = new_sumscore,
      V1, random_var),
    "One or more variables are not present in `data`: random_var"
  )

  expect_error(
    me_sscore(
      me_data = me_df,
      data = sample_data,
      new_name = new_sumscore,
      V1, V6),
    "One or more variables are not present in `me_data`: V6"
  )
})

test_that("me_sscore adds me class to valid me_data", {
  tmp <- me_df
  class(tmp) <- c("tbl_df", "tbl", "data.frame")

  noclass <- me_sscore(
    me_data = tmp,
    data = sample_data,
    new_name = new_sumscore,
    V1, V2
  )

  valid_class <- me_sscore(
    me_data = me_df,
    data = sample_data,
    new_name = new_sumscore,
    V1, V2
  )
  expect_identical(valid_class, noclass)
})

test_that("me_sscore checks that there's non_NA's in important arguments", {
  me_df <-
    tibble(question = paste0("V", 1:5),
           quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
           reliability = c(0.2, 0.4, 0.5, 0.5, 0.7),
           validity = c(0.4, NA, 0.6, 0.7, 0.8),
           random_var = NA_real_)


  me_df <- structure(me_df, class = c(class(me_df), "me"))

  expect_error(
    me_sscore(
    me_df,
    sample_data,
    new_name = new_sumscore,
    V1, V2
  ),
  paste0("`me_data` must have non-missing values at variable/s: ",
         paste0(me_env$me_columns, collapse = ", "))
  )

  me_df$validity[is.na(me_df$validity)] <- 0.5


  expect_error(
    me_sscore(
      me_df,
      sample_data,
      new_name = new_sumscore,
      V1, V2,
      wt = NA,
    "`weights` must be a non-NA numeric vector with the same length as the number of variables")
  )

  expect_error(
    me_sscore(
      me_df,
      sample_data,
      new_name = new_sumscore,
      V1, V2,
      wt = 1,
      "`weights` must be a non-NA numeric vector with the same length as the number of variables")
  )

  expect_error(
    me_sscore(
      me_df,
      sample_data,
      new_name = new_sumscore,
      V1, V2,
      wt = c(1, 2),
      "`weights` must be a non-NA numeric vector with the same length as the number of variables")
  )
})

ess_email <- Sys.getenv("ess_email")

test_that("me_sscore returns the exact result to decimal points", {
  selected_vars <- c("trstprl", "trstplt", "trstprt")

  the_data  <-
    essurvey::recode_missings(
      essurvey::import_country("Spain", 7, ess_email)[selected_vars]
    )

  all_qs <- c("ppltrst", "polintr", "psppsgv",
              "psppipl", "ptcpplt", "stflife", "stfeco", "stfedu", "stfhlth",
              "trstprl", "trstplt", "trstprt")

  # # Quality estimates
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

  score <- estimate_sscore(quality[quality$question %in% selected_vars, ], the_data, wt = NULL)
  expect_equal(score, 0.899183, tolerance = 0.01)
})
