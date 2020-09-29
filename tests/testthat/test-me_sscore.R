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
      .data = sample_data,
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
      .data = sample_data,
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
      .data = sample_data,
      new_name = new_sumscore,
      V3, V4, V4,
      .drop = FALSE
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
      .data = sample_data,
      new_name = new_sumscore
    ),
    "`.data` must have at least two columns"
  )

  tmp <- sample_data
  tmp$V5 <- as.character(tmp$V5)

  expect_error(
    me_sscore(
      me_data = me_df,
      .data = tmp,
      new_name = new_sumscore,
      V1, V5),
    "V1, V5 must be numeric variables in `.data`"
  )
})

test_that("me_sscore checks variables are in both dfs", {
  expect_error(
    me_sscore(
      me_data = me_df,
      .data = sample_data,
      new_name = new_sumscore,
      V1, random_var),
    "One or more variables are not present in `.data`: random_var"
  )

  expect_error(
    me_sscore(
      me_data = me_df,
      .data = sample_data,
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
    .data = sample_data,
    new_name = new_sumscore,
    V1, V2
  )

  valid_class <- me_sscore(
    me_data = me_df,
    .data = sample_data,
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
    "`weights` must be a non-NA numeric vector with the same length as the number of variables") #nolintr
  )

  expect_error(
    me_sscore(
      me_df,
      sample_data,
      new_name = new_sumscore,
      V1, V2,
      wt = 1,
      "`weights` must be a non-NA numeric vector with the same length as the number of variables") #nolintr
  )

  expect_error(
    me_sscore(
      me_df,
      sample_data,
      new_name = new_sumscore,
      V1, V2,
      wt = c(1, 2),
      "`weights` must be a non-NA numeric vector with the same length as the number of variables") #nolintr
  )
})

test_that("me_sscore returns the correct quality and method effect of a sscore", {
  # Political trust example
  data(ess7es)
  selected_vars <- c("trstprl", "trstplt", "trstprt")
  .data <- ess7es

  # Data from sqp
  me_data <-
    data.frame(
      question = c("trstprl", "trstplt", "trstprt"),
      reliability = c(0.812, 0.852, 0.858),
      validity = c(0.959, 0.965, 0.956),
      quality = c(0.779, 0.822, 0.821)
    )

   score <- me_sscore(me_data[me_data$question %in% selected_vars, ],
                      .data,
                      new_name = "s1",
                      trstprl, trstplt, trstprt,
                      wt = NULL)

  expect_equal(score$quality, 0.8936, tolerance = 0.0001)
  expect_equal(score$method_eff, 0.04215, tolerance = 0.0001)

  # State services example
  me_data <-
    data.frame(
      question = c("stfedu", "stfhlth"),
      reliability = c(0.757, 0.760),
      validity = c(0.838, 0.798),
      quality = c(0.635, 0.607)
    )

  selected_vars <- c("stfedu", "stfhlth")
  score <- me_sscore(me_data,
                     .data,
                     new_name = "s1",
                     stfedu, stfhlth,
                     wt = NULL)

  expect_equal(score$quality, 0.6841, tolerance = 0.0001)
  expect_equal(score$method_eff, 0.17079, tolerance = 0.0001)
})


test_that("estimate_sscore returns the correct quality and method effect of a sscore", {

  # Political trust example
  data(ess7es)
  selected_vars <- c("trstprl", "trstplt", "trstprt")
  .data <- ess7es
  all_qs <- c("ppltrst", "polintr", "psppsgv",
              "psppipl", "ptcpplt", "stflife",
              "stfeco", "stfedu", "stfhlth",
              "trstprl", "trstplt", "trstprt")

   quality <-
     data.frame(
       question = c("trstprl", "trstplt", "trstprt"),
       reliability = c(0.9011, 0.9230, 0.9262),
       validity = c(0.979285453787607, 0.982344135219425, 0.977752524926425),
       quality = c(0.779, 0.822, 0.821),
       quality_coef = c(0.8826, 0.9066, 0.9060),
       method_eff = c(0.1824, 0.1726, 0.1943)
     )

  .data <- scale_add_sscore(.data, "s1", paste0(selected_vars, collapse = "+"))
  score <- estimate_sscore(quality[quality$question %in% selected_vars, ],
                           .data,
                           new_name = "s1",
                           wt = NULL)

  expect_equal(score$quality, 0.8936, tolerance = 0.0001)
  expect_equal(score$method_eff, 0.04215, tolerance = 0.0001)

  # State services example
  quality <-
    data.frame(
      question = c("stfedu", "stfhlth"),
      reliability = c(0.870057469366248, 0.871779788708135),
      validity = c(0.915423399307664, 0.893308457365092),
      quality = c(0.796868872525461, 0.779102047231298),
      quality_coef = c(0.7968, 0.7791),
      method_eff = c(0.3501914, 0.3918163)
    )

  selected_vars <- c("stfedu", "stfhlth")
  .data <- scale_add_sscore(.data, "s1", paste0(selected_vars, collapse = "+"))
  score <- estimate_sscore(quality,
                           .data,
                           new_name = "s1",
                           wt = NULL)

  expect_equal(score$quality, 0.6841, tolerance = 0.0001)
  expect_equal(score$method_eff, 0.17079, tolerance = 0.0001)
})
