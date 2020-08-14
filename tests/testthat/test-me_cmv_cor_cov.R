context("test-me_cmv_cor.R")

set.seed(2131)
suppressWarnings(library(tibble))

original_df <- as.data.frame(matrix(rnorm(100, sd = 50), nrow = 20))
corr_tibble <- me_correlate(original_df, rnorm(5))

# test missing:
# When y is not from me_collect(), me_cmv_cor must throw an error
# Show that when y is not from me class, there's an error

me_df <-
  tibble(question = paste0("V", 1:5),
         quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
         reliability = c(1, 0.4, 0.5, 0.5, 0.7),
         validity = c(1, 1, 0.6, 0.7, 0.8)
         )

me_df <- as_me(me_df)

correct_format <- function(p) {
  expect_is(p, "data.frame")

  expect_equal(names(p)[[1]], "rowname")

  # First column is the row names
  expect_is(p[[1]], "character")

  # All other columns are numeric
  expect_true(all(vapply(p[-1], is.numeric, FUN.VALUE = logical(1))))

  # All row names have a at least one letter, which means
  # that the row names were not extracted raw if `x`
  # was a matrix
  expect_true(all(grepl("[[:alpha:]]{1,}", p[[1]])))

  # df is symmetric when excluding the rowname variables
  expect_equal(nrow(p), ncol(p) - 1)
  invisible(TRUE)
}

test_cor_cov <- function(fun, fun_str) {

  test_that(paste0(fun_str, " returns correct output"), {
    model <- "~ V4 + V5"
    m_des <- medesign(model, original_df, me_df)

    cmv_tib <- fun(m_des)

    correct_format(cmv_tib)
  })

  test_that(paste0(fun_str, "`x` argument works fine with data frame"), {
    random_vec <- rnorm(10, sd = 50)

    df_no_rows <- as.data.frame(matrix(random_vec, 5, 5))

    model <- "~ V4 + V5"
    m_des <- medesign(model, df_no_rows, me_df)
    cmv_tib <- fun(m_des)

    correct_format(cmv_tib)

    rownames(df_no_rows) <- paste0("V", seq_len(5))
    df_rows <- df_no_rows

    # df row names
    m_des <- medesign(model, df_rows, me_df)
    cmv_matr <- fun(m_des)
    correct_format(cmv_matr)
  })

  ## TODO: when you add the CMV argument again to cmv_cor and cov
  ## uncomment this test
  ## test_that(paste0(fun_str, " works with cmv argument"), {
  ##   me_df <-
  ##     tibble(question = paste0("V", 1:5),
  ##            quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
  ##            reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
  ##            validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

  ##   # For one CMV
  ##   filtered_df <- subset(me_df, question %in% c("V4", "V5"))
  ##   model <- "~ V4 + V5"
  ##   m_des <- medesign(model, original_df, me_df)
  ##   cmv_aut <- fun(m_des)
  ##   cmv_manual <- fun(m_des, cmv = estimate_cmv(filtered_df))
  ##   expect_equal(cmv_aut, cmv_manual)

  ##   # For multiple CMV
  ##   filtered_df <- subset(me_df, question %in% c("V2", "V3", "V4", "V5"))
  ##   model <- "~ V2 + V3;~ V4 + V5"
  ##   m_des <- medesign(model, original_df, me_df)
  ##   cmv_aut <- fun(m_des)

  ##   list_df <- list(filtered_df[1:2, ], filtered_df[3:4, ])
  ##   cmv_manual <- fun(m_des, cmv = vapply(list_df,
  ##                                         estimate_cmv,
  ##                                         FUN.VALUE = numeric(1)))
  ##   expect_equal(cmv_aut, cmv_manual)
  ## })

  test_that(paste0(fun_str, " gives same result when variables in .data are shuffled"), { #nolintr
    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    model <- "~ V3 + V4 + V5"
    shuffled_des <- medesign(model, original_df[c(2, 5, 4, 3, 1)], me_df)
    original_des <- medesign(model, original_df, me_df)

    cmv_shuffled <- fun(shuffled_des)
    cmv_original <- fun(original_des)

    reordered <- cmv_shuffled[order(cmv_shuffled$rowname), names(cmv_original)]

    expect_identical(cmv_original,
                     reordered)
  })

  test_that(paste0(fun_str, " gives same result when variables in medata are shuffled"), { #nolintr
    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    model <- "~ V3 + V4 + V5"
    shuffled_des <- medesign(model, original_df, me_df[c(2, 5, 4, 3, 1), ])
    original_des <- medesign(model, original_df, me_df)
    
    cmv_shuffled <- fun(shuffled_des)
    cmv_original <- fun(original_des)

    reordered <- cmv_shuffled[order(cmv_shuffled$rowname), names(cmv_original)]

    expect_identical(cmv_original,
                     reordered)
  })


  test_that(paste0(fun_str, " gives same result when variables in .data and medata are shuffled"), { #nolintr
    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    model <- "~ V3 + V4 + V5"
    shuffled_des <- medesign(model,
                             original_df[, c(2, 5, 4, 3, 1)],
                             me_df[c(3, 1, 5, 2, 4), ]
                             )

    original_des <- medesign(model, original_df, me_df)
    
    cmv_shuffled <- fun(shuffled_des)
    cmv_original <- fun(original_des)

    reordered <- cmv_shuffled[order(cmv_shuffled$rowname), names(cmv_original)]

    expect_identical(cmv_original,
                     reordered)
  })
  
  test_that(paste0(fun_str, " uses only unique variable names"), {
    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    model <- "~ V3 + V4 + V5"
    original_des <- medesign(model, original_df, me_df)

    cmv_tib <- fun(original_des)
    expect_is(cmv_tib, "data.frame")

    # First column is the row names
    expect_is(cmv_tib[[1]], "character")

    # All other columns are numeric
    expect_true(all(vapply(cmv_tib[-1], is.numeric, FUN.VALUE = logical(1))))

    # df is symmetric when excluding the rowname variables
    expect_equal(nrow(cmv_tib), ncol(cmv_tib) - 1)
  })

  test_that(paste0(fun_str, " replaces upper and lower triangle"), {
    up_equal <- function(x) {
      tp <- x[-1]
      all(all.equal(sort(tp[lower.tri(tp)]), sort(tp[upper.tri(tp)])))
    }

    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    model <- "~ V4 + V5"
    original_des <- medesign(model, original_df, me_df)

    cmv_tib <- as.data.frame(fun(original_des))

    # Two variables
    expect_true(up_equal(cmv_tib))

    # Three variables
    model <- "~ V3 + V4 + V5"
    original_des <- medesign(model, original_df, me_df)
    cmv_tib <- as.data.frame(fun(original_des))
    # Bc turning from tibble to df looses precision, I just
    # round to 4, any small decimals discrepancies I don't
    # mind
    cmv_tib[2:6] <- round(cmv_tib[2:6], 4)
    expect_true(up_equal(cmv_tib))
  })
}

test_cor_cov(me_cmv_cor, "me_cmv_cor")
## TODO: fix this once you've implemented the covariance CMV correction
## correctly
## test_cor_cov(me_cmv_cov, "me_cmv_cov")

test_input_errors <- function(fun, fun_str) {
  test_that(paste0(fun_str, " throws specific errors"), {
    expect_error(fun(list()),
                 regexp = "`.medesign` should be a measurement error design object given by `medesign`", #nolintr
                 fixed = TRUE
                 )
  })
}

test_input_errors(me_cmv_cor, "me_cmv_cor")
test_input_errors(me_cmv_cov, "me_cmv_cov")

test_that("me_cmv_cor corrects for CMV between simple concepts correctly - political trust", { #nolintr

  data(ess7es)
  .data <- ess7es[c("trstprl", "trstplt", "trstprt", "agea")]

  # Define model and me_data
  model_syntax <- "~ trstprl + trstplt + trstprt;"
  me_data <-
    data.frame(
      question = c("trstprl", "trstplt", "trstprt"),
      reliability = c(0.812, 0.852, 0.858),
      validity = c(0.959, 0.965, 0.956),
      quality = c(0.779, 0.822, 0.821)
    )

  .medesign <- medesign(model_syntax, .data, me_data)
  # This is the corrected CMV correlation with quality and CMV
  tmp_corrected_cor <- me_cmv_cor(.medesign)

  # Convert back to pre cov2cor
  cor_variances <- c(0.8826098, 0.9066422, 0.9060905, 1)
  d <- sqrt(cor_variances)

  opposite_cov2cor <-
    as.data.frame(
      outer(d, d) * as.matrix(tmp_corrected_cor[-1])
    )

  opposite_cov2cor$rowname <- tmp_corrected_cor$rowname
  order_cols <- c("rowname", setdiff(names(opposite_cov2cor), "rowname"))
  opposite_cov2cor <- opposite_cov2cor[order_cols]

  # Make sure that all variables **not** used in the me correction
  # are the same between the corrected CMV and original cor without correction
  sel_rows <- "agea"
  res1 <-
    as.data.frame(
      opposite_cov2cor[opposite_cov2cor$rowname %in% sel_rows, ],
      row.names = 1L
    )

  res2 <-
    as.data.frame(
      .medesign$corr[.medesign$corr$rowname %in% sel_rows, ]
    )

  expect_equal(res1, res2, tol = 0.000001)

  # Make sure that all variables which are supposed to be corrected for CMV
  # are corrected
  cmv_vars <- c("trstprl", "trstplt", "trstprt")
  row_order <- match(cmv_vars, opposite_cov2cor$rowname)
  res1 <- opposite_cov2cor[row_order, c("rowname", cmv_vars)]
  res1[2:4] <- lapply(res1[2:4], round, 4)

  correct_res <-
    data.frame(
      rowname = c("trstprl", "trstplt", "trstprt"),
      trstprl = c(0.8826, 0.6173, 0.563),
      trstplt = c(0.6173, 0.9066, 0.8086),
      trstprt = c(0.5630, 0.8086, 0.9061)
    )

  # We are checking up to three digits. They all match. Don't worry
  # if they don't match exactly after the three digits.
  # This check is alright with the pre cov2cor results. They
  # match the Excel calculations from Wiebke exactly.
  expect_equal(res1, correct_res, tol = 0.0000001)
})

test_that("me_cmv_cor corrects for CMV between simple concepts correctly - state services", { #nolintr

  data(ess7es)
  .data <- ess7es[c("stfedu", "stfhlth", "agea")]

  me_data <-
    data.frame(
      question = c("stfedu", "stfhlth"),
      reliability = c(0.757, 0.760),
      validity = c(0.838, 0.798),
      quality = c(0.635, 0.607)
    )

  # Define model and me_data
  model_syntax <- "~ stfedu + stfhlth"
  .medesign <- medesign(model_syntax, .data, me_data)

  # This is the corrected CMV correlation with quality and CMV
  tmp_corrected_cor <- me_cmv_cor(.medesign)

  # Convert back to pre cov2cor
  cor_variances <- c(0.7968689, 0.7791020, 1)
  d <- sqrt(cor_variances)

  opposite_cov2cor <-
    as.data.frame(
      outer(d, d) * as.matrix(tmp_corrected_cor[-1])
    )

  opposite_cov2cor$rowname <- tmp_corrected_cor$rowname
  order_cols <- c("rowname", setdiff(names(opposite_cov2cor), "rowname"))
  opposite_cov2cor <- opposite_cov2cor[order_cols]

  # Make sure that all variables **not** used in the me correction
  # are the same between the corrected CMV and original cor without correction
  sel_rows <- "agea"
  res1 <-
    as.data.frame(
      opposite_cov2cor[opposite_cov2cor$rowname %in% sel_rows, ],
      row.names = 1L
    )

  res2 <-
    as.data.frame(
      .medesign$corr[.medesign$corr$rowname %in% sel_rows, ]
    )

  expect_equal(res1, res2, tol = 0.000001)

  # Make sure that all variables which are supposed to be corrected for CMV
  # are corrected
  cmv_vars <- c("stfedu", "stfhlth")
  row_order <- match(cmv_vars, opposite_cov2cor$rowname)
  res1 <- opposite_cov2cor[row_order, c("rowname", cmv_vars)]
  res1[2:3] <- lapply(res1[2:3], round, 4)

  correct_res <-
    data.frame(
      rowname = c("stfedu", "stfhlth"),
      stfedu = c(0.7969, 0.4817),
      stfhlth = c(0.4817, 0.7791)
    )

  # We are checking up to three digits. They all match. Don't worry
  # if they don't match exactly after the three digits.
  # This check is alright with the pre cov2cor results. They
  # match the Excel calculations from Wiebke exactly.
  expect_equal(res1, correct_res, tol = 0.0000001)
})

test_that("me_cmv_cor corrects for CMV between simple and complex concept correctly", { #nolintr

  # ESS data
  data(ess7es)
  vars_ch <- c("trstprl", "trstplt", "trstprt", "stfedu", "stfhlth", "agea")
  .data <- ess7es[vars_ch]

  # Define measurement error relationship
  model_syntax <-
    "std(stfsscore) = stfedu + stfhlth;
     ~ trstprl + trstplt + trstprt + stfsscore"

  # data from sqp
  me_data <-
    data.frame(
      question = c("trstprl", "trstplt", "trstprt", "stfedu", "stfhlth"),
      reliability = c(0.812, 0.852, 0.858, 0.757, 0.76),
      validity = c(0.959, 0.965, 0.956, 0.838, 0.798),
      quality = c(0.779, 0.822, 0.821, 0.635, 0.607)
    )

  # Define the measurement error object
  .medesign <- medesign(model_syntax, .data, me_data)

  # This is the correlation with quality and CMV correction
  # However, this is **after** cov2cor.
  tmp_corrected_cor <- me_cmv_cor(.medesign)
  tmp_corrected_cor

  # Let's convert back to pre cov2cor to match Wiebke's results
  # exactly.
  cor_variances <- c(0.8826098, 0.9066422, 0.9060905,
                     0.7968689, 0.7791020, 1,
                     0.6841067)
  d <- sqrt(cor_variances)

  opposite_cov2cor <-
    as.data.frame(
      outer(d, d) * as.matrix(tmp_corrected_cor[-1])
    )

  opposite_cov2cor$rowname <- tmp_corrected_cor$rowname
  order_cols <- c("rowname", setdiff(names(opposite_cov2cor), "rowname"))
  opposite_cov2cor <- opposite_cov2cor[order_cols]

  # Make sure that all variables **not** used in the me correction
  # are the same between the corrected CMV and original cor without correction
  sel_rows <- "agea"
  sel_cols <- c("rowname", "stfedu", "stfhlth", "agea")
  res1 <-
    as.data.frame(
      opposite_cov2cor[opposite_cov2cor$rowname %in% sel_rows, sel_cols],
      row.names = 1L
    )

  res2 <-
    as.data.frame(
      .medesign$corr[.medesign$corr$rowname %in% sel_rows, sel_cols]
    )

  expect_equal(res1, res2, tol = 0.000001)

  # Make sure that all variables which are supposed to be corrected for CMV are
  # corrected
  cmv_vars <- c("trstprl", "trstplt", "trstprt", "stfsscore")
  row_order <- match(cmv_vars, opposite_cov2cor$rowname)
  res1 <- opposite_cov2cor[row_order, c("rowname", cmv_vars)]
  res1[2:4] <- lapply(res1[2:4], round, 4)
  row.names(res1) <- NULL

  correct_res <-
    data.frame(
      rowname = c("trstprl", "trstplt", "trstprt", "stfsscore"),
      trstprl = c(0.8826, 0.6197, 0.5628, 0.3484),
      trstplt = c(0.6197, 0.9066, 0.8055, 0.3716),
      trstprt = c(0.5628, 0.8055, 0.9061, 0.3072),
      stfsscore = c(0.348422757353293, 0.371591861806547, 
                    0.307183448085645, 0.6841067)
    )

  # We are checking up to three digits. They all match. Don't worry
  # if they don't match exactly after the three digits.
  # This check is alright with the pre cov2cor results. They
  # match the Excel calculations from Wiebke exactly.
  expect_equal(res1, correct_res, tol = 0.0000001)
})

test_that("me_cmv_cor corrects for CMV between complex and complex concept correctly", { #nolintr

  # ESS data
  data(ess7es)
  vars_ch <- c("trstprl", "trstplt", "trstprt", "stfedu", "stfhlth", "agea")
  .data <- ess7es[vars_ch]

  # Define measurement error relationship
  model_syntax <-
    "std(stfsscore) = stfedu + stfhlth;
     std(trstsscore) = trstprl + trstplt + trstprt
     ~ stfsscore + trstsscore"

  # data from sqp
  me_data <-
    data.frame(
      question = c("trstprl", "trstplt", "trstprt", "stfedu", "stfhlth"),
      reliability = c(0.812, 0.852, 0.858, 0.757, 0.76),
      validity = c(0.959, 0.965, 0.956, 0.838, 0.798),
      quality = c(0.779, 0.822, 0.821, 0.635, 0.607)
    )

  # Define the measurement error object
  .medesign <- medesign(model_syntax, .data, me_data)

  # This is the correlation with quality and CMV correction
  # However, this is **after** cov2cor.
  tmp_corrected_cor <- me_cmv_cor(.medesign)
  tmp_corrected_cor

  # Let's convert back to pre cov2cor to match Wiebke's results
  # exactly.
  cor_variances <- c(0.8826098, 0.9066422, 0.9060905, 0.7968689, 0.7791020, 1, 0.6841067, 0.8936418)
  d <- sqrt(cor_variances)

  opposite_cov2cor <-
    as.data.frame(
      outer(d, d) * as.matrix(tmp_corrected_cor[-1])
    )

  opposite_cov2cor$rowname <- tmp_corrected_cor$rowname
  order_cols <- c("rowname", setdiff(names(opposite_cov2cor), "rowname"))
  opposite_cov2cor <- opposite_cov2cor[order_cols]

  # Make sure that all variables **not** used in the me correction
  # are the same between the corrected CMV and original cor without correction
  sel_rows <- "agea"
  sel_cols <- c("rowname", "stfedu", "stfhlth", "agea")
  res1 <-
    as.data.frame(
      opposite_cov2cor[opposite_cov2cor$rowname %in% sel_rows, sel_cols],
      row.names = 1L
    )

  res2 <-
    as.data.frame(
      .medesign$corr[.medesign$corr$rowname %in% sel_rows, sel_cols]
    )

  expect_equal(res1, res2, tol = 0.000001)

  # Make sure that all variables which are supposed to be corrected for CMV are
  # corrected
  cmv_vars <- c("stfsscore", "trstsscore")
  row_order <- match(cmv_vars, opposite_cov2cor$rowname)
  res1 <- opposite_cov2cor[row_order, c("rowname", cmv_vars)]
  res1[2:3] <- lapply(res1[2:3], round, 4)
  row.names(res1) <- NULL

  correct_res <-
    data.frame(
      rowname = c("stfsscore", "trstsscore"),
      stfsscore = c(0.6841, 0.4113),
      trstsscore = c(0.4113, 0.8936)
    )

  # We are checking up to three digits. They all match. Don't worry
  # if they don't match exactly after the three digits.
  # This check is alright with the pre cov2cor results. They
  # match the Excel calculations from Wiebke exactly.
  expect_equal(res1, correct_res, tol = 0.0000001)
})

test_that("medesign calculates quality of sumscore correctly for cor and cov", {

  data(ess7es)
  var_names <- c("polintr", "ppltrst", "stfedu",
                 "stfhlth", "trstplt", "trstprl",
                 "trstprt")
  me_df <-
    data.frame(
      question = var_names,
      reliability = c(0.624, 0.737, 0.757, 0.76, 0.852, 0.812, 0.858),
      validity = c(0.964, 0.952, 0.838, 0.798, 0.965, 0.959, 0.956),
      quality = c(0.601, 0.702, 0.635, 0.607, 0.822, 0.779, 0.821)
    )

  selected_vars1 <- c("polintr", "ppltrst")
  selected_vars2 <- c("trstprl", "trstplt", "trstprt")
  selected_vars3 <- c("stfedu", "stfhlth")

  .data <- ess7es[c(selected_vars1, selected_vars2)]

  m_obj <- medesign("std(s1) = trstprl + trstplt + trstprt",
                    .data,
                    me_df)

  expect_equal(m_obj$corr$s1[6], 0.8936, tol = 0.001)
  expect_equal(m_obj$covv$s1[6], 6.3999, tol = 0.001)

  .data <- ess7es[c(selected_vars1, selected_vars3)]

  m_obj <- medesign("std(s1) = stfedu + stfhlth",
                    .data,
                    me_df)

  expect_equal(m_obj$corr$s1[5], 0.6841, tol = 0.001)
  expect_equal(m_obj$covv$s1[5], 2.2053, tol = 0.001)
})

# TODO: once you fix me_cmv_cov
## test_that("me_cmv_cov returns correct calculation", {

##   sel_vars <- c(selected_vars2, "trstplt")
##   .data <- ess7es[complete.cases(ess7es[sel_vars]), sel_vars]
##   m_obj <- medesign("~ ppltrst + trstplt", .data, me_df)
##   # TODO: The first correct example you did was using weights
##   # so I had to hack medesign a bit to produce weighted covariances
##   # and make sure that the results are correct
##   m_obj$covv <- cov.wt(.data, wt = wt, cor = TRUE)$cov
##   diag(m_obj$covv) <- diag(m_obj$covv) * me_df[me_df$question %in% sel_vars,]$quality
##   m_obj$covv <- matrix2tibble(m_obj$covv)
##   tmp_corrected_cov <- as.data.frame(me_cmv_cov(m_obj))

##   correct_df <-
##     data.frame(
##       stringsAsFactors=FALSE,
##       rowname = c("polintr", "ppltrst", "trstplt"),
##       polintr = c(0.536047529655009, -0.419466267364113, -0.35995941514413),
##       ppltrst = c(-0.419466267364113, 3.14154466545579, 0.716657167759028),
##       trstplt = c(-0.35995941514413, 0.716657167759028, 4.08409798275177)
##     )

##   expect_equivalent(tmp_corrected_cov, correct_df)
## })
