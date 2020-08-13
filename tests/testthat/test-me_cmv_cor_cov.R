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
test_cor_cov(me_cmv_cov, "me_cmv_cov")

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

library(essurvey)
selected_vars1 <- c("polintr", "ppltrst")
selected_vars2 <- c("trstprl", "trstplt", "trstprt")
selected_vars3 <- c("stfedu", "stfhlth")
selected_vars4 <- "agea"
wts <- c("pspwght", "pweight")
all_vars <- c(selected_vars1, selected_vars2, selected_vars3, selected_vars4, wts)
ess_email <- Sys.getenv("ess_email")
ess7es <- import_country("Spain", 7, ess_email)[all_vars]

## Using sqpr
library(sqpr)
sqp_login()

me_df <-
  get_sqp(
    study = "ESS Round 7",
    question_name = c(selected_vars1, selected_vars2, selected_vars3),
    country = "ES",
    lang = "spa"
  )

me_df <- me_df[order(me_df$question), ]

## TODO - erase this
## sel_vars <- c(selected_vars2)
## .data <- ess7es[complete.cases(ess7es[sel_vars]), c(sel_vars)]
## me_data <- me_df
## model_syntax <- "std(s1) = trstprl + trstplt + trstprt"
## tst <- medesign(model_syntax, .data, me_data)
## dput(as.matrix(me_cmv_cor(tst)[-1]))

test_that("me_cmv_cor corrects for CMV between simple concepts correctly", { #nolintr

  .data_filtered <- ess7es[c("trstprl", "trstplt", "trstprt", "agea")]

  # Define model and me_data
  model_syntax <- "~ trstprl + trstplt + trstprt;"
  me_data <-
    data.frame(
      question = c("trstprl", "trstplt", "trstprt"),
      reliability = c(0.901110426085505, 0.923038460737146, 0.926282894152753),
      validity = c(0.979285453787607, 0.982344135219425, 0.977752524926425),
      quality = c(0.882609766544649, 0.906642156531451, 0.906090503205943)
    )

  .medesign <- medesign(model_syntax, .data_filtered, me_data)
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
      trstprt = c(0.563, 0.8086, 0.9061)
    )

  # We are checking up to three digits. They all match. Don't worry
  # if they don't match exactly after the three digits.
  # This check is alright with the pre cov2cor results. They
  # match the Excel calculations from Wiebke exactly.
  expect_equal(res1, correct_res, tol = 0.0000001)
})

test_that("me_cmv_cor returns correct calculation after cov2cor - political trust",  {
  ############################# Political trust example #########################
  ###############################################################################

  ## TODO: Leaving this here for when you decide whether you want
  ## to include the weights arg in me_correlate/me_covariance
  ## Apply weighted correlation with pspwght
  # wt_cor_cv <- cov.wt(ess7es3var, wt = ess7es$pspwght, cor = TRUE)
  # original_corr_weighted <- wt_cor_cv$cor
  # original_corr <- cor(ess7es3var)
  # diag(original_corr) <- me_df$quality
  # diag(original_corr_weighted) <- me_df$quality

  sel_vars <- c(selected_vars2, "polintr")
  .data <- ess7es[complete.cases(ess7es[sel_vars]), sel_vars]
  m_obj <- medesign("~ trstprl + trstplt + trstprt", .data, me_df)
  tmp_corrected_cor <- as.data.frame(me_cmv_cor(m_obj))
  
  correct_df <- structure(list(rowname = c("trstprl", "trstplt", "trstprt", "polintr"
                                           ), trstprl = c(1, 0.773478172548603, 0.707083130161171, -0.217732553457805
                                                          ), trstplt = c(0.773478172548603, 1, 0.984173394837744, -0.248145950309348
                                                                         ), trstprt = c(0.707083130161171, 0.984173394837744, 1, -0.29670248109113
                                                                                        ), polintr = c(-0.217732553457805, -0.248145950309348, -0.29670248109113, 
                                                                                                       1)), row.names = c(NA, -4L), class = "data.frame")

  # Results are matched after cov2cor
  expect_equivalent(tmp_corrected_cor, correct_df)

  # Results are matched before cov2cor. This code is convert
  # back the correlation to the covariance. We need to supply
  # the variances in the diagonal manually here.
  cor_variances <- c(0.779, 0.822, 0.821, 0.601)
  d <- sqrt(cor_variances)
  opposite_cov2cor <- outer(d, d) * as.matrix(tmp_corrected_cor[-1])

  original_cov <- structure(c(0.779, 0.618945913734983, 0.565471580756803, -0.14898046677033, 
                              0.618945913734983, 0.822, 0.808498294106698, -0.174413596180498, 
                              0.565471580756803, 0.808498294106698, 0.821, -0.208415489422785, 
                              -0.14898046677033, -0.174413596180498, -0.208415489422785, 0.601
                              ), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("trstprl", "trstplt", 
                                                                            "trstprt", "polintr")))

  # Opposite cov2cor contains the actual values from Wiebke's
  # Excel file. If you want to always check whether the estimates
  # match precisely, this is the test to check.
  expect_equal(opposite_cov2cor, original_cov, tol = 0.00001)
})

test_that("me_cmv_cor returns correct calculation after cov2cor - state service",  {
  ############################# State services examples #########################
  ###############################################################################

  ## TODO: Leaving this here for when you decide whether you want
  ## to include the weights arg in me_correlate/me_covariance
  ## Apply weighted correlation with pspwght
  # wt_cor_cv <- cov.wt(ess7es3var, wt = ess7es$pspwght, cor = TRUE)
  # original_corr_weighted <- wt_cor_cv$cor
  # original_corr <- cor(ess7es3var)
  # diag(original_corr) <- me_df$quality
  # diag(original_corr_weighted) <- me_df$quality

  sel_vars <- c(selected_vars3, "trstplt")
  .data <- ess7es[complete.cases(ess7es[sel_vars]), sel_vars]
  m_obj <- medesign("~ stfedu + stfhlth", .data, me_df)
  tmp_corrected_cor <- as.data.frame(me_cmv_cor(m_obj))
  
  correct_df <- structure(list(rowname = c("stfedu", "stfhlth", "trstplt"), stfedu = c(1, 
                                                                                       0.772686712915193, 0.467064171958538), stfhlth = c(0.772686712915193, 
                                                                                                                                          1, 0.501378224441568), trstplt = c(0.467064171958538, 0.501378224441568, 
                                                                                                                                                                             1)), row.names = c(NA, -3L), class = "data.frame")

  # Results are matched after cov2cor
  expect_equivalent(tmp_corrected_cor, correct_df)

  # Results are matched before cov2cor. This code is convert
  # back the correlation to the covariance. We need to supply
  # the variances in the diagonal manually here.
  cor_variances <- c(0.635, 0.607, 0.822)
  d <- sqrt(cor_variances)
  opposite_cov2cor <- outer(d, d) * as.matrix(tmp_corrected_cor[-1])

  original_cov <- structure(c(0.635, 0.479716495545129, 0.337442147028845, 0.479716495545129, 
                              0.607, 0.354156912063628, 0.337442147028845, 0.354156912063628, 
                              0.822), .Dim = c(3L, 3L), .Dimnames = list(NULL, c("stfedu", 
                                                                                 "stfhlth", "trstplt")))

  # Opposite cov2cor contains the actual values from Wiebke's
  # Excel file. If you want to always check whether the estimates
  # match precisely, this is the test to check.
  expect_equal(opposite_cov2cor, original_cov, tol = 0.00001)
})


test_that("me_cmv_cov returns correct calculation", {

  sel_vars <- c(selected_vars2, "trstplt")
  .data <- ess7es[complete.cases(ess7es[sel_vars]), sel_vars]
  m_obj <- medesign("~ ppltrst + trstplt", .data, me_df)
  # TODO: The first correct example you did was using weights
  # so I had to hack medesign a bit to produce weighted covariances
  # and make sure that the results are correct
  m_obj$covv <- cov.wt(.data, wt = wt, cor = TRUE)$cov
  diag(m_obj$covv) <- diag(m_obj$covv) * me_df[me_df$question %in% sel_vars,]$quality
  m_obj$covv <- matrix2tibble(m_obj$covv)
  tmp_corrected_cov <- as.data.frame(me_cmv_cov(m_obj))

  correct_df <-
    data.frame(
      stringsAsFactors=FALSE,
      rowname = c("polintr", "ppltrst", "trstplt"),
      polintr = c(0.536047529655009, -0.419466267364113, -0.35995941514413),
      ppltrst = c(-0.419466267364113, 3.14154466545579, 0.716657167759028),
      trstplt = c(-0.35995941514413, 0.716657167759028, 4.08409798275177)
    )

  expect_equivalent(tmp_corrected_cov, correct_df)
})

test_that("medesign calculates quality of sumscore correctly for cor and cov", {
  .data <- ess7es[complete.cases(ess7es[selected_vars2]),
                  c(selected_vars1, selected_vars2)]

  m_obj <- medesign("std(s1) = trstprl + trstplt + trstprt",
                    .data,
                    me_df)

  expect_equal(m_obj$corr$s1[6], 0.8914, tol = 0.001)
  expect_equal(m_obj$covv$s1[6], 6.3999, tol = 0.001)

  .data <- ess7es[complete.cases(ess7es[selected_vars3]),
                  c(selected_vars1, selected_vars3)]

  m_obj <- medesign("std(s1) = stfedu + stfhlth",
                    .data,
                    me_df)

  expect_equal(m_obj$corr$s1[5], 0.6811, tol = 0.001)
  expect_equal(m_obj$covv$s1[5], 2.205, tol = 0.001)
})
