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
         reliability = c(NA, 0.4, 0.5, 0.5, 0.7),
         validity = c(NA, NA, 0.6, 0.7, 0.8)
         )

me_df <- structure(me_df, class = c(class(me_df), "me"))

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
    cmv_tib <- fun(x = corr_tibble,
                   me_data = me_df,
                   V4, V5)

    correct_format(cmv_tib)

    # Also handles character strings as variables
    expect_identical(cmv_tib, fun(corr_tibble, me_df, "V4", "V5"))
  })


  test_that(paste0(fun_str, "`x` argument works fine with matrix"), {
    random_vec <- rnorm(10, sd = 50)

    matr_nothing <- matrix(random_vec, 5, 5)

    matr_row <- matrix(random_vec, 5, 5,
                       dimnames = list(paste0("V", seq_len(5))))

    matr_col <- matrix(random_vec, 5, 5,
                       dimnames = list(NULL, paste0("V", seq_len(5))))

    matr_both <- matrix(random_vec, 5, 5,
                        dimnames = list(paste0("V", seq_len(5)),
                                        paste0("V", seq_len(5))))


    # Matrix no row or col names
    cmv_matr <- fun(matr_nothing, me_df, V4, V5)
    correct_format(cmv_matr)

    # Matrix row names
    cmv_matr <- fun(matr_row, me_df, V4, V5)
    correct_format(cmv_matr)

    # Matrix col names
    cmv_matr <- fun(matr_col, me_df, V4, V5)
    correct_format(cmv_matr)

    # Matrix row and col names
    cmv_matr <- fun(matr_both, me_df, V4, V5)
    correct_format(cmv_matr)
  })

  test_that(paste0(fun_str, "`x` argument works fine with data frame"), {
    random_vec <- rnorm(10, sd = 50)

    df_no_rows <- as.data.frame(matrix(random_vec, 5, 5))

    # df no row names
    cmv_matr <- fun(df_no_rows, me_df, V4, V5)
    correct_format(cmv_matr)

    rownames(df_no_rows) <- paste0("V", seq_len(5))
    df_rows <- df_no_rows

    # df row names
    cmv_matr <- fun(df_rows, me_df, V4, V5)
    correct_format(cmv_matr)
  })

  test_that(paste0(fun_str, " works with cmv argument"), {
    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    filtered_df <- subset(me_df, question %in% c("V4", "V5"))

    cmv_aut <- fun(corr_tibble, me_df, V4, V5)
    cmv_manual <- fun(corr_tibble, me_df, V4, V5, cmv = estimate_cmv(filtered_df))

    expect_equal(cmv_aut, cmv_manual)
  })

  test_that(paste0(fun_str, " gives same result when variables are shuffled"), {
    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    shuffled_corr <- corr_tibble[c(1, 3, 5, 4, 2), ]
    original_corr <- corr_tibble
    
    cmv_shuffled <- fun(shuffled_corr, me_df, V3, V4, V5)
    cmv_original <- fun(corr_tibble, me_df, V3, V4, V5)

    expect_equivalent(cmv_original,
                      cmv_shuffled[order(cmv_shuffled$rowname), ])

    expect_equal(names(cmv_shuffled), names(cmv_original))
  })
  
  test_that(paste0(fun_str, " uses only unique variable names"), {
    cmv_tib <- fun(corr_tibble, me_df, V4, V5, V5)
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
      all(sort(tp[lower.tri(tp)]) == sort(tp[upper.tri(tp)]))
    }

    # Two variables
    cmv_tib <- as.data.frame(fun(corr_tibble, me_df, V4, V5))
    expect_true(up_equal(cmv_tib))

    # Three variables
    cmv_tib <- as.data.frame(fun(corr_tibble, me_df, V3, V4, V5))
    expect_true(up_equal(cmv_tib))
  })

  test_that(paste0(fun_str, " adds me class to valid me_data"), {
    tmp <- me_df
    class(tmp) <- c("tbl_df", "tbl", "data.frame")

    noclass <- fun(
      corr_tibble,
      me_data = tmp,
      V4, V5
    )

    valid_class <- fun(
      corr_tibble,
      me_data = me_df,
      V4, V5
    )
    expect_identical(valid_class, noclass)
  })
}

test_cor_cov(me_cmv_cor, "me_cmv_cov")


# Given that original_data is not necessary in me_cmv_corr, I create
# me_cmv_cov with the original data argument prefilled so that all
# of the tests run this function and the me_cmv_cor and me_cmv_cov tests
# can be reused.

partial_cov <- function(x, me_data, ..., cmv = NULL) {
  me_cmv_cov(x = x, me_data = me_data, ... = ..., data = original_df, cmv = cmv)
}

test_cor_cov(partial_cov, "me_cmv_cov")


test_input_errors <- function(fun, fun_str) {
  type <- if (grepl("cor", fun_str)) "correlation" else "covariance"

  test_that(paste0(fun_str, " throws specific errors"), {
    expect_error(fun(list(), me_df),
                 paste0("`x` must be a ", type ," data frame or matrix"))

    expect_error(fun(corr_tibble, me_df, V2, V3),
                 "`me_data` must have non-missing values at columns reliability and validity for all variables")

    expect_error(fun(corr_tibble, me_df, hey, other),
                 "At least one variable not present in `x`: hey, other")
  })
}

test_input_errors(me_cmv_cor, "me_cmv_cor")
test_input_errors(partial_cov, "me_cmv_cov")


library(essurvey)
selected_vars <- c("polintr", "ppltrst", "trstplt")
ess_email <- Sys.getenv("ess_email")
ess7es <- import_country("Spain", 7, ess_email)[c(selected_vars, "pspwght", "pweight")]

ess7es <- ess7es[complete.cases(ess7es), ]
ess7es3var <- ess7es[selected_vars]

## Using sqpr
library(sqpr)
sqp_login()

question_ids <- find_questions("ESS Round 7", selected_vars)
question_ids <- question_ids[with(question_ids, country_iso == "ES" & language_iso == "spa"), "id", drop = TRUE]

me_df <- get_estimates(question_ids)
me_df <- me_df[order(me_df$question), ]

test_that("me_cmv_cor returns correct calculation after cov2cor",  {
  ## Apply weighted correlation with pspwght
  # wt_cor_cv <- cov.wt(ess7es3var, wt = ess7es$pspwght, cor = TRUE)
  # original_corr_weighted <- wt_cor_cv$cor
  original_corr <- cor(ess7es3var)

  diag(original_corr) <- me_df$quality
  # diag(original_corr_weighted) <- me_df$quality

  tst_cmv <- me_cmv_cor(original_corr, me_df, ppltrst, trstplt)
  tmp_corrected_cor <- as_tibble(cov2cor(as.matrix(tst_cmv[, selected_vars])))
  tmp_corrected_cor <- add_column(tmp_corrected_cor, rowname = tst_cmv$rowname, .before = 1)
  tmp_corrected_cor <- as.data.frame(tmp_corrected_cor)

  correct_df <- data.frame(stringsAsFactors=FALSE,
                           rowname = c("polintr", "ppltrst", "trstplt"),
                           polintr = c(1, -0.307183576911697, -0.246956866582566),
                           ppltrst = c(-0.307183576911697, 1, 0.195267934255757),
                           trstplt = c(-0.246956866582566, 0.195267934255757, 1)
                           )

  expect_equivalent(tmp_corrected_cor, correct_df)
})

test_that("me_cmv_cor returns correct calculate before cov2cor", {
  original_corr <- cor(ess7es3var)
  diag(original_corr) <- me_df$quality
  tst_cmv <- as.data.frame(me_cmv_cor(original_corr, me_df, ppltrst, trstplt))

  compare_tst_cmv <- data.frame(stringsAsFactors=FALSE,
                                rowname = c("polintr", "ppltrst", "trstplt"),
                                polintr = c(0.601, -0.199527970511773, -0.17357782848536),
                                ppltrst = c(-0.199527970511773, 0.702, 0.148332185882232),
                                trstplt = c(-0.17357782848536, 0.148332185882232, 0.822)
                                )

  expect_equivalent(tst_cmv, compare_tst_cmv)
})


test_that("me_cmv_cov returns correct calculation", {

  original_cov <- cov(ess7es3var, use = "complete.obs", method = "pearson")
  wt_cor_cv <- cov.wt(ess7es3var, wt = ess7es$pspwght, cor = TRUE)
  
  ## Apply weighted covariance with pspwght
  original_cov_weighted <- wt_cor_cv$cov

  #### Using `me`
  diag(original_cov_weighted) <- diag(original_cov_weighted) * me_df$quality  
  tst_corrected_cov <-
    me_cmv_cov(original_cov_weighted,
                me_df,
                ppltrst,
                trstplt,
                data = ess7es3var)
  tmp_corrected_cov <- as.data.frame(tst_corrected_cov)

  correct_df <- data.frame(stringsAsFactors=FALSE,
     rowname = c("polintr", "ppltrst", "trstplt"),
     polintr = c(0.536047529655009, -0.419466267364113, -0.35995941514413),
     ppltrst = c(-0.419466267364113, 3.14154466545579, 0.716657167759028),
     trstplt = c(-0.35995941514413, 0.716657167759028, 4.08409798275177)
     )

  expect_equivalent(tmp_corrected_cov, correct_df)
})
