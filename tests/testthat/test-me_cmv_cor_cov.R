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

fun <- me_cmv_cor
fun_str <- "me_cmv_cor"

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

  test_that(paste0(fun_str, " works with cmv argument"), {
    me_df <-
      tibble(question = paste0("V", 1:5),
             quality = c(0.2, 0.3, 0.5, 0.6, 0.9),
             reliability = c(0.6, 0.4, 0.5, 0.5, 0.7),
             validity = c(0.9, 0.5, 0.6, 0.7, 0.8))

    # For one CMV
    filtered_df <- subset(me_df, question %in% c("V4", "V5"))
    model <- "~ V4 + V5"
    m_des <- medesign(model, original_df, me_df)
    cmv_aut <- fun(m_des)
    cmv_manual <- fun(m_des, cmv = estimate_cmv(filtered_df))
    expect_equal(cmv_aut, cmv_manual)

    # For multiple CMV
    filtered_df <- subset(me_df, question %in% c("V2", "V3", "V4", "V5"))
    model <- "~ V2 + V3;~ V4 + V5"
    m_des <- medesign(model, original_df, me_df)
    cmv_aut <- fun(m_des)

    list_df <- list(filtered_df[1:2, ], filtered_df[3:4, ])
    cmv_manual <- fun(m_des, cmv = vapply(list_df,
                                          estimate_cmv,
                                          FUN.VALUE = numeric(1)))
    expect_equal(cmv_aut, cmv_manual)
  })

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
selected_vars <- c("polintr", "ppltrst", "trstplt")
ess_email <- Sys.getenv("ess_email")
ess7es <- import_country("Spain", 7, ess_email)[c(selected_vars, "pspwght", "pweight")]

ess7es <- ess7es[complete.cases(ess7es), ]
ess7es3var <- ess7es[selected_vars]

## Using sqpr
library(sqpr)
sqp_login()

me_df <-
  get_sqp(
    study = "ESS Round 7",
    question_name = selected_vars,
    country = "ES",
    lang = "spa"
  )

me_df <- me_df[order(me_df$question), ]

test_that("me_cmv_cor returns correct calculation after cov2cor",  {
  ## TODO: Leaving this here for when you decide whether you want
  ## to include the weights arg in me_correlate/me_covariance
  ## Apply weighted correlation with pspwght
  # wt_cor_cv <- cov.wt(ess7es3var, wt = ess7es$pspwght, cor = TRUE)
  # original_corr_weighted <- wt_cor_cv$cor
  # original_corr <- cor(ess7es3var)
  # diag(original_corr) <- me_df$quality
  # diag(original_corr_weighted) <- me_df$quality

  m_obj <- medesign("~ ppltrst + trstplt", ess7es3var, me_df)
  # TODO: Temporary fix until you figure out whether
  # all variables will be replace with the quality or only
  # the ones defined in the CMV in the model_syntax
  diag(m_obj$corr[1, 2]) <- me_df$quality[1]

  tmp_corrected_cor <- as.data.frame(me_cmv_cor(m_obj))

  correct_df <- data.frame(stringsAsFactors=FALSE,
                           rowname = c("polintr", "ppltrst", "trstplt"),
                           polintr = c(1, -0.307183576911697, -0.246956866582566),
                           ppltrst = c(-0.307183576911697, 1, 0.195267934255757),
                           trstplt = c(-0.246956866582566, 0.195267934255757, 1)
                           )

  expect_equivalent(tmp_corrected_cor, correct_df)
})

test_that("me_cmv_cov returns correct calculation", {

  m_obj <- medesign("~ ppltrst + trstplt", ess7es3var, me_df)
  # TODO: The first correct example you did was using weights
  # so I had to hack medesign a bit to produce weighted covariances
  # and make sure that the results are correct
  m_obj$covv <- cov.wt(ess7es3var, wt = ess7es$pspwght, cor = TRUE)$cov
  diag(m_obj$covv) <- diag(m_obj$covv) * me_df$quality
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
