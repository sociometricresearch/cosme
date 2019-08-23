context("test-seq_correlate.R")

test_matrix_operations <- function(fun, fun_str) {
  
  test_that(paste0(fun_str, " returns correct df"), {
    # New diagonal
    new_diagonal <- rnorm(ncol(mtcars))
    res_tibble <- fun(mtcars, new_diagonal)

    expect_is(res_tibble, "tbl_df")
    expect_is(res_tibble[[1]], "character")

    # If input is a matrix it returns the row/column
    # names
    ex_matrix <- matrix(rnorm(100, sd = 50), nrow = 20)
    res_matrix <- fun(ex_matrix, rnorm(ncol(ex_matrix)))

    expect_is(res_matrix[[1]], "character")

    # The rownames have at least 1 character in the names
    # This supposes that it's not the raw number but a
    # pre-processed column names
    expect_true(all(grepl("[[:alpha:]]{1,}", res_matrix[[1]])))
  })

  test_that(paste0(fun_str, " returns correct df"), {
    expect_error(fun(mtcars, 2:4),
                 "`diag_adj` must be the same length as the number of columns in `x`")

    # NA tests that there is NO error. This test that when diag_adj is set to 1
    # there is no error. That is, it tests that everything works when diag_adj is
    # of length 1
    expect_error(fun(mtcars, diag_adj = 1), NA)

    expect_error(fun(1:5, 2:3),
                 "supply both 'x' and 'y' or a matrix-like 'x'")

    expect_error(fun(mtcars, "wrong_diag"),
                 "`diag_adj` must be numeric")
  })

  test_that(paste0(fun_str, " works fine with weights"), {
    
    expect_error(fun(mtcars,
                     diag = rnorm(ncol(mtcars)),
                     wt = mpg),
                 "`diag_adj` must be the same length as the number of columns in `x`")

    expect_error(fun(mtcars,
                     diag = numeric(),
                     wt = mpg),
                 "`diag_adj` must be the same length as the number of columns in `x`")
    
    expect_error(fun(mtcars,
                     wt = whatever),
                 regexp = "Column whatever not present in data",
                 fixed = TRUE)


    
    standard_check <- function(expr) {
      expect_true(all(vapply(expr[, -1], is.numeric, FUN.VALUE = logical(1))))
      expect_is(expr, "tbl_df")
      expect_is(expr[[1]], "character")
      expect_true(!('mpg' %in% expr[[1]]) & !('mpg' %in% names(expr)))
    }
    
    wt_res <- fun(mtcars,
                  wt = mpg)
    
    standard_check(wt_res)

    wt_res <- fun(mtcars,
                  wt = "mpg")

    standard_check(wt_res)
    
    wt_res <- fun(mtcars,
                  diag = rep(1:(ncol(mtcars) - 1)),
                  wt = mpg)

    standard_check(wt_res)
    
    res <- fun(mtcars,
               diag = rep(1:ncol(mtcars)))

    # Test that the weights result is different from the unweighted
    expect_true(nrow(res) != nrow(wt_res))
    expect_true(ncol(res) != ncol(wt_res))
  })
}

test_matrix_operations(me_correlate, "me_correlate")
test_matrix_operations(me_covariance, "me_covariance")
