context("test-me_construct.R")

test_that("me_construct gives correct output", {

  new_me <-
    me_construct(new_question,
                  list(quality = 0.3, validity = 0.2))

  expect_equal(nrow(new_me), 1)
  expect_equal(ncol(new_me), 4)
  expect_equal(sum(is.na(new_me)), 1)

  expect_s3_class(new_me, "data.frame")
  expect_s3_class(new_me, "me")

  expect_true(all(vapply(new_me[-1], is.numeric, FUN.VALUE = logical(1))))
})


test_that("me_construct handles arguments in correct format", {

  expect_error(
    me_construct(
      c("one_question", "twoquestions"),
      list(quality = 0.3, validity = 0.2)),
      "`question_name` must have only one question"
               )

  expect_error(
    me_construct(one_question, list(quality = 0.3, hey = 0.2)),
    "One or more of the specified `metrics` don't match the me column names"
    )

  expect_error(
    me_construct(one_question, c(hey = 1)),
    "`metrics` must be a named numeric list"
  )

  expect_error(
    me_construct(one_question, list(hey = c(1, 2))),
    "`metrics` must contain only one element per name"
  )
})
