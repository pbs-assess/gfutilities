context("Test the verify_arguments() function")

f_numeric <- function(a){
  verify_argument(a, "numeric", 1)
}

f_number <- function(a){
  verify_argument(a, c("numeric", "integer"), 2)
}

f_character <- function(a){
  verify_argument(a, "character", 1)
}

f_df <- function(a){
  # 2 is the number of columns in the data frame
  verify_argument(a, "data.frame", 2)
}

f_list <- function(a){
  verify_argument(a, "list")
}

test_that("Output is TRU when inouts are correct", {
  expect_true(f_numeric(23))
  expect_true(f_number(c(1L, 3.01)))
  expect_true(f_character("a"))
  expect_true(f_df(data.frame(a = c(1, 2), b = c(3, 4))))
  expect_true(f_list(list(a = 1, b = 2, d = 3, e = 4)))
})

test_that("Errors are thrown correctly for erroneous inputs", {
  expect_error(f_numeric(c(1, 2)))
  expect_error(f_numeric("a"))
  expect_error(f_numeric(data.frame(a = c(1, 2), b = c(3, 4))))

  expect_error(f_number(c(1L, c(1, 2))))
  expect_error(f_number(c("a", 3.01)))
  expect_error(f_number(2L))

  expect_error(f_character(1))
  expect_error(f_character(c("a", "b")))
  expect_error(f_character(data.frame(a = c(1, 2), b = c(3, 4))))

  expect_error(f_df(1))
  expect_error(f_df("a"))

  expect_error(f_list(1))
  expect_error(f_list(1L))
  expect_error(f_list("a"))
  expect_error(f_list(data.frame(a = c(1, 2), b = c(3, 4))))
})
