context("Test the rasterify_cols() function")

tmp_a_b <- tribble(
  ~a, ~b, ~z,
  1, 2, 3,
  4, 5, 6)
tmp_x_y <- tribble(
  ~x, ~y, ~z,
  1, 2, 3,
  4, 5, 6)
tmp_X_Y <- tribble(
  ~X, ~Y, ~z,
  1, 2, 3,
  4, 5, 6)
tmp_x <- tribble(
  ~x, ~z,
  1, 2,
  4, 5)
tmp_y <- tribble(
  ~y, ~z,
  1, 2,
  4, 5)
tmp_both_x_y <- tribble(
  ~x, ~y, ~X, ~Y, ~Z,
  1, 2, 3, 4, 5,
  6, 7, 8, 9, 10,
  11, 12, 13, 14, 15,
  16, 17, 18, 19, 20)
tmp_both_x <- tribble(
  ~x, ~y, ~X,
  1, 2, 3,
  6, 7, 8,
  11, 12, 13,
  16, 17, 18,)
tmp_both_y <- tribble(
  ~Y, ~y, ~X,
  1, 2, 3,
  6, 7, 8,
  11, 12, 13,
  16, 17, 18,)

test_that("X and Y column names are correctly changed to lower or upper case", {
  expect_error(rasterify_cols(tmp_a_b))
  expect_error(rasterify_cols(tmp_x))
  expect_error(rasterify_cols(tmp_y))
  expect_error(rasterify_cols(tmp_both_x_y))
  expect_error(rasterify_cols(tmp_both_x))
  expect_error(rasterify_cols(tmp_both_y))
  expect_equal(rasterify_cols(tmp_x_y, TRUE), tmp_X_Y)
  expect_equal(rasterify_cols(tmp_x_y), tmp_x_y)
  expect_equal(rasterify_cols(tmp_X_Y, TRUE), tmp_X_Y)
  expect_equal(rasterify_cols(tmp_X_Y), tmp_x_y)
})
