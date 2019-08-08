## ------------------------------------------------------------------------------------------------
context("Test the has_specials() function")

test_that("Strings with special characters are properly identified", {
  expect_equal(has_specials("hello?", white = TRUE), TRUE)
  expect_equal(has_specials("hello?", white = FALSE), TRUE)
  expect_equal(has_specials("hello", white = TRUE), FALSE)
  expect_equal(has_specials("hello", white = FALSE), FALSE)
  expect_equal(has_specials(c("hello", "world!"), white = TRUE), c(FALSE, TRUE))
  expect_equal(has_specials(c("hello", "world!"), white = FALSE), c(FALSE, TRUE))
  expect_equal(has_specials(c("foo.", "fighter"), white = TRUE), c(TRUE, FALSE))
  expect_equal(has_specials(c("foo.", "fighter"), white = FALSE), c(TRUE, FALSE))
  expect_equal(has_specials(c("0<1", "1>0"), white = TRUE), c(TRUE, TRUE))
  expect_equal(has_specials(c("0<1", "1>0"), white = FALSE), c(TRUE, TRUE))
  expect_equal(has_specials(c("\\", "\\\\"), white = TRUE), c(TRUE, TRUE))
  expect_equal(has_specials(c("\\", "\\\\"), white = FALSE), c(TRUE, TRUE))
  expect_equal(has_specials(c("\\\\\\\\", "\\\\\\\\\\", "\\\\\\\\\\\\"), white = TRUE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("\\\\\\\\", "\\\\\\\\\\", "\\\\\\\\\\\\"), white = FALSE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("1|2", "3&4", "5:6"), white = TRUE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("1|2", "3&4", "5:6"), white = FALSE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("7;8", "8^9", "10,11"), white = TRUE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("7;8", "8^9", "10,11"), white = FALSE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("11@12", "13~14", "15`16`"), white = TRUE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("11@12", "13~14", "15`16`"), white = FALSE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("a[1", "2]b", "a(1"), white = TRUE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("a[1", "2]b", "a(1"), white = FALSE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("2)b", "c{1", "2}d"), white = TRUE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("2)b", "c{1", "2}d"), white = FALSE), c(TRUE, TRUE, TRUE))
  expect_equal(has_specials(c("test", "these ", "strings  "), white = TRUE), c(FALSE, TRUE, TRUE))
  expect_equal(has_specials(c("test", "these ", "strings  "), white = FALSE), c(FALSE, FALSE, FALSE))
  expect_equal(has_specials(c("test", "these   ", "strings     "), white = TRUE), c(FALSE, TRUE, TRUE))
  expect_equal(has_specials(c("test", "these   ", "strings     "), white = FALSE), c(FALSE, FALSE, FALSE))
})

## ------------------------------------------------------------------------------------------------
context("Test the file_addext() function")

test_that("Extensions applications work for filenames", {
  expect_error(file_addext("hello"))
  expect_error(file_addext(1))
  expect_error(file_addext(ext = "a"))
  expect_error(file_addext("hello", 1))
  expect_error(file_addext("hello", c("a", "b")))
  expect_error(file_addext(c("hello", "world", "good"), c("a", "b")))
  expect_error(file_addext(c("hello", "world"), c("a", "b", "c")))

  expect_equal(file_addext("helloworld", ".rds"), "helloworld.rds")
  expect_equal(file_addext("helloworld", "rds"), "helloworld.rds")
  expect_equal(file_addext("helloworld.rds", ".rds"), "helloworld.rds")
  expect_equal(file_addext("helloworld.rds", "rds"), "helloworld.rds")
  expect_equal(file_addext(c("hello", "world.rds"), c("txt", ".rds")), c("hello.txt", "world.rds"))
})

## ------------------------------------------------------------------------------------------------
context("Test the commify() function")
test_that("List of strings is correct", {
  expect_error(commify())
  expect_error(commify(1))
  expect_error(commify(c(1, 2)))
  expect_error(commify(c("a", "b"), 1))
  expect_equal(commify(c("a", "b", "c"), use_and = TRUE), "a, b, and c")
  expect_equal(commify(c("a", "b", "c"), use_and = FALSE), "a, b, c")
  expect_equal(commify(c("a", "b")), "a and b")
  expect_equal(commify(c("a", "b"), use_and = FALSE), "a and b")
})

## ------------------------------------------------------------------------------------------------
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
