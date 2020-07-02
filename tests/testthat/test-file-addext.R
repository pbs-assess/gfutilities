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
