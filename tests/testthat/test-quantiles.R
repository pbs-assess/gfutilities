context("Test the group_quantiles() function")

# Use a partial storms dataset
df <- dplyr::storms %>% dplyr::filter(year %in% 2000:2005)

probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

test_that("Arguments are acceptable", {
  # df argument
  expect_error(group_quantiles(NULL, "year", "wind", probs, FALSE))
  expect_error(group_quantiles(c(1, 2, 3), "year", "wind", probs, FALSE))
  expect_error(group_quantiles("a", "year", "wind", probs, FALSE))
  expect_error(group_quantiles(c("a", 2, 3), "year", "wind", probs, FALSE))
  expect_error(group_quantiles(matrix(1:4, nrow = 2), "year", "wind", probs, FALSE))

  # grp_cols argument
  expect_error(group_quantiles(df, NULL, "wind", probs, FALSE))
  expect_error(group_quantiles(df, c("year", "foo"), "wind", probs, FALSE))
  expect_error(group_quantiles(df, "foo", "wind", probs, FALSE))

  # cols argument
  expect_error(group_quantiles(df, "year", NULL, probs, FALSE))
  expect_error(group_quantiles(df, "year", "wind", NULL, FALSE))
  expect_error(group_quantiles(df, "year", "wind", probs, NULL))
  expect_error(group_quantiles(df, "year", "status", probs, NULL))

  # probs argument
  expect_error(group_quantiles(df, "year", "wind", "a"))
  expect_error(group_quantiles(df, "year", "wind", c(1, "a")))
  probs_err <- c(0, 0.5, 0.95)
  expect_error(group_quantiles(df, "year", "wind", probs_err))
  probs_err <- c(0.5, -21.01)
  expect_error(group_quantiles(df, "year", "wind", probs_err))

  # include_mean argument
  expect_error(group_quantiles(df, "year", "wind", probs, c(1, 2)))
  expect_error(group_quantiles(df, "year", "wind", probs, c(TRUE, TRUE)))
})

test_that("Values are correct", {
  grp <- group_quantiles(df, grp_col = c("year", "month", "day"), col = "wind", probs = probs)

  actual <- df %>% dplyr::filter(year == 2001, month == 6, day == 8)
  actual_q <- quantile(actual$wind, probs)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    dplyr::filter(year == 2001, month == 6, day == 8) %>%
    dplyr::select(-c(year, month, day, wind_avg)) %>%
    dplyr::slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

  actual <- df %>% dplyr::filter(year == 2005, month == 7, day == 12)
  actual_q <- quantile(actual$wind, probs)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    dplyr::filter(year == 2005, month == 7, day == 12) %>%
    dplyr::select(-c(year, month, day, wind_avg)) %>%
    dplyr::slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

  grp <- group_quantiles(df,
                         grp_col = "name",
                         col = c("wind", "pressure"),
                         probs = probs,
                         include_mean = FALSE)
  actual <- df %>% dplyr::filter(name == "Emily")
  actual_wind_q <- quantile(actual$wind, probs)
  actual_pressure_q <- quantile(actual$pressure, probs)
  actual_q <- c(actual_wind_q, actual_pressure_q)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    dplyr::filter(name == "Emily") %>%
    dplyr::select(-c(name)) %>%
    dplyr::slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

  actual <- df %>% dplyr::filter(name == "Claudette")
  actual_wind_q <- quantile(actual$wind, probs)
  actual_pressure_q <- quantile(actual$pressure, probs)
  actual_q <- c(actual_wind_q, actual_pressure_q)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    dplyr::filter(name == "Claudette") %>%
    dplyr::select(-c(name)) %>%
    dplyr::slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

  grp <- group_quantiles(df, grp_col = "category", col = "wind", probs = probs)
  expect_equal(levels(grp$category), c("-1", "0", "1", "2", "3", "4", "5"))
  k <- grp[grp$category == -1,] %>% unlist(use.names = FALSE)
  expect_equal(round(k, 3), c(1, 20, 25, 30, 30, 30, 27.603))
})
