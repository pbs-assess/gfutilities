context("Test the group_quantiles() function")

# Use a partial storms dataset
df <- storms %>% filter(year %in% 2000:2005)

probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)

test_that("Arguments are acceptable", {
  expect_error(group_quantiles(NULL, "year", "val", probs, FALSE))
  expect_error(group_quantiles(c(1, 2, 3), "year", "val", probs, FALSE))
  expect_error(group_quantiles("a", "year", "val", probs, FALSE))
  expect_error(group_quantiles(c("a", 2, 3), "year", "val", probs, FALSE))
  expect_error(group_quantiles(matrix(1:4, nrow = 2), "year", "val", probs, FALSE))

  expect_error(group_quantiles(df, NULL, "val", probs, FALSE))
  expect_error(group_quantiles(df, c(1, 2), "val", probs, FALSE))

  expect_error(group_quantiles(df, "year", NULL, probs, FALSE))
  expect_error(group_quantiles(df, "year", "val", NULL, FALSE))
  expect_error(group_quantiles(df, "year", "val", probs, NULL))

  expect_error(group_quantiles(df, "year", "val", probs, c(1, 2)))
  expect_error(group_quantiles(df, "year", "val", probs, c(TRUE, TRUE)))
})

test_that("Values are correct", {
  grp <- group_quantiles(df, grp_col = c("year", "month", "day"), col = "wind", probs = probs)

  actual <- df %>% filter(year == 2001, month == 6, day == 8)
  actual_q <- quantile(actual$wind, probs)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    filter(year == 2001, month == 6, day == 8) %>%
    select(-c(year, month, day, wind_avg)) %>%
    slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

  actual <- df %>% filter(year == 2005, month == 7, day == 12)
  actual_q <- quantile(actual$wind, probs)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    filter(year == 2005, month == 7, day == 12) %>%
    select(-c(year, month, day, wind_avg)) %>%
    slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

  grp <- group_quantiles(df, grp_col = "name", col = "wind", probs = probs)
  actual <- df %>% filter(name == "Emily")
  actual_q <- quantile(actual$wind, probs)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    filter(name == "Emily") %>%
    select(-c(name, wind_avg)) %>%
    slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

  actual <- df %>% filter(name == "Claudette")
  actual_q <- quantile(actual$wind, probs)
  attributes(actual_q)$names <- NULL
  grp_q <- grp %>%
    filter(name == "Claudette") %>%
    select(-c(name, wind_avg)) %>%
    slice(1) %>%
    unlist(use.names = FALSE)
  expect_equal(grp_q, actual_q)

})
