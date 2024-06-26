test_that("attributes_means", {
  skip_if_not_installed("emmeans")
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_means(model))
  expect_identical(attributes(estim)$by, "Species")
  expect_null(attributes(estim)$fixed)

  estim <- suppressMessages(estimate_means(model, fixed = "Sepal.Width"))
  expect_identical(attributes(estim)$by, "Species")
  expect_identical(attributes(estim)$fixed, "Sepal.Width")

  estim <- suppressMessages(estimate_means(model, by = "all"))
  expect_identical(attributes(estim)$by, c("Species", "Sepal.Width"))
})



test_that("attributes_contrasts", {
  skip_if_not_installed("emmeans")
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  estim <- suppressMessages(estimate_contrasts(model))
  expect_identical(attributes(estim)$contrast, "Species")
  expect_null(attributes(estim)$by)
  expect_null(attributes(estim)$fixed)

  estim <- suppressMessages(estimate_contrasts(model, fixed = "Sepal.Width"))
  expect_identical(attributes(estim)$contrast, "Species")
  expect_identical(attributes(estim)$fixed, "Sepal.Width")
  expect_null(attributes(estim)$modulate)
})


test_that("attributes_link", {
  skip_if_not_installed("emmeans")
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)

  estim <- estimate_link(model)
  expect_identical(attributes(estim)$response, "Sepal.Length")
})
