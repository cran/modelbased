#' Easy marginaleffects
#'
#' Modelbased-like API to create \pkg{marginaleffects} objects. This is
#' Work-in-progress.
#'
#' @inheritParams get_emmeans
#'
#' @examples
#' if (require("marginaleffects")) {
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#'   get_marginaleffects(model, trend = "Petal.Length", by = "Species")
#'   get_marginaleffects(model, trend = "Petal.Length", by = "Petal.Length")
#'   get_marginaleffects(model, trend = "Petal.Length", by = c("Species", "Petal.Length"))
#' }
#' @export
get_marginaleffects <- function(model,
                                trend = NULL,
                                by = NULL,
                                fixed = NULL,
                                at = NULL,
                                ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # Deprecation
  if (!is.null(at)) {
    insight::format_warning("The `at` argument is deprecated and will be removed in the future. Please use `by` instead.") # nolint
    by <- at
  }

  # Guess arguments
  if (is.null(trend)) {
    trend <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)[1]
    insight::format_warning(
      paste0("`trend` cannot be NULL when using marginaleffects to compute overall effects. Selecting '", trend, "'.")
    )
  }

  if (is.null(by)) {
    by <- insight::find_predictors(model, effects = "fixed", flatten = TRUE)
    by <- by[!by %in% trend]
  }

  newdata <- insight::get_datagrid(model, by = by, ...)

  fixed <- names(newdata)[!names(newdata) %in% c(by, trend)]
  if (length(fixed) == 0) fixed <- NULL

  # Compute stuff
  estimated <- marginaleffects::slopes(model, variables = trend, newdata = newdata, ...)

  attr(estimated, "trend") <- trend
  attr(estimated, "at") <- by
  attr(estimated, "by") <- by
  attr(estimated, "fixed") <- fixed
  estimated
}
