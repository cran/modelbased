# modelbased 0.6.0

- `estimate_link()` now does *not* transform predictions on the response scale for GLMs. To keep the previous behaviour, use the new `estimate_relation()` instead. This follows a change in how predictions are made internally (which now relies on [`get_predicted()`](https://easystats.github.io/insight/reference/get_predicted.html), so more details can be found there).

# modelbased 0.5.1

- Minor improvements.

# modelbased 0.3.0

## Breaking changes

- `Predicted` is now the name of the predicted column for Bayesian models
  (similarly to Frequentist ones), instead of the centrality index (e.g.,
  `Median`).

## New supported models

- Models from package *glmmTMB* are now supported.

## Bug fixes

- `estimate_slope()` now gives an informative error when no numeric predictor is
  present.

# modelbased 0.2.0

- Partial support of formulas.

- Refactor the emmeans wrapping.

# modelbased 0.1.3

- Fix CRAN check issues.

# modelbased 0.1.2

- Minor code changes to address changes from the forthcoming `parameters`
  package update.

# modelbased 0.1.1

- Fix CRAN check issues.

# modelbased 0.1.0

- Added a `NEWS.md` file to track changes to the package

