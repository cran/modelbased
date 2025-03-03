#' @rdname get_emmeans
#'
#' @examplesIf insight::check_if_installed("marginaleffects", quietly = TRUE)
#' model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#'
#' # By default, 'by' is set to "Species"
#' get_marginalmeans(model)
#'
#' # Overall mean (close to 'mean(iris$Sepal.Length)')
#' get_marginalmeans(model, by = NULL)
#'
#' \dontrun{
#' # One can estimate marginal means at several values of a 'modulate' variable
#' get_marginalmeans(model, by = "Petal.Width", length = 3)
#'
#' # Interactions
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'
#' get_marginalmeans(model)
#' get_marginalmeans(model, by = c("Species", "Petal.Length"), length = 2)
#' get_marginalmeans(model, by = c("Species", "Petal.Length = c(1, 3, 5)"), length = 2)
#' }
#' @export
get_marginalmeans <- function(model,
                              by = "auto",
                              predict = NULL,
                              ci = 0.95,
                              estimate = "average",
                              transform = NULL,
                              verbose = TRUE,
                              ...) {
  # check if available
  insight::check_if_installed("marginaleffects")

  # First step: process arguments --------------------------------------------
  # --------------------------------------------------------------------------

  dots <- list(...)
  comparison <- dots$hypothesis

  # validate input
  estimate <- insight::validate_argument(
    estimate,
    c("average", "population", "specific")
  )

  # Guess arguments
  my_args <- .guess_marginaleffects_arguments(model, by, verbose = verbose, ...)

  # find default response-type
  predict <- .get_marginaleffects_type_argument(model, predict, ...)


  # Second step: create a data grid -------------------------------------------
  # ---------------------------------------------------------------------------

  # exception: by = NULL computes overall mean
  if (is.null(by)) {
    datagrid <- datagrid_info <- NULL
  } else {
    # setup arguments to create the data grid
    dg_factors <- switch(estimate,
      specific = "reference",
      "all"
    )
    dg_args <- list(
      model,
      by = my_args$by,
      factors = dg_factors,
      include_random = TRUE,
      verbose = FALSE
    )
    # always show all theoretical values by default
    if (is.null(dots$preserve_range)) {
      dg_args$preserve_range <- FALSE
    }
    # add user-arguments from "...", but remove those arguments that are already set
    dots[c("by", "factors", "include_random", "verbose")] <- NULL
    dg_args <- insight::compact_list(c(dg_args, dots))

    # Get corresponding datagrid (and deal with particular ats)
    datagrid <- do.call(insight::get_datagrid, dg_args)
    datagrid_info <- attributes(datagrid)

    # restore data types -  if we have defined numbers in `by`, like
    # `by = "predictor = 5"`, and `predictor` was a factor, it is returned as
    # numeric in the data grid. Fix this here, else marginal effects will fail
    datagrid <- datawizard::data_restoretype(datagrid, insight::get_data(model, verbose = FALSE))

    # add user-arguments from "...", but remove those arguments that are
    # already used (see below) when calling marginaleffects
    dots[c("by", "conf_level", "df", "type", "verbose")] <- NULL
  }


  # Third step: prepare arguments for marginaleffects ------------------------
  # --------------------------------------------------------------------------

  # model df
  dof <- insight::get_df(model, type = "wald", verbose = FALSE)

  # sanity check
  if (!is.null(datagrid)) {
    datagrid <- datawizard::data_arrange(
      as.data.frame(datagrid),
      select = datagrid_info$at_specs$varname
    )
  }

  # setup arguments
  fun_args <- list(
    model,
    conf_level = ci,
    df = dof
  )

  # counterfactual predictions - we need the "variables" argument
  if (estimate == "population") {
    # sanity check
    if (is.null(datagrid)) {
      insight::format_error("Could not create data grid based on variables selected in `by`. Please check if all `by` variables are present in the data set.") # nolint
    }
    fun_args$variables <- lapply(datagrid, unique)[datagrid_info$at_specs$varname]
  } else {
    # all other "marginalizations"
    if (is.null(dots$newdata)) {
      # we allow individual "newdata" options, so do not
      # # overwrite if explicitly set
      fun_args$newdata <- datagrid
    }
    fun_args$by <- datagrid_info$at_specs$varname
  }

  # handle distributional parameters
  if (predict %in% .brms_aux_elements() && inherits(model, "brmsfit")) {
    fun_args$dpar <- predict
  } else {
    fun_args$type <- predict
  }

  # =========================================================================
  # only needed to estimate_contrasts() with custom hypothesis ==============
  # =========================================================================
  # for custom hypothesis, like "b2=b5" or "(b2-b1)=(b4-b3)", we need to renumber
  # the b-values internally, because we have a different sorting in our output
  # compared to what "avg_predictions()" returns... so let's check if we have to
  # take care of this
  if (.is_custom_comparison(comparison)) {
    dots$hypothesis <- .reorder_custom_hypothesis(
      comparison,
      datagrid,
      focal = datagrid_info$at_specs$varname
    )
  }

  # cleanup
  fun_args <- insight::compact_list(c(fun_args, dots))

  ## TODO: need to check against different mixed models results from other packages
  # set to NULL
  if (!"re.form" %in% names(dots)) {
    fun_args$re.form <- NULL
  }

  # transform reponse?
  if (isTRUE(transform)) {
    transform <- insight::get_transformation(model, verbose = FALSE)$inverse
  }
  if (!is.null(transform)) {
    fun_args$transform <- transform
  }


  # Fourth step: compute marginal means ---------------------------------------
  # ---------------------------------------------------------------------------

  # we can use this function for contrasts as well,
  # just need to add "hypothesis" argument
  means <- .call_marginaleffects(fun_args)

  # =========================================================================
  # only needed to estimate_contrasts() with custom hypothesis ==============
  # =========================================================================
  # fix term label for custom hypothesis
  if (.is_custom_comparison(comparison)) {
    ## TODO: check which column name is used in marginaleffects update, and
    ## keep only the new one later - or for safety, we can keep both code lines
    means$term <- gsub(" ", "", comparison, fixed = TRUE)
    means$hypothesis <- gsub(" ", "", comparison, fixed = TRUE)
  }

  # Last step: Save information in attributes  --------------------------------
  # ---------------------------------------------------------------------------

  means <- .add_attributes(
    means,
    by = my_args$by,
    info = c(
      datagrid_info,
      list(predict = predict, estimate = estimate, datagrid = datagrid)
    )
  )
  class(means) <- unique(c("marginaleffects_means", class(means)))

  means
}


# call marginaleffects and process potential errors ---------------------------

.call_marginaleffects <- function(fun_args, type = "means") {
  out <- tryCatch(
    suppressWarnings(do.call(marginaleffects::avg_predictions, fun_args)),
    error = function(e) e
  )

  # display informative error
  if (inherits(out, "simpleError")) {
    # what was requested?
    if (!is.null(fun_args$hypothesis)) {
      fun <- "marginal contrasts"
    } else {
      fun <- "marginal means"
    }
    msg <- paste0(
      "Sorry, calculating ", fun, " failed with following error:\n",
      insight::color_text(gsub("\n", "", out$message), "red")
    )
    # we get this error when we should use counterfactuals - tell
    # # user about possible solution
    if (grepl("not found in column names", out$message, fixed = TRUE)) {
      msg <- paste0(
        msg,
        "\n\nIt seems that not all required levels of the focal terms are available in the provided data. If you want predictions extrapolated to a hypothetical target population, try setting `estimate=\"population\"." # nolint
      )
    }
    # error
    insight::format_error(msg)
  }

  out
}


# handle attributes -----------------------------------------------------------

#' @keywords internal
.add_attributes <- function(x, by = NULL, info = NULL) {
  attr(x, "at") <- by
  attr(x, "by") <- by

  # compact list
  info <- insight::compact_list(info)

  if (!is.null(info) && length(info)) {
    if (!is.null(info$at_specs$varname)) {
      attr(x, "focal_terms") <- info$at_specs$varname
    }
    for (i in .info_elements()) {
      if (!is.null(info[[i]])) {
        attr(x, i) <- info[[i]]
      }
    }
  }
  x
}

.info_elements <- function() {
  c(
    "at", "by", "focal_terms", "adjusted_for", "predict", "trend", "comparison",
    "contrast", "estimate", "p_adjust", "datagrid", "preserve_range",
    "coef_name", "slope"
  )
}


# Guess -------------------------------------------------------------------

#' @keywords internal
.guess_marginaleffects_arguments <- function(model, by = NULL, contrast = NULL, verbose = TRUE, ...) {
  # Gather info and data from model
  model_data <- insight::get_data(model, verbose = FALSE)
  predictors <- intersect(
    colnames(model_data),
    insight::find_predictors(model, effects = "fixed", flatten = TRUE, ...)
  )

  validate_arg <- function(spec_value, spec_name) {
    if (identical(spec_value, "auto")) {
      # Find categorical predictors
      spec_value <- predictors[!vapply(model_data[predictors], is.numeric, logical(1))]
      if (!length(spec_value) || all(is.na(spec_value))) {
        insight::format_error(paste0(
          "Model contains no categorical predictor. Please specify `", spec_name, "`."
        ))
      }
      if (verbose) {
        insight::format_alert(paste0(
          "We selected `", spec_name, "=c(", toString(paste0('"', spec_value, '"')), ")`."
        ))
      }
    }
    spec_value
  }

  # Guess arguments 'by'
  by <- validate_arg(by, "by")
  # Guess arguments 'contrast'
  contrast <- validate_arg(contrast, "contrast")

  list(by = by, contrast = contrast)
}
