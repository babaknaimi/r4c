#' Classify Range Condition
#'
#' Calculates a six-factor range condition score from either:
#'
#' - a single SF data frame
#' - a list of replicated SF data frames
#' - an Excel workbook path
#'
#' The function is deliberately transparent: all factor scores are returned,
#' and the default scoring assumptions can be overridden.
#'
#' @param x A data frame, a list of data frames, or an Excel workbook path.
#' @param palatability Palatability classes for the species rows. Supply either
#'   a named vector or a two-column data frame mapping species to class.
#' @param sheets Optional sheet names when `x` is an Excel file.
#' @param merge_replicates Logical; if `TRUE`, replicated SF tables are merged
#'   before classification.
#' @param label_col Optional label column index for SF tables.
#' @param special_rows Mapping of factor rows in the SF input.
#' @param indicative_production Optional indicative-state production reference.
#'   If omitted, `TCP` is used as a proxy for present production.
#' @param factor_weights Named numeric vector of factor weights.
#' @param class_breaks Named numeric vector of lower score bounds.
#' @param aggregation_fun Summary function used across replicate columns or
#'   within-row plot values.
#' @param na.rm Passed to the summary function.
#'
#' @return An object of class `r4c_condition_result`.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#'
#' classify_range_condition(
#'   ex$sf_replicates,
#'   palatability = ex$palatability,
#'   indicative_production = 60
#' )
classify_range_condition <- function(
    x,
    palatability,
    sheets = NULL,
    merge_replicates = TRUE,
    label_col = NULL,
    special_rows = c(litter = "LFP", vigour = "SGP", canopy = "TCP", bare_soil = "BSP"),
    indicative_production = NULL,
    factor_weights = c(
      composition = 1,
      production = 1,
      canopy = 1,
      litter = 1,
      vigour = 1,
      soil_protection = 1
    ),
    class_breaks = default_class_breaks(),
    aggregation_fun = mean,
    na.rm = TRUE) {
  sf_tables <- if (is.character(x) && length(x) == 1L && file.exists(x)) {
    read_sf_excel(x, sheets = sheets)
  } else if (is.data.frame(x)) {
    list(input = x)
  } else if (is.list(x) && all(vapply(x, is.data.frame, logical(1)))) {
    x
  } else {
    stop("`x` must be a data frame, a list of data frames, or an Excel workbook path.", call. = FALSE)
  }

  if (!merge_replicates && length(sf_tables) > 1) {
    stop(
      "Multiple SF tables were supplied with `merge_replicates = FALSE`. Use `classify_range_condition_years()` for grouped year-wise outputs.",
      call. = FALSE
    )
  }

  sf_merged <- if (length(sf_tables) == 1) {
    sf_tables[[1]]
  } else {
    merge_sf_replicates(sf_tables, fun = aggregation_fun, na.rm = na.rm, label_col = label_col)
  }

  metrics <- extract_condition_metrics(
    sf_merged,
    palatability = palatability,
    label_col = label_col,
    special_rows = special_rows,
    indicative_production = indicative_production,
    aggregation_fun = aggregation_fun,
    na.rm = na.rm
  )

  make_condition_result(
    factor_values = metrics$factor_values,
    factor_weights = factor_weights,
    class_breaks = class_breaks,
    species_table = metrics$species_table,
    merged_data = metrics$merged_data,
    assumptions = metrics$assumptions
  )
}

#' Classify Range Condition Across Years
#'
#' Applies [classify_range_condition()] to a named set of years so the output
#' can be used for trend analysis.
#'
#' @param x A named list. Each element should be either a single SF data frame
#'   or a list of replicate SF data frames for one year. Alternatively, `x` can
#'   be an Excel workbook path if `sheet_groups` is supplied.
#' @param palatability Palatability classes for species rows.
#' @param sheet_groups Named list of sheet groups when `x` is an Excel path.
#' @param ... Additional arguments passed to [classify_range_condition()].
#'
#' @return An object of class `r4c_condition_series`.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#'
#' classify_range_condition_years(
#'   ex$sf_years,
#'   palatability = ex$palatability,
#'   indicative_production = 60
#' )
classify_range_condition_years <- function(x, palatability, sheet_groups = NULL, ...) {
  year_inputs <- if (is.character(x) && length(x) == 1L && file.exists(x)) {
    if (is.null(sheet_groups) || is.null(names(sheet_groups))) {
      stop("When `x` is a workbook path, `sheet_groups` must be a named list.", call. = FALSE)
    }
    lapply(sheet_groups, function(sheets) read_sf_excel(x, sheets = sheets))
  } else {
    x
  }

  if (is.null(names(year_inputs)) || any(!nzchar(names(year_inputs)))) {
    stop("`x` must be a named list when classifying multiple years.", call. = FALSE)
  }

  details <- lapply(
    year_inputs,
    function(year_input) classify_range_condition(year_input, palatability = palatability, ...)
  )

  summary <- data.frame(
    year = names(details),
    total_score = vapply(details, function(res) res$total_score, numeric(1)),
    condition_class = vapply(details, function(res) res$condition_class, character(1)),
    stringsAsFactors = FALSE
  )

  structure(
    list(summary = summary, details = details),
    class = "r4c_condition_series"
  )
}

#' Backward-Compatible Wrapper for Replicated Range Condition Input
#'
#' @param sfd_or_list A single SF data frame or a list of replicated SF data
#'   frames.
#' @param PC Palatability classes.
#' @param ... Additional arguments passed to [classify_range_condition()].
#'
#' @return An object of class `r4c_condition_result`.
#' @export
range_condition_class_rep <- function(sfd_or_list, PC, ...) {
  classify_range_condition(sfd_or_list, palatability = PC, ...)
}
