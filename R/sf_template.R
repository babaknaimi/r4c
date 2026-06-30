#' Read SF Template Sheets from an Excel Workbook
#'
#' Reads range-condition sheets that use the field template structure found in
#' the example workbook: `SP`, `PC`, then alternating `C1`/`S1`, `C2`/`S2`, ...
#' columns. This keeps the palatability (`PC`) and cover (`C`) fields separate
#' before classification.
#'
#' @param path Path to an Excel workbook.
#' @param sheets Optional sheet names to read. Defaults to replicated sheets
#'   named `sfd1`, `sfd2`, ... or `sfd`.
#'
#' @return A named list of data frames.
#' @export
read_sf_template_excel <- function(path, sheets = NULL) {
  all_sheets <- readxl::excel_sheets(path)

  if (is.null(sheets)) {
    replicated <- grep("^sfd[0-9]+$", all_sheets, value = TRUE)
    if (length(replicated) > 0) {
      sheets <- replicated
    } else if ("sfd" %in% all_sheets) {
      sheets <- "sfd"
    } else {
      stop(
        "No SF template sheet found. Expected 'sfd' or replicated sheets such as 'sfd1', 'sfd2', ...",
        call. = FALSE
      )
    }
  }

  out <- lapply(
    sheets,
    function(sheet) readxl::read_excel(path, sheet = sheet, col_names = TRUE)
  )
  names(out) <- sheets
  out
}

#' Prepare an SF Template for Classification
#'
#' Converts an SF template table into the simpler condition-classification
#' format used by [classify_range_condition()]. The species column and `C`
#' cover columns are retained, while the `PC` palatability column is returned as
#' a separate named vector.
#'
#' @param x A data frame in SF template format.
#' @param species_col Name of the species/row-label column.
#' @param palatability_col Name of the palatability class column.
#' @param cover_prefix Prefix used by cover columns. Defaults to `"C"`.
#' @param special_rows Summary-row labels used by the six-factor workflow.
#'
#' @return A list with `condition_data`, `palatability`, and `species`.
#' @export
prepare_sf_template <- function(
    x,
    species_col = "SP",
    palatability_col = "PC",
    cover_prefix = "C",
    special_rows = c("LFP", "SGP", "TCP", "BSP")) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

  # Support tables that were read with col_names = FALSE and still contain the
  # template header as their first data row.
  if (!species_col %in% names(x) && nrow(x) > 0 && species_col %in% as.character(unlist(x[1, ], use.names = FALSE))) {
    names(x) <- as.character(unlist(x[1, ], use.names = FALSE))
    x <- x[-1, , drop = FALSE]
  }

  if (!species_col %in% names(x)) {
    stop(sprintf("The SF template is missing the `%s` column.", species_col), call. = FALSE)
  }
  if (!palatability_col %in% names(x)) {
    stop(sprintf("The SF template is missing the `%s` column.", palatability_col), call. = FALSE)
  }

  if (nrow(x) > 0 && identical(as.character(x[[species_col]][[1]]), species_col)) {
    names(x) <- as.character(unlist(x[1, ], use.names = FALSE))
    x <- x[-1, , drop = FALSE]
  }

  labels <- as.character(x[[species_col]])
  cover_cols <- grep(sprintf("^%s[0-9]+$", cover_prefix), names(x), value = TRUE)
  if (length(cover_cols) == 0) {
    stop(sprintf("No cover columns matching `%s1`, `%s2`, ... were found.", cover_prefix, cover_prefix), call. = FALSE)
  }

  condition_data <- data.frame(
    label = labels,
    x[cover_cols],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  condition_data[cover_cols] <- lapply(condition_data[cover_cols], function(col) suppressWarnings(as.numeric(col)))

  is_special <- toupper(trimws(labels)) %in% toupper(special_rows)
  species <- labels[!is_special]
  palatability <- x[[palatability_col]][!is_special]
  names(palatability) <- species

  list(
    condition_data = condition_data,
    palatability = palatability,
    species = data.frame(
      species = species,
      palatability_class = unname(palatability),
      stringsAsFactors = FALSE
    )
  )
}

#' Classify Range Condition from an SF Template
#'
#' This is a convenience wrapper for the field-template format used in the
#' example workbook. By default it uses the transparent linear scoring already
#' implemented by [classify_range_condition()]. Set `score_profile =
#' "template_rank"` to apply a rank-style template profile that reproduces the
#' colleague-provided example output for the attached workbook while keeping the
#' profile parameters explicit and overridable.
#'
#' @param x A data frame in SF template format.
#' @param score_profile Either `"linear"` or `"template_rank"`.
#' @param score_max Maximum score for each six-factor component when
#'   `score_profile = "template_rank"`.
#' @param count_reference Reference count used to rescale `litter` and `vigour`
#'   rows when `score_profile = "template_rank"`.
#' @param rank_fun Function used to convert the unrounded template-rank sum to
#'   the reported total rank. Defaults to [floor].
#' @param class_breaks Named numeric vector of lower score bounds.
#' @param ... Additional arguments passed to [prepare_sf_template()] and
#'   [classify_range_condition()].
#'
#' @return An object of class `r4c_condition_result`.
#' @export
classify_sf_template <- function(
    x,
    score_profile = c("linear", "template_rank"),
    score_max = c(
      composition = 50,
      production = 15,
      canopy = 15,
      litter = 10,
      vigour = 5,
      soil_protection = 15
    ),
    count_reference = c(litter = 5, vigour = 5),
    rank_fun = floor,
    class_breaks = default_class_breaks(),
    ...) {
  score_profile <- match.arg(score_profile)
  prepared <- prepare_sf_template(x, ...)

  if (identical(score_profile, "linear")) {
    return(
      classify_range_condition(
        prepared$condition_data,
        palatability = prepared$palatability,
        label_col = 1,
        class_breaks = class_breaks
      )
    )
  }

  metrics <- extract_condition_metrics(
    prepared$condition_data,
    palatability = prepared$palatability,
    label_col = 1
  )

  required_scores <- c("composition", "production", "canopy", "litter", "vigour", "soil_protection")
  score_max <- score_max[required_scores]
  if (any(is.na(score_max))) {
    stop("`score_max` must include composition, production, canopy, litter, vigour, and soil_protection.", call. = FALSE)
  }

  count_reference <- count_reference[c("litter", "vigour")]
  if (any(is.na(count_reference)) || any(count_reference <= 0)) {
    stop("`count_reference` must include positive `litter` and `vigour` values.", call. = FALSE)
  }

  values <- metrics$factor_values
  factor_scores <- c(
    composition = score_max[["composition"]] * values[["composition"]] / 100,
    production = score_max[["production"]] * values[["production"]] / 100,
    canopy = score_max[["canopy"]] * values[["canopy"]] / 100,
    litter = score_max[["litter"]] * pmin(values[["litter"]] / count_reference[["litter"]], 1),
    vigour = score_max[["vigour"]] * pmin(values[["vigour"]] / count_reference[["vigour"]], 1),
    soil_protection = score_max[["soil_protection"]] * values[["soil_protection"]] / 100
  )

  unrounded_total <- sum(factor_scores, na.rm = TRUE)
  total_score <- unname(rank_fun(unrounded_total))
  condition_class <- condition_class_from_score(total_score, class_breaks)

  structure(
    list(
      total_score = total_score,
      unrounded_total_score = unrounded_total,
      condition_class = condition_class,
      factor_scores = data.frame(
        factor = names(values),
        value_pct = unname(values),
        max_score = unname(score_max[names(values)]),
        score = unname(factor_scores[names(values)]),
        stringsAsFactors = FALSE
      ),
      species = metrics$species_table,
      merged_data = metrics$merged_data,
      assumptions = c(
        metrics$assumptions,
        "Template-rank scoring uses explicit package defaults; replace `score_max`, `count_reference`, or `rank_fun` if your official scoring rubric differs."
      )
    ),
    class = "r4c_condition_result"
  )
}

#' Classify SF Template Sheets Across Years
#'
#' Applies [classify_sf_template()] to one template table per year or to named
#' sheets in an Excel workbook.
#'
#' @param x A named list of SF template data frames, or a workbook path.
#' @param sheets Optional sheet names when `x` is a workbook path.
#' @param year_names Optional labels to use in the summary output.
#' @param ... Additional arguments passed to [classify_sf_template()].
#'
#' @return An object of class `r4c_condition_series`.
#' @export
classify_sf_template_years <- function(x, sheets = NULL, year_names = NULL, ...) {
  templates <- if (is.character(x) && length(x) == 1L && file.exists(x)) {
    read_sf_template_excel(x, sheets = sheets)
  } else {
    x
  }

  if (!is.list(templates) || !all(vapply(templates, is.data.frame, logical(1)))) {
    stop("`x` must be a workbook path or a list of SF template data frames.", call. = FALSE)
  }

  if (!is.null(year_names)) {
    if (length(year_names) != length(templates)) {
      stop("`year_names` must have the same length as the number of SF templates.", call. = FALSE)
    }
    names(templates) <- year_names
  }

  if (is.null(names(templates)) || any(!nzchar(names(templates)))) {
    names(templates) <- paste0("year_", seq_along(templates))
  }

  details <- lapply(templates, classify_sf_template, ...)
  summary <- data.frame(
    year = names(details),
    total_score = unname(vapply(details, function(res) res$total_score, numeric(1))),
    condition_class = unname(vapply(details, function(res) res$condition_class, character(1))),
    stringsAsFactors = FALSE
  )

  structure(
    list(summary = summary, details = details),
    class = "r4c_condition_series"
  )
}
