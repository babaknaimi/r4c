`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

detect_label_col <- function(x) {
  non_numeric <- which(!vapply(x, is.numeric, logical(1)))
  if (length(non_numeric) == 0) {
    return(NA_integer_)
  }
  non_numeric[[1]]
}

split_sf_table <- function(x, label_col = NULL) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)

  if (is.null(label_col)) {
    label_col <- detect_label_col(x)
  }

  if (is.na(label_col)) {
    labels <- rownames(x)
    label_name <- "label"
    values <- x
  } else {
    labels <- as.character(x[[label_col]])
    label_name <- names(x)[[label_col]] %||% "label"
    values <- x[-label_col]
  }

  if (is.null(labels) || all(!nzchar(labels))) {
    labels <- paste0("row_", seq_len(nrow(x)))
  }

  values <- as.data.frame(
    lapply(values, function(col) suppressWarnings(as.numeric(col))),
    check.names = FALSE
  )

  if (ncol(values) == 0) {
    stop("The SF input must contain at least one numeric measurement column.", call. = FALSE)
  }

  list(
    labels = labels,
    label_name = label_name,
    values = values
  )
}

normalize_factor_weights <- function(weights) {
  default_names <- c(
    "composition",
    "production",
    "canopy",
    "litter",
    "vigour",
    "soil_protection"
  )

  if (is.null(names(weights))) {
    names(weights) <- default_names[seq_along(weights)]
  }

  weights <- weights[default_names]
  if (any(is.na(weights))) {
    stop(
      "Factor weights must include composition, production, canopy, litter, vigour, and soil_protection.",
      call. = FALSE
    )
  }

  if (sum(weights) <= 0) {
    stop("Factor weights must sum to a positive value.", call. = FALSE)
  }

  weights / sum(weights)
}

default_class_breaks <- function() {
  c(
    Unusable = 0,
    "Very Poor" = 20,
    Poor = 35,
    Fair = 50,
    Good = 65,
    Excellent = 80
  )
}

condition_class_from_score <- function(score, class_breaks) {
  class_breaks <- sort(class_breaks)
  idx <- max(which(score >= class_breaks))
  names(class_breaks)[[idx]]
}

palatability_to_weight <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  numeric_x <- suppressWarnings(as.numeric(x_chr))

  out <- rep(NA_real_, length(x_chr))

  out[!is.na(numeric_x) & numeric_x <= 1] <- 1
  out[!is.na(numeric_x) & numeric_x > 1 & numeric_x <= 2] <- 2 / 3
  out[!is.na(numeric_x) & numeric_x > 2] <- 1 / 3

  high_labels <- c("1", "high", "desirable", "preferred", "palatable", "decreaser")
  mid_labels <- c("2", "medium", "moderate", "intermediate", "less palatable", "increaser")
  low_labels <- c("3", "low", "undesirable", "unpalatable", "invader")

  out[x_chr %in% high_labels] <- 1
  out[x_chr %in% mid_labels] <- 2 / 3
  out[x_chr %in% low_labels] <- 1 / 3

  out
}

normalize_palatability <- function(palatability, species) {
  if (is.data.frame(palatability)) {
    if (ncol(palatability) < 2) {
      stop("A palatability data frame must contain at least two columns.", call. = FALSE)
    }
    palatability <- stats::setNames(palatability[[2]], as.character(palatability[[1]]))
  }

  if (is.null(names(palatability))) {
    if (length(palatability) != length(species)) {
      stop(
        "Unnamed palatability vectors must have the same length as the number of species rows.",
        call. = FALSE
      )
    }
    classes <- palatability
    names(classes) <- species
  } else {
    classes <- palatability[species]
    if (any(is.na(classes))) {
      missing_species <- species[is.na(classes)]
      stop(
        sprintf(
          "Palatability information is missing for: %s",
          paste(missing_species, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  weights <- palatability_to_weight(classes)
  if (any(is.na(weights))) {
    stop(
      "Palatability classes must be numeric 1-3 or recognizable labels such as high/medium/low.",
      call. = FALSE
    )
  }

  stats::setNames(weights, species)
}

normalize_palatability_classes <- function(palatability, species) {
  if (is.data.frame(palatability)) {
    palatability <- stats::setNames(palatability[[2]], as.character(palatability[[1]]))
  }

  if (is.null(names(palatability))) {
    if (length(palatability) != length(species)) {
      stop(
        "Unnamed palatability vectors must have the same length as the number of species rows.",
        call. = FALSE
      )
    }
    names(palatability) <- species
    return(palatability)
  }

  classes <- palatability[species]
  if (any(is.na(classes))) {
    missing_species <- species[is.na(classes)]
    stop(
      sprintf(
        "Palatability information is missing for: %s",
        paste(missing_species, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  classes
}

extract_condition_metrics <- function(
    x,
    palatability,
    label_col = NULL,
    special_rows = c(litter = "LFP", vigour = "SGP", canopy = "TCP", bare_soil = "BSP"),
    indicative_production = NULL,
    aggregation_fun = mean,
    na.rm = TRUE) {
  sf <- split_sf_table(x, label_col = label_col)
  labels_upper <- toupper(trimws(sf$labels))
  requested <- toupper(unname(special_rows))
  metric_rows <- match(requested, labels_upper)

  if (anyNA(metric_rows)) {
    missing_rows <- unname(special_rows)[is.na(metric_rows)]
    stop(
      sprintf(
        "The SF input is missing required summary rows: %s",
        paste(missing_rows, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  special_vals <- vapply(
    metric_rows,
    function(i) aggregation_fun(unlist(sf$values[i, , drop = TRUE]), na.rm = na.rm),
    numeric(1)
  )
  names(special_vals) <- names(special_rows)

  species_idx <- setdiff(seq_along(sf$labels), metric_rows)
  species <- sf$labels[species_idx]
  species_values <- sf$values[species_idx, , drop = FALSE]
  species_cover <- vapply(
    seq_len(nrow(species_values)),
    function(i) aggregation_fun(unlist(species_values[i, , drop = TRUE]), na.rm = na.rm),
    numeric(1)
  )
  names(species_cover) <- species

  total_cover <- sum(species_cover, na.rm = TRUE)
  pal_classes <- normalize_palatability_classes(palatability, species)
  pal_weights <- normalize_palatability(pal_classes, species)
  composition_pct <- if (total_cover > 0) {
    100 * sum(species_cover * pal_weights, na.rm = TRUE) / total_cover
  } else {
    0
  }

  present_production_pct <- if (!is.null(indicative_production)) {
    100 * total_cover / indicative_production
  } else {
    unname(special_vals[["canopy"]])
  }

  assumptions <- character()
  if (is.null(indicative_production)) {
    assumptions <- c(
      assumptions,
      "Present production defaulted to the TCP row because `indicative_production` was not supplied."
    )
  }

  factor_values <- c(
    composition = composition_pct,
    production = present_production_pct,
    canopy = unname(special_vals[["canopy"]]),
    litter = unname(special_vals[["litter"]]),
    vigour = unname(special_vals[["vigour"]]),
    soil_protection = 100 - unname(special_vals[["bare_soil"]])
  )
  factor_values <- stats::setNames(
    pmax(0, pmin(unname(factor_values), 100)),
    names(factor_values)
  )

  list(
    factor_values = factor_values,
    species_table = data.frame(
      species = species,
      mean_cover = unname(species_cover),
      palatability_class = unname(pal_classes),
      palatability_weight = unname(pal_weights),
      stringsAsFactors = FALSE
    ),
    merged_data = x,
    assumptions = assumptions
  )
}

make_condition_result <- function(
    factor_values,
    factor_weights,
    class_breaks,
    species_table,
    merged_data,
    assumptions) {
  factor_weights <- normalize_factor_weights(factor_weights)
  factor_scores <- 100 * factor_weights * factor_values / 100
  total_score <- sum(factor_scores)
  condition_class <- condition_class_from_score(total_score, class_breaks)

  structure(
    list(
      total_score = unname(total_score),
      condition_class = condition_class,
      factor_scores = data.frame(
        factor = names(factor_values),
        value_pct = unname(factor_values),
        weight = unname(factor_weights),
        score = unname(factor_scores),
        stringsAsFactors = FALSE
      ),
      species = species_table,
      merged_data = merged_data,
      assumptions = assumptions
    ),
    class = "r4c_condition_result"
  )
}

validate_required_columns <- function(x, required, arg) {
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "%s is missing required columns: %s",
        arg,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}
