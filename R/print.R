#' @export
print.r4c_condition_result <- function(x, ...) {
  cat("<r4c_condition_result>\n")
  cat(sprintf("Condition class: %s\n", x$condition_class))
  cat(sprintf("Total score: %.1f\n", x$total_score))
  print(x$factor_scores, row.names = FALSE)
  invisible(x)
}

#' @export
print.r4c_condition_series <- function(x, ...) {
  cat("<r4c_condition_series>\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
print.r4c_capacity_result <- function(x, ...) {
  cat("<r4c_capacity_result>\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
print.r4c_production_result <- function(x, ...) {
  cat("<r4c_production_result>\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
print.r4c_validation_result <- function(x, ...) {
  cat("<r4c_validation_result>\n")
  print(x$test)
  invisible(x)
}
