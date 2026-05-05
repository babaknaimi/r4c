#' Validate CY and DS Estimates Against CW Data
#'
#' Uses CW data to compare corrected dry matter estimates from the CY and DS
#' models. Validation can be done using ANOVA or Kruskal-Wallis.
#'
#' @param cw_df A CW validation data frame, an Excel workbook path, or `NULL`.
#' @param models Either the output of [fit_cy_ds_models()] or
#'   [carrying_capacity()].
#' @param test Statistical test to apply.
#' @param cw_sheet Sheet name used when `cw_df` is a workbook path.
#'
#' @return An object of class `r4c_validation_result`.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#' cc <- carrying_capacity(ex$cyd, ex$dsd)
#' validate_methods(ex$cwd, cc)
validate_methods <- function(cw_df, models, test = c("anova", "kruskal"), cw_sheet = "re_cwd") {
  test <- match.arg(test)

  if (is.character(cw_df) && length(cw_df) == 1L && file.exists(cw_df)) {
    cw_df <- read_cwd_excel(cw_df, sheet = cw_sheet)
  }

  if (inherits(models, "r4c_capacity_result")) {
    models <- models$models
  }

  validate_required_columns(cw_df, c("Xrk", "Xek", "Ydk"), "cw_df")

  cw_df <- as.data.frame(cw_df, stringsAsFactors = FALSE)
  cw_df$Yrgk <- stats::predict(models$cy$ygi, newdata = data.frame(Xri = cw_df$Xrk))
  cw_df$Yrdk <- stats::predict(models$cy$ydi, newdata = data.frame(Ygi = cw_df$Yrgk))

  cw_df$Yegk <- stats::predict(models$ds$ygj, newdata = data.frame(Xej = cw_df$Xek))
  cw_df$Yedk <- stats::predict(models$ds$ydj, newdata = data.frame(Ygj = cw_df$Yegk))

  comparison_data <- data.frame(
    Yd = c(cw_df$Ydk, cw_df$Yrdk, cw_df$Yedk),
    Method = factor(rep(c("CW", "CY", "DS"), each = nrow(cw_df)), levels = c("CW", "CY", "DS"))
  )

  test_out <- if (identical(test, "anova")) {
    summary(stats::aov(Yd ~ Method, data = comparison_data))
  } else {
    stats::kruskal.test(Yd ~ Method, data = comparison_data)
  }

  structure(
    list(
      test = test_out,
      comparison_data = comparison_data,
      cw_data = cw_df
    ),
    class = "r4c_validation_result"
  )
}

#' Plot Validation Results
#'
#' @param x An object returned by [validate_methods()].
#' @param type Plot type: `"boxplot"` or `"agreement"`.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#' cc <- carrying_capacity(ex$cyd, ex$dsd)
#' v <- validate_methods(ex$cwd, cc)
#' plot_validation(v)
plot_validation <- function(x, type = c("boxplot", "agreement")) {
  type <- match.arg(type)

  if (!inherits(x, "r4c_validation_result")) {
    stop("`x` must be the result of `validate_methods()`.", call. = FALSE)
  }

  if (identical(type, "boxplot")) {
    return(
      ggplot2::ggplot(x$comparison_data, ggplot2::aes(Method, Yd, fill = Method)) +
        ggplot2::geom_boxplot(alpha = 0.7, width = 0.7) +
        ggplot2::labs(
          x = NULL,
          y = "Dry matter",
          title = "CW, CY, and DS comparison"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    )
  }

  agreement <- data.frame(
    observed = rep(x$cw_data$Ydk, 2),
    predicted = c(x$cw_data$Yrdk, x$cw_data$Yedk),
    method = factor(rep(c("CY", "DS"), each = nrow(x$cw_data)), levels = c("CY", "DS"))
  )

  ggplot2::ggplot(agreement, ggplot2::aes(observed, predicted, color = method)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::facet_wrap(~method) +
    ggplot2::labs(
      x = "Observed CW dry matter",
      y = "Predicted dry matter",
      title = "Agreement between CW and operational methods"
    ) +
    ggplot2::theme_minimal()
}
