#' Estimate Fodder Production from CW, CY, and DS Data
#'
#' Uses calibration sheets (`cyd`, `dsd`) and the `re_cwd` field sheet to
#' summarize dry fodder production for cutting-and-weighing (CW), comparative
#' yield (CY), and double sampling (DS). CW is calculated from observed `Ydk`;
#' CY and DS are predicted from their calibration regressions.
#'
#' @param cyd A CY calibration data frame, or an Excel workbook path.
#' @param dsd A DS calibration data frame. Leave as `NULL` when `cyd` is a
#'   workbook path.
#' @param cwd A CW/CY/DS field data frame. Leave as `NULL` when `cyd` is a
#'   workbook path.
#' @param allowable_use Allowable use fraction.
#' @param range_area_ha Available range area in hectares.
#' @param au_weight_kg Average animal-unit body weight in kilograms.
#' @param grazing_months Grazing period in months.
#' @param days_per_month Days assumed per grazing month.
#' @param daily_intake_fraction Fraction of body weight consumed per day.
#' @param scale_kg_ha Multiplier used to convert plot dry matter to kg/ha.
#' @param cyd_sheet Excel sheet name used when `cyd` is a workbook path.
#' @param dsd_sheet Excel sheet name used when `cyd` is a workbook path.
#' @param cwd_sheet Excel sheet name used when `cyd` is a workbook path.
#'
#' @return An object of class `r4c_production_result`.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#' estimate_fodder_production(ex$cyd, ex$dsd, ex$cwd)
estimate_fodder_production <- function(
    cyd,
    dsd = NULL,
    cwd = NULL,
    allowable_use = 0.60,
    range_area_ha = 700,
    au_weight_kg = 60,
    grazing_months = 6,
    days_per_month = 30,
    daily_intake_fraction = 0.02,
    scale_kg_ha = 10,
    cyd_sheet = "cyd",
    dsd_sheet = "dsd",
    cwd_sheet = "re_cwd") {
  if (is.character(cyd) && length(cyd) == 1L && file.exists(cyd)) {
    path <- cyd
    cyd <- read_cyd_excel(path, sheet = cyd_sheet)
    dsd <- read_dsd_excel(path, sheet = dsd_sheet)
    cwd <- read_cwd_excel(path, sheet = cwd_sheet)
  }

  if (is.null(dsd) || is.null(cwd)) {
    stop("`dsd` and `cwd` must be supplied unless `cyd` is an Excel workbook path.", call. = FALSE)
  }

  validate_required_columns(cyd, c("Xri", "Ygi", "Ydi"), "cyd")
  validate_required_columns(dsd, c("Xej", "Ygj", "Ydj"), "dsd")
  validate_required_columns(cwd, c("Xrk", "Xek", "Ydk"), "cwd")

  models <- fit_cy_ds_models(cyd, dsd)
  cwd_pred <- as.data.frame(cwd, stringsAsFactors = FALSE)
  cwd_pred$Yrgk <- stats::predict(models$cy$ygi, newdata = data.frame(Xri = cwd_pred$Xrk))
  cwd_pred$Yrdk <- stats::predict(models$cy$ydi, newdata = data.frame(Ygi = cwd_pred$Yrgk))
  cwd_pred$Yegk <- stats::predict(models$ds$ygj, newdata = data.frame(Xej = cwd_pred$Xek))
  cwd_pred$Yedk <- stats::predict(models$ds$ydj, newdata = data.frame(Ygj = cwd_pred$Yegk))

  dry_by_method <- list(
    CW = cwd_pred$Ydk,
    CY = cwd_pred$Yrdk,
    DS = cwd_pred$Yedk
  )

  dry_fodder_kg_ha <- vapply(dry_by_method, mean, numeric(1), na.rm = TRUE) * scale_kg_ha
  total_available_fodder_kg <- dry_fodder_kg_ha * allowable_use * range_area_ha
  livestock_unit_demand_kg <- daily_intake_fraction * au_weight_kg * days_per_month * grazing_months

  summary <- data.frame(
    method = names(dry_fodder_kg_ha),
    dry_fodder_kg_ha = unname(dry_fodder_kg_ha),
    total_available_fodder_kg = unname(total_available_fodder_kg),
    livestock_units = unname(total_available_fodder_kg / livestock_unit_demand_kg),
    stringsAsFactors = FALSE
  )

  comparison_data <- data.frame(
    method = factor(rep(names(dry_by_method), each = nrow(cwd_pred)), levels = names(dry_by_method)),
    dry_matter = unlist(dry_by_method, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      models = models,
      predictions = cwd_pred,
      summary = summary,
      comparison_data = comparison_data,
      inputs = list(
        allowable_use = allowable_use,
        range_area_ha = range_area_ha,
        au_weight_kg = au_weight_kg,
        grazing_months = grazing_months,
        days_per_month = days_per_month,
        daily_intake_fraction = daily_intake_fraction,
        scale_kg_ha = scale_kg_ha
      )
    ),
    class = "r4c_production_result"
  )
}
