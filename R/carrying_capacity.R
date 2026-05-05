#' Fit CY and DS Regression Models
#'
#' @param cyd A data frame with columns `Xri`, `Ygi`, and `Ydi`.
#' @param dsd A data frame with columns `Xej`, `Ygj`, and `Ydj`.
#'
#' @return A named list of linear models.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#' fit_cy_ds_models(ex$cyd, ex$dsd)
fit_cy_ds_models <- function(cyd, dsd) {
  validate_required_columns(cyd, c("Xri", "Ygi", "Ydi"), "cyd")
  validate_required_columns(dsd, c("Xej", "Ygj", "Ydj"), "dsd")

  list(
    cy = list(
      ygi = stats::lm(Ygi ~ Xri, data = cyd),
      ydi = stats::lm(Ydi ~ Ygi, data = cyd)
    ),
    ds = list(
      ygj = stats::lm(Ygj ~ Xej, data = dsd),
      ydj = stats::lm(Ydj ~ Ygj, data = dsd)
    )
  )
}

#' Estimate Carrying Capacity
#'
#' Fits CY and DS regressions, predicts corrected dry matter, and reports
#' carrying capacity in both `AU` and `AUM`.
#'
#' @param cyd A CY data frame, or an Excel workbook path.
#' @param dsd A DS data frame. Leave as `NULL` when `cyd` is a workbook path.
#' @param allowable_use Allowable use fraction.
#' @param range_area_ha Available range area in hectares.
#' @param au_weight_kg Average animal-unit body weight in kilograms.
#' @param grazing_months Grazing period in months.
#' @param days_per_month Days assumed per grazing month.
#' @param daily_intake_fraction Fraction of body weight consumed per day.
#' @param cyd_sheet Excel sheet name used when `cyd` is a workbook path.
#' @param dsd_sheet Excel sheet name used when `cyd` is a workbook path.
#'
#' @return An object of class `r4c_capacity_result`.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#' carrying_capacity(ex$cyd, ex$dsd)
carrying_capacity <- function(
    cyd,
    dsd = NULL,
    allowable_use = 0.60,
    range_area_ha = 700,
    au_weight_kg = 60,
    grazing_months = 5,
    days_per_month = 30,
    daily_intake_fraction = 0.02,
    cyd_sheet = "cyd",
    dsd_sheet = "dsd") {
  if (is.character(cyd) && length(cyd) == 1L && file.exists(cyd)) {
    workbook <- read_r4c_excel(cyd, cyd_sheet = cyd_sheet, dsd_sheet = dsd_sheet)
    cyd <- workbook$cyd
    dsd <- workbook$dsd
  }

  if (is.null(dsd)) {
    stop("`dsd` must be supplied unless `cyd` is an Excel workbook path.", call. = FALSE)
  }

  models <- fit_cy_ds_models(cyd, dsd)

  cyd_pred <- cyd
  dsd_pred <- dsd

  cyd_pred$Ygi_hat <- stats::predict(models$cy$ygi, newdata = cyd_pred)
  cyd_pred$Ydi_hat <- stats::predict(models$cy$ydi, newdata = data.frame(Ygi = cyd_pred$Ygi_hat))

  dsd_pred$Ygj_hat <- stats::predict(models$ds$ygj, newdata = dsd_pred)
  dsd_pred$Ydj_hat <- stats::predict(models$ds$ydj, newdata = data.frame(Ygj = dsd_pred$Ygj_hat))

  dm_kg_ha <- c(
    CY = mean(cyd_pred$Ydi_hat, na.rm = TRUE) * 10,
    DS = mean(dsd_pred$Ydj_hat, na.rm = TRUE) * 10
  )
  taf_kg <- dm_kg_ha * allowable_use * range_area_ha

  daily_dm_kg <- daily_intake_fraction * au_weight_kg
  aum_kg <- daily_dm_kg * days_per_month

  summary <- data.frame(
    method = c("CY", "DS"),
    DM_kg_ha = unname(dm_kg_ha),
    TAF_kg = unname(taf_kg),
    CC_AU = unname(taf_kg / (aum_kg * grazing_months)),
    CC_AUM = unname(taf_kg / aum_kg),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      models = models,
      predictions = list(cyd = cyd_pred, dsd = dsd_pred),
      summary = summary,
      inputs = list(
        allowable_use = allowable_use,
        range_area_ha = range_area_ha,
        au_weight_kg = au_weight_kg,
        grazing_months = grazing_months,
        days_per_month = days_per_month,
        daily_intake_fraction = daily_intake_fraction
      )
    ),
    class = "r4c_capacity_result"
  )
}
