#' Example Data for r4c Workflows
#'
#' Returns a compact set of example data frames for range condition,
#' carrying-capacity estimation, and validation.
#'
#' @return A named list.
#' @export
#'
#' @examples
#' ex <- r4c_example_data()
#' names(ex)
r4c_example_data <- function() {
  sf_rep1 <- data.frame(
    label = c("Bouteloua", "Festuca", "Artemisia", "LFP", "SGP", "TCP", "BSP"),
    plot_1 = c(18, 22, 8, 58, 64, 52, 18),
    plot_2 = c(20, 21, 9, 61, 66, 54, 16),
    plot_3 = c(19, 23, 7, 59, 65, 53, 17),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  sf_rep2 <- data.frame(
    label = c("Bouteloua", "Festuca", "Artemisia", "LFP", "SGP", "TCP", "BSP"),
    plot_1 = c(17, 24, 9, 56, 63, 51, 19),
    plot_2 = c(21, 20, 10, 60, 67, 55, 15),
    plot_3 = c(18, 22, 8, 57, 64, 52, 18),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  sf_year_2_rep1 <- data.frame(
    label = c("Bouteloua", "Festuca", "Artemisia", "LFP", "SGP", "TCP", "BSP"),
    plot_1 = c(22, 24, 7, 62, 70, 58, 12),
    plot_2 = c(23, 23, 6, 64, 72, 60, 11),
    plot_3 = c(21, 25, 7, 63, 71, 59, 12),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  sf_year_2_rep2 <- data.frame(
    label = c("Bouteloua", "Festuca", "Artemisia", "LFP", "SGP", "TCP", "BSP"),
    plot_1 = c(21, 25, 8, 61, 69, 57, 13),
    plot_2 = c(24, 22, 7, 65, 73, 61, 10),
    plot_3 = c(22, 24, 6, 62, 70, 58, 12),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  cyd <- data.frame(
    Xri = c(1, 2, 3, 4, 5, 2, 4, 5),
    Ygi = c(12, 18, 28, 34, 41, 19, 35, 42),
    Ydi = c(8, 13, 19, 24, 29, 14, 25, 30)
  )

  dsd <- data.frame(
    Xej = c(10, 15, 20, 25, 30, 18, 22, 28),
    Ygj = c(11, 16, 21, 27, 31, 19, 24, 29),
    Ydj = c(7, 10, 14, 18, 21, 12, 16, 20)
  )

  cwd <- data.frame(
    Xrk = c(1, 2, 3, 4, 5, 3),
    Xek = c(10, 15, 20, 25, 30, 22),
    Ydk = c(8, 12, 18, 23, 28, 17)
  )

  list(
    sf_single = sf_rep1,
    sf_replicates = list(rep1 = sf_rep1, rep2 = sf_rep2),
    sf_years = list(
      `2024` = list(rep1 = sf_rep1, rep2 = sf_rep2),
      `2025` = list(rep1 = sf_year_2_rep1, rep2 = sf_year_2_rep2)
    ),
    palatability = c(Bouteloua = 1, Festuca = 1, Artemisia = 3),
    cyd = cyd,
    dsd = dsd,
    cwd = cwd
  )
}
