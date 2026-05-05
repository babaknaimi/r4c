#' Read SF Sheets from an Excel Workbook
#'
#' Reads range-condition sheets from an Excel workbook. By default, the
#' function looks for replicated sheets named `sfd1`, `sfd2`, and so on. If
#' they are not present, it falls back to a single sheet named `sfd`.
#'
#' @param path Path to an Excel workbook.
#' @param sheets Optional character vector of sheet names to read.
#' @param col_names Passed to [readxl::read_excel()].
#'
#' @return A named list of data frames.
#' @export
read_sf_excel <- function(path, sheets = NULL, col_names = FALSE) {
  all_sheets <- readxl::excel_sheets(path)

  if (is.null(sheets)) {
    replicated <- grep("^sfd[0-9]+$", all_sheets, value = TRUE)
    if (length(replicated) > 0) {
      sheets <- replicated
    } else if ("sfd" %in% all_sheets) {
      sheets <- "sfd"
    } else {
      stop(
        "No SF sheet found. Expected 'sfd' or replicated sheets such as 'sfd1', 'sfd2', ...",
        call. = FALSE
      )
    }
  }

  out <- lapply(
    sheets,
    function(sheet) readxl::read_excel(path, sheet = sheet, col_names = col_names)
  )
  names(out) <- sheets
  out
}

#' Read a CY Sheet from an Excel Workbook
#'
#' @param path Path to an Excel workbook.
#' @param sheet Sheet name. Defaults to `"cyd"`.
#'
#' @return A data frame.
#' @export
read_cyd_excel <- function(path, sheet = "cyd") {
  readxl::read_excel(path, sheet = sheet)
}

#' Read a DS Sheet from an Excel Workbook
#'
#' @param path Path to an Excel workbook.
#' @param sheet Sheet name. Defaults to `"dsd"`.
#'
#' @return A data frame.
#' @export
read_dsd_excel <- function(path, sheet = "dsd") {
  readxl::read_excel(path, sheet = sheet)
}

#' Read a CW Validation Sheet from an Excel Workbook
#'
#' @param path Path to an Excel workbook.
#' @param sheet Sheet name. Defaults to `"re_cwd"`.
#'
#' @return A data frame.
#' @export
read_cwd_excel <- function(path, sheet = "re_cwd") {
  readxl::read_excel(path, sheet = sheet)
}

#' Read All Main r4c Inputs from an Excel Workbook
#'
#' @param path Path to an Excel workbook.
#' @param sf_sheets Optional SF sheet names. Defaults to auto-detection.
#' @param cyd_sheet Name of the CY sheet.
#' @param dsd_sheet Name of the DS sheet.
#' @param cwd_sheet Name of the optional CW validation sheet.
#' @param col_names_sf Passed to [read_sf_excel()].
#' @param col_names_capacity Passed to [readxl::read_excel()] for CY, DS, and CW.
#'
#' @return A named list with elements `sf`, `cyd`, `dsd`, and `cwd`.
#' @export
read_r4c_excel <- function(
    path,
    sf_sheets = NULL,
    cyd_sheet = "cyd",
    dsd_sheet = "dsd",
    cwd_sheet = "re_cwd",
    col_names_sf = FALSE,
    col_names_capacity = TRUE) {
  all_sheets <- readxl::excel_sheets(path)

  read_optional <- function(sheet) {
    if (!sheet %in% all_sheets) {
      return(NULL)
    }
    readxl::read_excel(path, sheet = sheet, col_names = col_names_capacity)
  }

  list(
    sf = read_sf_excel(path, sheets = sf_sheets, col_names = col_names_sf),
    cyd = read_optional(cyd_sheet),
    dsd = read_optional(dsd_sheet),
    cwd = read_optional(cwd_sheet)
  )
}

#' Merge Replicated SF Tables
#'
#' Merges replicated SF tables by applying a cell-wise summary function to the
#' numeric measurement columns while preserving the label column.
#'
#' @param sf_list A list of data frames with identical structure.
#' @param fun Summary function used for the cell-wise merge. Defaults to [mean].
#' @param na.rm Passed to `fun`.
#' @param label_col Optional label column index. If omitted, the first
#'   non-numeric column is treated as the label column.
#'
#' @return A merged data frame.
#' @export
merge_sf_replicates <- function(sf_list, fun = mean, na.rm = TRUE, label_col = NULL) {
  stopifnot(is.list(sf_list), length(sf_list) >= 1)

  if (length(sf_list) == 1) {
    return(as.data.frame(sf_list[[1]], stringsAsFactors = FALSE, check.names = FALSE))
  }

  parts <- lapply(sf_list, split_sf_table, label_col = label_col)
  reference <- parts[[1]]

  for (part in parts[-1]) {
    if (!identical(reference$labels, part$labels)) {
      stop("All SF tables must contain the same labels in the same order.", call. = FALSE)
    }
    if (ncol(reference$values) != ncol(part$values)) {
      stop("All SF tables must have the same number of numeric measurement columns.", call. = FALSE)
    }
  }

  arr <- simplify2array(lapply(parts, function(part) as.matrix(part$values)))
  merged_values <- apply(arr, c(1, 2), fun, na.rm = na.rm)
  merged_values <- as.data.frame(merged_values, check.names = FALSE)
  names(merged_values) <- names(reference$values)

  data.frame(
    stats::setNames(list(reference$labels), reference$label_name),
    merged_values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @rdname read_sf_excel
#' @export
read_sfd_excel <- read_sf_excel

#' @rdname merge_sf_replicates
#' @export
merge_sfd_rep <- merge_sf_replicates

#' @rdname read_cyd_excel
#' @export
read_ccd_excel <- read_cyd_excel

#' @rdname read_dsd_excel
#' @export
read_dpd_excel <- read_dsd_excel
