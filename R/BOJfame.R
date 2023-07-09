if (getRversion() >= "2.15.1") utils::globalVariables(c("obs_value"))

# Download a file
.download_file <- function(file_url, ...) {
  # Save user options
  old_options <- options()

  # Restore user options on function exit
  on.exit(options(old_options))

  # Force minimum timeout of 300 for file download
  options(timeout = max(300, getOption("timeout")))

  file_path <- tryCatch({
    # Prepare temp file
    file_ext <- tools::file_ext(file_url)
    file_ext <- ifelse(file_ext == "", "", paste0(".", file_ext))
    tmp_file <- tempfile(fileext = file_ext)

    # Download data and store in temp file
    utils::download.file(file_url, tmp_file, mode = "wb")

    # Return path to temp tile
    file_path <- tmp_file

    file_path
  },
  error = function(x) {
    message(paste("Unable to download file:", file_url))
    message("The resource is unavailable or has changed.")
    message("To download large files, try increasing the download timeout:")
    message("options(timeout = 600)")
    message("Original error message:")
    message(x)
    return(NA)
  },
  warning = function(x) {
    message(paste("Unable to download file:", file_url))
    message("The resource is unavailable or has changed.")
    message("To download large files, try increasing the download timeout:")
    message("options(timeout = 600)")
    message("Original warning message:")
    message(x)
    return(NA)
  }
  )

  return(file_path)
}

#' Convert a BOJ FAME data set to long format
#'
#' @param tbl Tibble. A tibble data frame holding a BOJ FAME data set (usually
#' obtained via \code{get_bojfame(series_codes, auto_pivot = FALSE)}).
#'
#' @return A tibble data frame.
#' @export
#'
#' @examples
#' \donttest{
#' bojdata <- get_bojfame(series_codes = c("FM01'STRDCLUCON", "IR01'MADR1Z@D"),
#'                        auto_pivot = FALSE)
#' bojdata$data <- pivot_longer_bojfame(bojdata$data)
#' }
pivot_longer_bojfame <- function(tbl) {
  excl_cols <- "date"
  tbl <- tidyr::pivot_longer(data = tbl, cols = -tidyselect::all_of(excl_cols),
                             names_to = "indicator", values_to = "obs_value")
  tbl <- dplyr::mutate(tbl, obs_value = as.numeric(obs_value))

  return(tbl)
}

# Parse a BOJ FAME data set
.parse_bojfame <- function(file_path, series_codes, auto_pivot) {
  # Read CSV file (start from line 14 to skip meta data and empty lines)
  tbl <- readr::read_csv(file_path, skip = 13,
                         col_names = c("date", series_codes))

  # Read meta data (content up to line 9, excluding empty lines)
  tbl_meta <- readr::read_csv(file_path, n_max = 9,
                              col_names = c("Attribute", series_codes))

  # Pivot data from wide to long format
  if (auto_pivot) {
    tbl <- pivot_longer_bojfame(tbl)
  }

  return(list(data = tbl, meta = tbl_meta))
}

#' Download and parse data from BOJ Time-Series Data Search portal
#'
#' @param series_codes Character vector. Contains the series code(s) to be
#' retrieved. Note that all series must be of the same frequency.
#' @param start_year Numeric. Start year in format: YYYY (optional).
#' @param end_year Numeric. End year in format: YYYY (optional).
#' @param base_url Character. URL to generate BOJ data download page (optional).
#' @param auto_pivot Logical. Controls whether source data set is converted to
#' long format. Set this to \code{FALSE} to disable conversion (default: TRUE).
#' @param ... Arguments passed to \code{download.file()} (e.g.
#' \code{quiet = TRUE}).
#'
#' @return A list containing the time series data (\code{$data}), corresponding
#' meta data (\code{$meta}), the download page URL (\code{$page_url}), and the
#' CSV file URL ((\code{$item_url})).
#' @export
#'
#' @details If \code{start_year} and \code{end_year} are missing, the last 5
#' years of data (for daily series, the last year) will be downloaded.
#'
#' @examples
#' \donttest{
#' df <- get_bojfame(series_codes = c("FM01'STRDCLUCON", "IR01'MADR1Z@D"))
#' }
get_bojfame <- function(series_codes, start_year = NULL, end_year = NULL,
                        base_url = NULL, auto_pivot = TRUE, ...) {
  # Set base_url unless given
  if (is.null(base_url)) {
    base_url <- paste0("https://www.stat-search.boj.or.jp/ssi/cgi-bin/famecgi2",
                       "?cgi=%24nme_r030_en&chkfrq=DD&rdoheader=DETAIL",
                       "&rdodelimitar=COMMA&sw_freq=NONE&sw_yearend=NONE",
                       "&sw_observed=NONE")
  }

  # Append start year and end year to URL
  base_url <- paste0(base_url, "&hdnYyyyFrom=")
  if (!is.null(start_year)) {
    base_url <- paste0(base_url, as.character(as.numeric(start_year)))
  }

  base_url <- paste0(base_url, "&hdnYyyyTo=")
  if (!is.null(end_year)) {
    base_url <- paste0(base_url, as.character(as.numeric(end_year)))
  }

  # Append series codes to URL
  series_enc <- urltools::url_encode(series_codes)
  series_enc <- paste0("hdncode=", series_enc, collapse = "&")
  page_url   <- paste0(base_url, "&", series_enc)

  # Read download page
  page <- tryCatch({
    page <- xml2::read_html(page_url)

    page
  },
  error = function(x) {
    message(paste("Unable to download and parse homepage:", base_url))
    message("The resource is unavailable or has changed.")
    message("Original error message:")
    message(x)
    return(NA)
  },
  warning = function(x) {
    message(paste("Unable to download and parse homepage:", base_url))
    message("The resource is unavailable or has changed.")
    message("Original warning message:")
    message(x)
    return(NA)
  }
  )

  # Parse download page
  nodes     <- rvest::html_nodes(page, xpath = "//a[contains(@href, 'csv')]")
  item_url  <- xml2::url_absolute(rvest::html_attr(nodes, "href"), base_url)

  # Stop in case of error
  if (grepl("We are very sorry for being unable to display the page.",
            as.character(page)) ||
      (length(nodes) < 1)) {
    stop(paste0("Not found\n",
                "Common causes: One or more of your series codes may be ",
                "invalid, or you may be combining series with different ",
                "frequencies."))
  }

  # Download CSV file
  try(csv_file_path <- .download_file(item_url, ...), TRUE)

  # Read file and return data
  try(return(.parse_bojfame(file_path = csv_file_path,
                            series_codes = series_codes,
                            auto_pivot = auto_pivot)), TRUE)
}
