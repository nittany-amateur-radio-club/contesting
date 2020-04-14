read_adif <- function(fn) {
  if (!file.exists(fn)) stop("file '", fn, "' not found.")
  readtext(file = fn, )
}

extract_qsos_from_adif <- function(adif) {
  unlist(stringr::str_match_all(adif, "<.*<eor>\n"))
}

extract_qso_fields <- function(qso_line) {
  if (!is.character(qso_line)) stop("'qso_line' must be character.")
  field_label_value_regex <- "<([a-z]+[_a-z]*):([0-9]+)>([\\-\\+]?[a-zA-Z0-9/\\.]+)"
  df <- as.data.frame(stringr::str_match_all(qso_line, field_label_value_regex))
  
  if (is.null(df)) {
    stop("No fields matched.")
  }
  
  if (dim(df)[2] != 4) {
    stop("Extracted data frame does not have expected number of columns.")
  }
  df[, c(2,4)]
}

make_qso_wide <- function(qso_df) {
  tidyr::pivot_wider(qso_df, names_from = X2, values_from = X4)
}

make_wide_qso_df <- function(qso_line) {
  df <- extract_qso_fields(qso_line)
  make_qso_wide(df)
}

make_qsos_df <- function(adif) {
  qsos <- extract_qsos_from_adif(adif)
  purrr::map_df(qsos, make_wide_qso_df)
}

convert_db_to_rst <- function(rpt) {
  if (is.na(rpt) || rpt > 500) {
    as.character(rpt)
  } else {
    noise_floor <- -26
    db_to_s_units <- 6
    paste0("5", round((rpt - noise_floor)/6), "9")     
  }
}
