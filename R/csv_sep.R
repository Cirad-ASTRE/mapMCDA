#' Detection of separator in csv files
#' 
#' Returns the separator that consistently separates the lines in the same
#' number of fields, or fails.
#'
#' Tested separators are: ",", " ", "\\t", "|", ";", ":"
#' @param x character. File name.
#'
#' @return Character representation of the field separator.
#' @export
#' @importFrom utils count.fields
#'
#' @examples
#'   tf <- tempfile()
#'   td <- data.frame(T = LETTERS[1:10], N = 1:10)
#'   write.csv(td, tf, row.names = FALSE)
#'   csv_sep(tf)  # ","
#'   write.csv2(td, tf, row.names = FALSE)
#'   csv_sep(tf)  # ";"
csv_sep <- function(x) {
  header <- readLines(x, n = 1L)
  
  common_seps <- c(",", " ", "\t", "\\|", ";", ":")
  
  ## Separators present in the header (first line)
  seps_in_header <- common_seps[vapply(common_seps, grepl, TRUE, header)]

  all_identical <- function(x) all(vapply(x[-1], identical, TRUE, x[1]))
  
  ## Whether all lines have the same number of fields, assuming each of the
  ## seps_in_header
  nfields_sep <- 
    vapply(
      seps_in_header,
      function(s) all_identical(utils::count.fields(x, sep = s)),
      TRUE
    )

  if (sum(nfields_sep) != 1)
    stop("Cannot determine field separator of ", x)

  ## Unescape | if necessary
  names(nfields_sep) <- gsub("\\\\", "", names(nfields_sep))
  
  return(names(nfields_sep)[nfields_sep])

}
