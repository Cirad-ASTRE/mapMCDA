#' Remove filename extension
#'
#' Removes the trailing characters after and including the last point in a
#' string.
#' 
#' @param x Character
#'
#' @return character.
#' @export
#'
#' @examples
#'   rmext("some/long.filename.extension")
rmext <- function(x) {
  gsub("\\.\\w{1,}$", "", x)
}