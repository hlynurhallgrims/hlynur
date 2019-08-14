#' Sources R and RMD files in UTF-8
#'
#' @param x
#'
#' @description
#' An alternative to sourcing a file, is saucing it.
#'
#' @return
#' @export
#'
#' @examples
sauce <- function(x) {
  if (stringr::str_detect(x, pattern = "\\.R$|\\.r$")) {
    source(x, encoding = "UTF-8")
  } else {
    source(knitr::purl(x, documentation = 0L, encoding = "UTF-8"), encoding = "UTF-8")
  }
}
