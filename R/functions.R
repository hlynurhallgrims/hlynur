prosenta <- function (x, accuracy = NULL, scale = 100, prefix = "", suffix = "%",
                      big.mark = " ", decimal.mark = ",", trim = TRUE, ...)
{
  scales::number(x = x, accuracy = accuracy, scale = scale, prefix = prefix,
                 suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark,
                 trim = trim, ...)
}

punktur <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",")
}

excel_round <- function(x, n = 0) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

cement <- function(...) {
  args <- rlang::ensyms(...)
  purrr::map_chr(args, rlang::as_string)
}

sauce <- function (x)
{
  if (stringr::str_detect(x, pattern = "\\.R$|\\.r$")) {
    source(x, encoding = "UTF-8")
  }
  else {
    source(knitr::purl(x, documentation = 0L, encoding = "UTF-8"),
           encoding = "UTF-8")
  }
}
