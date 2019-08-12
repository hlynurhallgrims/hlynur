#' @title Cleans character strings
#'
#' @description
#' This is a variant on \code{\link[janitor]{clean_names}}. It produces strings that consist only of the \code{_} character, numbers, and letters. It differs from its parent function in that it neither avoids duplicates nor strings starting with a numeral.
#' Capitalization preferences can be specified using the \code{case} parameter.
#'
#' Accented characters are
#' transliterated to ASCII.  For example, an "o" with a German umlaut over it becomes "o", and the Spanish character "enye" becomes "n".
#'
#'
#' @param string the input string.
#' @param case The desired target case (default is \code{"snake"}), indicated by these possible values:
#' \itemize{
#'  \item{\code{"snake"} produces snake_case}
#'  \item{\code{"lower_camel"} or \code{"small_camel"} produces lowerCamel}
#'  \item{\code{"upper_camel"} or \code{"big_camel"} produces UpperCamel}
#'  \item{\code{"screaming_snake"} or \code{"all_caps"} produces ALL_CAPS}
#'  \item{\code{"lower_upper"} produces lowerUPPER}
#'  \item{\code{"upper_lower"} produces UPPERlower}
#'  \item{\code{"parsed"}, \code{"mixed"}, \code{"none"}: less-common cases offered by \code{snakecase::to_any_case}.  See \code{\link[snakecase]{to_any_case}} for details.}
#'  }
#'
#' @return Returns the clean string.
#' @export
#' @examples
#' # not run:
#' # clean_string(" Þetta: er hræðilegur textastrengur")


clean_string <- function (string, case = c("snake", "lower_camel", "upper_camel",
                                           "screaming_snake", "lower_upper", "upper_lower", "all_caps",
                                           "small_camel", "big_camel", "parsed", "mixed"))
{
  case <- match.arg(case)

  old_string <- string
  new_string <- old_string %>%
    gsub("'", "", .) %>%
    gsub("\"", "", .) %>%
    gsub("%", ".percent_", .) %>%
    gsub("#", ".number_", .) %>%
    gsub("^[[:space:][:punct:]]+", "", .) %>%
    snakecase::to_any_case(case = case, sep_in = "\\.", transliterations = c("Latin-ASCII"),
                           parsing_option = 1)

  new_string
}
