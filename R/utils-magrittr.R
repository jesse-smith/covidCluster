#' Magrittr Pipe Helpers
#'
#' The magrittr pipe is a common programming tool in \strong{R} that often makes
#' code much easier to read. In addition to the primary pipe operator, magrittr
#' offers several additional pipes, as well as functions to help perform common
#' infix-operations in a pipe-friendly manner.
#'
#' @md
#' @name magrittr_utils
#' @keywords internal
#' @importFrom magrittr extract extract2 inset inset2 use_series add subtract
#'   multiply_by raise_to_power multiply_by_matrix divide_by divide_by_int
#'   mod is_in and or equals is_greater_than is_weakly_greater_than is_less_than
#'   not set_colnames set_rownames set_names %>% %<>% %T>% %$%
NULL

# Suppress "no visible binding for global variable" when using `.`
if (getRversion() >= "2.15.1") utils::globalVariables(".")
