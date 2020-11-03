#' "Cut" a Numeric Variable into Categories
#'
#' \code{fct_cut} converts numeric variables to a factor using the \code{breaks}
#' provided. It is built on \code{\link[base]{cut}}, but does some additional
#' coercion before passing to \code{cut} and labels the output in more
#' lay-friendly manner. Note that intervals in \code{fct_cut} are closed on the
#' left (they include the lower value and exclude the higher - e.g.
#' \code{1 <= .x < 2}); this is the opposite of \code{cut}'s default behavior.
#'
#' @param .x A numeric vector, or a object that can be coerced to one. If
#'  \code{.x} is a factor, the level \emph{labels} will be coerced, not the
#'  underlying integer representation.
#'
#' @param breaks A numeric vector of break points for the categories of
#'   \code{.x}
#'
#' @param open Should the ends of \code{breaks} be treated as open or closed? In
#'   other words, should there be a category for values in \code{.x} below the
#'   lowest value of \code{breaks} (\code{.x < min(breaks)}) or above the
#'   highest value of \code{breaks} (\code{.x >= max(breaks)})? \code{TRUE}
#'   means both are open; \code{FALSE} means both are closed (the default).
#'   Behavior for each end can be controlled by passing a logical vector of
#'   length 2 where the first value controls the lower end and the second
#'   controls the upper end (e.g. \code{open = c(FALSE, TRUE)} for count data).
#'
#' @param ordered Should the resulting factor be ordered? Since numeric data is
#'  inherently ordered, the default is \code{TRUE}.
#'
#' @return A \code{factor}
#'
#' @export
fct_cut <- function(
  .x,
  breaks = quantile(.x),
  open = FALSE,
  ordered = TRUE
) {

  # Handle open as a vector
  open_lower <- open[[1]]
  open_upper <- tryCatch(open[[2]], error = function(err) open[[1]])

  # How should the lower endpoint be treated?
  if (open_lower & (min(breaks, na.rm = TRUE) <= min(.x, na.rm = TRUE))) {
    breaks[breaks == min(.x)] <- -Inf
  } else if (open_lower) {
    breaks %<>% append(values = -Inf, after = 0L)
  }

  # How should the upper endpoint be treated?
  if (open_upper & max(breaks, na.rm = TRUE) >= max(.x, na.rm = TRUE)) {
    breaks[breaks == max(.x)] <- Inf
  } else if (open_upper) {
    breaks %<>% append(values = Inf)
  }

  # Factors behave unexpectedly; must convert to character first
  if (is.factor(.x)) {
    .x %<>% as.character()
  }

  .x %>%
    as.numeric() %>%
    cut(breaks = breaks, right = FALSE, ordered_result = ordered) %>%
    forcats::fct_relabel(
      ~ .x %>%
        stringr::str_remove_all(stringr::coll("[")) %>%
        stringr::str_remove_all(stringr::coll("]")) %>%
        stringr::str_remove_all("[()]") %>%
        stringr::str_replace_all(pattern = "-Inf,", replacement = "<") %>%
        stringr::str_replace_all(pattern = ",Inf", replacement = "+") %>%
        stringr::str_replace_all(pattern = ",", replacement = "-")
    )
}
