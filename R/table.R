#' Create One-Way Table
#'
#' \code{create_table} summarizes a given variable in a one-way table with
#' percentages. It is mostly a wrapper around \code{\link[janitor]{tabyl}} that
#' allows more flexibility in ordering the output table.
#'
#' By default, \code{create_table} will order factor inputs by their level and
#' all other input by frequency. If \code{infreq == TRUE}; if
#' \code{infreq == FALSE}, it will order alpha-numerically. Note that the
#' \code{.by} variable will be converted to a factor with levels ordered by the
#' output table, regardless of input type or ordering.
#'
#' @param .data A dataframe or tibble in tidy format
#'
#' @param .by The variable in \code{.data} to analyze; can be specified as a
#'   normal variable or as a string
#'
#' @param infreq Should the output be ordered by frequency? The default depends
#'   on the input type; see details.
#'
#' @param show_missing_levels Should all levels be shown, even if empty?
#'
#' @param to_NA A character vector of values that should be considered missing
#'
#' @return A \code{\link[tibble]{tibble}} holding the summary table
construct_table <- function(
  .data,
  .by,
  infreq = NULL,
  to_NA = c("unknown", "missing", "NA", "N/A", ""),
  show_missing_levels = FALSE
) {

  # If .data is not a dataframe, coerce it to one
  if (!is.data.frame(.data)) {
    .data <- dplyr::as_tibble(.data)
  }

  # Make sure `.by` is a symbol
  .by <- rlang::enquo(.by)

  # By default, `infreq` should be FALSE if `.by` is a factor and TRUE otherwise
  if (rlang::is_empty(infreq)) {
    infreq <- .data %>% dplyr::pull({{ .by }}) %>% purrr::negate(is.factor)()
  }

  # Replace empty string in `to_NA` with "<NA>", b/c this is how it appears in
  # the coerced factor
  to_NA <- stringr::str_replace_all(to_NA, pattern = "^$", replacement = "<NA>")

  # Create one-way table of `.by` variable
  .data %>%
    # Coerce `.by` to an appropriately ordered factor
    dplyr::mutate(
      {{ .by }} := purrr::when(
        {{ .by }},
        # If `infreq` == TRUE, order by frequency of levels
        infreq ~ factor(.) %>% forcats::fct_infreq(ordered = TRUE),
        # Else if `.by` is already a factor, keep its ordering but ensure
        # `ordered` is true
        is.factor(.) ~ as.ordered(.),
        # Else coerce to factor with alphanumeric ordering
        ~ factor(.)
      )
    ) %>%
    dplyr::mutate(
      {{ .by }} := forcats::fct_relabel(
        {{ .by }},
        ~ gsub(
          pattern = stringr::str_flatten(to_NA, collapse = "|"),
          replacement = NA,
          x = .x,
          ignore.case = TRUE
        )
      ) %>%
        droplevels()
    ) %>%
    janitor::tabyl({{ .by }}) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange({{ .by }}) %>%
    # Change `NA` to "Missing"
    dplyr::mutate(
      {{ .by }} := forcats::fct_explicit_na({{ .by }}, na_level = "Missing")
    )
}

#' Create a One-Way Table from Multiple Variables
#'
#' \code{create_table} isdesigned to "pivot" the input into long format before summarizing. This is
#' most useful when you'd like to treat multiple variables as a single variable
#' in the summary table.
#'
#' \code{create_table} summarizes a given variable in a one-way table with
#' percentages. It is mostly a wrapper around \code{\link[janitor]{tabyl}} that
#' allows more flexibility in ordering the output table. It is designed to
#' handle multiple variables at once using tidyselect helpers.
#'
#' By default, \code{create_table} will order factor inputs by their level and
#' all other input by frequency. If \code{infreq == TRUE}; if
#' \code{infreq == FALSE}, it will order alpha-numerically. Note that the
#' \code{.by} variable will be converted to a factor with levels ordered by the
#' output table, regardless of input type or ordering.
#'
#' @param .data A dataframe or tibble in tidy format
#'
#' @param ... The variable(s) in \code{.data} to analyze; can be specified as
#'   normal variables, strings, or using tidyselect helpers (such as
#'   \code{\link[tidyselect]{starts_with}})
#'
#' @param to The name of the variable to "pivot" to; this defaults to the
#'   longest common starting string in the input variable names, or "value" if
#'   none exists
#'
#' @param infreq Should the output be ordered by frequency? The default depends
#'   on the input type; see details.
#'
#' @param to_NA A character vector of values that should be considered missing
#'
#' @param show_missing_levels Should all levels be shown, even if empty?
#'
#' @return A \code{\link[janitor]{tabyl}} in \code{\link[tibble]{tibble}} format
#'
#' @export
create_table <- function(
  .data,
  ...,
  to = NULL,
  infreq = NULL,
  to_NA = c("unknown", "missing", "NA", "N/A", ""),
  show_missing_levels = FALSE
) {

  # .data must be a data frame, so make sure it is
  # Selecting variables of interest is needed for multiple steps
  .data %>%
    purrr::when(
      is.data.frame(.) ~ .,
      ~ dplyr::as_tibble(.)
    ) %>%
    dplyr::select(...) ->
  selected_data

  # If `to` is not specified, use the longest common substring of the selected
  # column names. If there is none, use "value".
  if (rlang::is_empty(to)) {
    prefix <- selected_data %>%
      colnames() %>%
      hutils::longest_prefix(warn_if_no_prefix = FALSE) %>%
      stringr::str_remove_all(pattern = "[^a-zA-Z0-9]*$") %>%
      janitor::make_clean_names()
    to <- if (prefix == "") "value" else prefix
  }

  # Convert `to` to an expression; this allows flexibility in evaluation and
  # coercion of `to`
  to <- rlang::expr(!!rlang::ensym(to))

  # Pivot values and create table
  selected_data %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      values_to = rlang::expr_name(to)
    ) %>%
    construct_table(
      !!to,
      infreq = infreq,
      show_missing_levels = show_missing_levels
    )
}
