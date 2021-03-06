#' Create One-Way Table
#'
#' `construct_table()` summarizes a given variable in a one-way table with
#' percentages. It is mostly a wrapper around \code{\link[janitor]{tabyl}()}
#' that allows more flexibility in ordering the output table.
#'
#' By default, `construct_table()` will order factor inputs by their level
#' and all other input by frequency. If `infreq = TRUE`, it will all input by
#' frequency; if `infreq = FALSE`, it will order all input alpha-numerically.
#' Note that the `.by` variable will be converted to a factor with levels
#' ordered according to the output table, regardless of input type or ordering.
#'
#' @param .data A \link[base:data.frame]{data frame}, data frame
#'   extension (e.g. a \link[tibble:tbl_df]{tibble}), or a lazy data frame (e.g.
#'   from dbplyr or dtplyr)
#'
#' @param .by The variable in `.data` to analyze; can be specified as a
#'   normal variable or as a string
#'
#' @param infreq Should the output be ordered by frequency? The default depends
#'   on the input type; see details.
#'
#' @param show_missing_levels Should all levels be shown, even if empty?
#'
#' @return A tibble holding the summary table
#'
#' @keywords internal
construct_table <- function(
  .data,
  .by,
  infreq = NULL,
  to_na = c("unknown", "missing", "NA", "N/A", "<NA>", "^$"),
  show_missing_levels = FALSE
) {

  # If .data is not a dataframe, coerce it to one
  if (!is.data.frame(.data)) {
    .data <- dplyr::as_tibble(.data)
  }

  # Make sure `.by` is a symbol
  .by <- rlang::enquo(.by)

  # Create predicate function for date type
  is_dt_dttm <- purrr::as_mapper(
    ~ lubridate::is.Date(.x) | lubridate::is.POSIXt(.x)
  )

  # By default, `infreq` should be FALSE if `.by` has inherent ordering or TRUE
  # otherwise
  if (rlang::is_empty(infreq)) {
    infreq <- .data %>%
      dplyr::pull({{ .by }}) %>%
      purrr::negate(~ is.numeric(.x) | is_dt_dttm(.x) | is.ordered(.x))()
  }

  # Create one-way table of `.by` variable
  .data %>%
    # Coerce `.by` to an appropriately ordered factor
    dplyr::transmute(
      {{ .by }} := {{ .by }} %>%
        purrr::when(
          # If `infreq` == TRUE, order by frequency of levels
          infreq ~ factor(.) %>% forcats::fct_infreq(),
          # If `.by` is already a factor, it's fine as-is
          is.factor(.) ~ .,
          # If `.by` is a date, coerce to ordered factor
          lubridate::is.Date(.) ~ factor(., ordered = TRUE),
          # Same if `.by` is datetime, but coerce to date first
          lubridate::is.POSIXt(.) ~ lubridate::as_datetime(.) %>% factor(ordered = TRUE),
          # If `.by` is other numeric, coerce to ordered in sequence
          is.numeric(.) ~ factor(.) %>% forcats::fct_inseq(ordered = TRUE),
          # Else coerce to factor with alphabetical ordering
          ~ factor(.)
        )
    ) %>%
    janitor::tabyl({{ .by }}) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange({{ .by }}) %>%
    # Change `NA` to "Missing", add N and valid_N
    dplyr::mutate(
      {{ .by }} := forcats::fct_explicit_na({{ .by }}, na_level = "Missing"),
      n = as.integer(.data[["n"]]),
      N = sum(.data[["n"]]),
      valid_N = purrr::when(
        "valid_percent" %in% colnames(.),
        any(.) ~ .data[["n"]] %>%
          extract(!is.na(.data[["valid_percent"]])) %>%
          sum(),
        ~ NULL
      )
    )
}

#' Create a One-Way Table from Multiple Variables
#'
#' `create_table()` summarizes a given variable in a one-way table with
#' percentages. It is mostly a wrapper around \code{\link[janitor]{tabyl}} that
#' allows more flexibility in ordering the output table. It is designed to
#' handle multiple variables at once using tidyselect helpers and is able to
#' define percentages based on total observations in wide (input) or long
#' (pivoted) form.
#'
#' By default, `create_table()` will order factor inputs by their level and
#' all other input by frequency. If `infreq = TRUE`, it will all input by
#' frequency; if `infreq = FALSE`, it will order all input alpha-numerically.
#' Note that the `.by` variable will be converted to a factor with levels
#' ordered according to the output table, regardless of input type or ordering.
#'
#' @inheritParams construct_table
#'
#' @param ... The variable(s) in \code{.data} to analyze; can be specified as
#'   normal (unquoted) variables, strings, or using tidyselect helpers (such as
#'   \code{\link[tidyselect]{starts_with}})
#'
#' @param to The name of the variable to "pivot" to; this defaults to the
#'   longest common prefix in the input variable names, or "value" if
#'   none exists
#'
#' @param infreq Should the output be ordered by frequency? The default depends
#'   on the input type; see details.
#'
#' @param total_wide Should the total used for percentages come from the
#'   number of input observations (wide) or the number of pivoted observations
#'   (long)? This only matters when selecting multiple variables with `...`.
#'
#' @param to_na A character vector of values that should be considered missing,
#'   as regular expressions. Case is ignored.
#'
#' @return The output of `tabyl()`, modified as above and coerced to a tibble
#'
#' @export
create_table <- function(
  .data,
  ...,
  to = NULL,
  infreq = NULL,
  total_wide = TRUE,
  to_na = c("unknown", "missing", "NA", "N/A", "<NA>", "^$"),
  show_missing_levels = FALSE
) {

  # Predicate for character or factor
  is_characterish <- purrr::as_mapper(~ purrr::is_character(.x) | is.factor(.x))

  # .data must be a data frame, so make sure it is
  # Selecting variables of interest is needed for multiple steps
  .data %>%
    purrr::when(
      is.data.frame(.) ~ .,
      ~ dplyr::as_tibble(.)
    ) %>%
    dplyr::select(...) %>%
    # Make sure there were matches
    assertr::verify(
      NCOL(.) > 0,
      error_fun = error_abort(
        "There are no variables matching the input for `...`"
      )
    ) %>%
    # Convert `to_na` values to NA and drop other variables
    dplyr::mutate(
      dplyr::across(
        where(purrr::is_character),
        .fns = ~ .x %>%
          stringr::str_replace_all(
            pattern = to_na %>%
              stringr::str_flatten(collapse = "|") %>%
              stringr::regex(ignore_case = TRUE),
            replacement = NA_character_
          )
      ),
      dplyr::across(
        where(is.factor),
        .fns = ~ .x %>%
          forcats::fct_relabel(
            ~ .x %>%
              stringr::str_replace_all(
                pattern = to_na %>%
                  stringr::str_flatten(collapse = "|") %>%
                  stringr::regex(ignore_case = TRUE),
                replacement = NA_character_
              )
          )
      )
    ) ->
  selected_data

  # Store missing and total info for adjusting percentages
  if (rlang::is_true(total_wide) & NCOL(selected_data) > 1) {
    n_total <- NROW(selected_data)
    n_missing <- selected_data %>%
      dplyr::filter(dplyr::across(.fns = ~ is.na(.x))) %>%
      NROW()
  }

  # If `to` is not specified, use the longest common prefix of the selected
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
    ) ->
  tabyl

  # Adjust percentages and totals
  if (rlang::is_true(total_wide) & NCOL(selected_data) > 1) {

    is_missing <- tabyl[[1]] %>%
      as.character() %>%
      stringr::str_detect(pattern = "^Missing$")

    dplyr::mutate(
      tabyl,
      # Replace missing `n` with `n_missing` from above
      n = purrr::when(
        any(is_missing),
        . ~ replace(.data[["n"]], which(is_missing), n_missing),
        ~ .data[["n"]]
      ),
      # Re-calculate percent
      percent = .data[["n"]] / n_total,
      # Replace `N`
      N = n_total,
      # Re-calculate valid_percent
      valid_percent = purrr::when(
        "valid_percent" %in% colnames(tabyl),
        rlang::is_true(.) ~ (.data[["n"]] / (n_total - n_missing)) %>%
          replace(list = which(is_missing), values = NA_real_),
        ~ NULL
      ),
      # Re-calculate valid_N
      valid_N = purrr::when(
        "valid_N" %in% colnames(tabyl),
        rlang::is_true(.) ~ rep(n_total - n_missing, times = NROW(tabyl)) %>%
          replace(list = which(is_missing), values = NA_real_),
        ~ NULL
      )
    )
  } else {
    tabyl
  }

}

#' Custom error function(s) for \code{\link[assertr]{verify}}
#'
#' `error_abort()` is an error function for use with the assertr package. It
#' allows issuing of a simple custom error message via
#' \code{\link[rlang]{abort}}.
#'
#' @param message The error message to print on failure
#'
#' @inheritParams assertr::error_stop
#'
#' @noRd
#'
#' @keywords internal
error_abort <- function(message = NULL, error, data) {
  rlang::abort(message = message)
}

