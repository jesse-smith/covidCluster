#' Create Pie Chart from Table
#'
#' \code{create_pie} takes output from one of the \code{covidCluster::*_table}
#' functions and produces a pie chart.
#'
#' @param .table The output of \code{\link{create_table}}, which is a one-way
#'   \code{\link[janitor]{tabyl}} encoded to assist visualization
#'
#' @param .by The variable to chart; defaults to the first column of
#'   \code{.table}
#'
#' @param highlight A character vector specifying names of categories to
#'   highlight; if provided, these will be colored using `palette` and the
#'   remaining categories will be gray
#'
#' @param incl_missing Should the chart include missing values as a category?
#'
#' @param title The title of the chart; defaults to a title-case version of the
#'   variable name
#'
#' @param legend Should the chart have a legend?
#'
#' @param legend_title The title of the chart legend if `legend = TRUE`;
#' defaults to the chart title. This is ignored if `legend = FALSE`
#'
#' @param palette The color palette to use; this is a material design color
#'   palette generated by \code{\link[ggsci]{pal_material}}; see that
#'   function for details and available choices
#'
#' @param bottom_legend Should the legend be positioned at the bottom or on the
#'   right side?
#'
#' @param background What should the background color be? Defaults to a light gray.
#'
#' @param dodge Change the position of the labels along the radius of the
#'   circle; useful for separating overlapping labels. A value of \code{0}
#'   leaves the labels at the mid-point of the pie chart radius; a value of
#'   \code{1} spaces them evenly across the radius. Values in-between \code{0}
#'   and \code{1} spread the labels out across that fraction of the radius
#'   (centered on the midpoint).
#'
#' @return A ggplot object
#'
#' @export
plot_pie <- function(
  .table,
  .by = NULL,
  highlight = NULL,
  incl_missing = FALSE,
  title = NULL,
  legend_title = title,
  palette = c(
    "blue", "red", "pink", "purple", "deep-purple", "indigo", "light-blue",
    "cyan", "teal", "green", "light-green", "lime", "yellow", "amber", "orange",
    "deep-orange", "brown", "grey", "blue-grey"
  ),
  legend = FALSE,
  bottom_legend = TRUE,
  background = "gray93",
  dodge = 0
) {

  # Make sure `.by` is a symbol; defaults to the summarized variable
  if (rlang::is_empty(.by)) {
    .by <- .table %>%
      colnames() %>%
      extract2(1) %>%
      rlang::sym()
  } else {
    .by <- rlang::ensym(.by)
  }

  # Remove missing data, if desired
  if (rlang::is_false(incl_missing)) {
    .table %>%
      dplyr::filter(!!.by != "Missing") ->
      .table
  }

  # Set title to title-case `.by` if NULL
  if (rlang::is_empty(title)) {
    title <- rlang::as_name(.by) %>% janitor::make_clean_names(case = "title")
  }

  # Prep for plotting
  .table %>%
    # Drop unnecessary variables
    dplyr::transmute(
      {{ .by  }},
      n,
      N,
      # Force valid_percent
      valid_percent = purrr::when(
        "valid_percent" %in% colnames(.),
        any(.) ~ valid_percent,
        ~ percent
      ),
      # Force valid_N
      valid_N = purrr::when(
        "valid_N" %in% colnames(.),
        any(.) ~ valid_N,
        ~ N
      ),
      # Choose pct based on `incl_missing`
      pct = purrr::when(
        rlang::is_true(incl_missing),
        . ~ percent,
        ~ valid_percent
      )
    ) ->
  .table

  # Create color palettes
  pal <- ggsci::pal_material(
    palette = palette[[1]],
    n = NROW(.table) + 1,
    reverse = TRUE
  )

  gray_pal <- ggsci::pal_material(
    palette = "grey",
    n = NROW(.table) + 1,
    reverse = TRUE
  )

  lvl_names <- .table %>% dplyr::pull({{ .by }}) %>% unique() %>% as.character()

  if (rlang::is_empty(highlight)) {
    color_scale <- pal(NROW(.table)) %>% set_names(lvl_names)
  } else {
    hlt    <- lvl_names[lvl_names %in% highlight]
    no_hlt <- lvl_names[!lvl_names %in% highlight]

    color_scale <- c(pal(NROW(hlt)), gray_pal(NROW(.table))[NROW(hlt):NROW(.table) + 1]) %>%
      set_names(c(hlt, no_hlt)) %>%
      extract(order(lvl_names))
  }

  # Create in-plot labels for legend = FALSE
  by_label <- purrr::when(
    rlang::is_true(legend),
    . ~ NULL,
    ~ .table %>% dplyr::pull({{ .by }}) %>% paste0("<br>")
  )

  # Create plot
  plt <- ggplot2::ggplot(
    .table,
    ggplot2::aes(x = 1, y = .data[["pct"]], fill = !!.by)
  ) +
    ggplot2::geom_col(
      width = 1,
      position = "fill",
      alpha = 0.875,
      show.legend = legend
    ) +
    ggtext::geom_richtext(
      ggplot2::aes(
        y = cumsum(rev(.data[["pct"]])) - 0.5*rev(.data[["pct"]]),
        label = paste0(
          "**",
          rev(by_label),
          rev(.data[["n"]]),
          "** ",
          "(", round(100*rev(.data[["pct"]])), "%)"
        ),
        color = rev(!!.by),
      ),
      size = 5,
      fill = background,
      position = ggplot2::position_dodge2(width = dodge),
      show.legend = FALSE
    ) +
    ggplot2::coord_polar("y", start = 0, direction = 1) +
    ggplot2::scale_color_manual(
      name = legend_title,
      values = color_scale,
      aesthetics = c("color", "fill")
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = paste0(
        "Responded = ", .table[["valid_N"]][[1]]
      ),
      caption  = paste0(
        "Did not respond = ", .table[["N"]][[1]] - .table[["valid_N"]][[1]]
      )
    ) +
    ggthemes::theme_fivethirtyeight() +
    ggplot2::theme(
      line                  = ggplot2::element_blank(),
      axis.text             = ggplot2::element_blank(),
      legend.background     = ggplot2::element_rect(
        fill  = background,
        color = NA
      ),
      legend.key            = ggplot2::element_rect(
        fill  = background,
        color = NA
      ),
      legend.text           = ggplot2::element_text(
        color = "gray30",
        size  = 14
      ),
      legend.title          = ggplot2::element_text(
        face  = "bold",
        color = "grey30",
        size  = 18
      ),
      legend.box.background = ggplot2::element_rect(
        fill  = background,
        color = NA
      ),
      panel.background      = ggplot2::element_rect(
        fill  = background,
        color = NA
      ),
      panel.border          = ggplot2::element_blank(),
      panel.grid.major      = ggplot2::element_blank(),
      plot.background       = ggplot2::element_rect(
        fill  = background,
        color = NA
      ),
      plot.title            = ggplot2::element_text(
        face  = "bold",
        color = "gray30",
        size  = 28,
        hjust = 0.5
      ),
      plot.subtitle         = ggplot2::element_text(
        face  = "bold",
        size  = 18,
        hjust = 0.5
      ),
      plot.caption          = ggplot2::element_text(
        face  = "bold",
        size  = 14,
        hjust = 0.5
      )
    )

  if (rlang::is_false(bottom_legend)) {
    plt <- plt + ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical"
    )
  }

  attr(plt, which = "background") <- background

  plt
}

#' Create Bar Chart from Table
#'
#' \code{create_bar} takes output from one of the \code{*_table} functions and
#' produces a bar chart.
#'
#' @param .table The output of \code{\link{create_table}} which is a one-way
#'   \code{\link[janitor]{tabyl}} encoded to assist visualization
#'
#' @param .by The variable to chart; defaults to the first column of
#'   \code{.table}, which will always be correct if the output is from a
#'   \code{*_table} function
#'
#' @param incl_missing Should the chart include missing values as a category?
#'
#' @param title The title of the chart; defaults to a title-case version of the
#'   variable name
#'
#' @param x_lab Label for the x-axis; defaults to no label
#'
#' @param y_lab Label for the y-axis; defaults to no label
#'
#' @param legend Should a legend be displayed? Bar charts should usually not
#'   require a legend, so the default is \code{FALSE}
#'
#' @param legend_title The title of the chart legend; defaults to the chart
#'   title
#'
#' @param bottom_legend Should the legend be positioned at the bottom or on the
#'   right side? Default is bottom.
#'
#' @param palette The color palette to use; this is a material design color
#'   palette generated by \code{\link[ggsci]{pal_material}}; see that
#'   function for details and available choices
#'
#' @param background What should the background color be? Defaults to a light
#'   gray to lessen eye strain.
#'
#' @return A ggplot object
#'
#' @export
plot_bar <- function(
  .table,
  .by = NULL,
  highlight = NULL,
  incl_missing = FALSE,
  title = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend = FALSE,
  legend_title = title,
  bottom_legend = TRUE,
  palette = c(
    "blue", "red", "pink", "purple", "deep-purple", "indigo", "light-blue",
    "cyan", "teal", "green", "light-green", "lime", "yellow", "amber", "orange",
    "deep-orange", "brown", "grey", "blue-grey"
  ),
  background = "gray93"
) {

  # Make sure `.by` is a symbol; defaults to the summarized variable
  if (rlang::is_empty(.by)) {
    .by <- .table %>%
      colnames() %>%
      extract2(1) %>%
      rlang::sym()
  } else {
    .by <- rlang::ensym(.by)
  }

  # Remove missing data, if desired
  if (rlang::is_false(incl_missing)) {
    .table %>%
      dplyr::filter(!!.by != "Missing") ->
      .table
  }

  # Create color palettes
  pal <- ggsci::pal_material(
    palette = palette[[1]],
    n = 5,
    reverse = TRUE
  )(1) %>%
    rep(times = NROW(.table))

  gray_pal <- ggsci::pal_material(
    palette = "grey",
    n = 5,
    reverse = TRUE
  )(2)[[2]] %>%
    rep(times = NROW(.table))

  lvl_names <- .table %>% dplyr::pull({{ .by }}) %>% unique() %>% as.character()

  if (rlang::is_empty(highlight)) {
    color_scale <- pal %>% set_names(lvl_names)
    text_color <- "gray30"
  } else {
    hlt    <- lvl_names[lvl_names %in% highlight]
    no_hlt <- lvl_names[!lvl_names %in% highlight]

    color_scale <- c(pal[1:NROW(hlt)], gray_pal[1:NROW(no_hlt)]) %>%
      set_names(c(hlt, no_hlt)) %>%
      extract(order(lvl_names))

    text_color <- gray_pal %>%
      set_names(lvl_names) %>%
      replace(hlt, pal[[1]]) %>%
      unname()
  }

  # Set title to title-case `.by` if NULL
  if (rlang::is_empty(title)) {
    title <- rlang::as_name(.by) %>% janitor::make_clean_names(case = "title")
  }

  # Prep for plotting
  .table %>%
    # Drop unnecessary variables
    dplyr::transmute(
      {{ .by  }},
      n,
      N,
      # Force valid_percent
      valid_percent = purrr::when(
        "valid_percent" %in% colnames(.),
        any(.) ~ valid_percent,
        ~ percent
      ),
      # Force valid_N
      valid_N = purrr::when(
        "valid_N" %in% colnames(.),
        any(.) ~ valid_N,
        ~ N
      ),
      # Choose pct based on `incl_missing`
      pct = purrr::when(
        rlang::is_true(incl_missing),
        . ~ percent,
        ~ valid_percent
      )
    ) ->
  .table

  # Create plot
  plt <- ggplot2::ggplot(
    .table,
    ggplot2::aes(x = !!.by, color = !!.by, fill = !!.by)
  ) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data[["valid_N"]]),
      show.legend = legend,
      fill = "gray30",
      alpha = 0.1
    ) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data[["n"]]),
      show.legend = legend,
      color = NA,
      alpha = 2/3
    ) +
    ggtext::geom_textbox(
      ggplot2::aes(
        y = .data[["n"]],
        label = paste0(
          "**", .data[["n"]], "** ",
          "(", round(100*.data[["pct"]]), "%)"
        )
      ),
      maxwidth = ggplot2::unit(0.9^2/NROW(.table), units = "npc"),
      vjust = 0,
      halign = 0.5,
      size = 14 * 0.35278,
      fill = background,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      name = legend_title,
      values = color_scale,
      aesthetics = c("color", "fill")
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1.1*.table[["valid_N"]][[1]])) +
    ggplot2::labs(
      title = title,
      subtitle = paste0(
        "Responded = ", .table[["valid_N"]][[1]]
      ),
      caption = paste0("Did not respond = ", .table[["N"]][[1]] - .table[["valid_N"]])
    ) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggthemes::theme_fivethirtyeight(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 28,
        face = "bold",
        hjust = 0.5,
        color = "gray30"
      ),
      legend.title = ggplot2::element_text(
        size = 18,
        face = "bold",
        color = "grey30"
      ),
      legend.text = ggtext::element_markdown(
        size = 14,
        color = text_color
      ),
      plot.subtitle = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      plot.caption = ggplot2::element_text(size = 14, face = "italic", hjust = 0.5),
      legend.background = ggplot2::element_rect(color = NA, fill = background),
      legend.box.background = ggplot2::element_rect(color = NA, fill = background),
      legend.key = ggplot2::element_rect(color = NA, fill = background),
      panel.background = ggplot2::element_rect(color = NA, fill = background),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(color = NA, fill = background),
      line = ggplot2::element_blank(),
      axis.text = ggtext::element_markdown(size = 16),
      axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, vjust = 1, color = text_color),
      axis.title.x = purrr::when(
        rlang::is_empty(x_lab),
        . ~ ggplot2::element_blank(),
        ~ ggplot2::element_text(face = "bold", size = 18)
      ),
      axis.title.y = purrr::when(
        rlang::is_empty(y_lab),
        . ~ ggplot2::element_blank(),
        ~ ggplot2::element_text(face = "bold", size = 18)
      ),
    )

  if (rlang::is_false(bottom_legend)) {
    plt <- plt + ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical"
    )
  }

  attr(plt, which = "background") <- background

  plt
}

#' @export
plot_hist <- function(
  .table,
  .by = NULL,
  incl_missing = FALSE,
  title = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend = FALSE,
  legend_title = title,
  bottom_legend = TRUE,
  palette = c(
    "blue", "red", "pink", "purple", "deep-purple", "indigo", "light-blue",
    "cyan", "teal", "green", "light-green", "lime", "yellow", "amber", "orange",
    "deep-orange", "brown", "grey", "blue-grey"
  ),
  background = "gray93"
) {

  # Make sure `.by` is a symbol; defaults to the summarized variable
  if (rlang::is_empty(.by)) {
    .by <- .table %>%
      colnames() %>%
      extract2(1) %>%
      rlang::sym()
  } else {
    .by <- rlang::ensym(.by)
  }

  # Remove missing data, if desired
  if (rlang::is_false(incl_missing)) {
    .table %>%
      dplyr::filter(!!.by != "Missing") ->
      .table
  }

  # Create color palette
  pal <- ggsci::pal_material(
    palette = palette[[1]],
    n = NROW(.table) + 1,
    reverse = TRUE
  )

  # Set title to title-case `.by` if NULL
  if (rlang::is_empty(title)) {
    title <- rlang::as_name(.by) %>% janitor::make_clean_names(case = "title")
  }

  # Prep for plotting
  .table %>%
    # Drop unnecessary variables
    dplyr::transmute(
      {{ .by  }},
      n,
      N,
      # Force valid_percent
      valid_percent = purrr::when(
        "valid_percent" %in% colnames(.),
        any(.) ~ valid_percent,
        ~ percent
      ),
      # Force valid_N
      valid_N = purrr::when(
        "valid_N" %in% colnames(.),
        any(.) ~ valid_N,
        ~ N
      ),
      # Choose pct based on `incl_missing`
      pct = purrr::when(
        rlang::is_true(incl_missing),
        . ~ percent,
        ~ valid_percent
      )
    ) ->
    .table

  # Create plot
  plt <- ggplot2::ggplot(
    .table,
    ggplot2::aes(x = !!.by, fill = !!.by)
  ) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data[["n"]]),
      show.legend = legend,
      color = pal(1),
      fill = pal(NROW(.table)) %>% rev(),
      width = 1,
      alpha = 0.5
    ) +
  ggtext::geom_textbox(
    ggplot2::aes(
      y = .data[["n"]],
      label = paste0(
        "**", .data[["n"]], "** ",
        "(", round(100*.data[["pct"]]), "%)"
      )
    ),
    maxwidth = ggplot2::unit(0.9^2/NROW(.table), units = "npc"),
    vjust = 0,
    halign = 0.5,
    size = 14 * 0.35278,
    fill = background,
    color = pal(1),
    show.legend = FALSE
  ) +
    # ggplot2::scale_color_manual(
    #   name = legend_title,
    #   values = pal(NROW(.table)),
    #   aesthetics = c("color", "fill")
    # ) +
    ggplot2::coord_cartesian(ylim = c(0, 1.1*max(.table[["n"]], na.rm = TRUE))) +
    ggplot2::labs(
      title = title,
      subtitle = paste0(
        "Responded = ", .table[["valid_N"]][[1]]
      ),
      caption = paste0("\nDid not respond = ", .table[["N"]][[1]] - .table[["valid_N"]])
    ) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggthemes::theme_fivethirtyeight(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 28,
        face = "bold",
        hjust = 0.5,
        color = "gray30"
      ),
      legend.title = ggplot2::element_text(
        size = 18,
        face = "bold",
        color = "grey30"
      ),
      legend.text = ggplot2::element_text(
        size = 14,
        color = "gray30"
      ),
      plot.subtitle = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
      plot.caption = ggplot2::element_text(size = 14, face = "italic", hjust = 0.5),
      legend.background = ggplot2::element_rect(color = NA, fill = background),
      legend.box.background = ggplot2::element_rect(color = NA, fill = background),
      legend.key = ggplot2::element_rect(color = NA, fill = background),
      panel.background = ggplot2::element_rect(color = NA, fill = background),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(color = NA, fill = background),
      line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title.x = purrr::when(
        rlang::is_empty(x_lab) ~ ggplot2::element_blank(),
        ~ ggplot2::element_text(face = "bold", size = 18)
      ),
      axis.title.y = purrr::when(
        rlang::is_empty(y_lab) ~ ggplot2::element_blank(),
        ~ ggplot2::element_text(face = "bold", size = 18)
      ),
    )

  # if (.table[["N"]][[1]] > .table[["valid_N"]][[1]]) {
  #   plt <- plt + ggplot2::annotate(
  #     "label",
  #     x = round(NROW(.table) / 2),
  #     y = .table[["N"]][[1]],
  #     label = paste0(
  #       "Total = ", .table[["N"]][[1]]
  #     ),
  #     size = 14 * 0.35278,
  #     color = "gray30",
  #     fill = background,
  #     vjust = 0
  #   )
  # }

  if (rlang::is_false(bottom_legend)) {
    plt <- plt + ggplot2::theme(
      legend.position = "right",
      legend.direction = "vertical"
    )
  }


  attr(plt, which = "background") <- background

  plt
}
