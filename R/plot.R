#' @importFrom magrittr %>%



# Bar ---------------------------------------------------------------------



#' @export
plot_bar_v <- function(
  d,
  x,
  color = nolaps_blue(),
  title = "title",
  subtitle = NULL,
  title_legend = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = "Louisiana Department of Education"
) {

  d %>%
    ggplot2::ggplot(ggplot2::aes({{ x }})) +
    ggplot2::geom_bar(fill = color) +
    ggplot2::geom_text(
      stat = "count",
      ggplot2::aes(label = stat(count)),
      vjust = -1
    ) +
    labels_nolaps_bar(title, subtitle, title_legend, xlab, ylab, caption) +
    theme_bar_v(percent = FALSE)

}



#' @export
plot_bar_v_dodge <- function(
  d,
  x,
  fill,
  colors = c("gray", nolaps_blue()),
  title = "title",
  subtitle = NULL,
  title_legend = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = "Louisiana Department of Education"
) {

  d %>%
    ggplot2::ggplot(ggplot2::aes({{ x }}, fill = {{ fill }})) +
    ggplot2::geom_bar(position = ggplot2::position_dodge()) +
    ggplot2::geom_text(
      stat = "count",
      ggplot2::aes(label = stat(count)),
      position = ggplot2::position_dodge(1),
      vjust = -1
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    labels_nolaps_bar(title, subtitle, title_legend, xlab, ylab, caption) +
    theme_bar_v(percent = FALSE)

}



# Column ------------------------------------------------------------------



#' @export
plot_col_h <- function(
  d,
  x,
  y,
  percent = FALSE,
  digits = 0,
  color = "gray",
  bars_focus_1 = NULL,
  color_focus_1 = nolaps_blue(),
  bars_focus_2 = NULL,
  color_focus_2 = "black",
  xlab = NULL,
  ylab = NULL,
  title_legend = NULL,
  title = "title",
  subtitle = NULL,
  caption = "Louisiana Department of Education",
  ...
) {

  if (percent == TRUE) {
    lab <- d %>% dplyr::pull({{ y }}) %>% percentify(digits)
  } else {
    lab <- d %>% dplyr::pull({{ y }}) %>% round(digits) %>% as.character()
  }

  cols <- rep(color, nrow(d))

  if (!is.null(bars_focus_1)) {cols[bars_focus_1] <- color_focus_1}

  if (!is.null(bars_focus_2)) {cols[bars_focus_2] <- color_focus_2}

  d %>%
    dplyr::mutate(label = lab) %>%
    ggplot2::ggplot(ggplot2::aes(
      forcats::fct_reorder({{ x }}, {{ y }}, ...),
      {{ y }}
      )
    ) +
    ggplot2::geom_col(fill = cols) +
    ggplot2::geom_text(
      ggplot2::aes(label = lab),
      hjust = -0.5
    ) +
    ggplot2::geom_hline(yintercept = 0) +
    style_axis_y_bar(percent) +
    theme_bar_h() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    labels_nolaps(xlab, ylab, title_legend, title, subtitle, caption) +
    ggplot2::coord_flip()

}



#' @export
plot_col_h_dodge <- function(
  d,
  x,
  y,
  fill,
  percent = FALSE,
  digits = 0,
  title_legend = "title_legend",
  title = "title",
  subtitle = "subtitle",
  caption = "caption",
  ...
) {

  if (percent == TRUE) {
    label <- d %>% pull({{ y }}) %>% percentify(digits = digits)
  } else {
    label <- d %>% pull({{ y }})
  }

  d %>%
    mutate(label = label) %>%
    ggplot(aes(
      fct_reorder({{ x }}, {{ y }}, ...),
      {{ y }},
      fill = factor({{ fill }})
    )
    ) +
    geom_col(position = position_dodge()) +
    geom_text(
      aes(label = label),
      position = position_dodge(1),
      hjust = -0.5
    ) +
    style_axis_y_bar(percent) +
    theme_bar_h() +
    xlab(NULL) +
    ylab(NULL) +
    guides(fill = guide_legend(title = title_legend, reverse = TRUE, direction = "vertical")) +
    ggtitle(title, subtitle) +
    labs(caption = caption) +
    coord_flip()

}



#' @export
plot_col_v_dodge <- function(
  d,
  x,
  y,
  fill,
  percent = FALSE,
  digits = 0,
  colors = c("gray", nolaps_blue()),
  title = "title",
  subtitle = NULL,
  title_legend = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = "Louisiana Department of Education",
  ...
) {

  if (percent == TRUE) {
    label <- d %>% pull({{ y }}) %>% percentify(digits)
  } else {
    label <- d %>% pull({{ y }})
  }

  d %>%
    mutate(label = label) %>%
    ggplot(aes(
      fct_rev(fct_reorder({{ x }}, {{ y }}, ...)),
      {{ y }},
      fill = factor({{ fill }})
      )
    ) +
    geom_col(position = position_dodge()) +
    geom_text(
      aes(label = label),
      position = position_dodge(1),
      vjust = -1
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    labels_nolaps_bar(title, subtitle, title_legend, xlab, ylab, caption) +
    theme_bar_v(percent)

}



# Proportionally stacked --------------------------------------------------



#' @export
plot_stack100_comp <- function(
  d,
  x,
  y,
  lab_min = 0.05,
  cols = c(nolaps_red(), nolaps_yellow(), nolaps_teal(), nolaps_blue(), nolaps_navy(), "gray"),
  xlab = NULL,
  ylab = NULL,
  title_legend = "title_legend",
  title = "title",
  subtitle = "subtitle",
  caption = "Louisiana Department of Education"
) {

  d %>%
    mutate(across({{ x }}, factor)) %>%
    group_by({{ x }}, {{ y }}) %>%
    summarize(n_records = n()) %>%
    group_by({{ x }}) %>%
    mutate(rate = n_records / sum(n_records)) %>%
    ggplot(aes(x = {{ x }}, y = rate, fill = {{ y }})) +
    geom_col() +
    geom_text(
      aes(label = if_else(rate >= lab_min, percentify(rate), "")),
      position = position_fill(vjust = 0.5)
    ) +
    ggplot2::scale_fill_manual(values = rep(cols, 2)) +
    style_axis_y_bar(percent = TRUE) +
    theme_stack_v() +
    labels_nolaps(xlab, ylab, title_legend, title, subtitle, caption)

}



#' @export
plot_line_comp <- function(
  d,
  x,
  y,
  color,
  miny = 0,
  maxy,
  labels_wanted = NULL,
  percent = FALSE,
  digits = 0,
  colors = c(nolaps_blue(), "black", "gray"),
  title = "title",
  subtitle = NULL,
  title_legend = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = "Louisiana Department of Education"
) {

  if (percent == TRUE) {
    d <- d %>% dplyr::mutate(label = percentify({{ y }}, digits))
  } else {
    d <- d %>% dplyr::mutate(label = as.character({{ y }}))
  }

  l <-
    d %>%
    dplyr::filter({{ x }} == max({{ x }})) %>%
    dplyr::arrange(desc({{ y }})) %>%
    dplyr::pull({{ color }})

  d %>%
    dplyr::mutate(label = dplyr::case_when(
      {{ x }} == min({{ x }}) ~ label,
      {{ x }} == max({{ x }}) ~ label,
      {{ x }} %in% c(labels_wanted) ~ label,
      TRUE ~ ""
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes({{ x }}, {{ y }}, color = factor({{ color }}, levels = l))) +
    ggplot2::geom_line(size = 1) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = label),
      color = "black"
    ) +
    ggplot2::scale_color_manual(values = colors) +
    labels_nolaps_line(title, subtitle, title_legend, xlab, ylab, caption) +
    theme_line(percent, miny, maxy)

}



# Colors ------------------------------------------------------------------



#' @export
nolaps_blue <- function() {"#408BC7"}

#' @export
nolaps_navy <- function() {"#2B5565"}

#' @export
nolaps_red <- function() {"#DD5055"}

#' @export
nolaps_teal <- function() {"#75C3CD"}

#' @export
nolaps_yellow <- function() {"#F0CB5D"}



# Helpers -----------------------------------------------------------------



percentify <- function(x, digits = 0) {str_c(round(x, digits), "%")}



setlims_y <- function(min, max) {ggplot2::coord_cartesian(ylim = c(min, max))}



axis_bar_h <- function(percent) {

  ggplot2::scale_x_continuous(
    breaks = 0,
    labels = if (percent == TRUE) {"0%"} else {"0"},
    expand = expansion(mult = c(.0, .15))
  )

}



axis_bar_v <- function(percent) {

  ggplot2::scale_y_continuous(
    breaks = 0,
    labels = if (percent == TRUE) {"0%"} else {"0"},
    expand = ggplot2::expansion(mult = c(.0, .15))
  )

}



axis_line <- function(percent) {

  ggplot2::scale_y_continuous(
    breaks = 0,
    labels = if (percent == TRUE) {"0%"} else {"0"},
    expand = ggplot2::expansion(mult = c(.0, .15))
  )

}



theme_bar_h <- function() {

  ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(legend.justification = "right") +
    ggplot2::theme(plot.title.position = "plot")

}



theme_bar_v <- function(percent) {

  list(
    ggplot2::theme_classic(),
    ggplot2::theme(plot.title.position = "plot"),
    ggplot2::theme(plot.caption.position = "plot"),
    ggplot2::theme(legend.position = "right"),
    ggplot2::theme(legend.justification = "top"),
    ggplot2::theme(axis.ticks = ggplot2::element_blank()),
    ggplot2::theme(axis.line.y = ggplot2::element_blank()),
    axis_bar_v(percent)
  )

}



theme_line <- function(percent, miny, maxy) {

  list(
    ggplot2::theme_classic(),
    ggplot2::theme(plot.title.position = "plot"),
    ggplot2::theme(plot.caption.position = "plot"),
    ggplot2::theme(legend.position = "right"),
    ggplot2::theme(legend.justification = "top"),
    ggplot2::theme(axis.ticks = ggplot2::element_blank()),
    ggplot2::theme(axis.line.y = ggplot2::element_blank()),
    axis_line(percent),
    setlims_y(miny, maxy)
  )

}



theme_stack_v <- function() {

  ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::theme(legend.justification = "top")

}



labels_nolaps_bar <- function(title, subtitle, title_legend, xlab, ylab, caption) {

  list(
    ggplot2::ggtitle(title, subtitle),
    ggplot2::xlab(xlab),
    ggplot2::ylab(ylab),
    ggplot2::labs(caption = caption),
    ggplot2::guides(fill = ggplot2::guide_legend(title = title_legend))
  )

}



labels_nolaps_line <- function(title, subtitle, title_legend, xlab, ylab, caption) {

  list(
    ggplot2::ggtitle(title, subtitle),
    ggplot2::xlab(xlab),
    ggplot2::ylab(ylab),
    ggplot2::labs(caption = caption),
    ggplot2::guides(color = ggplot2::guide_legend(title = title_legend))
  )

}


