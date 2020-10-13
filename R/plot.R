#' @importFrom magrittr "%>%"



# Bar and column ----------------------------------------------------------



#' @export
plot_bar_v <- function(
  d,
  x,
  xlab = NULL,
  ylab = NULL,
  title_legend = "legend",
  title = "title",
  subtitle = "subtitle",
  caption = "caption"
) {

  d %>%
    ggplot2::ggplot(ggplot2::aes({{ x }})) +
    ggplot2::geom_bar() +
    ggplot2::geom_text(
      stat = "count",
      ggplot2::aes(label = stat(count)),
      vjust = -1
    ) +
    style_axis_y_bar(percent = FALSE) +
    theme_bar_v() +
    labels_nolaps(xlab, ylab, title_legend, title, subtitle, caption)

}


#' @export
plot_bar_v_dodge <- function(
  d,
  x,
  fill,
  color_focus = nolaps_yellow(),
  color_comparison = "gray",
  xlab = NULL,
  ylab = NULL,
  title_legend = "title_legend",
  title = "title",
  subtitle = "subtitle",
  caption = "caption"
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
    ggplot2::scale_fill_manual(values = nolaps_colors_2(color_focus, color_comparison)) +
    style_axis_y_bar(percent = FALSE) +
    theme_bar_v() +
    labels_nolaps(xlab, ylab, title_legend, title, subtitle, caption)

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
    style_axis_y_bar(percent) +
    theme_bar_v() +
    labels_nolaps()

}



# Stacked -----------------------------------------------------------------



#' @export
plot_stack100_comp <- function(
  d,
  x,
  y,
  lab_min = 0.05,
  xlab = NULL,
  ylab = NULL,
  title_legend = "title_legend",
  title = "title",
  subtitle = "subtitle",
  caption = "caption"
) {

  d %>%
    group_by({{ x }}, {{ y }}) %>%
    summarize(n_records = n()) %>%
    group_by({{ x }}) %>%
    mutate(rate = n_records / sum(n_records)) %>%
    ggplot(aes(x = {{ x }}, y = rate, fill = {{ y }})) +
    geom_col() +
    geom_text(
      aes(label = percentify(if_else(rate >= lab_min, rate, NULL))),
      position = position_fill(vjust = 0.5)
    ) +
    style_axis_y_bar(percent = TRUE) +
    theme_stack100_v_comp() +
    labels_nolaps()

}



# Line --------------------------------------------------------------------



#' @export
plot_line_comp <- function(
  d,
  x,
  y,
  color,
  miny = 0,
  maxy,
  percent = FALSE,
  labels_wanted = NULL,
  xlab = NULL,
  ylab = NULL,
  title_legend = "title_legend",
  title = "title",
  subtitle = "subtitle",
  caption = "caption",
  ...
) {

  if (percent == TRUE) {

    d <- d %>% mutate(label = percentify(as.numeric({{ y }}), ...))

  } else {

    d <- d %>% mutate(label = {{ y }})

  }

  d %>%
    mutate(label = case_when(
      year == min({{ x }}) ~ label,
      year == max({{ x }}) ~ label,
      year %in% c(labels_wanted) ~ label,
      TRUE ~ ""
    )
    ) %>%
    ggplot(aes({{ x }}, as.numeric({{ y }}), color = factor({{ color }}))) +
    geom_line(size = 1) +
    geom_text(
      aes(label = label),
      vjust = -1,
      color = "black"
    ) +
    theme_line_v() +
    style_axis_y_line(percent) +
    setlim_y(miny, maxy) +
    labels_nolaps()

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



nolaps_colors_2 <- function(col_focus = nolaps_teal(), col_comparison = "gray") {

  c(col_comparison, col_focus)

}



# Helpers -----------------------------------------------------------------



percentify <- function(x, digits = 0) {str_c(round(x * 100, digits), "%")}



setlims_y <- function(min, max) {coord_cartesian(ylim = c(min, max))}



style_axis_x_bar <- function(percent) {

  if (percent == TRUE) {zero <- "0%"} else {zero <- "0"}

  ggplot2::scale_x_continuous(
    breaks = 0,
    labels = zero,
    expand = expansion(mult = c(.0, .15))
  )

}



style_axis_y_bar <- function(percent) {

  if (percent == TRUE) {zero <- "0%"} else {zero <- "0"}

  ggplot2::scale_y_continuous(
    breaks = 0,
    labels = zero,
    expand = ggplot2::expansion(mult = c(.0, .15))
  )

}



style_axis_y_line <- function(percent) {

  ggplot2::scale_y_continuous(
    breaks = 0,
    labels = if (percent == TRUE) {"0%"} else {"0"},
    expand = ggplot2::expansion(mult = c(.0, .15))
  )

}



theme_bar_h <- function() {

  ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(legend.justification = "right")

}



theme_bar_v <- function() {

  ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(legend.justification = "right")

}



theme_line_v <- function() {

  ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::theme(legend.justification = "top")

}



theme_stack100_v_comp <- function() {

  ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::theme(legend.justification = "top")

}



labels_nolaps <- function(xlab, ylab, title_legend, title, subtitle, caption) {

  list(
    ggplot2::xlab(xlab),
    ggplot2::ylab(ylab),
    ggplot2::guides(fill = ggplot2::guide_legend(title = title_legend)),
    ggplot2::ggtitle(title, subtitle),
    ggplot2::labs(caption = caption)
  )

}


