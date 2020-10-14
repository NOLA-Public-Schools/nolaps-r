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
  xlab = NULL,
  ylab = NULL,
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
    labels_nolaps(xlab, ylab, title_legend, title, subtitle, caption)

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
  colors = c(nolaps_blue(), "darkgray", "gray"),
  xlab = NULL,
  ylab = NULL,
  title_legend = "title_legend",
  title = "title",
  caption = "Louisiana Department of Education",
  subtitle = NULL,
  ...
) {

  if (percent == TRUE) {

    d <- d %>% mutate(label = percentify({{ y }}, ...))

  } else {

    d <- d %>% mutate(label = as.character({{ y }}))

  }

  l <-
    d %>%
    filter({{ x }} == max({{ x }})) %>%
    arrange(desc({{ y }})) %>%
    pull({{ color }})

  d %>%
    mutate(label = case_when(
      {{ x }} == min({{ x }}) ~ label,
      {{ x }} == max({{ x }}) ~ label,
      {{ x }} %in% c(labels_wanted) ~ label,
      TRUE ~ ""
      )
    ) %>%
    ggplot(aes({{ x }}, {{ y }}, color = factor({{ color }}, levels = l))) +
    geom_line(size = 1) +
    ggrepel::geom_text_repel(
      aes(label = label),
      color = "black"
    ) +
    ggplot2::scale_color_manual(values = colors) +
    theme_line_v() +
    style_axis_y_line(percent) +
    setlims_y(miny, maxy) +
    labels_nolaps_line(xlab, ylab, title_legend, title, subtitle, caption)

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



#' @export
nolaps_colors_2 <- function(col_focus = nolaps_blue(), col_comparison = "gray") {

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
    ggplot2::theme(axis.line = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(legend.justification = "right") +
    ggplot2::theme(plot.title.position = "plot")

}



theme_bar_v <- function() {

  ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(legend.justification = "right")

}



theme_line_v <- function() {

  ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::theme(legend.justification = "top") +
    ggplot2::theme(plot.caption.position = "plot")

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



labels_nolaps_line <- function(xlab, ylab, title_legend, title, subtitle, caption) {

  list(
    ggplot2::xlab(xlab),
    ggplot2::ylab(ylab),
    ggplot2::guides(color = ggplot2::guide_legend(title = title_legend)),
    ggplot2::ggtitle(title, subtitle),
    ggplot2::labs(caption = caption)
  )

}


