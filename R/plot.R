#' @importFrom magrittr %>%



# Bar ---------------------------------------------------------------------



#' @export
plot_bar_v <- function(
  d,
  x,
  title = "title",
  subtitle = NULL,
  caption = captions("ldoe"),
  xlab = NULL,
  ylab = NULL,
  title_legend = NULL,
  color = "gray"
  ) {

  d %>%
    ggplot2::ggplot(ggplot2::aes({{ x }})) +
    ggplot2::geom_bar(fill = color) +
    ggplot2::geom_text(
      stat = "count",
      ggplot2::aes(label = stat(count)),
      vjust = -1
    ) +
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_bar(percent = FALSE)

}



#' @export
plot_bar_v_dodge <- function(
  d,
  x,
  fill,
  title = "title",
  subtitle = NULL,
  caption = captions("ldoe"),
  xlab = NULL,
  ylab = NULL,
  title_legend = NULL,
  colors = c("gray", nolaps_blue())
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
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_bar(percent = FALSE)

}



# Column ------------------------------------------------------------------



#' @export
plot_col_h <- function(
  d,
  x,
  y,
  title = "title",
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  caption = captions("ldoe"),
  title_legend = NULL,
  digits = 0,
  percent = FALSE,
  color = "gray",
  bars_focus_1 = NULL,
  bars_focus_2 = NULL,
  color_focus_1 = NULL,
  color_focus_2 = NULL,
  .fun = median
) {

  colors <- rep(color, nrow(d))

  if (!is.null(bars_focus_1)) {colors[bars_focus_1] <- color_focus_1}

  if (!is.null(bars_focus_2)) {colors[bars_focus_2] <- color_focus_2}

  d %>%
    addlabels({{ y }}, digits, percent) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = forcats::fct_reorder({{ x }}, {{ y }}, .fun),
      y = {{ y }}
      )
    ) +
    ggplot2::geom_col(fill = colors) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      hjust = -0.5
    ) +
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_bar(percent) +
    ggplot2::coord_flip()

}



#' @export
plot_col_v <- function(
  d,
  x,
  y,
  title = "title",
  subtitle = NULL,
  caption = captions("ldoe"),
  xlab = NULL,
  ylab = NULL,
  title_legend = NULL,
  digits = 0,
  percent = FALSE,
  color = "gray"
) {

  d %>%
    addlabels({{ y }}, digits, percent) %>%
    ggplot2::ggplot(ggplot2::aes({{ x }}, {{ y }})) +
    ggplot2::geom_col(fill = color) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -1
    ) +
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_bar(percent)

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
  levels = NULL,
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

  if (!is.null(levels)) {
    l <- d %>% dplyr::pull({{ fill }}) %>% factor() %>% forcats::fct_relevel(levels) %>% levels()
  } else {
    l <- d %>% dplyr::pull({{ fill }}) %>% factor() %>% levels()
  }

  d %>%
    dplyr::mutate(label = label) %>%
    ggplot2::ggplot(ggplot2::aes(
      forcats::fct_rev(forcats::fct_reorder({{ x }}, {{ y }}, ...)),
      {{ y }},
      fill = factor({{ fill }}, levels = l)
      )
    ) +
    ggplot2::geom_col(position = position_dodge()) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
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



#' @export
palette_greys_light <- function(n) {

  RColorBrewer::brewer.pal(n + 1, "Greys")[(2):(n + 1)]

}



#' @export
palette_greys_dark <- function(n) {

  RColorBrewer::brewer.pal(n + 2, "Greys")[(3):(n + 2)]

}



# Captions ----------------------------------------------------------------



#' @export
captions <- function(x) {

  switch(
    x,
    enrollment = "NOLA Public Schools enrollment data",
    ldoe = "Louisiana Department of Education",
    match = "NOLA Public Schools match data",
    nolaps = "NOLA Public Schools",
    nolaps_ldoe = "NOLA Public Schools analysis of LDOE data"
  )

}



# Helpers -----------------------------------------------------------------



percentify <- function(x) {stringr::str_c(x, "%")}



addlabels <- function(d, labelcol, digits, percent) {

  d <- d %>% dplyr::mutate(label = as.character(round({{ labelcol }}, digits)))

  if (percent == TRUE) {

    d <- d %>% dplyr::mutate(dplyr::across(label, percentify))

  }

  d

}



# Labels ------------------------------------------------------------------



labels_nolaps <- function(title, subtitle, caption, xlab, ylab) {

  list(
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ),
    ggplot2::xlab(label = xlab),
    ggplot2::ylab(label = ylab)
  )

}



labels_nolaps_bar <- function(title, subtitle, caption, xlab, ylab, title_legend) {

  list(
    labels_nolaps(title, subtitle, caption, xlab, ylab),
    ggplot2::guides(fill = ggplot2::guide_legend(title = title_legend))
  )

}



labels_nolaps_line <- function(title, subtitle, caption, xlab, ylab, title_legend) {

  list(
    labels_nolaps(title, subtitle, caption, xlab, ylab),
    ggplot2::guides(color = ggplot2::guide_legend(title = title_legend))
  )

}



# Theme -------------------------------------------------------------------



axis_nolaps <- function(percent) {

  ggplot2::scale_y_continuous(
    breaks = 0,
    labels = if (percent == TRUE) {"0%"} else {"0"},
    expand = ggplot2::expansion(mult = c(.0, .15))
  )

}



setlims_y <- function(min, max) {ggplot2::coord_cartesian(ylim = c(min, max))}



theme_nolaps <- function(percent) {

  list(
    ggplot2::theme_classic(),
    ggplot2::theme(plot.title.position = "plot"),
    ggplot2::theme(plot.caption.position = "plot"),
    ggplot2::theme(legend.position = "right"),
    ggplot2::theme(legend.justification = "top"),
    ggplot2::theme(axis.ticks = ggplot2::element_blank()),
    ggplot2::theme(axis.line = ggplot2::element_blank()),
    axis_nolaps(percent)
  )

}



theme_bar <- function(percent) {

  list(
    theme_nolaps(percent),
    ggplot2::geom_hline(yintercept = 0)
  )

}



theme_line <- function(percent, miny, maxy) {

  list(
    theme_nolaps(percent),
    ggplot2::geom_hline(yintercept = 0),
    setlims_y(miny, maxy)
  )

}



theme_stack100 <- function() {

  list(
    theme_nolaps(percent)
  )

}


