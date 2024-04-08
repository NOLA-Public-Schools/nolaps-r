# Bar ---------------------------------------------------------------------



#' @export
plot_bar_v <- function(
    d,
    x,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL,
    color = "gray") {
  d %>%
    ggplot(aes({{ x }})) +
    geom_bar(fill = color) +
    geom_text(
      stat = "count",
      aes(label = stat(count)),
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
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL,
    colors = c("gray", nolaps_blue()),
    column_exclude = NULL,
    values_exclude = NULL) {
  d %>%
    ggplot(aes({{ x }}, fill = {{ fill }})) +
    geom_bar(position = position_dodge()) +
    geom_text_repel(
      stat = "count",
      aes(label = stat(count)),
      position = position_dodge(1),
      direction = "y"
    ) +
    scale_fill_manual(values = colors) +
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_bar(percent = FALSE)
}



# Column ------------------------------------------------------------------



#' @export
plot_col_h <- function(
    d,
    x,
    y,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL,
    digits = 0,
    percent = FALSE,
    color = "gray",
    bars_focus_1 = NULL,
    bars_focus_2 = NULL,
    color_focus_1 = NULL,
    color_focus_2 = NULL,
    .fun = median) {
  colors <- rep(color, nrow(d))

  if (!is.null(bars_focus_1)) {
    colors[bars_focus_1] <- color_focus_1
  }

  if (!is.null(bars_focus_2)) {
    colors[bars_focus_2] <- color_focus_2
  }

  d %>%
    addlabels({{ y }}, digits, percent) %>%
    ggplot(aes(
      x = fct_reorder({{ x }}, {{ y }}, .fun),
      y = {{ y }}
    )) +
    geom_col(fill = colors) +
    geom_text(
      aes(label = label),
      hjust = -0.5
    ) +
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_bar(percent) +
    coord_flip()
}



#' @export
plot_col_v <- function(
    d,
    x,
    y,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL,
    digits = 0,
    percent = FALSE,
    color = "gray") {
  d %>%
    addlabels({{ y }}, digits, percent) %>%
    ggplot(aes({{ x }}, {{ y }})) +
    geom_col(fill = color) +
    geom_text(
      aes(label = label),
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
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL,
    ...) {
  if (percent == TRUE) {
    label <- d %>%
      pull({{ y }}) %>%
      percentify()
  } else {
    label <- d %>% pull({{ y }})
  }

  d %>%
    addlabels({{ y }}, digits, percent) %>%
    ggplot(aes({{ x }}, {{ y }}, fill = {{ fill }})) +
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
    colors = c("gray", "darkgray"),
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL,
    percent = FALSE,
    digits = 0) {
  d %>%
    addlabels({{ y }}, digits, percent) %>%
    ggplot(aes({{ x }}, {{ y }}, fill = {{ fill }})) +
    geom_col(position = position_dodge()) +
    geom_text(
      aes(label = label),
      position = position_dodge(1),
      vjust = -1
    ) +
    scale_fill_manual(values = colors) +
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_bar(percent)
}



# Proportionally stacked --------------------------------------------------



#' @export
plot_stackfill_comp <- function(
    d,
    x,
    y,
    digits = 0,
    lab_min = 5,
    cols = c(nolaps_red(), nolaps_yellow(), nolaps_teal(), nolaps_blue(), nolaps_navy(), "gray"),
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL) {
  d %>%
    mutate(across({{ x }}, factor)) %>%
    group_by({{ x }}, {{ y }}) %>%
    summarize(n_records = n()) %>%
    group_by({{ x }}) %>%
    mutate(prop = n_records / sum(n_records) * 100) %>%
    ggplot(aes(x = {{ x }}, y = prop, fill = factor({{ y }}))) +
    geom_col() +
    geom_text(
      aes(label = if_else(prop >= lab_min, percentify(round(prop, digits)), "")),
      position = position_stack(vjust = 0.5)
    ) +
    scale_fill_manual(values = rep(cols, 2)) +
    theme_stackfill(percent = TRUE) +
    labels_nolaps_bar(title, subtitle, caption, xlab, ylab, title_legend)
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
    labels_wanted = NULL,
    percent = FALSE,
    digits = 0,
    colors = c(nolaps_blue(), "black", "gray"),
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = NULL,
    ylab = NULL,
    title_legend = NULL) {
  d %>%
    addlabels({{ y }}, digits, percent) %>%
    mutate(label = case_when(
      {{ x }} == min({{ x }}) ~ label,
      {{ x }} == max({{ x }}) ~ label,
      {{ x }} %in% c(labels_wanted) ~ label,
      TRUE ~ ""
    )) %>%
    ggplot(aes({{ x }}, {{ y }}, color = factor({{ color }}))) +
    geom_line(size = 1) +
    geom_text_repel(
      aes(label = label),
      color = "black"
    ) +
    scale_color_manual(values = colors) +
    labels_nolaps_line(title, subtitle, caption, xlab, ylab, title_legend) +
    theme_line(percent, miny, maxy)
}



# Colors ------------------------------------------------------------------



#' @export
nolaps_blue <- function() {
  "#408BC7"
}

#' @export
nolaps_navy <- function() {
  "#2B5565"
}

#' @export
nolaps_red <- function() {
  "#DD5055"
}

#' @export
nolaps_teal <- function() {
  "#75C3CD"
}

#' @export
nolaps_yellow <- function() {
  "#F0CB5D"
}



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
  switch(x,
    nolaps = "NOLA Public Schools",
    application = "NOLA Public Schools application data",
    enrollment = "NOLA Public Schools enrollment data",
    match = "NOLA Public Schools match data",
    ldoe = "Louisiana Department of Education",
    nolaps_ldoe = "NOLA Public Schools analysis of LDOE data"
  )
}



# Helpers -----------------------------------------------------------------



percentify <- function(x) {
  str_c(x, "%")
}



addlabels <- function(d, labelcol, digits, percent) {
  d <- d %>% mutate(label = as.character(round({{ labelcol }}, digits)))

  if (percent == TRUE) {
    d <- d %>% mutate(across(label, percentify))
  }

  d
}



# Labels ------------------------------------------------------------------



labels_nolaps <- function(title, subtitle, caption, xlab, ylab) {
  list(
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ),
    xlab(label = xlab),
    ylab(label = ylab)
  )
}



labels_nolaps_bar <- function(title, subtitle, caption, xlab, ylab, title_legend) {
  list(
    labels_nolaps(title, subtitle, caption, xlab, ylab),
    guides(fill = guide_legend(title = title_legend))
  )
}



labels_nolaps_line <- function(title, subtitle, caption, xlab, ylab, title_legend) {
  list(
    labels_nolaps(title, subtitle, caption, xlab, ylab),
    guides(color = guide_legend(title = title_legend))
  )
}



# Theme -------------------------------------------------------------------



axis_nolaps <- function(percent) {
  scale_y_continuous(
    breaks = 0,
    labels = if (percent == TRUE) {
      "0%"
    } else {
      "0"
    },
    expand = expansion(mult = c(.0, .15))
  )
}



axis_stackfill <- function() {
  scale_y_continuous(
    breaks = 0,
    labels = "",
    expand = expansion(mult = c(.0, .0))
  )
}



setlims_y <- function(min, max) {
  coord_cartesian(ylim = c(min, max))
}



theme_nolaps <- function(percent) {
  list(
    theme_classic(),
    theme(plot.title.position = "plot"),
    theme(plot.caption.position = "plot"),
    theme(legend.position = "right"),
    theme(legend.justification = "top"),
    theme(axis.ticks = element_blank()),
    theme(axis.line = element_blank()),
    axis_nolaps(percent)
  )
}



theme_bar <- function(percent) {
  list(
    theme_nolaps(percent),
    geom_hline(yintercept = 0)
  )
}



theme_line <- function(percent, miny, maxy) {
  list(
    theme_nolaps(percent),
    geom_hline(yintercept = 0),
    setlims_y(miny, maxy)
  )
}



theme_stackfill <- function(percent) {
  list(
    theme_nolaps(percent),
    axis_stackfill(),
    geom_hline(yintercept = 0),
    geom_hline(yintercept = 100)
  )
}
