#----plot-helper----
remove_legend <- theme(legend.position = "none")
remove_y_axis <- theme(
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank()
)
remove_margin <- theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
get_max_range <- function(plots, axes) {
  limits <- map(plots, function(x)
    layer_scales(x)[[axes]]$range$range)
  limits_lower <- map_dbl(limits, 1)
  limits_upper <- map_dbl(limits, 2)
  c(min(limits_lower), max(limits_upper))
}
replace_data <- function(newdata, plot) {
  plot$data <- newdata
  plot
}
add_layer <- function(plots, ...)map(plots, ...)

#----align-polar----
prepare_polar <- function(left, right, datas) {
  lefts <- map(datas, replace_data, left)
  rights <- map(datas, replace_data, right)
  polar_range <- get_max_range(c(lefts, rights), "y")
  lefts <- suppressWarnings(imap(
    lefts,
    ~ .x +
      remove_margin +
      remove_legend +
      scale_y_continuous(limits = polar_range) +
      labs(tag = .y)
  ))
  rights <- suppressWarnings(map(rights, ~ .x +
                                   remove_margin +
                                   remove_legend +
                                   remove_y_axis))
  first_rights <- rights[[1]]
  last_rights <- rights[[length(rights)]]
  rights <- map(rights, ~ .x +
                  theme(axis.text = element_blank(),
                        axis.title = element_blank()))
  rights[[1]] <- first_rights +
    theme(axis.title = element_blank())
  rights[[length(rights)]] <- last_rights +
    theme(axis.text.x = element_blank())
  
  first_lefts <- lefts[[1]] +
    theme(axis.title = element_blank())
  last_lefts <- lefts[[length(lefts)]]
  lefts <- map(lefts, ~ .x +
                 theme(axis.text = element_blank(),
                       axis.title = element_blank()))
  lefts[[1]] <- first_lefts
  lefts[[length(lefts)]] <- last_lefts +
    theme(axis.text = element_blank(),
          axis.title.y = element_blank())
  list(lefts = lefts, rights = rights)
}
align_polar <- function(polar){
  rows <- seq_along(polar$lefts)
  pos_left <- map(rows, ~area(.x, 1, .x, 1))
  pos_right <- map(rows, ~area(.x, 2, .x, 2))
  do.call(c, c(pos_left, pos_right))
}

#----align-alltime----
prepare_alltime <- function(plot, datas){
  plots <- map(datas, replace_data, plot)
  plots <- map(plots, ~.x +
                 remove_margin +
                 remove_legend +
                 theme(axis.ticks = element_blank(),
                       axis.title.y = element_blank()) +
                 scale_y_continuous(position = "right")
  )
  last <- plots[[length(plots)]]
  plots <- map(plots, ~.x +
                 theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank()))
  plots[[length(plots)]] <- last
  plots
}

align_alltime <- function(plots, l, r){
  rows <- seq_along(plots)
  pos <- map(rows, ~area(.x, l, .x, r))
  do.call(c, pos)
}

#----color----
article_order <- function(views){
  views %>%
    group_by(cluster, article) %>%
    summarise(mean = mean(views_depattern_smooth, na.rm = TRUE)) %>% 
    arrange(cluster, desc(mean)) %>% 
    select(cluster, article)
}


max_color_diff <- function(color, cluster, n = 100){
  col2hue <- function(x)rgb2hsv(col2rgb(x))[1, ]
  sshue <- function(colors, cluster){
    colors <- tibble(colors, cluster, hue = col2hue(colors)) %>% 
      group_by(cluster) %>%
      summarise(sshue = var(hue))
    mean(colors$sshue, na.rm = TRUE)
  }
  shuffle <- function(x)sample(x, length(x))
  colors <- replicate(n, shuffle(color), simplify = FALSE)
  colors[[which.max(map_dbl(colors, sshue, cluster))]]
}

add_colors <- function(plots, views){
  article_order <- article_order(views)
  colors <- max_color_diff(scales::hue_pal()(length(article_order$article)),
                           article_order$cluster)
  names(colors) <- article_order$article
  add_layer(
    plots,
    ~ .x +
      scale_color_manual(values = colors,
                         breaks = article_order$article) +
      scale_fill_manual(values = colors,
                        breaks = article_order$article)
  )
}

#----add-legend----
add_legend <- function(plots, designs, views, which = length(plots)){
  legend <- get_legend(replace_data(views, plots[[which]]) +
                         theme(legend.position = "right"))
  plots <- c(plots, list(legend))
  nclust <- length(unique(views$cluster))
  designs <- c(designs, area(1, 5, nclust, 5))
  wrap_plots(plots, design = designs, widths = c(1, 1, 1, 1, 0.5))
}

#----final-plot----
plot_final <- function(views){
  wday <- plot_wday(views)
  year <- plot_year(views)
  alltime <- plot_alltime(views)
  clusters <- break_clusters(views)
  nclust <- length(clusters)
  plots <- list(polar = prepare_polar(wday, year, clusters),
                alltime = prepare_alltime(alltime, clusters))
  designs <- c(align_polar(plots$polar),
               align_alltime(plots$alltime, 3, 4))
  plots <- c(flatten(plots$polar), plots$alltime)
  plots <- add_colors(plots, views)
  add_legend(plots, designs, views) +
    plot_annotation(title = "Wikipedia Trends",
                    caption = "Polar plots (left) show averaged trends in standard deviation, while cartesian plots (right) represents the raw count (shaded area +/- 1SD smothed over ten days).\nTrends shown in plots to the left are removed from plots to the right.\nRows are created by clustering articles by similarity.",
                    theme = theme(plot.caption = element_text(
                      face = "plain",
                      colour = "darkgrey",
                      size = 10),
                      plot.title = element_text(
                        face = "plain",
                        colour = "darkgrey",
                        size = 15)))
}
