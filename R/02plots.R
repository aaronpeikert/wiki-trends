#----plot-wday----
plot_wday <- function(views) {
  views %>%
    ggplot(aes(wday, views_wday, group = article, color = article)) +
    geom_line() +
    coord_polar() +
    theme_minimal()
}
#----plot-year----
plot_year <- function(views) {
  views %>%
    ggplot(aes(doy, views_year, group = article, color = article)) +
    geom_line() +
    coord_polar() +
    theme_minimal()
}
#----plot-alltime----
plot_alltime <- function(views){
  ggplot(
    views,
    aes(
      date,
      views_depattern,
      ymin = views_depattern_smooth + trend_sd,
      ymax = views_depattern_smooth - trend_sd,
      group = article,
      color = article,
      fill = article
    )
  ) +
    geom_ribbon(aes(alpha = .5, color = NULL)) +
    geom_line(aes(alpha = .5)) +
    guides(alpha = FALSE) + theme_minimal()
}
