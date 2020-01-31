#----plot-wday----
theme_text <- theme(
  title = element_text(
    face = "plain",
    colour = "darkgrey",
    size = 11,
    lineheight = 0.9,
    hjust = 0.5,
    vjust = 0.5,
    angle = 0,
    margin = margin(),
    debug = FALSE
  ),
  axis.text = element_text(
    face = "plain",
    colour = "darkgrey",
    size = 11,
    lineheight = 0.9,
    hjust = 0.5,
    vjust = 0.5,
    angle = 0,
    margin = margin(),
    debug = FALSE
  ),
  legend.text = element_text(
    face = "plain",
    colour = "darkgrey",
    size = 9,
    lineheight = 0.9,
    hjust = 0.5,
    vjust = 0.5,
    angle = 0,
    margin = margin(),
    debug = FALSE
  ),
  legend.title = element_blank()
)
plot_wday <- function(views) {
  breaks_wday <- function(x)seq(x[1], x[2], length.out = 7)
  wdays <- c("Su", "Tu", "We", "Th", "Fr", "Sa", "Mo")
  views %>%
    ggplot(aes(wday, views_wday, group = article, color = article)) +
    geom_line() +
    coord_polar() +
    theme_minimal() +
    scale_x_continuous("Weekdays", breaks = breaks_wday, labels = wdays) +
    theme(panel.grid.minor = element_blank()) +
    theme_text +
    ylab("Views in SD")
}
#----plot-year----
plot_year <- function(views) {
  months <- c("J", "F","M", "A", "M", "J", "J", "A", "S", "O", "N", "D", "")
  months <- c(12, 2:11, 1)
  breaks_year <- function(x)seq(x[1], x[2], length.out = 12)
  views %>%
    ggplot(aes(doy, views_year, group = article, color = article)) +
    geom_line() +
    coord_polar() +
    theme_minimal() +
    scale_x_continuous("Months", breaks = breaks_year, labels = months) +
    theme(panel.grid.minor = element_blank()) +
    theme_text
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
    guides(alpha = FALSE) +
    theme_minimal() +
    theme(axis.title.x = element_blank()) +
    theme_text
}
