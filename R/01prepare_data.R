#----get-articles----
get_articles <- function(articles, start, end){
  article_pageviews(
    project = c("en.wikipedia"),
    article = articles,
    platform = c("desktop", "mobile-web", "mobile-app"),
    start = pageview_timestamps(start),
    end = pageview_timestamps(end)
  )
}

#----remove-outliers----
remove_outliers <- function(views){
  views %>%
    # sum over devices and agents
    group_by(article, date) %>%
    summarise(views = sum (views)) %>% 
    # remove extremes
    mutate(trimed_mean = winsor.mean(views, trim = 0.05),
           trimed_sd = winsor.sd(views, trim = .05),
           cutoff = trimed_mean + 4*trimed_sd,
           views = if_else(views < cutoff, views, NA_real_),
           date = as_date(date)) %>% 
    ungroup()
}

#----extract-trend----
extract_trend <- function(views){
  views %>%
    as_tsibble(article, date) %>% 
    group_by(article) %>%
    mutate(
      trend = slide_dbl(
        views,
        mean,
        na.rm = TRUE,
        .size = 365,
        .align = "center-left"
      ),
      trend_sd = slide_dbl(
        views,
        sd,
        na.rm = TRUE,
        .size = 365,
        .align = "center-left"
      ),
      views_std = (views - trend)/trend_sd
    ) %>%
    ungroup() %>%
    filter(!is.na(trend)) %>% 
    as_tibble()
}

#----add-dates----
add_dates <- function(views){
  mutate(views,
         year = year(date),
         month = month(date),
         week = week(date),
         day = day(date),
         wday = wday(date),
         doy = yday(date))
}

#----extract-wday-trend----
extract_wday_trend <- function(views) {
  views %>%
    as_tibble() %>%
    group_by(article, wday) %>%
    mutate(views_wday = mean(views_std, na.rm = TRUE)) %>%
    ungroup()
}

#----extract-year-trend----
extract_year_trend <- function(views){
  views_year <- views %>%
    group_by(article, doy) %>% 
    summarise(views = mean(views_std - views_wday, na.rm = TRUE)) %>% 
    list() %>% 
    rep(3) %>% 
    bind_rows() %>% 
    group_by(article) %>% 
    mutate(doy2 = 1,
           doy2 = cumsum(doy2)) %>% 
    as_tsibble(article, doy2) %>% 
    mutate(
      views_year = slide_dbl(
        views,
        mean,
        na.rm = TRUE,
        .size = 30,
        .align = "center-left"
      )) %>% 
    as_tibble() %>% 
    ungroup() %>% 
    filter(between(doy2, max(doy), (2*max(doy)) - 1)) %>% 
    select(article, doy, views_year)
  left_join(views, views_year, by = c("article", "doy"))
}

#----remove_pattern----
remove_pattern <- function(views){
  views %>%
    mutate(
      views_dewday = views_std - views_wday,
      views_depattern = views_dewday - views_year,
      views_depattern = views_depattern * trend_sd + trend
    ) %>% 
    as_tsibble(article, date) %>% 
    mutate(views_depattern_smooth = slide_dbl(
      views_depattern,
      mean,
      na.rm = TRUE,
      .size = 30,
      .align = "center-left"
    )) %>% as_tibble()
}

#----cluster-ts----
cluster_ts <- function(data, nclust, id_cols, names_from, values_from){
  id_cols <- enquo(id_cols)
  names_from <- enquo(names_from)
  values_from <- enquo(values_from)
  wide <- pivot_wider(data,
                      id_cols = !!id_cols,
                      names_from = !!names_from,
                      values_from = !!values_from) %>% 
    select(-c(!!id_cols))
  dist <- pdcDist(as.matrix(wide))
  out <- tibble(cluster = kmed::fastkmed(dist, nclust, iterate = 100)$cluster,
                article = names(wide))
  recode_by_size <- function(x) {
    t <- table(x)
    size <- order(t)
    out <- names(t)
    names(out) <- size
    as.numeric(recode(x, !!!out))
  }
  out <- mutate(out, cluster = recode_by_size(cluster))
  out
}
add_cluster <- function(views, nclust){
  cluster <- cluster_ts(
    views,
    nclust = nclust,
    id_cols = date,
    names_from = article,
    values_from = views_std
  )
  views %>% select(-starts_with("cluster")) %>% 
    left_join(cluster, by = "article")
}
break_clusters <- function(views){
  arrange(nest(group_by(views, cluster)), cluster)$data
}

