shinyServer(function(input, output) {
  library("fs")
  library("tidyverse")
  library("psych")
  library("lubridate")
  library("here")
  library("tsibble")
  library("pdc")
  library("cowplot")
  library("patchwork")
  library("pageviews")
  dir_walk(here("R"), source)
  dir_walk(here("R"), knitr::read_chunk)
  articles <- reactive(str_squish(flatten_chr(str_split(input$articles, ","))))
  views <- reactive(
    get_articles(articles(), ymd("2015-01-01"), ymd("2019-12-31")) %>%
      remove_outliers() %>%
      extract_trend() %>%
      add_dates() %>%
      extract_wday_trend() %>%
      extract_year_trend() %>%
      remove_pattern() %>%
      add_cluster(nclust = input$nclust)
  )
  output$plot <- reactivePlot(function() {
    plot_final(views())
  })
  output$download <- downloadHandler("wiki-trends.pdf",
                                     function(file)ggsave(file, plot_final(views()), width = 16, height = 9))
})
