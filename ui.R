horizontalSplitLayout <- function(left, right, lwidth='370px') {
  bootstrapPage(
    tags$head(tags$style(type="text/css", "html, body {width: 100%; height: 100%; overflow: hidden}")),
    tags$div(style="position: absolute; top: 0; left: 0; right: 0; bottom: 0",
      tags$div(style=paste("position: absolute; top: 0; left: 0; width:", lwidth, "; bottom: 0; overflow: auto"), left),
      tags$div(style=paste("position: absolute; top: 0; left:", lwidth, "; right: 0; bottom: 0; overflow: auto"), right)
    )
  )
}

sidebarSplitLayout <- function(controls, main) {
  horizontalSplitLayout(
    left = tags$div(class="well", style="margin: 30px",
      controls
    ),
    right = main,
    lwidth = '200px'
  )
}

shinyUI(sidebarSplitLayout(
  tagList(
    sliderInput("nclust", "Number of clusters:", 3, min = 1, max = 6),
    textAreaInput("articles", "Articles:", "Alcohol, Nicotine, Caffeine, Cannabis, Methamphetamine, Cocaine, Heroin, Ecstasy, Fentanyl",
                  width="100%", height="400px"),
    downloadButton("download", "Save")
  ),
  plotOutput("plot", width="100%", height="100%")
))
