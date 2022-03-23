make_page <- function(name, content) {
  tgs <- tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.3/css/bulma.min.css"
      )
    ),
    tags$body(
      tags$div(class = "tabs",
        tags$ul(
          tags$li(class = ifelse(name == "index", "is-active", ""),
            tags$a("Results", href = "index.html")
          ),
          tags$li(class = ifelse(name == "stats", "is-active", ""),
            tags$a("Statistics", href = "stats.html")
          ),
          tags$li(class = ifelse(name == "adjust", "is-active", ""),
            tags$a("Adjustment variables", href = "adjust.html")
          )
        )
      ),
      tags$div(class = "container is-fluid",
        content
      )
    )
  )
}
