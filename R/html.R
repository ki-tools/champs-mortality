

make_site <- function(obj, path = tempfile()) {
  if (!dir.exists(path))
    dir.create(path)

  write_page(index_page(obj), path)
  write_page(index_page(obj), path)
  write_page(fac_adj_page(obj, dss = TRUE), path)
  write_page(fac_adj_page(obj, dss = FALSE), path)
  write_page(stats_page(obj, mits = TRUE, dss = TRUE), path)
  write_page(stats_page(obj, mits = TRUE, dss = FALSE), path)
  write_page(stats_page(obj, mits = FALSE, dss = TRUE), path)
  write_page(stats_page(obj, mits = FALSE, dss = FALSE), path)

  path
}

write_page <- function(obj, path) {
  htmltools::save_html(obj$tags, file.path(path, obj$page))
}

index_page <- function(obj) {
  p1 <- plot_rates_fracs(obj, "frac")
  p2 <- plot_rates_fracs(obj, "rate")

  make_page("index",
    tagList(
      tags$div(class = "plot-container",
        tags$div(p1),
        tags$div(p2)
      )
    ),
    condition = obj$condition,
    causal_chain = obj$causal_chain
  )
}

fac_adj_page <- function(obj, dss = TRUE) {
  suffix <- ifelse(dss, "_dss", "_non_dss")
  tmp <- list(
    MITS = obj[[paste0("mits", suffix)]],
    other = obj[[paste0("cond", suffix)]]
  )
  names(tmp)[2] <- obj$cond_name
  tbl <- combine_decision_tables(tmp)
  tb <- table_adjust_decision(tbl)

  name <- paste0("fac_adj", suffix)
  make_page(name,
    tagList(
      tb
    ),
    condition = obj$condition,
    causal_chain = obj$causal_chain
  )
}

stats_page <- function(obj, mits = TRUE, dss = TRUE) {
  suffix <- ifelse(dss, "_dss", "_non_dss")
  prefix <- ifelse(dss, "mits", "cond")
  tmp <- obj[[paste0(prefix, suffix)]]
  tbls <- split(tmp, tmp$site) %>%
    lapply(table_factor_sig_stats)

  content <- tagList(
    tags$div(class = "table-container",
      tbls
    )
  )

  name <- paste0("stats_", prefix, suffix)
  make_page(name,
    tagList(
      content
    ),
    condition = obj$condition,
    causal_chain = obj$causal_chain
  )
}

make_page <- function(name, content, condition, causal_chain) {
  cc <- ifelse(causal_chain, "In the causal chain", "In the underlying cause")

  tgs <- tagList(
    tags$head(
      tags$style(HTML("
body {
  font-family: 'Poppins', sans-serif;
  padding: 0px;
  margin: 0px;
}

.table-container {
  display: flex;
  flex-direction: row;
  overflow: auto;
  align-items: center;
  justify-content: center;
}
.table-container > * {
  margin-left: 5px;
  margin-right: 5px;
  overflow-x:unset;
  overflow-y:unset;
}
[class^='gt_'], [class*=' gt_']{
  white-space: nowrap;
}

.tabs {
  border-bottom: 1px solid #e7e7e7;
  background: #90caf9;
  color: white;
  display: flex;
  flex-direction: row;
  margin-bottom: 20px;
}

.tabs > div {
  background: #454545;
  padding-left: 40px;
  padding-right: 60px;
  padding-top: 2px;
  padding-bottom: 2px;
  display: flex;
  flex-direction: column;
}

.tabs > div > div.condition {
  font-size: 18px;
}

.tabs > div > div.cc {
  font-size: 11px;
}

.tabs > ul {
  display: flex;
  flex-direction: row;
  list-style-type: none;
  margin: 0;
  padding: 0;
}

.tabs > ul > li {
  padding-top: 11px;
  padding-bottom: 10px;
  padding-left: 30px;
  padding-right: 30px;
}

.tabs > ul > li > a {
  text-decoration: none;
  color: white;
  font-weight: 300;
  font-size: 16px;
}

.tabs > ul > li.is-active > a {
  color: white;
}

.tabs > ul > li.is-active {
  border-bottom: 2px solid #1e88e5;
  background: #64b5f6;
}

.plot-container {
  width: 100%;
  height: calc(65vh - 48px);
  padding: 10px;
  display: flex;
  flex-direction: row;
}

.plot-container > div {
  width: calc(50vw - 10px);
}
      ")),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
        crossorigin = ""),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@200;300;400;500;600&amp;display=swap",
        rel = "stylesheet")
    ),
    tags$body(
      tags$div(class = "tabs",
        tags$div(
          tags$div(class = "condition", condition),
          tags$div(class = "cc", cc)
        ),
        tags$ul(
          tags$li(class = ifelse(name == "index", "is-active", ""),
            tags$a("Results", href = "index.html")
          ),
          tags$li(class = ifelse(name == "fac_adj_dss", "is-active", ""),
            tags$a("Factor Adj. (DSS)", href = "fac_adj_dss.html")
          ),
          tags$li(class = ifelse(name == "fac_adj_non_dss", "is-active", ""),
            tags$a("Factor Adj. (non-DSS)", href = "fac_adj_non_dss.html")
          ),
          tags$li(class = ifelse(name == "stats_dss", "is-active", ""),
            tags$a("MITS Stats (DSS)", href = "stats_mits_dss.html")
          ),
          tags$li(class = ifelse(name == "stats_mits_non_dss", "is-active", ""),
            tags$a("MITS Stats (non-DSS)", href = "stats_non_dss.html")
          ),
          tags$li(class = ifelse(name == "stats_dss", "is-active", ""),
            tags$a("Cond Stats (DSS)", href = "stats_cond_dss.html")
          ),
          tags$li(class = ifelse(name == "stats_non_dss", "is-active", ""),
            tags$a("Cond Stats (non-DSS)", href = "stats_cond_non_dss.html")
          )
        )
      ),
      content
    )
  )

  list(tags = tgs, page = paste0(name, ".html"))
}
