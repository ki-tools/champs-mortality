
#' @export
make_site <- function(obj, path = tempfile()) {
  if (!dir.exists(path))
    dir.create(path)

  write_page(index_page(obj), path)
  write_page(index_page(obj), path)
  write_page(fac_adj_page(obj), path)
  write_page(stats_page(obj, mits = TRUE), path)
  write_page(stats_page(obj, mits = FALSE), path)

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

fac_adj_page <- function(obj) {
  tmp1 <- bind_rows(
    res$mits_dss %>% mutate(dss = TRUE),
    res$mits_non_dss %>% mutate(dss = FALSE)
  ) %>%
    arrange(site, catchment)
  tmp2 <- bind_rows(
    res$cond_dss %>% mutate(dss = TRUE),
    res$cond_non_dss %>% mutate(dss = FALSE)
  ) %>%
    arrange(site, catchment)
  tmp <- list(
    MITS = tmp1,
    other = tmp2
  )
  tbl <- combine_decision_tables(tmp)
  tb <- table_adjust_decision(tbl)

  name <- "fac_adj"
  make_page(name,
    tagList(
      tb
    ),
    condition = obj$condition,
    causal_chain = obj$causal_chain
  )
}

stats_page <- function(obj, mits = TRUE) {
  prefix <- ifelse(mits, "mits", "cond")

  tmp1 <- obj[[paste0(prefix, "_dss")]]
  tmp2 <- obj[[paste0(prefix, "_non_dss")]]
  tmp <- bind_rows(
    tmp1 %>% mutate(dss = TRUE),
    tmp2 %>% mutate(dss = FALSE)
  ) %>%
    arrange(site, catchment)

  tbls <- split(tmp, paste(tmp$site, tmp$catchment)) %>%
    lapply(table_factor_sig_stats)

  content <- tagList(
    tags$div(class = "table-container",
      tbls
    )
  )

  name <- paste0("stats_", prefix)
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

.header {
  border-bottom: 1px solid #e7e7e7;
  background: #90caf9;
  color: white;
  display: flex;
  flex-direction: row;
  margin-bottom: 20px;
}

.title {
  background: #454545;
  padding-left: 40px;
  padding-right: 60px;
  padding-top: 2px;
  padding-bottom: 2px;
  display: flex;
  flex-direction: column;
}

.title > div.condition {
  font-size: 18px;
}

.title > div.cc {
  font-size: 11px;
}

.tabs {
  display: flex;
  flex-direction: row;
  list-style-type: none;
  margin: 0;
  padding: 0;
}

.tabs > div.tab-item {
  padding-top: 11px;
  padding-bottom: 10px;
  border-right: 1px solid rgba(0, 0, 0, 0.1);
}

.tab-item > a {
  text-decoration: none;
  color: white;
  font-weight: 300;
  font-size: 16px;
  padding-left: 30px;
  padding-right: 30px;
}

.tab-item:hover {
  background: #64b5f6;
}

.tab-item.is-active > a {
  color: white;
}

.tab-item.is-active {
  border-bottom: 2px solid #1e88e5;
  background: #42a5f5;
}

.tab-group {
  display: flex;
  flex-direction: column;
  text-align: center;
}

.tab-group-header {
  font-size: 14px;
  background: #64b5f6;
  border-right: 1px solid rgba(0, 0, 0, 0.1);
}

.tab-group-content {
  display: flex;
  flex-direction: row;
}

.tab-group-content > div.tab-item {
  border-right: 1px solid rgba(0, 0, 0, 0.1);
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
      tags$div(class = "header",
        tags$div(class = "title",
          tags$div(class = "condition", condition),
          tags$div(class = "cc", cc)
        ),
        tags$div(class = "tabs",
          tags$div(class = ifelse(
            name == "index", "tab-item is-active", "tab-item"),
            tags$a("Results", href = "index.html")
          ),
          # tags$div(class = "tab-group",
          #   tags$div(class = "tab-group-header",
          #     "DSS catchment background"
          #   ),
          #   tags$div(class = "tab-group-content",
              tags$div(class = ifelse(
                name == "fac_adj", "tab-item is-active", "tab-item"),
                tags$a("Factor Adjustment Table", href = "fac_adj.html")
              ),
              tags$div(class = ifelse(
                name == "stats_mits", "tab-item is-active", "tab-item"),
                tags$a("MITS Selection Stats", href = "stats_mits.html")
              ),
              tags$div(class = ifelse(
                name == "stats_cond", "tab-item is-active", "tab-item"),
                tags$a("Condition Selection Stats", href = "stats_cond.html")
              ),
          #   )
          # ),
          # tags$div(class = "tab-group",
          #   tags$div(class = "tab-group-header",
          #     "non-DSS catchment background"
          #   ),
          #   tags$div(class = "tab-group-content",
          #     tags$div(class = ifelse(
          #       name == "fac_adj_non_dss", "tab-item is-active", "tab-item"),
          #       tags$a("Factor Adj.", href = "fac_adj_non_dss.html")
          #     ),
          #     tags$div(class = ifelse(
          #       name == "stats_mits_non_dss", "tab-item is-active", "tab-item"),
          #       tags$a("MITS Stats", href = "stats_mits_non_dss.html")
          #     ),
          #     tags$div(class = ifelse(
          #       name == "stats_cond_non_dss", "tab-item is-active", "tab-item"),
          #       tags$a("Cond Stats", href = "stats_cond_non_dss.html")
          #     )
          #   )
          # )
        )
      ),
      content
    )
  )

  list(tags = tgs, page = paste0(name, ".html"))
}
