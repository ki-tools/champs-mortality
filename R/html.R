tags <- htmltools::tags

#' Create html outputs for a given mortality rate/fraction analysis
#' @param obj an object that comes from [get_rates_and_fractions()]
#' @param path path to where the output html will be stored
#' @export
make_outputs <- function(obj, path = tempfile()) {
  assertthat::assert_that(inherits(obj, "rate_frac_multi_site"),
    msg = cli::format_error("'obj' must come from get_rates_and_fractions()")
  )

  if (!dir.exists(path))
    dir.create(path)

  message("index")
  write_page(index_page(obj), path)
  message("factor adjustment table")
  write_page(fac_adj_page(obj), path)
  message("MITS selection tables")
  write_page(stats_page(obj, mits = TRUE), path)
  message("Condition selection tables")
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
    htmltools::tagList(
      tags$div(class = "plot-container",
        tags$div(p1),
        tags$div(p2)
      ),
      tags$div(table_overview(obj))
    ),
    condition = obj[[1]]$condition,
    causal_chain = obj[[1]]$causal_chain
  )
}



fac_adj_page <- function(obj) {
  tbl <- table_adjust_decision(obj)

  name <- "fac_adj"
  make_page(name,
    htmltools::tagList(
      tbl
    ),
    condition = obj[[1]]$condition,
    causal_chain = obj[[1]]$causal_chain
  )
}

stats_page <- function(obj, mits = TRUE) {
  prefix <- ifelse(mits, "mits", "cond")

  tmp1 <- lapply(obj, function(x) x[[paste0(prefix, "_dss")]]) %>%
    dplyr::bind_rows()
  tmp2 <- lapply(obj, function(x) x[[paste0(prefix, "_non_dss")]]) %>%
    dplyr::bind_rows()

  tmp <- dplyr::bind_rows(
    tmp1 %>% dplyr::mutate(dss = TRUE),
    tmp2 %>% dplyr::mutate(dss = FALSE)
  ) %>%
    dplyr::arrange_at(c("site", "catchment"))

  tbls <- split(tmp, paste(tmp$site, tmp$catchment)) %>%
    lapply(table_factor_sig_stats)

  content <- htmltools::tagList(
    tags$div(class = "table-container",
      tbls
    )
  )

  name <- paste0("stats_", prefix)
  make_page(name,
    htmltools::tagList(
      content
    ),
    condition = obj[[1]]$condition,
    causal_chain = obj[[1]]$causal_chain
  )
}

make_page <- function(name, content, condition, causal_chain) {
  cc <- ifelse(causal_chain, "In the causal chain", "In the underlying cause")

  tgs <- htmltools::tagList(
    tags$head(
      tags$style(htmltools::HTML("
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

.tabs > a > div.tab-item {
  padding-top: 11px;
  padding-bottom: 10px;
  border-right: 1px solid rgba(0, 0, 0, 0.1);
}

.tabs > a > div {
  font-weight: 300;
  font-size: 16px;
  padding-left: 30px;
  padding-right: 30px;
}

.tabs > a {
  text-decoration: none;
  color: white;
}

.tab-item:hover {
  background: #64b5f6;
}

.tabs > a > div.is-active {
  border-bottom: 2px solid #1e88e5;
  background: #42a5f5;
}

.plot-container {
  width: 100%;
  height: calc(60vh - 48px);
  padding: 10px;
  display: flex;
  flex-direction: row;
}

.plot-container > div {
  width: calc(50vw - 10px);
}

#notice {
  position: fixed;
  bottom: 0;
  right: 0;
  background: rgb(255, 255, 255, 0.4);
  color: darkgray;
  font-weight: 300;
  font-size: 14px;
  padding: 5px;
}

a {
  color: black;
}
      ")),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
        crossorigin = ""),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@200;300;400;500;600&amp;display=swap",
        rel = "stylesheet")
    ),
    tags$body(
      tags$div(id = "notice",
        "These results are produced using the ",
        tags$a(href = "https://github.com/ki-tools/champs-mortality", "champs-mortality"),
        " R package, which is under active development. The statistics provided have not been fully validated and should not yet be treated as reliable."
      ),
      tags$div(class = "header",
        tags$div(class = "title",
          tags$div(class = "condition", condition),
          tags$div(class = "cc", cc)
        ),
        tags$div(class = "tabs",
          tags$a(
            tags$div(class = ifelse(
              name == "index", "tab-item is-active", "tab-item"),
              "Results"
            ),
            href = "index.html"
          ),
          tags$a(
            tags$div(class = ifelse(
              name == "fac_adj", "tab-item is-active", "tab-item"),
              "Factor Adjustment Table"
            ),
            href = "fac_adj.html"
          ),
          tags$a(
            tags$div(class = ifelse(
              name == "stats_mits", "tab-item is-active", "tab-item"),
              "MITS Selection Stats"
            ),
            href = "stats_mits.html"
          ),
          tags$a(
            tags$div(class = ifelse(
              name == "stats_cond", "tab-item is-active", "tab-item"),
              "Condition Selection Stats"
            ),
            href = "stats_cond.html"
          ),
        )
      ),
      content
    )
  )

  list(tags = tgs, page = paste0(name, ".html"))
}
