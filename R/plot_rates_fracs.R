#' Create a plot of rates or fractions
#' @param obj an object from [get_rates_and_fractions()]
#' @param type one of "frac" or "rate"
#' @param plotly should the result be a plotly graph?
#' @export
#' @importFrom ggplot2 aes_string geom_point geom_errorbar labs
#' theme_minimal theme element_line position_dodge
#' @importFrom ggthemes scale_color_tableau
#' @importFrom plotly ggplotly layout config
#' @importFrom forcats fct_reorder
plot_rates_fracs <- function(obj, type = "frac", plotly = TRUE) {
  assertthat::assert_that(inherits(obj, "rate_frac_multi_site"),
    msg = cli::format_error("'obj' must come from get_rates_and_fractions()")
  )
  per <- obj[[1]]$per
  datr <- lapply(obj, function(x) x$rate) %>%
    dplyr::bind_rows()
  datf <- lapply(obj, function(x) x$frac) %>%
    dplyr::bind_rows()

  pdat_frac <- datf %>%
    dplyr::mutate(
      type = factor(ifelse(.data$var == "aCSMF", "Actual", "Crude"),
        levels = c("Crude", "Actual")),
      estimate = round(.data$est, 2),
      lower = round(.data$lower, 2),
      upper = round(.data$upper, 2),
      site = factor(paste0(.data$site, "<br>(", .data$catchments, ")")),
      site = forcats::fct_reorder(.data$site, .data$est),
      text = paste0(.data$site, "<br>", .data$type, ": ", .data$estimate,
        "% (", .data$lower, ", ", .data$upper, ")")
    )
  pdat_rate <- datr %>%
    dplyr::mutate(
      type = factor(ifelse(.data$var == "aTU5MR", "Actual", "Crude"),
        levels = c("Crude", "Actual")),
      estimate = round(.data$est, 2),
      lower = round(.data$lower, 2),
      upper = round(.data$upper, 2),
      site = factor(paste0(.data$site, "<br>(", .data$catchments, ")"),
        levels = levels(pdat_frac$site)),
      text = paste0(.data$site, "<br>", .data$type, ": ", .data$estimate,
        " (", .data$lower, ", ", .data$upper, ")")
    )

  if (type == "frac") {
    make_plot(
      pdat_frac,
      xlb = "Estimate, % (90% Bayesian CrI)",
      ttl = "Cause-specfic mortality fractions (CSMF)",
      plotly = plotly
    )
  } else {
    make_plot(
      pdat_rate,
      xlb = paste("Estimate, per", per, "deaths (90% Bayesian CrI)"),
      ttl = "Actual total under 5 mortality rates (TU5MR)",
      plotly = plotly
    )
  }
}

make_plot <- function(pdat, xlb, ttl, plotly = TRUE) {
  pd <- ggplot2::position_dodge(0.3)
  p <- ggplot2::ggplot(pdat, ggplot2::aes_string("estimate", "site",
    xmin = "lower", xmax = "upper", color = "type", text = "text")) +
    # geom_rect(
    #   ymin = as.numeric(pdat$site) - 0.3,
    #   ymax = as.numeric(pdat$site) + 0.3,
    #   xmin = -Inf, xmax = Inf,
    #   fill = "#ebebeb77") +
    ggplot2::geom_point(position = pd) +
    ggplot2::geom_errorbar(position = pd, width = 0.4) +
    ggplot2::labs(
      x = xlb,
      y = NULL,
      title = ttl
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        size = 20, color = "#ebebeb77"),
      legend.position = "bottom"
    ) +
    ggthemes::scale_color_tableau(name = NULL)

  if (!plotly) {
    return(p)
  }

  pp <- plotly::ggplotly(p, width = NULL, height = NULL, tooltip = "text") %>%
    plotly::layout(
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.12),
      font = list(family = "Poppins, sans-serif"),
      yaxis = list(automargin = FALSE)
    ) %>%
    plotly::config(
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d",
        "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian"
      ),
      displaylogo = FALSE,
      responsive = TRUE,
      toImageButtonOptions = list(
        format = "png",
        filename = "custom_image",
        height = NULL,
        width = NULL,
        scale = 2
      )
    )
  pp$height <- "100%"
  pp$x$layout$margin$l <- 150
  pp$x$layout$margin$t <- 35
  # pp$x$layout$yaxis$automargin <- TRUE

  pp
}