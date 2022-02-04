
msg_wrap <- function(..., indent = 0, exdent = 0, blank = TRUE) {
  wdth <- getOption("width") - 2 - indent
  msg <- paste0(..., sep = "")
  content <- strwrap(msg, width = wdth, indent = indent, exdent = exdent)
  paste0(ifelse(blank, "\n", ""),
    paste0(content, collapse = "\n"))
}
