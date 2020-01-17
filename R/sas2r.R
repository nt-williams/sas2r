#' Find table indexes from an html document matching patterns.
#'
#' @param x An object of class \code{xml_document}
#' @param pattern Pattern to search for within html nodes of CSS class "table"
#'
#' @return a vector of indexes that indicates nodes matching the pattern.
#' @export
search_tables <- function(x, pattern) {
  nodes <- as.character(rvest::html_nodes(x, css = "table"))
  i <- grep(pattern, nodes)
  return(i)
}

#' Parse an html table into a tibble.
#'
#' @param x An object of class \code{xml_document}
#' @param i html node index to parse as a tibble
#'
#' @return The html node as a tibble.
#' @export
get_table <- function(x, i) {
  node <- rvest::html_table(rvest::html_nodes(x, css = "table")[[i]], fill = T)
  node_tbl <- tibble::as_tibble(node, .name_repair = "unique")
  return(node_tbl)
}

#' Parse HTML tables matching patterns into tibbles.
#'
#' @param x An object of class \code{xml_document}
#' @param pattern Pattern to search for within html nodes of CSS class "table"
#'
#' @return A list of tibbles.
#' @export
sas2r <- function(x, pattern) {
  i <- search_tables(x, pattern)
  out <- purrr::map(i, get_table)
  return(out)
}
