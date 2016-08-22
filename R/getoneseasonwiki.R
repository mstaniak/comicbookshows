#' Download ratings table from wikipedia page.
#'
#' @param link Url of a wikipedia page that containts the ratings table.
#' @param selector Unique css selector of ratings table.
#'
#' @return List returned by rvest html_table function.
#'
#' @export

get_one_season_wiki <- function(link, selector) {
  read_html(link) %>%
    html_nodes(css = selector) %>%
    html_table()
}
