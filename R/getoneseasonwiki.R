#' Download ratings table from wikipedia page.
#'
#' @param link Url of a wikipedia page that containts the ratings table.
#' @param selector Unique css selector of ratings table.
#'
#' @return List returned by rvest html_table function.
#'
#' @export
#'

getOneSeasonWiki <- function(link, selector) {
  read_html(link) %>%
    html_nodes(css = selector) %>%
    html_table()
}

#' Short function for changing column names after downloading the data.
#'
#' @param df Data frame returned by getOneSeasonWiki function after manipulations from datadownload.R file.
#'
#' @return Data frame df with changed colnames.
#'
#' @export
#'

changeColnames <- function(df) {
  colnames(df) <- c("episode", "ratingShare", "viewers")
  return(df)
}
