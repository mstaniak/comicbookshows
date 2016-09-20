#' Links to all episodes of one season of one show.
#'
#' @param season String character containing link to a season page.
#'
#' @return Vector with links to episodes from a given season.
#'
#' @export

getOneSeason <- function(season) {
  read_html(season) %>%
    html_node(".list.detail.eplist") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique() %>%
    paste0("http://www.imdb.com", .) -> tmp
  tmp <- data.frame(tmp, stringsAsFactors = F)
  colnames(tmp) <- "seas_link"
  tmp %>%
    filter(grepl(seas_link, pattern = "ref", fixed = T)) %>%
    unlist(use.names = F)
}


#' Links to all episodes of one show.
#'
#' @param seasons Character vector of links to season episodes as returned by getSeasonLinks function.
#'
#' @return List. Each element of the list is a vector with links to episodes of one season.
#'
#' @export

get_episodes_links <- function(seasons) {
  unlist(lapply(seasons, getOneSeason))
}
