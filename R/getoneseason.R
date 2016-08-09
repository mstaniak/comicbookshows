#' Links to all episodes of one season of one show.
#'
#' @param season String character containing link to a season page.
#'
#' @return Vector with links to episodes from a given season.
#'
#' @export

get_one_season <- function(season) {
  read_html(season) %>%
    html_node(".list.detail.eplist") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique() %>%
    paste0("http://www.imdb.com", .) %>%
    str_split("\\?") %>%
    lapply({function(x) return(x[1])}) %>%
    unlist()
}
