#' Get links to all seasons for a given show.
#'
#' @param show_link Link to the show's main page in IMDb.
#'
#' @return Vector of full links to show's seasons.
#'
#' @export
#'

get_seasons_links <- function(show_link, no_of_seasons) {
  read_html(show_link) %>%
    html_nodes(".seasons-and-year-nav") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    filter(grepl(., pattern = "season")) -> seasons
  str_split(paste0("http://www.imdb.com", unlist(seasons)), "&") %>%
    lapply({function(x) return(x[1])}) %>%
    unlist() %>%
    sort() -> seasons_raw
  return(seasons_raw[1:no_of_seasons])
}
