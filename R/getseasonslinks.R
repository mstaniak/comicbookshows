#' Get links to all seasons for a given show.
#'
#' @param show_link Link to the show's main page in IMDb.
#'
#' @return Vector of full links to show's seasons.
#'
#' @export
#'

get_seasons_links <- function(show_link) {
  read_html(show_link) %>%
    html_nodes(".seasons-and-year-nav") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    filter(grepl(., pattern = "season")) -> seasons
    unlist(str_split(paste0("https://imdb.com", unlist(seasons)), "&", n = 1))
}
