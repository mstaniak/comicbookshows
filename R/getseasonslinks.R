#' Get links to all seasons for a given show.
#'
#' @param showLink Link to the show's main page in IMDb.
#' @param noOfSeasons Number of show's seasons.
#'
#' @return Vector of full links to show's seasons.
#'
#' @export
#'

getSeasonsLinks <- function(showLink, noOfSeasons) {
  read_html(showLink) %>%
    html_nodes(".seasons-and-year-nav") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    as.data.frame() %>%
    filter(grepl(., pattern = "season")) -> seasons
  str_split(paste0("http://www.imdb.com", unlist(seasons)), "&") %>%
    lapply({function(x) return(x[1])}) %>%
    unlist() %>%
    sort() -> seasonsRaw
  return(seasonsRaw[1:noOfSeasons])
}
