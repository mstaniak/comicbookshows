#' Links to all episodes of one show.
#'
#' @param seasons Character vector of links to season episodes as returned by get_seasons_links function.
#'
#' @return List. Each element of the list is a vector with links to episodes of one season.
#'
#' @export

get_episodes_links <- function(seasons) {
  unlist(lapply(seasons, get_one_season))
}
