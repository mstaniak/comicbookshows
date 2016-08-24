#' Information about one episode.
#'
#' @param ep_link Character string: link to episode's IMDb page.
#'
#' @return vector containing show name, value indicating if the show was cancelled,
#'                season and episode number, air date, episode title, rating and number of votes.
#'
#' @export

one_episode_info <- function(ep_link) {
  read_html(curl(ep_link, handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>%
    html_node("#title-overview-widget") -> tmp_panel

  tmp_panel %>%
    html_node(".titleParent") %>%
    html_node("a") %>%
    html_text() -> show_name

  tmp_panel %>%
    html_node(".parentDate") %>%
    html_text() %>%
    substr(2, nchar(.) - 1) %>%
    substr(6, nchar(.)) -> canc
  cancelled <- !canc == " "

  tmp_panel %>%
    html_node(".bp_heading") %>%
    html_text() %>%
    str_split("\\|") %>%
    unlist() %>%
    str_extract("[0-9]+") -> seasep
  seas <- seasep[1]
  ep <- seasep[2]

  tmp_panel %>%
    html_nodes("meta") %>%
    html_attr("content") %>%
    last() -> emis_date

  tmp_panel %>%
    html_node("h1") %>%
    html_text() -> ep_title

  tmp_panel %>%
    html_node(".ratingValue") %>%
    html_node("span") %>%
    html_text() -> ep_rating

  tmp_panel %>%
    html_node(".imdbRating") %>%
    html_node("a") %>%
    html_text() -> votes_no

  return(c(show_name,
           cancelled,
           seas,
           ep,
           emis_date,
           ep_title,
           ep_rating,
           votes_no))
}


#' Information about given episodes extracted from IMDb pages.
#'
#' @param eps_link Links to episodes returned from get_episodes_links function.
#'
#' @return Data frame with columns show (character, name of the show), cancelled (logical, TRUE if show has been cancelled),
#'         season (character, season number), episode (episode numebr within season),
#'         air_date (date, air date), rating (numeric, IMDb rating), num_of_votes (integer, number of rating votes).
#'
#' @export

get_episodes_info <- function(eps_links) {
  tmp_result <- data.frame(t(apply(data.frame(eps_links), 1, one_episode_info)), stringsAsFactors = F)
  colnames(tmp_result) <- c("show", "cancelled", "season", "episode", "air_date",
                            "title", "rating", "num_of_votes")
  tmp_result %>%
    mutate(cancelled = as.logical(cancelled),
           rating = as.numeric(rating),
           num_of_votes = as.integer(str_replace(num_of_votes, pattern = ",", replacement = ""))) %>%
    as_tibble()
}
