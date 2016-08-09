#' Information about one episode.
#'
#' @param ep_link Character string: link to episode's IMDb page.
#'
#' @return vector containing show name, value indicating if the show was cancelled,
#'                season and episode number, air date, episode title, rating and number of votes.
#'
#' @export

one_episode_info <- function(ep_link) {
  read_html(ep_link) %>%
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
    str_extract("[1-9]+") -> seasep
  seas <- seasep[1]
  ep <- seasep[2]

  tmp_panel %>%
    html_node("meta") %>%
    html_attr("content") -> emis_date

  tmp_panel %>%
    # html_node(".title_wraper") %>%
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
