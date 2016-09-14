#' Information about one episode.
#'
#' @param epLink Character string: link to episode's IMDb page.
#'
#' @return vector containing show name, value indicating if the show was cancelled,
#'                season and episode number, air date, episode title, rating and number of votes.
#'
#' @export

oneEpisodeInfo <- function(epLink) {
  read_html(epLink) %>%
    html_node("#title-overview-widget") -> tmpPanel

  tmPanel %>%
    html_node(".titleParent") %>%
    html_node("a") %>%
    html_text() -> showName

  tmpPanel %>%
    html_node(".parentDate") %>%
    html_text() %>%
    substr(2, nchar(.) - 1) %>%
    substr(6, nchar(.)) -> canc
  cancelled <- !(canc == " ")

  tmpPanel %>%
    html_node(".bp_heading") %>%
    html_text() %>%
    str_split("\\|") %>%
    unlist() %>%
    str_extract("[0-9]+") -> seasEp
  seas <- seasEp[1]
  ep <- seasEp[2]

  tmpPanel %>%
    html_nodes("meta") %>%
    html_attr("content") %>%
    last() -> emisDate

  tmpPanel %>%
    html_node("h1") %>%
    html_text() -> epTitle

  tmpPanel %>%
    html_node(".ratingValue") %>%
    html_node("span") %>%
    html_text() -> epRating

  tmpPanel %>%
    html_node(".imdbRating") %>%
    html_node("a") %>%
    html_text() -> votesNo

  return(c(showName,
           cancelled,
           seas,
           ep,
           emisDate,
           epTitle,
           epRating,
           votesNo))
}


#' Information about given episodes extracted from IMDb pages.
#'
#' @param epLinks Links to episodes returned from get_episodes_links function.
#'
#' @return Data frame with columns show (character, name of the show), cancelled (logical, TRUE if show has been cancelled),
#'         season (character, season number), episode (episode numebr within season),
#'         air_date (date, air date), rating (numeric, IMDb rating), num_of_votes (integer, number of rating votes).
#'
#' @export

getEpisodesInfo <- function(epLinks) {
  tmpResult <- data.frame(t(apply(data.frame(epLinks), 1, oneEpisodeInfo)), stringsAsFactors = F)
  colnames(tmp_result) <- c("showTitle", "cancelled", "season", "episode", "airDate",
                            "title", "imdbRating", "numOfVotes")
  tmp_result %>%
    mutate(cancelled = as.logical(cancelled),
           rating = as.numeric(imdbRating),
           num_of_votes = as.integer(str_replace(numOfVotes, pattern = ",", replacement = ""))) %>%
    as_tibble()
}
