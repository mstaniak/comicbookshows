#' Information about given episodes extracted from IMDb pages.
#'
#' @param eps_link Links to episodes returned from get_episodes_links function.
#'
#' @return Data frame with columns show (character, name of the show), cancelled (logical, TRUE if show has been cancelled),
#'         season (character, season number), episode (episode numebr within season),
#'         air_date (date, air date), rating (numeric, IMDb rating), num_of_votes (integer, number of rating votes).
#'

get_episodes_info <- function(eps_links) {
  tmp_result <- data.frame(t(apply(data.frame(eps_links), 1, one_episode_info)), stringsAsFactors = F)
  colnames(tmp_result) <- c("show", "cancelled", "season", "episode", "air_date",
                            "title", "rating", "num_of_votes")
  tmp_result %>%
    mutate(cancelled = as.logical(cancelled),
           # season = as.factor(season),
           # episode = as.factor(episode),
           # air_date = as_date(air_date),
           rating = as.numeric(rating),
           num_of_votes = as.integer(str_replace(num_of_votes, pattern = ",", replacement = ""))) %>%
    as_tibble()
}
