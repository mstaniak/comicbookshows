#' Filter data to plot Netflix show's ratings or compare it to other show.
#'
#' @param showNames chr, full titles of shows to plot.
#' @param seasons chr, one season for each show in showNames.
#' @param minRating dbl, minimum rating to plot.
#' @param maxRating db, maximum rating to plot.
#'
#' @return Data frame with added column with episodes signature seasonxepisode.
#'
#' @export
#'

filterNetflix <- function(showNames, seasons, minRating = 0, maxRating = 10) {
  episodes %>%
    select(showTitle, season, episode, imdbRating, numOfVotes) %>%
    filter(((showTitle == showNames[1] & season == seasons[1]) |
	   (showTitle == showNames[2] & season == seasons[2])),
	   imdbRating >= minRating,
	   imdbRating <= maxRating) %>%
    mutate(ep = paste(season,  episode, sep = "x"))
}


#' Plot Netflix show's rating or compare it with other show.
#'
#' @param sourceT Data frame returned by filterNetflix function.
#'
#' @return ggplot2 object.
#'
#' @export
#'

plotNetflix <- function(sourceT) {
  ggplot(sourceT, aes(x = ep, y = imdbRating)) +
    geom_point(size = 3) +
    geom_line(aes(group = showTitle), linetype = 2) +
    theme_bw() +
    xlab("") +
    ylab("") +
    facet_grid(~showTitle)
}

