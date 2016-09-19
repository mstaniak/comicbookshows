#' Filter data to plot Netflix show's ratings or compare it to other show.
#'
#' @param showNames chr, full titles of shows to plot.
#' @param seasons list of two vectors with seasons, 
#'                only first element is used.
#' @param minRating dbl, minimum rating to plot.
#' @param maxRating db, maximum rating to plot.
#'
#' @return Data frame with added column with episodes signature seasonxepisode.
#'
#' @export
#'

filterNetflix <- function(showNames, seasons, minRating = 0, maxRating = 10) {
  episodesPlus %>%
    filter(((showTitle == showNames[1] & season == seasons[[1]][1]) |
	   (showTitle == showNames[2] & season == seasons[[2]][1])),
	   typeRating == "imdbRating",
	   rating >= minRating,
	   rating <= maxRating) %>%
    mutate(ep = paste(season, episode, sep = "x"))
}


#' Plot Netflix show's rating or compare it with other show.
#'
#' @param sourceT Data frame returned by filterNetflix function.
#' @param trend lgl, if TRUE, trend line will be ploted.
#'
#' @return ggplot2 object.
#'
#' @export
#'

plotNetflix <- function(sourceT, trend = FALSE) {
  nShow <- n_distinct(sourceT$showTitle)
  plot <- ggplot(sourceT, aes(x = reorder(ep, as.integer(episode)), y = rating)) +
            geom_point(size = 5, color = "white") +
	    geom_line(aes(group = showTitle), linetype = 2, size = 2, color = "white") +
	    theme_hc(bgcolor = "darkunica", base_size = 18) +
	    scale_fill_hc("darkunica") +
	    xlab("") +
	    ylab("") +
	    facet_wrap(~showTitle, scales = "free", ncol = 1) 

  if(trend) {
    plot <- plot + geom_smooth(aes(group = paste(showTitle, season)), method = "lm",
			       size = 1.5, se = FALSE)
  }
  
  plot <- plot + theme(axis.text = element_text(color = "white"),
		       plot.background = element_rect(fill = "#222222"))
  
  return(plot)
}

