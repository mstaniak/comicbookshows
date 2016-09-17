#' Text info about given episode.
#'
#' @param filtData Data frame returned by filterToPlot function.
#' @param click shiny object returned on plot click.
#' @param typeRating chr, "imdbRating" or "nielsenRating".
#' @param netflix lgl, if TRUE, xvar will be "ep"
#'
#' @return html with show name, episode's signature, episode's title,
#'         rating and number of votes/numbers of viewers, air date in 
#'         separate lines.
#'
#' @export
#'

giveDetails <- function(filtData, click, typeRating, netflix = FALSE) {
  tooltipText <- ""
  x <- "airDate"
  if(netflix) {
    x <- "ep"
  }
  if(is.null(click)) {
    tooltipText <- "Click a point to display details about the episode."
  } else {
    point <- nearPoints(filtData, click,  xvar = x, yvar = "rating")
    if(dim(point)[1] == 0) {
      tooltipText <- "Click a point to display details about the episode."
    } else {
      tooltipText <- paste(paste("Episode details:", point[["showTitle"]], 
				 paste(point[["season"]], point[["episode"]], sep = "x")),
			   paste("Title:", point[["epTitle"]]),
# 			   paste("Aired:", point[["airDate"]]),
			   paste("Rating:", round(as.numeric(point[["rating"]]), 2)),
			   sep = "<br />")
      if(typeRating == "imdbRating") {
	tooltipText <- paste(tooltipText,
			     paste("Number of votes:", point[["numOfVotes"]]),
			     sep = "<br />")

      } else {
	tooltipText <- paste(tooltipText,
			     paste("Viewers in milions:", point[["viewers"]]),
			     sep = "<br />")
      }
    }
  }
  return(HTML(tooltipText))
}
