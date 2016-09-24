#' Text info about given episode.
#'
#' @param filtData Data frame returned by filterToPlot function.
#' @param click shiny object returned on plot click.
#' @param typeRating chr, "imdbRating" or "nielsenRating".
#'
#' @return html with show name, episode's signature, episode's title,
#'         rating and number of votes/numbers of viewers, air date in 
#'         separate lines.
#'
#' @export
#'

giveDetails <- function(filtData, click, typeRating) {
  sourceData <- filtData[[2]]
  tooltipText <- ""
  if(is.null(click)) {
    tooltipText <- "Click a point to display details about the episode."
  } else {
    point <- nearPoints(sourceData, click,  xvar = "airDate", yvar = "rating")
    if(dim(point)[1] == 0) {
      tooltipText <- "Click a point to display details about the episode."
    } else if(dim(point)[1] == 1){
      tooltipText <- paste(paste(point[["showTitle"]], 
				 paste(point[["season"]], point[["episode"]], sep = "x"),
				 point[["epTitle"]]),
			   paste("Aired:", point[["airDate"]]),
			   paste("Rating:", round(as.numeric(point[["rating"]]), 2)),
			   sep = "<br />")
      if(typeRating == "nielsenRating") {
        tooltipText <- paste(tooltipText,
			     paste("Viewers in milions:", point[["viewers"]]),
			     sep = "<br />")
      } 
	filtData[[1]] %>%
	  filter(datePlot == point[["airDate"]]) -> greyLineInfo
	if(dim(greyLineInfo)[1] != 0) {
	  form <- "shows"
	  if(greyLineInfo[["noOfShows"]] == 1) {
	    form <- "show"
	  } 
	  tooltipText <- paste(tooltipText, 
			       paste(greyLineInfo[["noOfShows"]], "other", form, "aired that week."),
			       sep = "<br />")
	}
      
    } else {
      tooltipText <- "Click on a single point to display details about the episode."
    }
  } 
  return(HTML(tooltipText))
}


#' Display shorter episode info under comparison plot.
#'
#' @param filData Data frame returned by filterToPlot or filterNetflix function.
#' @param click Shiny on_click object.
#' @param seasons numeric, vector of plotted seasons numbers.
#'
#' @return character with show title, episode signature, title and IMDb ratings info.
#'
#' @export
#'

giveShortDetails <- function(filtData, click, seasons) {
  tooltipText <- ""
  if(length(seasons) > 1) {
    tooltipText <- ""
  } else if(length(seasons) == 1) {
    if(is.null(click)) {
      tooltipText <- "Click on a point to display episode details."
    } else {
      filtData %>%
	arrange(as.integer(episode)) -> tmp
      point <- tmp[round(click$x), c("showTitle", "epTitle", "ep", "rating", "numOfVotes")]
      tooltipText <- paste(paste(point$showTitle, point$ep, point$epTitle),
			   paste("Rating:", point$rating),
			   sep = "<br />")
    }
  }
  return(HTML(tooltipText))
}
