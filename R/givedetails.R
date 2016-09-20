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
  sourceData <- filtData
  tooltipText <- ""
  x <- "ep"
  if(!netflix) {
    x <- "airDate"
    sourceData <- filtData[[2]]
  }
  if(is.null(click)) {
    tooltipText <- "Click a point to display details about the episode."
  } else {
    point <- nearPoints(sourceData, click,  xvar = x, yvar = "rating")
    if(dim(point)[1] == 0) {
      tooltipText <- "Click a point to display details about the episode."
    } else if(dim(point)[1] == 1){
      tooltipText <- paste(paste(point[["showTitle"]], 
				 paste(point[["season"]], point[["episode"]], sep = "x"),
				 point[["epTitle"]]),
			   paste("Aired:", point[["airDate"]]),
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
      if(!netflix) {
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
      }
    } else {
      tooltipText <- "Click on a single point to display details about the episode."
    }
  } 
  return(HTML(tooltipText))
}

# 
# #' Display shorter episode info under comparison plot.
# #'
# #' @param filData Data frame returned by filterToPlot or filterNetflix function.
# #' @param click Shiny on_click object.
# #' @param netflix lgl, if TRUE "ep" variable will be used as xvar.
# #'
# #' @return character with show title, episode signature and title.
# #'
# #' @export
# #'
# 
# giveShortDetails <- function(filtData, click, netflix) {
#   sourceData <- filtData
#   tooltipText <- ""
#   x <- "airDate"
#   if(netflix) {
#     x <- "ep"
#   } else {
#     sourceData <- filtData[[2]]
#   }
#   if(is.null(click)) {
#     tooltipText <- "Click on a point to display episode info."
#   }
#   else {
#     point <- nearPoints(sourceData, click,  xvar = x, yvar = "rating")
#     if(dim(point)[1] == 1) {
#       tooltipText <- paste(point[["showTitle"]], 
# 			   paste(point[["season"]], point[["episode"]], sep = "x"),
# 			   point[["epTitle"]])
#     } else if(dim(point)[1] > 1) {
#       tooltipText <- "Click on a single point to display episode info."
#     }
#   }
#   return(tooltipText)
# }
