#' Function that returns a tooltip for plotOneShow plot.
#' 
#' @param key Identifier from plotOneShow function.
#'
#' @return HTML generating tooltip.
#' 
#' @export

giveTooltip <- function(key) {
  sel <- strsplit(key$key, split = "_", fixed = TRUE)
  labelSrc <- as.list(episodes[paste(episodes$showTitle, episodes$season, episodes$episode, sep = "_") == key$key,])
  if(labelSrc$episode < 10) 
    labelSrc$episode <- paste0("0", labelSrc$episode)
  return(paste("Show:", labelSrc$showTitle, "<br />",
	       "Episode:", paste(labelSrc$season, labelSrc$episode, sep = "x"), "<br />",
	       "Title:", labelSrc$title, "<br />"
	      ))
}
