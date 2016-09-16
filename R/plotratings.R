#' Helper function to change day of the week to given day of the week.
#'
#' @param actualDate Date to be modified by changing day of the week.
#' @param targetWday Day of the week as coded in lubridate package.
#'
#' @return Date given date with day chosen to closest given day of the week.
#'

changeWday <- function(actualDate, targetWday) {
  actualDate - wday(actualDate) + targetWday
}


#' Filter data for plotting function.
#'
#' @param showNames chr vector with one or two full names of shows.
#' @param typeRating chr, one of vales "imdbRating", "nielsenRating". 
#' @param seasons int vector with numbers of season to display.
#' @param minRating dbl, only episodes with higher rating will be displayed.
#' @param maxRating dbl, only episodes with lower rating will be displayed.
#' @param minDate date, minimum air date to display.
#' @param maxDate date, maximum air date to display.
#'
#' @return list of two data frame: first with data for background line,
#'         second with data for chosen shows.
#'
#' @export
#'

filterToPlot <- function(showNames, typeRating, seasons = defaultSeasons,
                         minRating = 0, maxRating = 10,
		         minDate = defaultMinDate, maxDate = defaultMaxDate) {
  
  episodesPlus %>%
    select(showTitle, airDate) %>%
    filter(showTitle %in% showNames) %>%
    summarise(first_ep = min(airDate),
              last_ep = max(airDate)) %>%
    unlist() %>%
    as_date() -> dates
  
  episodesPlus %>%
    filter(!(showTitle %in% showNames),
             airDate >= max(dates["first_ep"], minDate),
             airDate <= min(dates["last_ep"], maxDate),
             channel != "Netflix") %>%
    filter_(interp("c >= minR",
                   c = as.name(typeRating), minR = minRating),
            interp("c <= maxR",
                   c = as.name(typeRating), maxR = maxRating)) %>%
    rename_(.dots = setNames(typeRating, "rating")) %>%
    mutate(datePlot = changeWday(airDate, wday(dates["first_ep"]))) %>%
    group_by(datePlot) %>%
    summarise(rating = mean(rating)) -> otherShows

  episodesPlus %>%
    filter(showTitle %in% showNames,
	   airDate >= minDate,
           airDate <= maxDate,
	   season %in% 1:seasons) %>%
    filter_(interp("c >= minR",
                   c = as.name(typeRating), minR = minRating),
            interp("c <= maxR",
                   c = as.name(typeRating), maxR = maxRating)) %>%
    rename_(.dots = setNames(typeRating, "rating")) %>%
    select(showTitle, airDate, season, rating) -> chosenShows 
 
  return(list(otherShows, chosenShows))
} 


#' Plot IMDb ratings of one show.
#'
#' @param background  logical, if TRUE, ratings for other shows will be displayed.
#'
#' @param trend logical, if TRUE, trend line for season is plotted.
#' @param ... arguments passed to filterToPlot function.
#'
#' @return ggplot2 object.
#'
#' @export

plotRatings <- function(background = TRUE, trend = FALSE, ...) {

  sources <- filterToPlot(...)

  plot <-  ggplot(sources[[2]], aes(x = airDate, y = rating, color = showTitle)) 

  if(background)
    plot <- plot + geom_line(data = sources[[1]], aes(x = datePlot, y = rating),
                             inherit.aes = FALSE, color = "grey")

  plot <- plot + geom_line(aes(group = paste0(showTitle, season)), size = 1.5) +
                 theme_bw() +
                 xlab("") +
                 ylab("")
  if(trend)
    plot <- plot + geom_smooth(aes(group = paste0(showTitle, season)), method = "lm",
			       se = FALSE, linetype = 2, size = 0.8)
  return(plot)
}

