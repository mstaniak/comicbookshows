#' Helper function to change day of the week to given day of the week.
#'
#' @param actualDate Date to be modified by changing day of the week.
#' @param targetWday Day of the week as coded in lubridate package.
#'
#' @return Date given date with day chosen to closest given day of the week.
#'
#' @export
#'
changeWday <- function(actualDate, targetWday) {
  actualDate - wday(actualDate) + targetWday
}


#' Filter data for plotting function.
#'
#' @param showNames chr vector with one or two full names of shows.
#' @param chosenRating chr, one of vales "imdbRating", "nielsenRating", "vs". 
#' @param seasons list with vectors of seasons to display.
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

filterToPlot <- function(showNames, chosenRating, seasons = list(1:defaultSeasons, 1:defaultSeasons),
                         minRating = 0, maxRating = 10,
		         minDate = defaultMinDate, maxDate = defaultMaxDate) {
  
  episodesPlus %>%
    select(showTitle, airDate, season) %>%
    filter(((showTitle == showNames[1] & season %in% seasons[[1]]) |
	    (showTitle == showNames[2] & season %in% seasons[[2]]))) %>%
    select(-season) %>%
    summarise(firstEp = min(airDate),
              lastEp = max(airDate)) %>%
    unlist() %>%
    as_date() -> dates
  
  episodesPlus %>%
    filter(!(showTitle %in% showNames),
	   airDate >= max(dates["firstEp"], minDate),
	   airDate <= min(dates["lastEp"], maxDate),
	   channel != "Netflix") -> otherShows
  if(chosenRating != "vs")
    otherShows %>% {
      filter(typeRating == chosenRating,
	     rating >= minRating,
	     rating <= maxRating) -> otherShows
  }
  otherShows %>%
    mutate(datePlot = changeWday(airDate, wday(dates["firstEp"]))) %>%
    group_by(datePlot) %>%
    summarise(rating = mean(rating),
	      noOfShows = n_distinct(showTitle)) -> otherShows
 
  episodesPlus %>%
    filter(((showTitle == showNames[1] & season %in% seasons[[1]]) |
	   (showTitle == showNames[2] & season %in% seasons[[2]])),
	   airDate >= minDate,
	   airDate <= maxDate) -> chosenShows
  if(chosenRating != "vs") {
    chosenShows %>%
      filter(typeRating == "chosenRating",
	     rating >= minRating,
	     rating <= maxRating) 
      select(-typeRating) -> chosenShows
  }
 
  return(list(otherShows, chosenShows))

} 


#' Plot IMDb ratings of one show.
#'
#' @param sources list returned by filterToPlot function.
#' @param background  logical, if TRUE, ratings for other shows will be displayed.
#' @param trend logical, if TRUE, trend line for season is plotted.
#'
#' @return ggplot2 object.
#'
#' @export
#'

plotRatings <- function(sources, background = TRUE, trend = FALSE) {
  nShow <- n_distinct(sources[[2]]$showTitle)
  vs <- any(colnames(sources[[2]]) == "typeRating")
  showNames <- unique(sources[[2]]$showTitle)
  names(showNames) <- showNames
  plot <-  ggplot(sources[[2]], aes(x = airDate, y = rating, color = showTitle)) 

  if(background)
    plot <- plot + geom_line(data = sources[[1]], aes(x = datePlot, y = rating),
                             inherit.aes = FALSE, color = "grey", size = 2)

  plot <- plot + geom_line(aes(group = paste0(showTitle, season)), linetype = 2, size = 1.5) +
	         geom_point(size = 4) +
                 theme_bw() +
                 xlab("") +
                 ylab("")
  if(trend)
    plot <- plot + geom_smooth(aes(group = paste0(showTitle, season)), method = "lm",
			       se = FALSE, size = 1.5)

  if(nShow > 1) {
    plot <- plot + scale_color_discrete(name = "Show")
   } else {
       plot <- plot +  guides(color = "none")
     }
  if(vs & nShow == 1) {
    plot <- plot + facet_wrap(~typeRating, scales = "free", ncol = 1,
			      labeller = as_labeller(c("imdbRating" = "IMDb ratings",
						       "nielsenRating" = "Nielsen ratings")))
  }

  return(plot)
}


#' Plot IMDb ratings vs Nielsen ratings for two shows.
#' 
#' @param sources Data frame returned by filterToPlot function.
#' @param trend lgl, if TRUE, trend line will be displayed. 
#'
#' @return ggplot2 object.
#'
#' @export
#'

plotRatingsCompareVS <- function(sources, trend = FALSE) {
  showNames <- unique(sources[[2]]$showTitle)
  names(showNames) <- showNames
  plot <- ggplot(sources[[2]], aes(x = airDate, y = rating, color = showTitle)) +
            geom_point(size = 4) +
	    geom_line(aes(group = paste0(showTitle, season, typeRating)),
		      linetype = 2, size = 1.5) +
	    theme_bw() +
	    scale_color_discrete(name = "Show") +
	    xlab("") +
	    ylab("") +
	    facet_wrap(~typeRating, scales = "free", ncol = 1,
		       labeller = as_labeller(c("imdbRating" = "IMDb ratings",
						"nielsenRating" = "Nielsen ratings")))
  
  if(trend) {
    plot <- plot + geom_smooth(aes(group = paste0(showTitle, season)),
			       method = "lm", se = FALSE, size = 1.5)
  }
  return(plot)
}

