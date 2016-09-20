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
                         minRating = 0, maxRating = 10, minDate = defaultMinDate, maxDate = defaultMaxDate) {
 
  nShow <- length(unique(showNames))
  nSeason <- "1"
  if(nShow == 1) {
    nSeason <- length(seasons[[1]])
  }

  episodesPlus %>%
    select(showTitle, airDate, season) %>%
    filter(((showTitle == showNames[1] & season %in% seasons[[1]]) |
	    (showTitle == showNames[2] & season %in% seasons[[2]]))) %>%
    summarise(firstEp = min(airDate),
              lastEp = max(airDate)) %>%
    unlist() %>%
    as_date() -> dates
  
  episodesPlus %>%
    filter(showTitle != showNames[1],
	   showTitle != showNames[2],
	   airDate >= max(dates["firstEp"], minDate),
	   airDate <= min(dates["lastEp"], maxDate),
	   channel != "Netflix") -> otherShows
  if(chosenRating != "vs") {
    otherShows %>% 
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
	   airDate <= maxDate) %>%
    select(-channel, -comic, -cancelled, -critics,
	   -audience, -criticsAverage, -audienceAverage) -> chosenShows
  if(chosenRating != "vs") {
    chosenShows %>%
      filter(typeRating == chosenRating,
	     rating >= minRating,
	     rating <= maxRating) %>% 
      select(-typeRating) -> chosenShows
  }

  if(nShow == 1 & nSeason > 1 ) {
    otherShows$season <- apply(as.matrix(otherShows$datePlot), 1, {function(x)
			       if(any(chosenShows$airDate == x)) {
				 return(unique(chosenShows$season[chosenShows$airDate == x]))
			       } else {
				 return(NA)
			       }
	                  })
    otherShows %>%
      filter(!is.na(season)) -> otherShows
  }
  
  return(list(otherShows, chosenShows))
} 


#' Plot IMDb ratings of one show.
#'
#' @param sources list returned by filterToPlot function.
#' @param background  logical, if TRUE, ratings for other shows will be displayed.
#' @param trend logical, if TRUE, trend line for season is plotted.
#' @param separated lgl, if TRUE, facets will be used.
#'
#' @return ggplot2 object.
#'
#' @export
#'

plotRatings <- function(sources, background = FALSE, trend = FALSE, separate = FALSE) {
  nShow <- n_distinct(sources[[2]]$showTitle)
  vs <- any(colnames(sources[[2]]) == "typeRating")
  showNames <- unique(sources[[2]]$showTitle)
  names(showNames) <- showNames
  nSeason <- max(as.integer(sources[[2]]$season))
  if(nShow == 2) {
    background <- FALSE
  }

  plot <-  ggplot(sources[[2]], aes(x = airDate, y = rating, color = showTitle)) 

  if(background & nSeason > 1) {
    plot <- plot + geom_line(data = sources[[1]], aes(x = datePlot, y = rating, group = season),
                             inherit.aes = FALSE, color = "grey", size = 1)
  } else if(background & nSeason  == 1) {
    plot <- plot + geom_line(data = sources[[1]], aes(x = datePlot, y = rating),
			     inherit.aes = FALSE, color = "grey", size = 1)
  } else if(background & nShow > 1) {
    plot <- plot + geom_line(data = sources[[1]], aes(x = datePlot, y = rating, group = season),
                             inherit.aes = FALSE, color = "grey", size = 1)
  }

  plot <- plot + geom_line(aes(group = paste0(showTitle, season)), linetype = 2, size = 2) +
	         geom_point(size = 5) +
		 theme_hc(bgcolor = "darkunica", base_size = 18) +
		 scale_color_hc("darkunica", name = "Show") +             
		 scale_x_date(date_labels = "%m-%Y") +
	         xlab("") +
                 ylab("")
  if(trend)
    plot <- plot + geom_smooth(aes(group = paste0(showTitle, season)), method = "lm",
			       se = FALSE, size = 1.5)

  if(nShow == 1) {
       plot <- plot + guides(color = "none")
  }
  if(vs & nShow == 1) {
    plot <- plot + facet_wrap(~typeRating, scales = "free", ncol = 1,
			      labeller = as_labeller(c("imdbRating" = "IMDb ratings",
						       "nielsenRating" = "Nielsen ratings"))) + theme(legend.position = "none") 
  }
  if(nShow == 1 & nSeason > 1 & separate) {
    plot <- plot + facet_wrap(~season, scales = "free", ncol = 1)
  }

  if(nShow == 2 & separate) {
    plot <- plot + facet_wrap(~showTitle, scales = "free", ncol = 1)
  }

  plot <- plot + theme(axis.text = element_text(color = "white"),
		       plot.background = element_rect(fill = "#222222"),
		       legend.position = "none")

  return(plot)
}


#' Plot IMDb ratings vs Nielsen ratings for two shows.
#' 
#' @param sources Data frame returned by filterToPlot function.
#' @param trend lgl, if TRUE, trend line will be displayed.
#' @param separate lgl, if TRUE, shows will be displayed on separate panels.
#'
#' @return ggplot2 object.
#'
#' @export
#'

plotRatingsCompareVS <- function(sources, trend = FALSE, separate = TRUE) {
  showNames <- unique(sources[[2]]$showTitle)
  names(showNames) <- showNames
  rtNames <- c("imdbRating" = "IMDb ratings", "nielsenRating" = "Nielsen ratings")
  nShow <- length(showNames)

  plot <- ggplot(sources[[2]], aes(x = airDate, y = rating, color = showTitle)) +
            geom_point(size = 5) +
	    geom_line(aes(group = paste0(showTitle, season, typeRating)),
		      linetype = 2, size = 2) +
            theme_hc(bgcolor = "darkunica", base_size = 18) +
            scale_color_hc("darkunica", name = "Show") +             
	    scale_x_date(date_labels = "%m-%Y") +
	    xlab("") +
	    ylab("")
	 
  if(trend) {
    plot <- plot + geom_smooth(aes(group = paste0(showTitle, season)),
			       method = "lm", se = FALSE, size = 1.5)
  }

  if(nShow == 1) {
    plot <- plot + facet_wrap(~typeRating, scale = "free", ncol = 1,
			      labeller = as_labeller(rtNames))
  } else if(separate & nShow > 1) {
    plot <- plot + facet_wrap(showTitle ~ typeRating, scales = "free",
			      labeller = as_labeller(c(rtNames, showNames)))
  } else if(!separate){
    plot <- plot + facet_wrap(~typeRating, scales = "free",
			      labeller = as_labeller(c(rtNames)))
  }

  plot <- plot + theme(axis.text = element_text(color = "white"),
		       plot.background = element_rect(fill = "#222222"),
		       legend.position = "none")
  
  return(plot)
}

