#' Filter data to plot given type of ratings (Nielsen/IMDb) for a given show.
#'
#' @param showName Character with show's full name.
#' @param typeRating Character with type of ratings
#'        that matches one of column names in episodes tibble.
#' @param seasons Integer vector with numbers of seasons to display.
#' @param minRating Double minimum ratings value to display. 
#' @param maxRating Double miximum ratings value to display.
#' @param minDate Date earliest date to display.
#' @param maxDate Date most recent date to display.
#'
#' @return Tibble which is filtered episodes tibble with added
#'         'key' column for plotting function.
#'
#' @export
#'


filterOneShow <- function(showName, typeRating, seasons = NULL,
                     minRating = 0, maxRating = 10,
		     minDate = defaultMinDate, maxDate = defaultMaxDate) {
episodes %>% 
  filter(showTitle == showName,
	 airDate >= minDate,
	 airDate <= maxDate,
	 season %in% seasons) %>%
  filter_(interp("c >= minR",
        	 c = as.name(typeRating), minR = minRating),
          interp("c <= maxR",
		 c = as.name(typeRating), maxR = maxRating)) %>%
  rename_(.dots = setNames(typeRating, "rating")) %>%
  mutate(key = paste(showTitle, season, episode, sep = "_"))
}


#' Plot one show's rating in time.
#'
#' @param sourceT Tibble returned by filterOneShow function.
#' 
#' @return ggvis plot
#'
#' @export
#'

plotOneShow <- function(sourceT) {
  ggvis(sourceT, x = ~airDate, y = ~rating, key := ~key) %>%
    layer_lines() %>%
    layer_points() %>%
    add_tooltip(giveTooltip) %>%
    add_axis("x", title = "") %>%
    add_axis("y", title = "") 
}

