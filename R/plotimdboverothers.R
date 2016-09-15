#' Change day of the week to given day of the week.
#'
#' @param actualDate Date to be modified by changing day of the week.
#' @param targetWday Day of the week as coded in lubridate package.
#'
#' @return Date given date with day chosen to closest given day of the week.
#'

changeWday <- function(actualDate, targetWday) {
  actualDate - wday(actualDate) + targetWday
}


#' Plot IMDb ratings of one show.
#'
#' @param showName Name of the show to plot.
#' @param background  logical, if TRUE, ratings for other shows will be displayed.
#'
#' @param trend logical, if TRUE, trend line for season is plotted.
#' @return ggplot2 object.
#'
#' @export

plotImdbRatings <- function(showName, background = FALSE, trend = FALSE) {
  episodesPlus %>%
    select(showTitle, airDate) %>%
    filter(showTitle == showName) %>%
    summarise(first_ep = min(airDate),
              last_ep = max(airDate)) %>%
    unlist() %>%
    as_date() -> dates

  episodesPlus %>%
    filter(showTitle != showName &
             airDate >= dates["first_ep"] &
             airDate <= dates["last_ep"] &
             channel != "Netflix") %>%
    mutate(datePlot = changeWday(airDate, wday(dates["first_ep"]))) %>%
    group_by(datePlot) %>%
    summarise(rating = mean(imdbRating)) -> otherShows

 episodesPlus %>%
    filter(showTitle == showName) %>%
    ggplot(aes(x = airDate, y = imdbRating, group = season)) -> plot

  if(background)
    plot <- plot + geom_line(data = otherShows, aes(x = datePlot, y = rating),
                             inherit.aes = FALSE, color = "grey")

  plot <- plot + geom_line(size = 1.5) +
                 theme_bw() +
                 xlab("") +
                 ylab("")
  if(trend)
    plot <- plot + geom_smooth(aes(group = season), method = "lm", se = FALSE,
			     linetype = 2, size = 0.8)
  return(plot)
}
