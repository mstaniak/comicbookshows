#' Change day of the week to given day of the week.
#'
#' @param actualDate Date to be modified by changing day of the week.
#' @param targetWday Day of the week as coded in lubridate package.
#'
#' @return Date given date with day chosen to closest given day of the week.
#'

changeDay <- function(actualDate, targetWday) {
  actualDate - wday(actualDate) + targetWday
}


#' Plot IMDb ratings of one show with other show's mean rating in the background.
#'
#' @param showName Name of the show to plot.
#' @param background  logical, if TRUE, ratings for other shows will be displayed.
#'
#' @return ggplot2 object.
#'
#' @export

plotImdbRatings <- function(showName, background = FALSE) {
  shows %>%
    select(show, airDate) %>%
    filter(show == showName) %>%
    summarise(first_ep = min(airDate),
              last_ep = max(airDate)) %>%
    unlist() %>%
    as_date() -> dates

  shows %>%
    filter(show != showName &
             airDate >= dates["first_ep"] &
             airDate <= dates["last_ep"] &
             channel != "Netflix") %>%
    mutate(datePlot = changeWday(airDate, wday(dates["first_ep"]))) %>%
    group_by(datePlot) %>%
    summarise(rating = mean(imdbRating)) -> otherShows

  shows %>%
    filter(show == showName) %>%
    ggplot(aes(x = airDate, y = imdbRating, group = season)) -> plot

  if(background)
    plot <- plot + geom_line(data = otherShows, aes(x = datePlot, y = rating),
                             inherit.aes = FALSE, color = "grey")

  plot <- plot + geom_line(size = 1.5) +
                 theme_bw() +
                 xlab("") +
                 ylab("")
  return(plot)
}
