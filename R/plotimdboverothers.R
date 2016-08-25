#' Change day of the week to given day of the week.
#'
#' @param actual_date Date to be modified by changing day of the week.
#' @param target_wday Day of the week as coded in lubridate package.
#'
#' @return Date given date with day chosen to closest given day of the week.
#'

change_wday <- function(actual_date, target_wday) {
  actual_date - wday(actual_date) + target_wday
}

#' Plot IMDb ratings of one show with other show's mean rating in the background.
#'
#' @param show_name Name of the show to plot.
#'
#' @return ggplot2 object.
#'
#' @export

plot_imdb_over_others <- function(show_name) {
  shows %>%
    select(show, air_date) %>%
    filter(show == show_name) %>%
    summarise(first_ep = min(air_date),
              last_ep = max(air_date)) %>%
    unlist() %>%
    as_date() -> dates

  shows %>%
    filter(show != show_name &
             air_date >= dates["first_ep"] &
             air_date <= dates["last_ep"] &
             channel != "Netflix") %>%
    mutate(date_plot = change_wday(air_date, wday(dates["first_ep"]))) %>%
    group_by(date_plot) %>%
    summarise(rating = mean(imdb_rating)) -> other_shows

  shows %>%
    filter(show == show_name) %>%
    ggplot(aes(x = air_date, y = imdb_rating, group = season)) +
    geom_line(data = other_shows, aes(x = date_plot, y = rating),
              inherit.aes = FALSE, color = "grey") +
    geom_line(size = 1.5) +
    theme_bw() +
    xlab("") +
    ylab("")
}
