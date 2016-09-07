#' Data on episodes.
#'
#' @format A tibble with 409 rows and 9 columns:
#' \describe{
#'   \item{show, chr: full show name (excluding "Marvel's" or "DC's")}
#'   \item{season, chr: season number}
#'   \item{episode, chr: episode number}
#'   \item{air_date, date: day when episode first aired on TV. Release date for Netflix shows}
#'   \item{title, chr: full episode title}
#'   \item{imdb_rating: dbl: IMDb rating on scale 1-10}
#'   \item{num_of_votes, int: number of people who voted on this episode on IMDb}
#'   \item{nielsen_rating: dbl: Nielsen rating}
#'   \item{viewers: dbl, number of live viewers in milions}
#'  }
#'
#'  @source \url{https://imdb.com}, \url{https://wikipedia.org},
#'          \url{https://rottentomatoes.com}, \url{https://tvseriesfinale.com}
#'
"episodes"


#' Data on shows' seasons.
#'
#' @format Data frame with 23 rows and 9 columns,
#'   \describe{
#'     \item{show, chr: full show name (excluding "Marvel's" or "DC's")}
#'     \item{season, chr: season number}
#'     \item{episode, chr: episode number}
#'     \item{cancelled, lgl: TRUE if the show is now cancelled}
#'     \item{channel, chr: name of channel on which the show air or Netflix for Netflix shows}
#'     \item{comic, chr: name of comic book publisher on whose comic books the show is based}
#'     \item{critics, int: percentage of "fresh" ratings from critics on RottenTomatoes}
#'     \item{audience, int: percentage of "fresh" ratings from audience on RT}
#'     \item{critics_average, dbl: average score from critics on scale 1-10 from RT}
#'     \item{audience_average, dbl: average score from audience on scale 1-5 from RT}
#'   }
#'  @source \url{https://imdb.com}, \url{https://wikipedia.org},
#'          \url{https://rottentomatoes.com}, \url{https://tvseriesfinale.com}
#'
"shows"
