#' Function for creating links to show's seasons on RottenTomatoes.
#'
#' @param shows_info_df Data frame created in datadownload.R file.
#'
#' @export
#'

seasons_links_paste <- function(shows_info_df) {
  lapply(data.frame(1:(shows_info_df$seasons_number)),
         {function(y)
           return(paste0(shows_info_df$tv_shows_rt,"/s0", y, "/"))})
}


#' Function for creating data frame with links to all shows' seasons from RT.
#'
#' @inheritParams seasons_links_paste
#'
#' @export
#'

seasons_links_rt <- function(shows_info_df) {
  shows_info_df %>%
    lapply(seasons_links_paste) %>%
    lapply({function(x) return(x[[1]])}) %>%
    lapply(function(x) return(data.frame(x))) %>%
    bind_rows()
}
