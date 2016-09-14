#' Function for creating links to show's seasons on RottenTomatoes.
#'
#' @param showsInfoDf Data frame created in datadownload.R file.
#'
#' @export
#'

seasonsLinksPaste <- function(showsInfoDf) {
  lapply(data.frame(1:(showsInfoDf$seasonsNumber)),
         {function(y)
           return(paste0(showsInfoDf$tvShowsRT,"/s0", y, "/"))})
}


#' Function for creating data frame with links to all shows' seasons from RT.
#'
#' @inheritParams seasonsLinksPaste
#'
#' @export
#'

seasonsLinksRT <- function(showsInfoDf) {
  showsInfoDf %>%
    lapply(seasonsLinksPaste) %>%
    lapply({function(x) return(x[[1]])}) %>%
    lapply(function(x) return(data.frame(x))) %>%
    bind_rows()
}
