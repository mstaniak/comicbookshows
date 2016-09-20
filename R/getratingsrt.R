#' Helper function for getRatingsRT
#'
#' @param x Vector.
#'
#' @export
#'

pickSec <- function(x) {
  return(x[2])
}


#' Download ratings from one RottenTomatoes page.
#'
#' @param link Link to RT page.
#'
#' @export
#'

getRatingsRT <- function(link) {
  read_html(link) %>%
    html_node(css = "span.meter-value") %>%
    html_text() -> cri

  read_html(link) %>%
    html_node(css = "div.meter-value > span:nth-child(1)") %>%
    html_text() -> aud

  read_html(link) %>%
    html_node(css = "#all-critics-numbers > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1)") %>%
    html_text() %>%
    str_split(":[:space:]+") %>%
    unlist(use.names = FALSE) %>%
    pickSec() -> avgRTcrit

  read_html(link) %>%
    html_node(css = ".audience-info > div:nth-child(1)") %>%
    html_text() %>%
    str_split(":[:space:]+") %>%
    unlist(use.names = FALSE) %>%
    pickSec() -> avgRTaud

  return(list(critics = cri,
              audience = aud,
              criticsAverage = avgRTcrit,
              audienceAverage = avgRTaud))
}
