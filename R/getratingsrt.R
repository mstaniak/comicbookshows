#' Helper function for get_ratings_rt
#'
#' @param x Vector.
#'
#' @export
#'

pic_sec <- function(x) {
  return(x[2])
}


#' Download ratings from one RottenTomatoes page.
#'
#' @param link Link to RT page.
#'
#' @export
#'

get_ratings_rt <- function(link) {
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
    pick_sec() -> avg_rt_crit

  read_html(link) %>%
    html_node(css = ".audience-info > div:nth-child(1)") %>%
    html_text() %>%
    str_split(":[:space:]+") %>%
    unlist(use.names = FALSE) %>%
    pic_sec() -> avg_rt_aud

  return(list(critics = cri,
              audience = aud,
              critics_average = avg_rt_crit,
              audience_average = avg_rt_aud))
}
