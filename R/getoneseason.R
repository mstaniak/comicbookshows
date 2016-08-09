#' Links to all episodes of one season of one show.
#'
#' @param season String character containing link to a season page.
#'
#' @return Vector with links to episodes from a given season.
#'
#' @export

get_one_season <- function(season) {
read_html(season) %>%
    html_node(".list.detail.eplist") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique() %>%
    paste0("http://www.imdb.com", .) -> tmp
tmp <- data.frame(tmp, stringsAsFactors = F)
colnames(tmp) <- "seas_link"
tmp %>%
    filter(grepl(seas_link, pattern = "ref", fixed = T)) %>%
    unlist(use.names = F)
}
