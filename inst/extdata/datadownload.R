### Download data form IMDb -----------------------------------------

# Required packages.
library(rvest)
library(dplyr)
library(tibble)
library(lubridate)
library(stringr)
library(ToolsForTVShowsApp)

## Download imdb ratings --------------------------------------------
tv_shows <- c(
  "Agent Carter" = "http://www.imdb.com/title/tt3475734",
  "Agents of SHIELD" = "http://www.imdb.com/title/tt2364582",
  "Arrow" = "http://www.imdb.com/title/tt2193021",
  "Constantine" = "http://www.imdb.com/title/tt3489184",
  "Daredevil" = "http://www.imdb.com/title/tt3322312",
  "Gotham" = "http://www.imdb.com/title/tt3749900",
  "iZombie" = "http://www.imdb.com/title/tt3501584",
  "Jessica Jones" = "http://www.imdb.com/title/tt2357547",
  "Legends of Tomorrow" = "http://www.imdb.com/title/tt4532368",
  "Lucifer" = "http://www.imdb.com/title/tt4052886",
  "Preacher" = "http://www.imdb.com/title/tt5016504",
  "Supergirl" = "http://www.imdb.com/title/tt4016454",
  "The Flash" = "http://www.imdb.com/title/tt3107288"
)

seasons_number <- c(
  "Agent Carter" = 2,
  "Agents of SHIELD" = 3,
  "Arrow" = 4,
  "Constantine" = 1,
  "Daredevil" = 2,
  "Gotham" = 2,
  "iZombie" = 2,
  "Jessica Jones" = 1,
  "Legends of Tomorrow" = 1,
  "Lucifer" = 1,
  "Preacher" = 1,
  "Supergirl" = 1,
  "The Flash" = 2
)

# Download data.
tmp <- data.frame(tv_shows, seasons_number, stringsAsFactors = FALSE)
shows_info <- setNames(split(tmp, seq(nrow(tmp))), rownames(tmp))
all_shows <- lapply(shows_info,
                    {function(x) return(get_episodes_info(get_episodes_links(get_seasons_links(x$tv_shows, x$seasons_number))))})

shows <- bind_rows(all_shows)
shows %>%
  filter(!is.na(episode)) -> shows

# Change names to english.
shows %>% mutate(show = ifelse(show == "Agenci T.A.R.C.Z.Y.", "Agents of S.H.I.E.L.D.",
                               ifelse(show == "Agentka Carter", "Agent Carter", show))) -> shows
# Remove special episode.
shows %>%
  mutate(data2 = as_date(air_date)) %>%
  filter(!is.na(data2)) %>%
  select(-data2) -> shows

# Set proper variables types.
shows %>%
  mutate(air_date = as_date(air_date)) -> shows

save(shows, file = "data/shows.rda")
