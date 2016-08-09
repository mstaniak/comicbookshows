### Download data form IMDb -----------------------------------------

tv_shows <- c(
  "Agent Carter" = "http://www.imdb.com/title/tt3475734",
  "Agents of SHIELD" = "http://www.imdb.com/title/tt2364582",
  "Arrow" = "http://www.imdb.com/title/tt2193021",
  "Constantine" = "http://www.imdb.com/title/tt3489184",
  "Gotham" = "http://www.imdb.com/title/tt3749900",
  "iZombie" = "http://www.imdb.com/title/tt3501584",
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
  "Gotham" = 2,
  "iZombie" = 2,
  "Legends of Tomorrow" = 1,
  "Lucifer" = 1,
  "Preacher" = 1,
  "Supergirl" = 1,
  "The Flash" = 2
)

all_shows <- vector("list", length(tv_shows))

for(i in 1:length(tv_shows)) {
  all_shows[[i]] <- get_episodes_info(get_episodes_links(get_seasons_links(tv_shows[i], seasons_number[i])))
}

shows <- bind_rows(all_shows)

# getwd()
# Check data validity.
# lapply(shows, {function(x) return(sum(is.na(x)))})

shows %>% filter(!is.na(episode)) -> shows
shows %>% mutate(show = ifelse(show == "Agenci T.A.R.C.Z.Y.", "Agents of S.H.I.E.L.D.",
                        ifelse(show == "Agentka Carter", "Agent Carter", show))) -> shows

save(shows, file = "data/shows.rda")

# shows %>% mutate(air_date = as_date(air_date)) -> shows

shows %>%
  mutate(data2 = as_date(air_date)) %>%
  filter(is.na(data2)) %>%
  select(show) %>%
  distinct()
