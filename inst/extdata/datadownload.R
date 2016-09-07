### Required packages. ------------------------
library(curl)
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

seasons_number <- c("Agent Carter" = 2, "Agents of SHIELD" = 3, "Arrow" = 4, "Constantine" = 1,
                    "Daredevil" = 2, "Gotham" = 2, "iZombie" = 2, "Jessica Jones" = 1,
                    "Legends of Tomorrow" = 1, "Lucifer" = 1, "Preacher" = 1, "Supergirl" = 1, "The Flash" = 2)

# Download data.
tmp <- data.frame(tv_shows, seasons_number, stringsAsFactors = FALSE)
shows_info <- setNames(split(tmp, seq(nrow(tmp))), rownames(tmp))
all_shows <- lapply(shows_info,
                    {function(x)
                      return(get_episodes_info(
                        get_episodes_links(get_seasons_links(x$tv_shows, x$seasons_number))))})

shows <- as_tibble(bind_rows(all_shows))
shows %>% filter(!is.na(episode)) -> shows

# Change names to english.
shows %>% mutate(show = ifelse(show == "Agenci T.A.R.C.Z.Y.", "Agents of S.H.I.E.L.D.",
                               ifelse(show == "Agentka Carter", "Agent Carter", show))) -> shows
# Remove special episode.
shows %>%
  mutate(data2 = as_date(air_date)) %>%
  filter(!is.na(data2)) %>%
  select(-data2) -> shows

# Set proper variables types.
shows %>% mutate(air_date = as_date(air_date)) -> shows

# Save result.
# save(shows, file = "data/shows.rda")

# Remove temporary objects
rm(tv_shows, seasons_number, tmp, shows_info, all_shows)

### Download Nielsen ratings data from wikipedia. -------------------------------
# These selectors should be checked before download.
selectors <- c(
  "https://en.wikipedia.org/wiki/List_of_The_Flash_episodes" = "table.wikitable:nth-child(19)", # Flash s1
  "https://en.wikipedia.org/wiki/List_of_The_Flash_episodes" = "table.wikitable:nth-child(21)", # Flash s2
  "https://en.wikipedia.org/wiki/List_of_Lucifer_episodes" = "table.wikitable:nth-child(15)", #Lucifer s1
  "https://en.wikipedia.org/wiki/List_of_iZombie_episodes" = "table.wikitable:nth-child(15)", #iZombie s1
  "https://en.wikipedia.org/wiki/List_of_iZombie_episodes" = "table.wikitable:nth-child(17)", #iZombie s2
  "https://en.wikipedia.org/wiki/Legends_of_Tomorrow" = "table.wikitable:nth-child(56)", #LoT s1
  "https://en.wikipedia.org/wiki/Constantine_(TV_series)" = "table.wikitable:nth-child(26)", #Constantine s1
  "https://en.wikipedia.org/wiki/Supergirl_(TV_series)" = "table.wikitable:nth-child(43)", #Supergirl s1
  "https://en.wikipedia.org/wiki/List_of_Gotham_episodes" = "table.wikitable:nth-child(17)", #Gotham s1
  "https://en.wikipedia.org/wiki/List_of_Gotham_episodes" = "table.wikitable:nth-child(19)", #Gotham s2
  "https://en.wikipedia.org/wiki/Agent_Carter_(season_1)" = "table.wikitable:nth-child(42)", #Carter s1
  "https://en.wikipedia.org/wiki/Agent_Carter_(season_2)" = "table.wikitable:nth-child(45)", #Carter s2
  "https://en.wikipedia.org/wiki/Preacher_(TV_series)" = "table.wikitable:nth-child(31)", #Precher s1
  "https://en.wikipedia.org/wiki/Agents_of_S.H.I.E.L.D._(season_1)" = "table.wikitable:nth-child(55)", #AoS s1
  "https://en.wikipedia.org/wiki/Agents_of_S.H.I.E.L.D._(season_2)" = "table.wikitable:nth-child(57)", #AoS s2
  "https://en.wikipedia.org/wiki/Agents_of_S.H.I.E.L.D._(season_3)" = "table.wikitable:nth-child(56)" #AoS s3
)

# Download.
ratings_tbl <- vector("list", length(selectors))
for(i in 1:length(selectors)) {
  ratings_tbl[[i]] <- get_one_season_wiki(names(selectors[i]), selectors[i])
}

# Convert to tibble.
ratings_dfs <- lapply(ratings_tbl, {function(x) return(x[[1]])})
ratings_dfs <- lapply(ratings_dfs, as_tibble)

# Select variables with episode number, Nielsen rating and number of viewers.
rtdfs <- lapply(ratings_dfs, {function(x) return(x[, c(1,4,5)])})
rtdfs <- lapply(rtdfs, change_colnames)

# Delete wikipedia references.
rtdfs <- lapply(rtdfs, {function(x)
  x %>%
    mutate(viewers = str_replace_all(pattern = "\\[[:alnum:]+\\]",
                                     replacement = "",
                                     string = viewers),
           rating_share = str_replace_all(pattern = "\\[[:alnum:]+\\]",
                                          replacement = "",
                                          string = rating_share))})

# Fix variable types.
rtdfs <- lapply(rtdfs, {function(x) x %>% mutate(viewers_num = as.numeric(viewers))})
rtdfs <- lapply(rtdfs, {function(x) x %>% select(-viewers) %>% rename(viewers = viewers_num)})

# Extract Nielsen rating from rating/share format.
rtdfs <- lapply(rtdfs,
                {function(x) x %>%
                    mutate(rating_share = str_replace_all(string = rating_share,
                                                          pattern = "[^0-9.]",
                                                          replacement = "_")) %>%
                    mutate(rating_share = substr(rating_share, 1, 3)) %>%
                    rename(nielsen_rating = rating_share) %>%
                    mutate(nielsen_rating = as.numeric(nielsen_rating))})

seasons_tmp <- c("1" = "Flash", "2" = "Flash", "1" = "Lucifer",  "1" = "iZombie",  "2" = "iZombie",
  "1" = "Legends of Tomorrow", "1" = "Constantine", "1" = "Supergirl", "1" = "Gotham",  "2" = "Gotham",
  "1" = "Agent Carter", "2" = "Agent Carter", "1" = "Preacher", "1" = "Agents_of_S.H.I.E.L.D.",
  "2" = "Agents_of_S.H.I.E.L.D.", "3" = "Agents_of_S.H.I.E.L.D.")

# Add show names and season number, fixes episode variable type.
for (i in 1:length(seasons_tmp)) {
  rtdfs[[i]] %>%
    mutate(show = seasons_tmp[i],
           season = names(seasons_tmp)[i],
           episode = as.character(episode)) -> rtdfs[[i]]
}

no_arrow <- bind_rows(rtdfs)

# Fix show name.
no_arrow %>%
  mutate(show = ifelse(show == "Agents_of_S.H.I.E.L.D.", "Agents of S.H.I.E.L.D.", show)) -> no_arrow


# Save result.
# save(no_arrow, file = "noarrow.rda")

# Arrow ratings had to be downloaded manually from tvseriesfinale.com
# load("noarrow.rda")
# arrow <- read.csv("ext/instdata/arrow.csv", sep = ";", dec = ",", header = T)

# Fix variable types.
arrow %>%
  mutate(episode = as.character(episode), season = as.character(season)) -> arrow

ratings <- bind_rows(no_arrow, arrow)

# Separate AoS s2 finale parts.
ratings <- rbind(ratings, ratings[255, ])
ratings[255,1] <- "21"
ratings[370,1] <- "22"

shows <- left_join(shows, ratings, by = c("show", "season", "episode"))

# Add channel name.
shows %>%
  mutate(channel = ifelse(show %in% c("Agents of S.H.I.E.L.D.", "Agent Carter"), "ABC",
                          ifelse(show %in% c("Arrow", "Flash", "iZombie", "Legends of Tomorrow"), "CW",
                                 ifelse(show == "Preacher", "AMC",
                                        ifelse(show %in% c("Gotham", "Lucifer"), "Fox",
                                               ifelse(show == "Constantine", "NBC",
                                                      ifelse(show == "Supergirl", "CBS", "Netflix"))))))) -> shows

# Add comicbook publisher.
shows %>%
  mutate(comic_by = ifelse(show %in% c("Agent Carter", "Agents of S.H.I.E.L.D.",
                                        "Daredevil", "Jessica Jones"), "Marvel", "DC")) -> shows

# Save result.
# save(shows, file = "data/shows.rda")

# Remove temporary objects.
rm(selectors, ratings_tbl, ratings_dfs, rtdfs, seasons_tmp, no_arrow, arrow, ratings, i)

### Download RottenTomatoes data. ----------------------------------
# Links to shows pages.
tv_shows_rt <- c(
  "Agent Carter" = "https://www.rottentomatoes.com/tv/marvel_s_agent_carter",
  "Agents of SHIELD" = "https://www.rottentomatoes.com/tv/marvel_s_agents_of_s_h_i_e_l_d_",
  "Arrow" = "https://www.rottentomatoes.com/tv/arrow",
  "Constantine" = "https://www.rottentomatoes.com/tv/constantine",
  "Daredevil" = "https://www.rottentomatoes.com/tv/daredevil",
  "Gotham" = "https://www.rottentomatoes.com/tv/gotham",
  "iZombie" = "https://www.rottentomatoes.com/tv/izombie",
  "Jessica Jones" = "https://www.rottentomatoes.com/tv/jessica_jones",
  "Legends of Tomorrow" = "https://www.rottentomatoes.com/tv/dc_s_legends_of_tomorrow",
  "Lucifer" = "https://www.rottentomatoes.com/tv/lucifer",
  "Preacher" = "https://www.rottentomatoes.com/tv/preacher",
  "Supergirl" = "https://www.rottentomatoes.com/tv/supergirl",
  "The Flash" = "https://www.rottentomatoes.com/tv/flash"
)

seasons_number <- c("Agent Carter" = 2, "Agents of SHIELD" = 3, "Arrow" = 4, "Constantine" = 1,
                    "Daredevil" = 2, "Gotham" = 2, "iZombie" = 2, "Jessica Jones" = 1,
                    "Legends of Tomorrow" = 1, "Lucifer" = 1, "Preacher" = 1, "Supergirl" = 1, "The Flash" = 2)

tmp <- data.frame(tv_shows_rt, seasons_number, stringsAsFactors = FALSE)
shows_info <- setNames(split(tmp, seq(nrow(tmp))), rownames(tmp))

# Download links to seasons.
rt_links <- seasons_links_rt(shows_info)

# Download easons' ratings.
rt_ratings <- vector("list", 23)
for(i in 1:23) {
  rt_ratings[[i]] <- get_ratings_rt(rt_links[i, 1])
}

# Clean data.
rt_ratings <- bind_rows(rt_ratings)

rt_ratings %>%
  mutate(critics = str_replace_all(critics, "%", ""),
         audience = str_replace_all(audience, "%", ""),
         critics_average = str_replace_all(critics_average, "/10", ""),
         audience_average = str_replace_all(audience_average, "/5", "")) -> rt_ratings

rep(names(seasons_number), times = seasons_number) %>%
  as_tibble() %>%
  rename(show = value) %>%
  group_by(show) %>%
  mutate(season = as.character(row_number(show))) %>%
  bind_cols(rt_ratings) -> rt_ratings
rt_ratings %>% 
  ungroup() -> rt_ratings
rt_ratings %>% 
  mutate(show = ifelse(show == "Agents of SHIELD", 
                       "Agents of S.H.I.E.L.D.", 
                       show)) -> rt_ratings

inner_join(shows, rt_ratings, by = c("show", "season")) %>%
  arrange(show, season) -> shows

# Save results.
# save(shows, file = "data/shows.rda")

# Remove temporary objects.
rm(tv_shows_rt, seasons_number, tmp, shows_info, links_rt, rt_ratings)
