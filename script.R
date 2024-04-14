library(jsonlite)
library(tidyverse)
library(tidyjson)
library(lubridate)

# get data
download.file("https://github.com/PokemonTCG/pokemon-tcg-data/archive/refs/heads/master.zip", destfile = "data.zip")

if(file.exists("data.zip"))
{
  unzip("data.zip")
}

if(file.exists("data.zip"))
{
  file.remove("data.zip")
}

if(file.exists("pokemon-tcg-data-master"))
{
  if(file.exists("data"))
  {
    unlink("data", recursive = TRUE)
    file.rename(from = "pokemon-tcg-data-master", to = "data")
  }
}

base_dir <- getwd()

# read and nnest JSON into dataframe
cards <- list.files("data/cards/en", full.names = TRUE) %>% map(jsonlite::read_json) %>% map_df(spread_all)

cards$level <- as.numeric(cards$level)
cards$hp <- as.numeric(cards$hp)
cards$number <- as.numeric(cards$number)

pok_unique <- cards %>% filter(supertype == "Pokémon") %>% group_by(name) %>% slice_tail(n = 1)
plot(level ~ hp, data = pok_unique)
identify(pok_unique$level ~ pok_unique$hp, n = 5, labels = pok_unique$name)

pokemon <- cards %>% filter(supertype == "Pokémon") %>% filter(!is.na(level))
plot(level ~ hp, data = pokemon)
identify(pokemon$level ~ pokemon$hp, n = 2, labels = pokemon$name)

# Read set data
sets <- jsonlite::read_json('data/sets/en.json')
sets <- spread_all(sets)

# join cards and sets
cards_sets <- cards %>% mutate(set = str_extract(id,'[:alnum:]*(?=-)'))
cards_sets <- cards_sets %>% left_join(., sets, by = c('set' = 'id'))
cards_sets$releaseDate <- parse_date(cards_sets$releaseDate)

cards_sets %>% mutate(releaseYear = year(releaseDate)) %>% 
  ggplot(aes(x = as.factor(releaseYear), y = hp)) + geom_violin()

cards_sets %>% mutate(releaseYear = year(releaseDate)) %>% 
  group_by(releaseYear) %>% 
  summarise(mean_hp = mean(hp,na.rm = T)) %>% 
  ggplot(aes(x = releaseYear, y = mean_hp)) + geom_point() + geom_line()

cards_sets %>% mutate(releaseYear = year(releaseDate)) %>% 
  ggplot(aes(x = releaseYear, y = hp, group = releaseYear)) + geom_boxplot()

pokemon %>% enter_object(types) %>% gather_array %>% 
  append_values_string(column.name = 'type') %>% 
  group_by(type) %>% 
  ggplot(aes(x = type, y = hp)) + geom_boxplot()

pokemon %>% enter_object(attacks) %>% 
  gather_array() %>%  spread_all() %>% 
  append_values_string(column.name = 'attack') %>% head
