library(tidyverse)
library(rvest)

url <- "https://editorial.rottentomatoes.com/guide/the-200-best-movies-of-the-2010s/"

html_obj <- read_html(url)

html_obj %>%
  html_nodes(".article_movie_title") %>%
  html_nodes("a") %>%
  html_text()

url <- "https://en.wikipedia.org/wiki/List_of_American_films_of_"

get_film_actor_edgelist_year_until2013 <- function(.year) {
  print(.year)
  str_c(url, .year) %>%
    read_html() %>%
    html_node(".wikitable") %>%
    html_table(fill = TRUE) %>%
    as_tibble() %>%
    filter(Genre != "Documentary") %>%
    mutate(title = Title,
           cast = map(.[[3]], ~str_split(.x, ", ")[[1]]),
           year = .year) %>%
    select(title, cast, year) %>%
    unnest(cast)
}

edgelist_until2013 <- map_dfr(2010:2013, get_film_actor_edgelist_year)

# FAIL
.year <- 2014
str_c(url, .year) %>%
  read_html() %>%
  html_nodes(".wikitable") %>%
  tail(4)

get_film_actor_edgelist_year_until2013 <- function(.year) {
    print(.year)
    str_c(url, .year) %>%
      htmltab(which = "//*[contains(@class, 'wikitable sortable')]") %>%
      as_tibble() %>%
      filter(Genre != "Documentary") %>%
      mutate(title = Title,
             cast = map(.[[3]], ~str_split(.x, ", ")[[1]]),
             year = .year) %>%
      select(title, cast, year) %>%
      unnest(cast)
  }

edgelist_until2013 <- map_dfr(2010:2013, get_film_actor_edgelist_year)

# FAIL
.year <- 2014
n_table <- 3
temp <- str_c(url, .year) %>%
  htmltab(which = n_table, rm_nodata_cols = FALSE)
colnames(temp)[1] <- "x"
temp %>%
  as_tibble()

.year <- 2014
n_table <- 1

str_c(url, .year) %>%
  read_html() %>%
  html_nodes(".wikitable") %>%
  tail(4) %>%
  .[n_table] %>%
  html_nodes("tr") %>% .[3] %>% html_children() %>% .[5] %>% html_children() %>% html_text() %>% discard(str_length(.) == 0)
