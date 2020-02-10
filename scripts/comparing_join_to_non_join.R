# george lucas characters
baseR_no_join <- function() {
  unique(characters$name[characters$film_id %in% films$film_id[films$director == "George Lucas"]])
}

baseR_with_join <- function() {
  unique(merge(characters, films[films$director == "George Lucas", ], by = "film_id")$name)
}

dplyr_no_join <- function() {
  characters %>%
    filter(film_id %in% (films %>%
                           filter(director=="George Lucas") %>%
                           pull(film_id))) %>%
    pull(name) %>%
    unique()
}

dplyr_with_join <- function() {
  characters %>%
    inner_join(films %>% filter(director=="George Lucas"), by = "film_id") %>%
    pull(name) %>%
    unique()
}

library(microbenchmark)

res <- microbenchmark(baseR_no_join(), baseR_with_join(),
                      dplyr_no_join(), dplyr_with_join())

autoplot(res)

# george lucas characters whose homeworld is Alderaan

baseR_no_join <- function(df1, df2) {
  colnames(df1)[1:3] <- c("film_id.x", "scene_id.x", "minute_id.x")
  colnames(df2)[1:3] <- c("film_id.y", "scene_id.y", "minute_id.y")
  expanded <- cbind(df1[rep(1:nrow(df1), times = nrow(df2)), ],
                    df2[rep(1:nrow(df2), each = nrow(df1)), ])
  joined <- expanded[
    expanded$film_id.x == expanded$film_id.y &
      expanded$scene_id.x == expanded$scene_id.y &
      expanded$minute_id.x == expanded$minute_id.y,
    -c(1:3)
    ]
  colnames(joined)[3:5] <- c("film_id", "scene_id", "minute_id")
  joined[, c("film_id", "scene_id", "minute_id", "character_id", "line", "location")]
}

baseR_with_join <- function() {
  merge(lines, locations, by = c("film_id", "scene_id", "minute_id"))
}

dplyr_join <- function() {
  inner_join(lines, locations, by = c("film_id", "scene_id", "minute_id"))
}

res <- microbenchmark(baseR_no_join(lines, locations),
                      baseR_with_join(), dplyr_join(),
                      times = 20)

autoplot(res)