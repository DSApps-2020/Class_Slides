library(tidyverse)
library(microbenchmark)

set.seed(2018)
nr <- 10^3
lst <- replicate(2, data.frame(
  key1 = sample(letters[1:5], nr, replace = T),
  key2 = sample(LETTERS[6:10], nr, replace = T),
  value = runif(nr)), simplify = F)

without_merge <- function() {
  expanded <- cbind(lst[[1]][rep(1:nrow(lst[[1]]), times = nrow(lst[[2]])), ],
                    lst[[2]][rep(1:nrow(lst[[2]]), each = nrow(lst[[1]])), ])
  colnames(expanded) <- c("key1.x", "key2.x", "value.x", "key1.y", "key2.y","value.y")
  joined <- expanded[expanded$key1.x == expanded$key1.y, -1]
  colnames(joined) <- c("key2.x", "value.x", "key1", "key2.y","value.y")
  joined
}

res <- microbenchmark(
  dplyr_join = inner_join(lst[[1]], lst[[2]], by = c("key1")),
  base_R_with_merge = merge(lst[[1]], lst[[2]], by = c("key1")),
  base_R_without_merge = without_merge(),
  times = 20
)
res

autoplot(res)

####

library(nycflights13)

join_naive <- function() {
  flights %>%
    inner_join(weather, by = c("origin", "year", "month", "day", "hour")) %>%
    filter(temp > 90) %>%
    select(tailnum) %>%
    drop_na() %>%
    pull(tailnum) %>%
    unique()
}

join_smarter <- function() {
  flights %>%
    select(origin, year, month, day, hour, tailnum) %>%
    inner_join(weather %>%
                 select(origin, year, month, day, hour, temp) %>%
                 filter(temp > 90),
               by = c("origin", "year", "month", "day", "hour")) %>%
    select(tailnum) %>%
    drop_na() %>%
    pull(tailnum) %>%
    unique()
}

all.equal(join_naive(), join_smarter())

###

weather_over_90 <- weather[weather$temp > 90, c("origin", "year", "month", "day", "hour")]
weather_over_90 <- weather_over_90[complete.cases(weather_over_90), ]
flights_minimum <- flights[
  flights$origin %in% weather_over_90$origin &
    flights$year %in% weather_over_90$year &
    flights$month %in% weather_over_90$month &
    flights$day %in% weather_over_90$day &
    flights$hour %in% weather_over_90$hour,
  c("origin", "year", "month", "day", "hour", "tailnum")
  ]
flights_minimum <- flights_minimum[complete.cases(flights_minimum), ]

with_merge <- function() {
  unique(merge(weather_over_90, flights_minimum, by = c("origin", "year", "month", "day", "hour"))$tailnum)
}

all.equal(sort(with_merge()), sort(join_smarter()))

###

without_merge <- function(df1, df2) {
  colnames(df1) <- c("origin.x", "year.x", "month.x", "day.x", "hour.x")
  colnames(df2) <- c("origin.y", "year.y", "month.y", "day.y", "hour.y", "tailnum")
  expanded <- cbind(df1[rep(1:nrow(df1), times = nrow(df2)), ],
                    df2[rep(1:nrow(df2), each = nrow(df1)), ])
  joined <- expanded[
    expanded$origin.x == expanded$origin.y &
      expanded$year.x == expanded$year.y &
      expanded$month.x == expanded$month.y &
      expanded$day.x == expanded$day.y &
      expanded$hour.x == expanded$hour.y,
    -c(1:5)
    ]
  unique(joined$tailnum)
}

all.equal(sort(without_merge(weather_over_90, flights_minimum)),
          sort(join_smarter()))

###

res <- microbenchmark(
  dplyr_join_smart = join_smarter(),
  dplyr_join_naive = join_naive(),
  base_R_with_merge = with_merge(),
  base_R_without_merge = without_merge(weather_over_90, flights_minimum),
  times = 20
)
res

autoplot(res)