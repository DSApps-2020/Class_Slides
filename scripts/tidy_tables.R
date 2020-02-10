okcupid_religion2 <- okcupid %>% mutate(religion = case_when(
  is.na(religion) | str_detect(religion, "other") ~ "unspecified",
  str_detect(religion, "agnosticism") | str_detect(religion, "atheism") ~ "atheist",
  str_detect(religion, "buddhism") ~ "buddhist",
  str_detect(religion, "christianity") | str_detect(religion, "catholicism") ~ "christian",
  str_detect(religion, "judaism") ~ "jewish",
  str_detect(religion, "hinduism") ~ "hindu",
  str_detect(religion, "islam") ~ "muslim",
  TRUE ~ "NA"
))

# table 1
table1 <- okcupid_religion2 %>%
  mutate(yob = 2012 - age) %>%
  filter(yob >= 1950) %>%
  count(religion, yob, orientation) %>%
  group_by(religion, yob) %>%
  mutate(n_total = sum(n)) %>%
  ungroup() %>%
  filter(orientation == "straight") %>%
  rename(n_straight = n) %>%
  select(-orientation) %>%
  complete(religion, nesting(yob), fill = list(n_straight = 0, n_total = 0)) %>% arrange(yob)

# table 2
table2 <- table1 %>%
  rename(straight = n_straight, total = n_total) %>%
  pivot_longer(c(straight, total), names_to = "type", values_to = "n") %>%
  arrange(yob)

# table 3
table3 <- table1 %>%
  mutate(pct_straight = map2_chr(n_straight, n_total, function(x, y) str_c(x, "/", y))) %>%
  select(-c(n_straight, n_total)) %>%
  complete(religion, nesting(yob), fill = list(pct_straight = "0/0")) %>%
  arrange(yob)

# table 4
table4 <- table1 %>%
  pivot_wider(names_from = yob,
              values_from = c(n_total, n_straight),
              values_fill = list(n_total = 0, n_straight = 0))

write_rds(list(table1 = table1, table2 = table2, table3 = table3, table4 = table4),
          "u1_d01-tidyverse/data/tidy_tables.rds")
