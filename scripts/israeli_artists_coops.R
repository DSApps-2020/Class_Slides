library(tidyverse)
library(rvest)

url <- "http://pizmonet.co.il/wiki/3.10.1999"
html_obj <- read_html(url)

all_urls <- read_html("http://pizmonet.co.il/wiki/%D7%9E%D7%A6%D7%A2%D7%93%D7%99_%D7%A4%D7%96%D7%9E%D7%95%D7%A0%D7%99%D7%9D_%D7%A9%D7%91%D7%95%D7%A2%D7%99%D7%99%D7%9D") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  keep(str_detect(., "\\.20")) %>%
  map_chr(., ~str_c("http://pizmonet.co.il", .x))

get_all_songs <- function(url) {
  read_html(url) %>%
    html_table() %>%
    nth(2) %>%
    rename(artist = "ביצוע", loc = "מיקום", song = "שיר") %>%
    mutate(url = url) %>%
    select(url, loc, artist, song) %>%
    as_tibble()
}


all_weeks <- all_urls %>%
  map_dfr(possibly(get_all_songs, otherwise = tibble(url = NA, loc = NA, artist = NA, song = NA)))

write_rds(all_weeks, "data/all_weeks.rds")
# all_weeks <- read_rds("data/all_weeks.rds")

irregulars <- c("איגי וקסמן", "אביב גפן והתעויוט", "אורנה ומשה דץ",
                "שרית וינו־אלעד וחני פירסטנברג", "מומי לוי ומייקל וינסלו", "ג'וזי כץ וברק ויס",
                "קפה שחור חזק ויוסי ואסה", "סאבלימינל וגלעד ויטל",
                "אביב גפן והתעויוט עם אריק איינשטיין")

coops_only <- all_weeks %>%
  filter(str_detect(artist, " ו"), !artist %in% irregulars, !is.na(url)) %>%
  mutate(coop = map(artist, ~str_split(.x, " ו|, ")[[1]]), id = 1:n()) %>%
  unnest(coop) %>%
  filter(id != 2916) %>%# nechi nech
  group_by(id) %>%
  mutate(name = str_c("name", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = name, values_from = coop) %>%
  mutate(coop_sorted_concat =
           map2_chr(name1, name2,~str_c(sort(c(.x, .y)), collapse = "|"))) %>%
  distinct(coop_sorted_concat, .keep_all = TRUE) %>%
  select(-coop_sorted_concat)

library(ggraph)
library(tidygraph)

nodes <- tibble(label = c(coops_only$name1, coops_only$name2)) %>%
  distinct(label) %>%
  mutate(id = 1:n())

edges <- coops_only %>%
  select(name1, name2) %>%
  inner_join(nodes, by = c("name1" = "label")) %>%
  inner_join(nodes, by = c("name2" = "label")) %>%
  rename(from = id.x, to = id.y) %>%
  add_count(from) %>%
  rename(n_from = n) %>%
  add_count(to) %>%
  rename(n_to = n) #%>%
  # filter(n_from > 2 | n_to > 2)


nodes_with_genre <- read_csv("data/israeli_artists.csv")
nodes <- nodes %>% inner_join(nodes_with_genre)

graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE) %>%
  filter(group_components() == 1) %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(),
         b_cent = centrality_betweenness(),
         e_cent = centrality_eigen(),
         c_cent = centrality_closeness()) #%>%
  #filter(degree > 1) #%>%
  # activate(edges) %>%
  # filter(to %in% .N()$id, from %in% .N()$id)

graph %>%
  arrange(-degree)

graph %>%
  activate(nodes) %>%
  arrange(-b_cent)

graph %>%
  activate(nodes) %>%
  arrange(-e_cent)

ggraph(graph, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_label(aes(label = label, fill = genre), size = 3) +
  # geom_node_point(aes(size = degree), color = "lightblue", alpha = 0.5) +
  # geom_node_label(aes(label = label), color = "blue", repel = TRUE, fill = "white", size = 3) +
  theme_graph(background = "white") +
  theme(plot.title = element_text(hjust = 1)) +
  labs(title = "שיתופי פעולה במוסיקה הישראלית בשנות ה-2000", caption = "כל קשת מסמנת לפחות שיר אחד משותף, שהגיע למצעד הפזמונים ברשת ג בשנות האלפיים")

ggraph(graph, layout = "fr") +
  geom_edge_link(alpha = 0.5) +
  geom_node_text(aes(label = label), size = 2) +
  geom_node_point(aes(size = degree, color = genre), alpha = 0.5) +
  # geom_node_label(aes(label = label), color = "blue", repel = TRUE, fill = "white", size = 3) +
  theme_graph(background = "white") +
  theme(plot.title = element_text(hjust = 1)) +
  labs(title = "שיתופי פעולה במוסיקה הישראלית בשנות ה-2000", caption = "כל קשת מסמנת לפחות שיר אחד משותף, שהגיע למצעד הפזמונים ברשת ג בשנות האלפיים")

ggraph(graph, layout = "fr") +
  geom_edge_link(alpha = 0.3, color = "blue") +
  # geom_node_text(aes(label = label), size = 2, repel = TRUE) +
  geom_node_point(aes(size = degree, color = genre), alpha = 1.0) +
  # geom_node_label(aes(label = label), color = "blue", repel = TRUE, fill = "white", size = 3) +
  theme_graph(background = "white") +
  theme(plot.title = element_text(hjust = 1), plot.caption = element_text(hjust = 0)) +
  labs(size = "מס' שיתופים", color = "ז'אנר", title = "שיתופי פעולה במוסיקה הישראלית בשנות ה-2000", caption = "כל צומת הוא אמן או להקה, כל קשר מסמן לפחות שיר אחד משותף, שהגיע למצעד הפזמונים ברשת ג בשנות האלפיים, רק הקומפוננט הגדול מוצג")

# only some nodes label
ggraph(graph, layout = "fr") +
  geom_edge_link(alpha = 0.3, color = "blue") +
  geom_node_text(aes(filter = degree > 3, label = label), size = 2, repel = TRUE) +
  geom_node_point(aes(size = degree, color = genre), alpha = 1.0) +
  # geom_node_label(aes(label = label), color = "blue", repel = TRUE, fill = "white", size = 3) +
  theme_graph(background = "white") +
  theme(plot.title = element_text(hjust = 1), plot.caption = element_text(hjust = 0)) +
  labs(size = "מס' שיתופים", color = "ז'אנר", title = "שיתופי פעולה במוסיקה הישראלית בשנות ה-2000", caption = "כל צומת הוא אמן או להקה, כל קשר מסמן לפחות שיר אחד משותף, שהגיע למצעד הפזמונים ברשת ג בשנות האלפיים, רק הקומפוננט הגדול מוצג")