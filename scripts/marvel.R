# Source: https://felixluginbuhl.com/network/
library(rvest)

url <- "https://fr.wikipedia.org/w/index.php?title=Liste_des_films_de_l%27univers_cin%C3%A9matographique_Marvel&oldid=144793972#Personnages"

marvel_df <- url %>%
  read_html() %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE) %>%
  .[[5]]

marvel_df[1,1] <- "Iron Man 1"
marvel_df[2,1] <- "The Incredible Hulk"
marvel_df[4,1] <- "Thor 1"
marvel_df[8,1] <- "Thor: The Dark World"
marvel_df[9,1] <- "Captain America 2"
marvel_df[10,1] <- "Guardians of the Galaxy"
marvel_df[11,1] <- "Avengers: Age of Ultron"
marvel_df[12,1] <- "Ant-Man 1"
marvel_df[14,1] <- "Doctor Strange 1"
marvel_df[15,1] <- "Guardians of the Galaxy Vol. 2"
marvel_df[18,1] <- "Black Panther 1"
marvel_df[20,1] <- "Ant-Man and the Wasp"
marvel_df[21,1] <- "Captain Marvel 1"

marvel_df <- marvel_df %>%
  rename("Black Widow" = "Veuve noire",
         "Hawkeye" = "Œil de Faucon",
         "Scarlet Witch" = "Sorcière rouge") %>%
  mutate(Film = factor(Film, levels = unique(Film))) %>%
  mutate_all(funs(str_replace_all(., c("Oui" = "1", "^$" = "0")))) #^$ is for empty string

write_csv(marvel_df, "data/marvel_incidence_matrix.csv")

marvel_tidy <- marvel_df %>%
  reshape2::melt(id.vars = "Film", value.name = "Value") %>%
  rename("Character" = "variable") %>%
  as_tibble()

ggplot(marvel_tidy, aes(x = Character, y = Film)) +
  geom_tile(aes(fill = Value)) + 
  scale_fill_manual(values=c("0"="grey", "1"="lightgreen"),
                    name="", labels=c("Out","In")) + 
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(colour = "dimgrey"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Characters appearance in the Marvel Movies",
       caption = "Félix Luginbühl (@lgnbhl)\nData source: Wikipedia")

write_csv(marvel_tidy, "data/marvel.csv")

# graph

library(tidygraph)

marvel_graph <- marvel_tidy %>%
  filter(Value != "0") %>%
  select(-Value) %>%
  as_tbl_graph(directed = FALSE) %>%
  mutate(degree = centrality_degree(),
         closeness = centrality_closeness(),
         betweenness = centrality_betweenness()) %>%
  #create type variable
  full_join(tibble(film = marvel_df$Film, type = "Movie"), by = c("name" = "film")) %>%
  mutate(type = replace_na(type, "Character"))

# get only nodes table!
marvel_graph %>% 
  activate(nodes) %>% 
  as_tibble() %>%
  arrange(desc(degree))

with_graph(marvel_graph, graph_is_bipartite())
# TRUE

# bipartite
ggraph(marvel_graph %>%
         activate(nodes) %>%
         mutate(type = ifelse(type == "Character", FALSE, TRUE)),
       layout = "bipartite") + 
  geom_edge_diagonal(alpha = 0.2) + 
  geom_node_point(aes(size = degree, color = as.factor(type)), alpha = 0.8) + 
  scale_color_brewer(palette = "Set1", name = "Type") +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "#f8f2e4")) +
  labs(title = "Marvel Cinematic Universe Bipartite Network",
       size = "Degree") +
  guides(color = FALSE, size = FALSE)

# trying to cluster
ggraph(marvel_graph %>%
         activate(nodes) %>%
         mutate(type = ifelse(type == "Character", FALSE, TRUE),
                cluster = group_edge_betweenness()),
       layout = "bipartite") + 
  geom_edge_diagonal(alpha = 0.2) + 
  geom_node_point(aes(size = degree, color = as.factor(cluster)), alpha = 0.8) + 
  scale_color_brewer(palette = "Set1", name = "Type") +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "#f8f2e4")) +
  labs(title = "Marvel Cinematic Universe Bipartite Network",
       size = "Degree") +
  guides(color = FALSE, size = FALSE)
