
# incidence matrix
B <- as.matrix(rbind(c(1, 1, 0, 0, 0),
                     c(1, 1, 1, 1, 0),
                     c(0, 1, 1, 0, 1),
                     c(0, 0, 1, 1, 1)))

# 4x4 groups matrix
Pg <- B %*% t(B)
Pg[Pg > 0] <- 1
diag(Pg) <- 0
Pg

g <- as_tbl_graph(Pg)

g %>% activate(nodes) %>% mutate(name = 1:4) %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "#00bfc4", size = 5) +
  geom_edge_link() +
  geom_node_text(aes(label = name), size = 15, vjust = 0.3, hjust = -0.2) +
  theme_graph()

# 5x5 items matrix
Pi <- t(B) %*% B
Pi[Pi > 0] <- 1
diag(Pi) <- 0
Pi

g <- as_tbl_graph(Pi)

g %>% activate(nodes) %>% mutate(name = letters[1:5]) %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "#f8766d", size = 5) +
  geom_edge_link() +
  geom_node_text(aes(label = name), size = 15, vjust = 0.3, hjust = -0.2) +
  theme_graph()
