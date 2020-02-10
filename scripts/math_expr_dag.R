# (2 + 3)^2 - 6 * (-1 + 5)

nodes <- tribble(
  ~id, ~label,
  1, "-",
  2, "2",
  3, "+",
  4, "3",
  5, "6",
  6, "*",
  7, "-1",
  8, "+",
  9, "5",
  10, "2",
  11, "^"
)

edges <- tribble(
  ~from, ~to,
  1, 11,
  11, 3,
  11, 10,
  3, 4,
  3, 2,
  1, 6,
  6, 5,
  6, 8,
  8, 7,
  8, 9
)

math_expr <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(math_expr, layout = "tree") +
  geom_node_point(size = 20, color = "red", alpha = 0.1) +
  coord_flip() +
  geom_node_text(aes(label = label), repel = FALSE, size = 10, color = "blue", vjust = 1) +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm'), type = "closed"), alpha = 0.7) +
  theme_graph() +
  theme(plot.title = element_text(hjust = 1, color = "blue")) +
  labs(title = "(2 + 3)^2 - 6 * (-1 + 5)")


