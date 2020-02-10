nodes <- tribble(
  ~id, ~year, ~event,
  1, 1940, "Birth",
  2, 1956, "First Guitar",
  3, 1957, "Meets Paul",
  4, 1958, "Mother dies",
  5, 1962, "First Single",
  6, 1963, "Marries Cynthia",
  7, 1969, "Marries Yoko",
  8, 1969, "Leaves Beatles",
  9, 1971, "Imagine",
  10, 1980, "Murdered"
)

edges <- tribble(
  ~from, ~to,
  1, 2,
  2, 3,
  3, 4,
  4, 5,
  5, 6,
  6, 7,
  7, 8,
  8, 9,
  9, 10
)

john <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(john, layout = "nicely") +
  geom_node_point(size = 20, color = "lightblue", alpha = 0.5) +
  geom_node_text(aes(label = str_c(year, ": ", event)), repel = TRUE, size = 4) +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm'))) +
  theme_graph()

