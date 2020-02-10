sci_fi <- read_csv("data/sci_fi_books.csv")

cor_mat <- sci_fi %>%
  mutate(n_features = rowSums(.[8:18] != 0)) %>%
  filter(popular == 1, n_features >= 2) %>%
  select(-date, -author, -frequency, -author_gender, -quarter_century, -century, -popular, -n_features) %>%
  pivot_longer(cols = 2:ncol(.), names_to = "property") %>% 
  pivot_wider(names_from = book, values_from = value) %>%
  select(-property) %>%
  cor(method = "spearman") %>%
  round(., 2)

cor_mat[1:5, 1:5]

cor_mat_tidy <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column(var = "book1") %>%
  as_tibble() %>%
  pivot_longer(cols = 2:ncol(.), names_to = "book2", values_to = "corr")

cor_mat_tidy %>%
  ggplot(aes(book1, book2, fill = corr)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = NULL, y = NULL)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cor_mat)
upper_tri

cor_mat_tidy_upper <- upper_tri %>%
  as.data.frame() %>%
  rownames_to_column(var = "book1") %>%
  as_tibble() %>%
  pivot_longer(cols = 2:ncol(.), names_to = "book2", values_to = "corr", values_drop_na = TRUE) %>%
  mutate(book1 = fct_relevel(book1, rownames(upper_tri)),
         book2 = fct_relevel(book2, rownames(upper_tri)))

cor_mat_tidy_upper %>%
  ggplot(aes(book1, book2, fill = corr)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL)

# maybe looks better
cor_mat_tidy_upper %>%
  ggplot(aes(book1, book2, fill = corr)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  labs(x = NULL, y = NULL)



# re-order
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cor_mat_ordered <- reorder_cormat(cor_mat)
upper_tri_ordered <- get_upper_tri(cor_mat_ordered)
# Melt the correlation matrix
cor_mat_tidy_upper_ordered <- upper_tri_ordered %>%
  as.data.frame() %>%
  rownames_to_column(var = "book1") %>%
  as_tibble() %>%
  pivot_longer(cols = 2:ncol(.), names_to = "book2", values_to = "corr", values_drop_na = TRUE) %>%
  mutate(book1 = fct_relevel(book1, rownames(upper_tri_ordered)),
         book2 = fct_relevel(book2, rownames(upper_tri_ordered)))

# Create a ggheatmap
cor_mat_tidy_upper_ordered %>%
  ggplot(aes(book1, book2, fill = corr)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0, 
                                   size = 10, hjust = 1))+
  # coord_fixed() +
  labs(x = NULL, y = NULL)

### ggraph ###
nodes <- tibble(id = 1:dim(cor_mat)[1], book = factor(rownames(cor_mat), levels = rownames(cor_mat)))
edges <- cor_mat_tidy_upper %>%
  filter(book1 != book2) %>%
  inner_join(nodes, by = c("book1" = "book")) %>%
  rename(from = id) %>%
  inner_join(nodes, by = c("book2" = "book")) %>%
  rename(to = id)

sci_fi_books <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

sci_fi_books %>%
  activate(edges) %>%
  filter(corr > 0.5) %>%
  ggraph(layout = "fr") +
  geom_node_point(size = 20, color = "red", alpha = 0.2) +
  geom_node_text(aes(label = book), repel = FALSE, size = 4) +
  geom_edge_link(aes(width = corr), color = "red", alpha = 0.2) +
  theme_graph()

### corrr package full of bugs... ###
# install.packages("corrr")
library(corrr)

x <- sci_fi %>%
  filter(popular == 1, book != "1984") %>%
  select(-date, -author, -frequency, -author_gender) %>%
  pivot_longer(cols = 2:ncol(.), names_to = "property") %>% 
  pivot_wider(names_from = book, values_from = value) %>%
  select(-property) %>%
  correlate(method = "spearman") #%>%    # Create correlation data frame (cor_df)
  #rearrange() %>%  # rearrange by correlations
  #shave() # Shave off the upper triangle for a clean result
#> 
#> Correlation method: 'pearson'
#> Missing treated using: 'pairwise.complete.obs'
#> Registered S3 method overwritten by 'seriation':
#>   method         from 
#>   reorder.hclust gclus

fashion(x)

x %>% rearrange(absolute = FALSE) %>% shave() %>% rplot()

x %>% network_plot(min_cor = .6)
