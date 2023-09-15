# Moskowitz and Romero. The Ultimate Vegan Cookbook Veganomicon 10th Anniversary Edition. Da Capo Press, 2017.
# Data courtesy of https://www.kaggle.com/datasets/timotheeschlumberger/recipesingredients-count

library(igraph)
library(ggraph)

long_data <- read_csv("recipesIngredients_countLong.csv")
ingredient_counts <- long_data %>% group_by(ingredient) %>% summarise(count = n())

# Export unique ingredients
write.csv(ingredient_counts %>% select("ingredient"),
         "ingredient_counts.csv",
         quote=TRUE, row.names=FALSE)
 
# Add categories by hand and re-import. Some explanations:
#   nuts => includes nuts and seeds (some seeds are classed as spices, life is complicated)
#   spices => includes herbs and spices
#   extracts => includes extracts and zests
#   bread => includes flours, breads, and pastas, and controversially, seitan
ingredient_categories <- read_csv("ingredient_categories.csv")
unique_categories = sort(unique(c(ingredient_categories$category)))

pairs <- merge(long_data, long_data, by = "recipe") %>% 
  filter(ingredient.x < ingredient.y) %>% 
  select(recipe, ingredient.x, ingredient.y)

all_edges <- pairs %>% group_by(ingredient.x, ingredient.y) %>%
  summarise(weight = n())

# TODO: extract filtering into a function
# Filter edges by frequency and/or category
lower_limit <- 2
upper_limit <- 1000
edges_with_x_category <- merge(all_edges, ingredient_categories,
                               by.x = "ingredient.x", by.y = "ingredient") %>% 
  rename(category.x = category)
edges_with_categories <- merge(edges_with_x_category, ingredient_categories,
                               by.x = "ingredient.y", by.y = "ingredient") %>% 
  rename(category.y = category)
edges <- edges_with_categories %>%
  filter(weight > lower_limit & weight < upper_limit) %>% 
  filter(category.x == "vegetables" & category.y == "vegetables")

ingredients_in_edges <- tibble(ingredient = unique(c(edges$ingredient.x, edges$ingredient.y)))
vertices <- merge(ingredients_in_edges, ingredient_counts)

net <- graph_from_data_frame(d=edges,
                             vertices=vertices,
                             directed=FALSE)

# Visualization: node network
# plot(net)

# TODO: Extract into function
# Visualization: arc diagram
ggraph(net, layout="linear") +
  geom_edge_arc(color="#999999",
                aes(edge_width = weight),
                alpha = 0.5,
                show.legend = FALSE) +
  geom_node_point(color="steelblue",
                  aes(size=vertices$count, alpha=0.5),
                  show.legend = FALSE) +
  geom_node_text(aes(label = vertices$ingredient),
                 size=3,
                 color="#999999",
                 repel=FALSE,
                 hjust = 1,
                 vjust = 0,
                 nudge_x = 0.15,
                 nudge_y = -0.3,
                 angle = 90) +
  coord_cartesian(clip = 'off') +
  theme_void() + 
  theme(plot.margin=unit(c(0,0,4,0), 'cm'))
  