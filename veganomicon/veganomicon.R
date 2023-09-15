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

# Filter edges by frequency and/or category
filtered_edges <- function(lower, upper, first_category, second_category) {
  edges <- all_edges

  lower_limit <- coalesce(lower, 1)
  upper_limit <- coalesce(upper, 1000)
  edges <- edges %>%
    filter(weight > lower_limit & weight < upper_limit)

  if (!is.null(first_category)) {
    edges <- merge(edges, ingredient_categories,
                   by.x = "ingredient.x", by.y = "ingredient") %>%
      rename(category.x = category)
    edges <- edges %>% filter(category.x == first_category)
  }
  if (!is.null(second_category)) {
    edges <- merge(edges, ingredient_categories,
                   by.x = "ingredient.y", by.y = "ingredient") %>%
      rename(category.y = category)
    edges <- edges %>% filter(category.y == second_category)
  }

  return(edges)
}

get_vertices <- function(edges) {
  ingredients_in_edges <- tibble(ingredient = unique(c(edges$ingredient.x, edges$ingredient.y)))
  vertices <- merge(ingredients_in_edges, ingredient_counts)
  return(vertices)
}

# Visualization: node network
draw_network <- function(net) {
  plot(net)
}

# Visualization: arc diagram
draw_arc_diagram <- function(net, vertices) {
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
                   color="black",
                   repel=FALSE,
                   hjust = 1,
                   vjust = 0,
                   nudge_x = 0.15,
                   nudge_y = -0.3,
                   angle = 90) +
    coord_cartesian(clip = 'off') +
    theme_void() +
    theme(plot.margin=unit(c(0,0,4,0), 'cm'))
}

run <- function(lower, upper, first_category, second_category) {
  edges <- filtered_edges(lower, upper, first_category, second_category)
  vertices <- get_vertices(edges)
  net <- graph_from_data_frame(d=edges,
                               vertices=vertices,
                               directed=FALSE)
  draw_arc_diagram(net, vertices)
}

# Most frequent combinations: it all comes back to salt, garlic, and olive oil
run(25, NULL, NULL, NULL)

# Spice combinations: highly common, fairly common
run(5, 10, "spices", "spices")

# Condiment combinations
run(NULL, NULL, "condiments", "condiments")

# Repeating vegetable combinations
run(2, NULL, "vegetables", "vegetables")

# Categories: what goes with...?
run(NULL, NULL, "fungi", NULL)
run(NULL, NULL, "tofu", NULL)
