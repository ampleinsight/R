library(ggraph)
library(igraph)
library(tidyverse)
library(dendextend)
library(colormap)
library(kableExtra)
options(knitr.table.format = "html")



data=data.frame(
  level1="CEO",
  level2=c( rep("boss1",4), rep("boss2",4)),
  level3=paste0("mister_", letters[1:8])
)

# transform it to a edge list!
edges_level1_2 = data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 = data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)


# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point(color="#69b3a2", size=3) +
  geom_node_text(
    aes(  label=c("CEO", "Manager", "Manager", LETTERS[8:1]) ), 
    hjust=c(1,0.5, 0.5, rep(0,8)), 
    nudge_y = c(-.02, 0, 0, rep(.02,8)),
    nudge_x = c(0, .3, .3, rep(0,8))
  ) +
  theme_void() +
  coord_flip() +
  scale_y_reverse() 