library(tidyverse)

dat <- tar_read("d_tagged_switch")
d <- dat
d <- d %>% mutate(
  is_mentioning = n_mentions > 0
)


set.seed(42)
test <- d %>%
  filter(is_edchatde_member) %>% # in edchat
  filter(user_switched) %>% # only switchers
  filter(!user_has_switched) %>% # before switch
  sample_n(10000)

# select relevant vars, reduce payload
test <- test %>% select(
  text, user_id, status_id, is_retweet, is_mentioning, is_quote, is_reply, is_head, is_original, is_twlz, is_edchatde,
  is_quote, conversation_id, created_at, quoted_user_id, retweeted_user_id,
  replied_user_id, -mentions
)

all_mentions <- test$text %>% str_extract_all("(?<=@)[[:alnum:]_]+") # exclude @ automatically
all_mentions[which(test$is_retweet)] <- vector(mode = "list", length = sum(test$is_retweet)) # add NULL to retweets

test <- test %>% mutate(
  mentions_clean = all_mentions,
  interacts = pmap(
    list(quoted_user_id, retweeted_user_id, replied_user_id, mentions_clean),
    ~ c(...) %>% discard(is.na)
  )
)

# TODO: deal with anons???

# Make Graph --------------------------------------------------------------
library(tidygraph)
library(igraph)

# first two cols are from to, rest edge attributes
edges <- test %>%
  unchop(interacts) %>%
  select(from = user_id, to = interacts, created_at, is_retweet, is_mentioning, is_quote, is_reply)

nodes <- edges %>%
  count(from, sort = TRUE) %>%
  select(user = from, n_interactions = n)
# full_join(test %>% select(interacts), by = c("user" = "interacts"))

ig <- igraph::graph_from_data_frame(d = edges, directed = FALSE)
graph <- ig %>% tidygraph::as_tbl_graph()

# Stats -------------------------------------------------------------------

graph_stats <- graph %>%
  activate(nodes) %>%
  mutate(
    centrality_degree = centrality_degree(),
    centrality_closeness = centrality_closeness(),
    centrality_betweenness = centrality_betweenness(),
    centrality_eigen = centrality_eigen(),
    is_isolated = node_is_isolated(),
    is_leaf = node_is_leaf(),
    subgroup_louvain = group_louvain()
  ) %>%
  as_tibble() %>%
  rename(user_id = name)

### Community detection?? ###

# -> https://tidygraph.data-imaginist.com/reference/index.html#community-detection

# group_louvain()
# group_edge_betweenness()

# Export ------------------------------------------------------------------



d <- d %>% left_join(graph_stats, by = "user_id")
saveRDS("data/d_added_graphs.rds")


# Plot --------------------------------------------------------------------
library(ggraph)

# igraph plotting
# plot(ig)
#
# graph %>% ggraph() +
#   geom_edge_link() +
#   geom_node_point()
#
#
# graph %>%
#   ggraph(layout = "fr") +
#   geom_edge_arc(
#     colour = "gray50",
#     lineend = "round",
#     strength = .1,
#     alpha = .9
#   ) +
#   geom_node_point(size = 1, alpha = 0.2)


# use is_reply etc as edge type?
