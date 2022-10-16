
library(tidyverse)
library(tidygraph)

fn <- 'data/main-full.rds'
d <- readRDS(fn)

concat_na_omit <- function(a, b) {
  res <- c(a, b)
  return(res[!is.na(res)])
}

get_interaction_graph_data <- function(d) {
  # User-to-user transactions with time-stamp

  mentions <- str_extract_all(d$text, "(?<=@)[[:alnum:]_]+") # exclude @ automatically
  mentions[which(d$is_retweet)] <- vector(mode = "list", length = sum(d$is_retweet))

  # Each row is user a interacting with tweet of user b in second column
  interactions <- d %>%
    mutate(mentions = mentions) %>%
    select(created_at, matches('user_id'), mentions) %>%
    mutate(interacts = map2(quoted_user_id, retweeted_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, replied_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, mentions, concat_na_omit)) %>%
    select(user_id, created_at, interacts) %>%
    unchop(interacts) %>%
    mutate(interacts = interacts %>% str_replace_all('@', ''))
    # %>%
    #as_tbl_graph()

  return(interactions)
}

twlz_transactions <- d %>%
  filter(is_twlz) %>%
  get_interaction_graph_data()

edchatde_transactions <- d %>%
  filter(is_edchatde) %>%
  get_interaction_graph_data()
