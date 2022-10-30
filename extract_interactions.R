
library(tidyverse)
library(tidygraph)

source('utils.R')

fn <- 'data/main-full.rds'
d <- readRDS(fn)

twlz_transactions <- d %>%
  filter(is_twlz) %>%
  get_interaction_graph_data()

edchatde_transactions <- d %>%
  filter(is_edchatde) %>%
  get_interaction_graph_data()
