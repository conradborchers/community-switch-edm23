library(targets)
library(tarchetypes)

tar_option_set(packages = c("tidyverse", "sjPlot", "tidygraph", "igraph", "lubridate"))
source(here::here("R", "functions.R"))

targets <- list(
  #### SETUP ####
  tar_target(input_f, here::here("data", "twlz_edchatde_sample_FIX.rds")),
  tar_target(input_d, readRDS(input_f) %>% filter(!is_bot)), # 255,061 tweets and 170 users less
  tar_target(sample_hashtags, readr::read_csv("data/sample_hashtags.csv")),

  #### ANALYSIS DATA SET ####
  tar_target(d_added_variables, input_d %>% add_transaction_variables(sample_hashtags)),
  tar_target(d_tagged_membership, d_added_variables %>% add_membership_variables(n_interactions_for_membership = 2)),
  tar_target(d_tagged_switch, d_tagged_membership %>% add_membership_exit_variables(exit_quantile = 0.9)),
  tar_target(d_social, d_tagged_switch %>% run_social()),
  tar_target(d_modeling, d_social %>% run_user_social()),
  tar_target(d_analysis, d_modeling %>% run_postprocessing())
)
