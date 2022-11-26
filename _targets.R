library(targets)
library(tarchetypes)

# FYI see the new Targets Markdown interactive workflow <3
# https://books.ropensci.org/targets/literate-programming.html#target-markdown

tar_option_set(packages = c("tidyverse", "sjPlot"))
source(here::here("R", "functions.R"))

targets <- list(
  #### SETUP ####
  tar_target(input_f, here::here("data", "twlz_edchatde_sample_FIX.rds")),
  tar_target(input_d, readRDS(input_f)),
  # tar_target(input_d_cleaned, clean_data(input_d)),
  tar_target(sample_hashtags, readr::read_csv("data/sample_hashtags.csv")),

  #### ANALYSIS DATA SET ####
  tar_target(d_added_variables, input_d %>% add_transaction_variables(sample_hashtags)),
  tar_target(d_tagged_membership, d_added_variables %>% add_membership_variables(n_interactions_for_membership = 1)),
  tar_target(d_tagged_switch, d_tagged_membership %>% add_membership_exit_variables(exit_quantile = 0.9))

  #### ANALYSIS OUTPUT ####
  # Descriptive table of community sizes and overlap
  # Mosaic plot user types
  # Inferential model of user switching with model table
  # RMSE of machine-learned point in time prediction (Python, Hayden?)
)


