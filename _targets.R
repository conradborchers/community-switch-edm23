library(targets)

tar_option_set(packages = c("tidyverse"))

source(here::here("R", "functions.R"))

targets <- list(

  #### SETUP ####
  tar_target(input_f, 'C://TwitterGermanyProjectData/FINAL/twlz_edchatde_sample.rds'),
  tar_target(input_d, readRDS(input_f)),
  tar_target(gsheet_url, "https://docs.google.com/spreadsheets/d/1q-kQA7c8lfJt9ITvfKO1LM4sAVfOhY6lZkS0Lir8Lr4/edit#gid=0"),
  tar_target(input_h, googlesheets4::read_sheet(gsheet_url, sheet = "REPORT-DONE")),

  #### ANALYSIS DATA SET ####
  tar_target(d_added_variables, add_transaction_variables(input_d, input_h)),
  tar_target(d_tagged_membership, d_added_variables %>% add_membership_variables(n_interactions_for_membership = 1))


  #### ANALYSIS OUTPUT ####
  # Descriptive table of community sizes and overlap
  # Mosaic plot user types
  # Inferential model of user switching with model table
  # RMSE of machine-learned point in time prediction (Python, Hayden?)
)
