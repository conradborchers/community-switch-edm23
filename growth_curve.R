library(tidyverse)

source('utils.R')

fn <- 'data/main-full.rds'
d <- readRDS(fn)

# Fit and plot curve for different thresholds

# Step 1: Define membership
d['is_twlz_member'] <- d$n_interactions_twlz_cumsum > 3
d['is_edchatde_member'] <- d$n_interactions_edchatde_cumsum > 3

# Step 2: Extract first points in time when membership was present for each user
twlz_entries <- d %>%
  filter(is_twlz_member) %>%
  group_by(user_id) %>%
  summarize(twlz_entry = min(created_at)) %>%
  ungroup() %>%
  arrange(twlz_entry) %>%
  mutate(n_members = 1:n()) %>%
  mutate(n_members_perc = n_members / n())

# Step 3: Plot time series
twlz_entries %>%
  sample_n(1000) %>% # takes too long to plot all
  arrange(n_members) %>%
  ggplot(aes(x=twlz_entry, y=n_members_perc)) +
  geom_point()

# Repeat for edchat
edchatde_entries <- d %>%
  filter(is_edchatde_member) %>%
  group_by(user_id) %>%
  summarize(edchatde_entry = min(created_at)) %>%
  ungroup() %>%
  arrange(edchatde_entry) %>%
  mutate(n_members = 1:n()) %>%
  mutate(n_members_perc = n_members / n())

edchatde_entries %>%
  sample_n(1000) %>% # takes too long to plot all
  arrange(n_members) %>%
  ggplot(aes(x=edchatde_entry, y=n_members_perc)) +
  geom_point()

# Time until doubling
edchatde_entries
