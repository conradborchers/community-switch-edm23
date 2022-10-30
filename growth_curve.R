library(tidyverse)

source('utils.R')

fn <- 'data/main-full.rds'
d <- readRDS(fn)

# Simple crosstab for membership groups

d_user <- d %>%
  distinct(user_id, .keep_all=TRUE)

xtabs(~twlz_member_group + edchatde_member_group, d_user) %>%
  mosaicplot(shade = TRUE)

xtabs(~twlz_member_group + edchatde_member_group, d_user)

chisq.test(d_user$twlz_member_group, d_user$edchatde_member_group)

## OLD CODE ##

# Step 3: Plot time series
twlz_entries %>%
  sample_n(1000) %>% # takes too long to plot all
  arrange(n_members) %>%
  ggplot(aes(x=twlz_entry, y=n_members_perc)) +
  geom_point()

edchatde_entries %>%
  sample_n(1000) %>% # takes too long to plot all
  arrange(n_members) %>%
  ggplot(aes(x=edchatde_entry, y=n_members_perc)) +
  geom_point()
