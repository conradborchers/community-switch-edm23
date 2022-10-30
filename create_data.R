library(tidyverse)
source('utils.R')

fn <- 'C://TwitterGermanyProjectData/FINAL/final_prop_bot_user_21.10.rds'
d <- readRDS(fn)

# Get relevant hashtags
hashtag_list <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1q-kQA7c8lfJt9ITvfKO1LM4sAVfOhY6lZkS0Lir8Lr4/edit#gid=0",
                                          sheet = "REPORT-DONE")

hashtag_sample <- hashtag_list %>%
  select(hashtag = hashtag_new, not_k12, is_chat, is_twlz, is_subject, is_state, state, subject, community) %>%
  filter(not_k12 != 1)

# Sample relevant hashtags
twlz_hashtags <- hashtag_sample %>%
  mutate(is_twlz = is_twlz == 1) %>%
  filter(is_twlz) %>%
  pull(hashtag)
# > twlz_hashtags
# [1] "#lehrerzimmer"        "#twitterkollegium"    "#twitterlehrerzimmer" "#twitterlz"           "#twlz"

chats <- hashtag_sample %>%
  mutate(is_chat = is_chat == 1) %>%
  filter(is_chat) %>%
  pull(hashtag)
# > chats
# [1] "#edchatde"

match2vecs <- function(v1, v2=base::union(twlz_hashtags, chats)) {
  return(
    v1 %>%
      base::intersect(v2) %>%
      length() %>%
      (function(x){return(x>0)})
  )
}

# Step 1: Get unique conversation IDs associated with sampled hashtags
whitelist <- d %>%
  filter(hashtags %>% map_lgl(match2vecs)) %>%
  pull(conversation_id) %>%
  unique()

# Step 2: Filter these conversation IDs
out <- d %>%
  filter(conversation_id %in% whitelist)

# Step 3: Tag communities
out['is_twlz'] <- out$hashtags %>% map_lgl(~match2vecs(., v2=twlz_hashtags))
out['is_edchatde'] <- out$hashtags %>% map_lgl(~match2vecs(., v2=chats))

out <- out %>%
  mutate(community = ifelse(is_twlz & is_edchatde, 'both', ifelse(is_twlz, 'twlz', ifelse(is_edchatde, 'edchatde', 'neither'))))

# Add transaction variables

d <- out

# Step 1: Binary variable "is twlz transaction" and "is edchat transaction"

twlz_transactions <- d %>%
  filter(is_twlz) %>%
  get_n_interactions(rename_variable = 'n_interactions_twlz')

edchatde_transactions <- d %>%
  filter(is_edchatde) %>%
  get_n_interactions(rename_variable = 'n_interactions_edchat')

d2 <- d %>%
  left_join(twlz_transactions, by='status_id') %>%
  left_join(edchatde_transactions, by='status_id')

d2$n_interactions_twlz[is.na(d2$n_interactions_twlz)] <- 0
d2$n_interactions_edchat[is.na(d2$n_interactions_edchat)] <- 0

# Step 2: Sort by user id, created at and then group by user 1:n

d2 <- d2 %>%
  arrange(user_id, created_at)

d2['n_interactions_twlz_cumsum'] <- ave(d2$n_interactions_twlz, d2$user_id, FUN=cumsum)
d2['n_interactions_edchatde_cumsum'] <- ave(d2$n_interactions_edchat, d2$user_id, FUN=cumsum)

# Export
fn <- 'data/main-full.rds'
saveRDS(d2, fn)
