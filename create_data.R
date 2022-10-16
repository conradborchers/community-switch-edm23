library(tidyverse)
fn <- 'C://TwitterGermanyProjectData/FINAL/final_propagated_botchecked_08.10.2022.rds'
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

# Export
fn <- 'data/main-full.rds'
saveRDS(out, fn)
