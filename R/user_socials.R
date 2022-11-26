library(tidyverse)
library(lubridate)

dat <- tar_read("d_tagged_switch")

dat <- dat %>% mutate(
  created_at = created_at %>% lubridate::ymd_hms(tz = "UTC", locale = "en_US.UTF-8"),
  repost_count = retweet_count + quote_count
)


# FIXME: have interacts globally available in targets!

# TODO: run network first!

# all_mentions <- dat$text %>% str_extract_all("(?<=@)[[:alnum:]_]+") # exclude @ automatically
# all_mentions[which(dat$is_retweet)] <- vector(mode = "list", length = sum(dat$is_retweet)) # add NULL to retweets
#
# dat <- dat %>% mutate(
#   mentions_clean = all_mentions,
#   interacts = pmap(
#     list(quoted_user_id, retweeted_user_id, replied_user_id, mentions_clean),
#     ~ c(...) %>% discard(is.na)
#   ),
#   n_interactions = interacts %>% map_int(length)
# )


### Add cutoff time ####

# variable named based on lag value
add_lag_vars <- function(dat, lag_days) {
  users_grouped <- all %>%
    group_by(user_id) %>%
    mutate(
      edchatde_exit_date = edchatde_exit %>% as.Date(),
      "user_time_cutoff_{{lag_days}}" := edchatde_exit_date - lubridate::days(lag_days)
    )

  result <- users_grouped %>% ungroup()
  return(result)
}

dat <- dat %>%
  add_lag_vars(30) %>%
  add_lag_vars(60) %>%
  add_lag_vars(90)


user_grouped <- dat %>% group_by(user_id)
user_ids <- users_grouped %>% group_keys()

# split into grouped list
user_split <- users_grouped %>%
  group_split()

### Filter user subframes down to cutoff period

user_cut <- user_split %>%
  map(~ .x %>% filter(created_at > user_time_cutoff))

# make stats
user_stats <- tibble(
  user_id = user_ids,

  # stats
  n_lag_posted_tweets_all = user_cut %>% map_int(nrow),
  n_lag_posted_tweets_original = user_cut %>% map_int(~ .x$is_original %>% sum()),

  # Social
  n_lag_likes = user_cut %>% map_int(~ .x$like_count %>% sum()),
  n_lag_reposts = user_cut %>% map_int(~ .x$repost_count %>% sum()),
  n_lag_replies = user_cut %>% map_int(~ .x$reply_count %>% sum()),
  n_lag_convs = user_cut %>% map_int(~ .x %>%
    filter(!is_head) %>%
    nrow()),

  # Connect
  n_lag_mentions = user_cut %>% map_int(~ .x$n_mentions %>% sum()),
  n_lag_interactions = user_cut %>% map_int(~ .x$n_interactions %>% sum())
)


# Export ------------------------------------------------------------------

dat <- dat %>% left_join(user_stats, by = "user_id")


# Plot tweets before leave ------------------------------------------------
quit_stat <- users_grouped %>%
  ungroup() %>%
  mutate(
    days_before_last = date_created - edchatde_exit_date,
    days_before_last_int = days_before_last %>% as.integer()
  )

quit_time <- quit_stat %>%
  count(days_before_last_int) %>%
  arrange(desc(days_before_last_int)) %>%
  filter(days_before_last_int > -1000) %>%
  filter(days_before_last_int != 0)

quit_time %>%
  ggplot(aes(x = days_before_last_int, y = n)) +
  geom_bar(stat = "identity")

quit <- quit_stat %>%
  ggplot(aes(x = days_before_last_int)) +
  geom_histogram()

ggsave("plots/quitters.png", width = 8, height = 5)
