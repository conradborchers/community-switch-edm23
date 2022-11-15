library(tidyverse)
library(lubridate)

dat <- final %>%
  select(status_id, user_id, created_at, text, is_head, is_chat, is_twlz, is_original, is_retweet, is_quote, is_reply, date_created, year, year_month)

# only chats!
# dat <- dat %>% filter(is_chat)

# Sys.timezone()

# check different time zones from created_at
dat <- dat %>% mutate(
  created_at = created_at %>% ymd_hms(tz = "UTC", locale = "en_US.UTF-8"),
  created_at_DE = created_at %>% with_tz(tzone = "Europe/Berlin")
)

# extract hour
dat <- dat %>% mutate(
  hour = created_at_DE %>% format("%H"),
  hour_num = hour %>% as.numeric(),
  hour_f = hour %>% factor()
)

dat <- dat %>% mutate(
  community = if_else(is_chat, "#EdChatDE", "TWLZ"),
  type = case_when(
    is_original ~ "Original",
    !is_head ~ "Conversation",
    is_retweet | is_quote ~ "Repost",
    TRUE ~ "Other"
  )
)

dat %>% count(type)

# for freqs
community_count <- dat %>% count(community)

# Plot --------------------------------------------------------------------

stat_comp <- dat %>%
  group_by(hour, community) %>%
  summarise(
    n = n()
  )

stat_comp %>%
  ggplot(aes(x = hour, y = n, group = community, fill = community)) +
  coord_polar(start = -0.15, direction = 1) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Community clocks", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme_light() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(filename = "plots/clock_comparison_totals.png", width = 6, height = 5)


eddchat_stats <- dat %>%
  filter(is_chat) %>%
  group_by(hour) %>%
  summarise(
    n = n()
  )

eddchat_stats %>%
  ggplot(aes(x = hour, y = n)) +
  coord_polar(start = -0.15, direction = 1) +
  geom_bar(stat = "identity") +
  labs(title = "The #EdChatDE clock", subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
  theme_light() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(filename = "plots/edchat_clock.png", width = 5, height = 5)
