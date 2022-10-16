
library(tidyverse)

fn <- 'data/main-full.rds'
d <- readRDS(fn)


twlz_users <- d$user_id[d$is_twlz] %>% unique()
edchat_users <- d$user_id[d$is_edchatde] %>% unique()

# N users stats
length(base::intersect(edchat_users, twlz_users))
length(edchat_users)
length(base::intersect(edchat_users, twlz_users))/length(edchat_users)

length(base::intersect(twlz_users, edchat_users))
length(twlz_users)
length(base::intersect(edchat_users, twlz_users))/length(twlz_users)

# User minmax
overlap_users <- base::intersect(edchat_users, twlz_users)
d_overlap <- d %>%
  filter(user_id %in% overlap_users)

d_plot <- d_overlap %>%
  group_by(user_id, community) %>%
  summarize(
    first = min(created_at) %>% as.Date(),
    last = max(created_at) %>% as.Date()
  ) %>%
  ungroup() %>%
  filter(community %in% c('edchatde', 'twlz'))

plot(1, axes=FALSE, type="n", xlab="", ylab="", xlim=c(min(d_plot$first[d_plot$community=='edchatde']), max(d_plot$last)), ylim=c(0, 10))
first_edchatde = min(d_plot$first[d_plot$community=='edchatde'])
height = 0.001
for (i in 1:nrow(d_plot)) {
  start = d_plot$first[i]
  end = d_plot$last[i]
  if (start <= first_edchatde)
    start = first_edchatde
  #cat(i)
  if (d_plot$community[i]=='twlz')
    segments(x0=start,x1=end,y0=height,y1=height,col="red")
  else
    segments(x0=start,x1=end,y0=height,y1=height,col="blue")
  if (d_plot$community[i]=='twlz')
    height = height + 0.005
}
axis(1, d_plot$first, format(d_plot$first, "%m/%Y"), cex.axis = 1)
