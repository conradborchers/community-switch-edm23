
#### MASTER FUNCTIONS ####

add_transaction_variables <- function(d, hashtag_list) {

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

  return(d2)
}

add_membership_variables <- function(d, n_interactions_for_membership = 3) {
  # Add membership variables based on DOI model
  d <- d %>%
    add_member_group(reference='twlz', n_interactions_for_membership = n_interactions_for_membership)

  d <- d %>%
    add_member_group(reference='edchatde', n_interactions_for_membership = n_interactions_for_membership)
  return(d)
}

add_membership_exit_variables <- function(d, exit_quantile = 0.9) {

  twlz_exits <- d %>%
    distinct(user_id, n_interactions_twlz_cumsum) %>%
    group_by(user_id) %>%
    summarize(twlz_exit_quantile = quantile(n_interactions_twlz_cumsum, exit_quantile)) %>%
    ungroup()

  edchatde_exits <- d %>%
    distinct(user_id, n_interactions_edchatde_cumsum) %>%
    group_by(user_id) %>%
    summarize(edchatde_exit_quantile = quantile(n_interactions_edchatde_cumsum, exit_quantile)) %>%
    ungroup()

  d <- d %>%
    left_join(twlz_exits, by='user_id') %>%
    left_join(edchatde_exits, by='user_id')

  exit_times <- d %>%
    group_by(user_id) %>%
    summarize(
      twlz_exit = min(created_at[n_interactions_twlz_cumsum >= twlz_exit_quantile]),
      edchatde_exit = min(created_at[n_interactions_edchatde_cumsum >= edchatde_exit_quantile])
      ) %>%
    ungroup()

  d <- d %>%
    left_join(exit_times, by='user_id')

  d['has_entered_twlz'] <- d$created_at >= d$twlz_entry
  d['has_exited_twlz'] <- d$created_at >= d$twlz_exit
  d['currently_twlz'] <- d$has_entered_twlz & (!d$has_exited_twlz)

  d['has_entered_edchatde'] <- d$created_at >= d$edchatde_entry
  d['has_exited_edchatde'] <- d$created_at >= d$edchatde_exit
  d['currently_edchatde'] <- d$has_entered_edchatde & (!d$has_exited_edchatde)

  d['user_switched'] <- NA
  d$user_switched[which(d$is_edchatde_member)] <- d$twlz_exit[which(d$is_edchatde_member)] >= d$edchatde_exit[which(d$is_edchatde_member)]
  d['user_switch_time'] <- NA
  d$user_switch_time[which(d$user_switched)] <- d$edchatde_exit[which(d$user_switched)]
  d['user_has_switched'] <- d$created_at >= d$user_switch_time

  return(d)
}

#### ANALYSIS FUNCTIONS ####

get_descriptive_statistics <- function(d) {

  # TODO:

  d %>%
    distinct(user_id, .keep_all = TRUE) %>%
    filter(is_edchatde_member) %>%
    pull(user_switched) %>%
    table()

  # How many Tweets

  # First tweet

  # Last Tweets

  twlz_users <- d$user_id[d$is_twlz] %>% unique()
  edchat_users <- d$user_id[d$is_edchatde] %>% unique()

  # N users stats
  length(base::intersect(edchat_users, twlz_users))
  length(edchat_users)
  length(base::intersect(edchat_users, twlz_users))/length(edchat_users)

  length(base::intersect(twlz_users, edchat_users))
  length(twlz_users)
  length(base::intersect(edchat_users, twlz_users))/length(twlz_users)

  return(TRUE)
}

sigmoid_curve <- functioN(d) {

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

}

mosaic <- function(d) {
  d_user <- d %>%
    distinct(user_id, .keep_all=TRUE)

  xtabs(~twlz_member_group + edchatde_member_group, d_user) %>%
    mosaicplot(shade = TRUE)

  xtabs(~twlz_member_group + edchatde_member_group, d_user)

  chisq.test(d_user$twlz_member_group, d_user$edchatde_member_group)

}

binary_switch_modeling <- function(d) {


  return(d)
}

time_point_ml <-function(d) {

  return(d)
}


#### HELPER FUNCTIONS ####

concat_na_omit <- function(a, b) {
  res <- c(a, b)
  return(res[!is.na(res)])
}

get_interaction_graph_data <- function(d, parsed_only_community_tweets=TRUE) {
  # User-to-user transactions with time-stamp

  mentions <- str_extract_all(d$text, "(?<=@)[[:alnum:]_]+") # exclude @ automatically
  mentions[which(d$is_retweet)] <- vector(mode = "list", length = sum(d$is_retweet))

  # Each row is user a interacting with tweet of user b in second column
  interactions <- d %>%
    mutate(mentions = mentions) %>%
    select(status_id, created_at, matches('user_id'), mentions, is_twlz, is_edchatde) %>%
    mutate(interacts = map2(quoted_user_id, retweeted_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, replied_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, mentions, concat_na_omit)) %>%
    select(status_id, created_at, user_id, created_at, interacts, is_twlz, is_edchatde) %>%
    unchop(interacts) %>%
    mutate(interacts = interacts %>% str_replace_all('@', ''))
  # %>%
  #as_tbl_graph()

  # Every post itself is an interactions with the community
  if (parsed_only_community_tweets)
    interactions$n_interactions <- interactions$n_interactions + 1

  return(interactions)
}

get_n_interactions <- function(d, rename_variable='', parsed_only_community_tweets=TRUE) {
  # User-to-user transactions with time-stamp

  mentions <- str_extract_all(d$text, "(?<=@)[[:alnum:]_]+") # exclude @ automatically
  mentions[which(d$is_retweet)] <- vector(mode = "list", length = sum(d$is_retweet))

  # Each row is user a interacting with tweet of user b in second column
  interactions <- d %>%
    mutate(mentions = mentions) %>%
    select(status_id, created_at, matches('user_id'), mentions, is_twlz, is_edchatde) %>%
    mutate(interacts = map2(quoted_user_id, retweeted_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, replied_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, mentions, concat_na_omit)) %>%
    mutate(n_interactions = map_int(interacts, length)) %>%
    select(status_id, n_interactions)

  # Every post itelsef is an interactions with the community
  if (parsed_only_community_tweets)
    interactions$n_interactions <- interactions$n_interactions + 1

  if (rename_variable != '')
    names(interactions)[names(interactions) == 'n_interactions'] <- rename_variable

  return(interactions)
}

add_member_group <- function(d, reference='twlz', n_interactions_for_membership = 3) {
  if (reference == 'twlz') {
    d['is_twlz_member'] <- d$n_interactions_twlz_cumsum > n_interactions_for_membership
    twlz_entries <- d %>%
      filter(is_twlz_member) %>%
      group_by(user_id) %>%
      summarize(twlz_entry = min(created_at)) %>%
      ungroup() %>%
      arrange(twlz_entry) %>%
      mutate(n_members = 1:n()) %>%
      mutate(n_members_perc = n_members / n()) %>%
      mutate(twlz_member_group = case_when(
        n_members_perc <= 0.025 ~ 'innovators',
        n_members_perc <= 0.025+0.135 ~ 'early adopters',
        n_members_perc <= 0.025+0.135+0.34 ~ 'early majority',
        n_members_perc <= 0.025+0.135+0.34+0.34 ~ 'late majority',
        TRUE ~ 'laggards'
      ))
    out <- d %>%
      left_join(twlz_entries %>% select(user_id, twlz_entry, twlz_member_group), by='user_id')
    #out$twlz_member_group[out$created_at < out$twlz_entry] <- NA # mask group before it occurs
    return(out)
  } else if (reference == 'edchatde') {
    d['is_edchatde_member'] <- d$n_interactions_edchatde_cumsum > n_interactions_for_membership
    edchatde_entries <- d %>%
      filter(is_edchatde_member) %>%
      group_by(user_id) %>%
      summarize(edchatde_entry = min(created_at)) %>%
      ungroup() %>%
      arrange(edchatde_entry) %>%
      mutate(n_members = 1:n()) %>%
      mutate(n_members_perc = n_members / n()) %>%
      mutate(edchatde_member_group = case_when(
        n_members_perc <= 0.025 ~ 'innovators',
        n_members_perc <= 0.025+0.135 ~ 'early adopters',
        n_members_perc <= 0.025+0.135+0.34 ~ 'early majority',
        n_members_perc <= 0.025+0.135+0.34+0.34 ~ 'late majority',
        TRUE ~ 'laggards'
      ))
    out <- d %>%
      left_join(edchatde_entries %>% select(user_id, edchatde_entry, edchatde_member_group), by='user_id')
    #out$edchatde_member_group[out$created_at < out$edchatde_entry] <- NA # mask group before it occurs
    return(out)
  } else {
    return(d)
  }
}

#### OLD FUNCTIONS ####

old_overlap_plot <- function(d) {

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


  return(d)
}
