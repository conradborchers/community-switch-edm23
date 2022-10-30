concat_na_omit <- function(a, b) {
  res <- c(a, b)
  return(res[!is.na(res)])
}

get_interaction_graph_data <- function(d) {
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

  return(interactions)
}

get_n_interactions <- function(d, rename_variable='') {
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
