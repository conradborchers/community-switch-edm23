export_gradients <- function(d) {
  # d <- targets::tar_read(d_analysis)
  d_user <- d %>%
    distinct(user_id, .keep_all = TRUE)
  tab <- xtabs(~ twlz_member_group + edchatde_member_group, d_user)
  questionr::chisq.residuals(tab, digits = 2, std = FALSE, raw = FALSE) %>%
    as.data.frame() %>%
    saveRDS("gradients.rds")
  return(TRUE)
}

get_fit <- function(times) {
  times2 <- times %>%
    mutate(enter = as.numeric(enter))
  out <- nls(rel ~ SSlogis(log(enter), Asym, xmid, scal), times2)
  res <- out$m$deviance()
  return(res)
}

get_times <- function(d, n_interactions = 1) {
  d["is_edchatde_member"] <- d$n_interactions_edchatde_cumsum > n_interactions
  edchatde_entries <- d %>%
    filter(is_edchatde_member) %>%
    group_by(user_id) %>%
    summarize(edchatde_entry = min(created_at)) %>%
    ungroup()
  chat_entries <- edchatde_entries %>%
    pull(edchatde_entry) %>%
    (function(v) {
      return(v[!is.na(v)])
    }) %>%
    sort()
  times <- tibble(
    enter = chat_entries,
    rel = 1:length(chat_entries) / length(chat_entries)
  )
  return(times)
}

run_doi_gof <- function(d) {
  # d <- targets::tar_read(d_analysis)

  d_user <- d %>%
    distinct(user_id, .keep_all = TRUE)

  # Sanity check
  d_user %>%
    pull(edchatde_member_group) %>%
    table() %>%
    (function(tab) {
      return(tab / sum(tab))
    })

  # Main
  times <- d_user %>% get_times(n_interactions = 1)

  times %>% ggplot(aes(enter, rel)) +
    geom_point()

  times %>% get_fit()

  # Multiple cutoffs for different numbers of required community interactions for community membership
  ii <- c()
  fits <- c()
  nn <- c()
  for (i in 1:10) {
    cat(i, "\n")
    ii <- c(ii, i)
    fits <- c(fits, d_user %>% get_times(n_interactions = i) %>% get_fit())
    nn <- c(nn, d_user %>% get_times(n_interactions = i) %>% nrow())
  }

  data.frame(
    i = ii,
    fit = fits,
    nn = nn
  )

  d_user %>%
    get_times(n_interactions = 2) %>%
    get_fit()
  d_user %>%
    get_times(n_interactions = 2) %>%
    ggplot(aes(enter, rel)) +
    geom_point()

  return(TRUE)
}

run_postprocessing <- function(d) {
  ##
  # Post-processing to adjust variables from original data set the sub-sample was taken from
  ##

  n_tweets_edchat <- d %>%
    filter(is_edchatde) %>%
    count(user_id, name = "n_tweets_edchat")

  n_days_active <- d %>%
    filter(is_edchatde) %>%
    group_by(user_id) %>%
    summarize(n_days_active_edchatde = length(unique(date_created))) %>%
    ungroup()

  join_this <- inner_join(n_tweets_edchat, n_days_active, by = "user_id") %>%
    mutate(n_tweets_edchat_per_day = n_tweets_edchat / n_days_active_edchatde) %>%
    select(user_id, n_tweets_edchat_per_day)

  res <- d %>%
    left_join(join_this, by = "user_id")

  return(res)
}

run_user_social <- function(dat) {
  dat <- dat %>% mutate(
    created_at = created_at %>% lubridate::ymd_hms(tz = "UTC", locale = "en_US.UTF-8"),
    repost_count = retweet_count + quote_count
  )

  ### Add cutoff time ###
  # variable named based on lag value
  add_lag_vars <- function(dat, lag_days) {
    users_grouped <- dat %>%
      group_by(user_id) %>%
      mutate(
        switch_date = user_switch_time %>% as.Date(),
        "user_time_cutoff_{{lag_days}}" := switch_date - lubridate::days(lag_days)
      )
    result <- users_grouped %>% ungroup()
    return(result)
  }

  dat <- dat %>%
    add_lag_vars(30)

  users_grouped <- dat %>% group_by(user_id)
  user_ids <- users_grouped %>% group_keys()

  # split into grouped list
  user_split <- users_grouped %>%
    group_split()

  ### Filter user subframes down to cutoff period
  user_cut <- user_split %>%
    # (function(x){return(x[1:1000])}) %>% # For debugging
    map(~ .x %>% filter(created_at > user_time_cutoff_30 & created_at <= switch_date))

  # make stats
  user_stats <- tibble(
    user_id = user_ids %>% unlist(),

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
    n_lag_interactions = user_cut %>% map_int(~ .x$n_interactions_edchat %>%
      sum() %>%
      as.integer())
  )

  # Export ------------------------------------------------------------------
  dat <- dat %>% left_join(user_stats, by = "user_id")

  return(dat)
}

run_social <- function(d) {
  d <- d %>% mutate(
    is_mentioning = n_mentions > 0
  )

  test <- d %>%
    filter(is_edchatde_member) %>% # in edchat
    # filter(user_switched) %>% # for filtering only switchers
    filter(!user_has_switched) %>% # before switch
    # sample_n(10000) %>% # for debugging
    I()

  # select relevant vars, reduce payload
  test <- test %>% select(
    text, user_id, status_id, is_retweet, is_mentioning, is_quote, is_reply, is_head, is_original, is_twlz, is_edchatde,
    is_quote, conversation_id, created_at, quoted_user_id, retweeted_user_id,
    replied_user_id, -mentions
  )

  all_mentions <- test$text %>% str_extract_all("(?<=@)[[:alnum:]_]+") # extract mentions without @
  all_mentions[which(test$is_retweet)] <- vector(mode = "list", length = sum(test$is_retweet)) # preserve vector length

  test <- test %>% mutate(
    mentions_clean = all_mentions,
    interacts = pmap(
      list(quoted_user_id, retweeted_user_id, replied_user_id, mentions_clean),
      ~ c(...) %>% discard(is.na)
    )
  )

  # first two cols are from to, the remaining columns are edge attributes
  edges <- test %>%
    unchop(interacts) %>%
    select(from = user_id, to = interacts, created_at, is_retweet, is_mentioning, is_quote, is_reply)

  nodes <- edges %>%
    count(from, sort = TRUE) %>%
    select(user = from, n_interactions = n)

  ig <- igraph::graph_from_data_frame(d = edges, directed = FALSE, vertices = NULL)
  graph <- ig %>% tidygraph::as_tbl_graph()

  # Stats -------------------------------------------------------------------
  graph_stats <- graph %>%
    activate(nodes) %>%
    mutate(
      centrality_degree = centrality_degree(),
      centrality_closeness = centrality_closeness(),
      centrality_betweenness = centrality_betweenness(),
      centrality_eigen = centrality_eigen(),
      is_isolated = node_is_isolated(),
      is_leaf = node_is_leaf(),
      subgroup_louvain = group_louvain()
    ) %>%
    as_tibble() %>%
    rename(user_id = name)

  d <- d %>% left_join(graph_stats, by = "user_id")

  return(d)
}


#### MASTER FUNCTIONS ####

add_transaction_variables <- function(d, hashtag_list) {
  ### Sample relevant hashtags
  twlz_hashtags <- hashtag_list %>%
    filter(is_twlz) %>%
    pull(hashtag)

  chats <- hashtag_list %>%
    filter(is_chat) %>%
    pull(hashtag)

  d <- d %>%
    mutate(
      is_edchatde = is_chat,
      community = case_when(
        is_twlz & is_edchatde ~ "both",
        is_twlz ~ "twlz",
        is_chat ~ "edchatde",
        !is_twlz & !is_edchatde ~ "neither"
      )
    )

  #### Add transaction variables

  # Step 1: Binary variable "is twlz transaction" and "is edchat transaction"
  twlz_transactions <- d %>%
    filter(is_twlz) %>%
    get_n_interactions(rename_variable = "n_interactions_twlz")

  edchatde_transactions <- d %>%
    filter(is_edchatde) %>%
    get_n_interactions(rename_variable = "n_interactions_edchat")

  d2 <- d %>%
    left_join(twlz_transactions, by = "status_id") %>%
    left_join(edchatde_transactions, by = "status_id")

  # set NAs to 0
  d2$n_interactions_twlz[is.na(d2$n_interactions_twlz)] <- 0
  d2$n_interactions_edchat[is.na(d2$n_interactions_edchat)] <- 0

  # Step 2: Sort by user id, created at and then group by user 1:n
  d2 <- d2 %>%
    arrange(user_id, created_at)

  d2["n_interactions_twlz_cumsum"] <- ave(d2$n_interactions_twlz, d2$user_id, FUN = cumsum)
  d2["n_interactions_edchatde_cumsum"] <- ave(d2$n_interactions_edchat, d2$user_id, FUN = cumsum)

  return(d2)
}

add_membership_variables <- function(d, n_interactions_for_membership = 3) {
  # Add membership variables based on DOI model
  d <- d %>%
    add_member_group(reference = "twlz", n_interactions_for_membership = n_interactions_for_membership)

  d <- d %>%
    add_member_group(reference = "edchatde", n_interactions_for_membership = n_interactions_for_membership)
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
    left_join(twlz_exits, by = "user_id") %>%
    left_join(edchatde_exits, by = "user_id")

  exit_times <- d %>%
    group_by(user_id) %>%
    summarize(
      twlz_exit = min(created_at[n_interactions_twlz_cumsum >= twlz_exit_quantile]),
      edchatde_exit = min(created_at[n_interactions_edchatde_cumsum >= edchatde_exit_quantile])
    ) %>%
    ungroup()

  d <- d %>%
    left_join(exit_times, by = "user_id")

  d["has_entered_twlz"] <- d$created_at >= d$twlz_entry
  d["has_exited_twlz"] <- d$created_at >= d$twlz_exit
  d["currently_twlz"] <- d$has_entered_twlz & (!d$has_exited_twlz)

  d["has_entered_edchatde"] <- d$created_at >= d$edchatde_entry
  d["has_exited_edchatde"] <- d$created_at >= d$edchatde_exit
  d["currently_edchatde"] <- d$has_entered_edchatde & (!d$has_exited_edchatde)

  d["user_switched"] <- NA
  d$user_switched[which(d$is_edchatde_member)] <- d$twlz_exit[which(d$is_edchatde_member)] > d$edchatde_exit[which(d$is_edchatde_member)]
  d$user_switched[is.na(d$user_switched) & d$is_edchatde_member] <- FALSE
  d["user_switch_time"] <- NA
  d$user_switch_time[which(d$user_switched)] <- pmax(d$edchatde_exit[which(d$user_switched)], d$twlz_entry[which(d$user_switched)], na.rm = FALSE)
  d["user_has_switched"] <- d$created_at >= d$user_switch_time

  return(d)
}

#### HELPER FUNCTIONS ####

match2vecs <- function(v1, v2 = base::union(twlz_hashtags, chats)) {
  return(
    v1 %>%
      base::intersect(v2) %>%
      length() %>%
      (function(x) {
        return(x > 0)
      })
  )
}

concat_na_omit <- function(a, b) {
  res <- c(a, b)
  return(res[!is.na(res)])
}

# combines interactions and count them up
get_n_interactions <- function(d, rename_variable = "", parsed_only_community_tweets = TRUE) {
  cat("\n get_n_interactions...")
  # User-to-user transactions with time-stamp

  all_mentions <- str_extract_all(d$text, "(?<=@)[[:alnum:]_]+")
  all_mentions[which(d$is_retweet)] <- vector(mode = "list", length = sum(d$is_retweet))

  # Each row is user a interacting with tweet of user b in second column
  interactions <- d %>%
    mutate(mentions = all_mentions) %>%
    select(status_id, created_at, matches("user_id"), mentions, is_twlz, is_edchatde) %>%
    # grow interacts  vector
    mutate(interacts = map2(quoted_user_id, retweeted_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, replied_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, mentions, concat_na_omit)) %>%
    mutate(n_interactions = map_int(interacts, length)) %>%
    select(status_id, n_interactions)

  # Every post itelsef is an interaction with the community
  if (parsed_only_community_tweets) {
    interactions$n_interactions <- interactions$n_interactions + 1
  }

  if (rename_variable != "") {
    names(interactions)[names(interactions) == "n_interactions"] <- rename_variable
  }

  return(interactions)
}

# Unchops the the interactions for edge list
get_interaction_graph_data <- function(d, parsed_only_community_tweets = TRUE) {
  cat("\n get_interaction_graph_data...")

  # User-to-user transactions with time-stamp
  all_mentions <- str_extract_all(d$text, "(?<=@)[[:alnum:]_]+") # exclude @ automatically
  all_mentions[which(d$is_retweet)] <- vector(mode = "list", length = sum(d$is_retweet))

  # Each row is user a interacting with tweet of user b in second column
  interactions <- d %>%
    select(status_id, created_at, matches("user_id"), mentions, is_twlz, is_edchatde) %>%
    mutate(interacts = map2(quoted_user_id, retweeted_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, replied_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, mentions, concat_na_omit)) %>%
    select(status_id, created_at, user_id, created_at, interacts, is_twlz, is_edchatde) %>%
    unchop(interacts) %>%
    mutate(interacts = interacts %>% str_replace_all("@", ""))

  # Every post itself is an interactions with the community
  if (parsed_only_community_tweets) {
    interactions$n_interactions <- interactions$n_interactions + 1
  }

  return(interactions)
}


add_member_group <- function(d, reference = "twlz", n_interactions_for_membership = 3) {
  if (reference == "twlz") {
    d["is_twlz_member"] <- d$n_interactions_twlz_cumsum > n_interactions_for_membership
    twlz_entries <- d %>%
      filter(is_twlz_member) %>%
      group_by(user_id) %>%
      summarize(twlz_entry = min(created_at)) %>%
      ungroup() %>%
      arrange(twlz_entry) %>%
      mutate(n_members = 1:n()) %>%
      mutate(n_members_perc = n_members / n()) %>%
      mutate(twlz_member_group = case_when(
        n_members_perc <= 0.025 ~ "innovators",
        n_members_perc <= 0.025 + 0.135 ~ "early adopters",
        n_members_perc <= 0.025 + 0.135 + 0.34 ~ "early majority",
        n_members_perc <= 0.025 + 0.135 + 0.34 + 0.34 ~ "late majority",
        TRUE ~ "laggards"
      ))
    out <- d %>%
      left_join(twlz_entries %>% select(user_id, twlz_entry, twlz_member_group), by = "user_id")
    return(out)
  } else if (reference == "edchatde") {
    d["is_edchatde_member"] <- d$n_interactions_edchatde_cumsum > n_interactions_for_membership
    edchatde_entries <- d %>%
      filter(is_edchatde_member) %>%
      group_by(user_id) %>%
      summarize(edchatde_entry = min(created_at)) %>%
      ungroup() %>%
      arrange(edchatde_entry) %>%
      mutate(n_members = 1:n()) %>%
      mutate(n_members_perc = n_members / n()) %>%
      mutate(edchatde_member_group = case_when(
        n_members_perc <= 0.025 ~ "innovators",
        n_members_perc <= 0.025 + 0.135 ~ "early adopters",
        n_members_perc <= 0.025 + 0.135 + 0.34 ~ "early majority",
        n_members_perc <= 0.025 + 0.135 + 0.34 + 0.34 ~ "late majority",
        TRUE ~ "laggards"
      ))
    out <- d %>%
      left_join(edchatde_entries %>% select(user_id, edchatde_entry, edchatde_member_group), by = "user_id")
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
    filter(community %in% c("edchatde", "twlz"))

  plot(1, axes = FALSE, type = "n", xlab = "", ylab = "", xlim = c(min(d_plot$first[d_plot$community == "edchatde"]), max(d_plot$last)), ylim = c(0, 10))
  first_edchatde <- min(d_plot$first[d_plot$community == "edchatde"])
  height <- 0.001
  for (i in 1:nrow(d_plot)) {
    start <- d_plot$first[i]
    end <- d_plot$last[i]
    if (start <= first_edchatde) {
      start <- first_edchatde
    }
    # cat(i)
    if (d_plot$community[i] == "twlz") {
      segments(x0 = start, x1 = end, y0 = height, y1 = height, col = "red")
    } else {
      segments(x0 = start, x1 = end, y0 = height, y1 = height, col = "blue")
    }
    if (d_plot$community[i] == "twlz") {
      height <- height + 0.005
    }
  }
  axis(1, d_plot$first, format(d_plot$first, "%m/%Y"), cex.axis = 1)

  return(d)
}

additional_lk_plots <- function(d) {
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
  return(TRUE)
}
