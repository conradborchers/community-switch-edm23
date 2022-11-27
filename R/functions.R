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
    add_lag_vars(30) #%>%
    #add_lag_vars(60) %>%
    #add_lag_vars(90)

  users_grouped <- dat %>% group_by(user_id)
  user_ids <- users_grouped %>% group_keys()

  # split into grouped list
  user_split <- users_grouped %>%
    group_split()

  ### Filter user subframes down to cutoff period
  user_cut <- user_split %>%
    #(function(x){return(x[1:1000])}) %>%
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
    n_lag_interactions = user_cut %>% map_int(~ .x$n_interactions_edchat %>% sum() %>% as.integer())
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
    #filter(user_switched) %>% # only switchers
    filter(!user_has_switched) %>% # before switch
    #sample_n(10000) %>%
    I()

  # select relevant vars, reduce payload
  test <- test %>% select(
    text, user_id, status_id, is_retweet, is_mentioning, is_quote, is_reply, is_head, is_original, is_twlz, is_edchatde,
    is_quote, conversation_id, created_at, quoted_user_id, retweeted_user_id,
    replied_user_id, -mentions
  )

  all_mentions <- test$text %>% str_extract_all("(?<=@)[[:alnum:]_]+") # exclude @ automatically
  all_mentions[which(test$is_retweet)] <- vector(mode = "list", length = sum(test$is_retweet)) # add NULL to retweets

  test <- test %>% mutate(
    mentions_clean = all_mentions,
    interacts = pmap(
      list(quoted_user_id, retweeted_user_id, replied_user_id, mentions_clean),
      ~ c(...) %>% discard(is.na)
    )
  )

  # first two cols are from to, rest edge attributes
  edges <- test %>%
    unchop(interacts) %>%
    select(from = user_id, to = interacts, created_at, is_retweet, is_mentioning, is_quote, is_reply)

  nodes <- edges %>%
    count(from, sort = TRUE) %>%
    select(user = from, n_interactions = n)
  # full_join(test %>% select(interacts), by = c("user" = "interacts"))
  # add interacts outside of dataset to node list

  # vertices = nodes (node attributes)
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
      subgroup_louvain = group_louvain()#,
      #bridging_score = node_bridging_score()
      # group_edge_betweenness() ???
    ) %>%
    as_tibble() %>%
    rename(user_id = name)

  d <- d %>% left_join(graph_stats, by = "user_id")

  return(d)
}


#### MASTER FUNCTIONS ####

#
clean_data <- function(dat) {
  # select vars, reduce payload
  # check dupes
  # check relevant vars
  # etc.

  return(TRUE)
}


add_transaction_variables <- function(d, hashtag_list) {
  ### Sample relevant hashtags
  twlz_hashtags <- hashtag_list %>%
    filter(is_twlz) %>%
    pull(hashtag)

  # LK FIXED: there are *6* TWLZ hashtags!!
  # > twlz_hashtags
  # [1] "#tlz"                 "#lehrerzimmer"        "#twitterkollegium"    "#twitterlehrerzimmer"
  # [5] "#twitterlz"           "#twlz"

  chats <- hashtag_list %>%
    filter(is_chat) %>%
    pull(hashtag)
  # > chats
  # [1] "#edchatde"

  ### FIXME:
  # @CB Conversations are already tagged!
  # chat have is_chat
  # twlz tweets + replies all have is_twlz

  ## these are based on:
  # filter(str_detect(sample_hashtags_str, "#edchatde"))
  # filter(str_detect(sample_hashtags_str, "#tlz|#lehrerzimmer|#twitterkollegium|#twitterlehrerzimmer|#twitterlz|#twlz"))
  # which seems to be the fastest way to lookup hashtags as of now

  # sample_hashtags_str = sample_hashtags %>% map_chr(paste, collapse = " ")
  # sample_hashtags only includes hashtags from all_hashtags (propagated) that are in our final sample to further reduce lookup times

  ## Conversations in Sample
  # > d %>% filter(!is_head) %>% nrow()
  # [1] 1074516 (replies/quotes in conversations, i.e,, not original)

  # Step 1: Get unique conversation IDs associated with sampled hashtags
  # whitelist <- d %>%
  #   filter(hashtags %>% map_lgl(match2vecs)) %>%
  #   pull(conversation_id) %>%
  #   unique()

  # Step 2: Filter these conversation IDs
  # out <- d %>%
  #   filter(conversation_id %in% whitelist)

  # Step 3: Tag communities
  # FIXME: redundant, see notes above
  # out["is_twlz"] <- out$hashtags %>% map_lgl(~ match2vecs(., v2 = twlz_hashtags))
  # out["is_edchatde"] <- out$hashtags %>% map_lgl(~ match2vecs(., v2 = chats))


  d <- d %>%
    mutate(
      is_edchatde = is_chat,
      # community = ifelse(is_twlz & is_edchatde, "both", ifelse(is_twlz, "twlz", ifelse(is_edchatde, "edchatde", "neither"))))
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
  d$user_switched[which(d$is_edchatde_member)] <- d$twlz_exit[which(d$is_edchatde_member)] >= d$edchatde_exit[which(d$is_edchatde_member)]
  d$user_switched[is.na(d$user_switched) & d$is_edchatde_member] <- FALSE
  d["user_switch_time"] <- NA
  d$user_switch_time[which(d$user_switched)] <- d$edchatde_exit[which(d$user_switched)]
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

  all_mentions <- str_extract_all(d$text, "(?<=@)[[:alnum:]_]+") # exclude @ automatically
  # overwite retweets with empty list
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
    # mutate(mentions = all_mentions) %>%
    select(status_id, created_at, matches("user_id"), mentions, is_twlz, is_edchatde) %>%
    mutate(interacts = map2(quoted_user_id, retweeted_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, replied_user_id, concat_na_omit)) %>%
    mutate(interacts = map2(interacts, mentions, concat_na_omit)) %>%
    select(status_id, created_at, user_id, created_at, interacts, is_twlz, is_edchatde) %>%
    unchop(interacts) %>%
    mutate(interacts = interacts %>% str_replace_all("@", ""))
  # %>%
  # as_tbl_graph()

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
    # out$twlz_member_group[out$created_at < out$twlz_entry] <- NA # mask group before it occurs
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
    # out$edchatde_member_group[out$created_at < out$edchatde_entry] <- NA # mask group before it occurs
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


#### ANALYSIS FUNCTIONS ####

get_descriptive_statistics <- function(d) {

  # d <- targets::tar_read(d_modeling)

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
  length(base::intersect(edchat_users, twlz_users)) / length(edchat_users)

  length(base::intersect(twlz_users, edchat_users))
  length(twlz_users)
  length(base::intersect(edchat_users, twlz_users)) / length(twlz_users)

  return(TRUE)
}

sigmoid_curve <- function(d) {

  # Step 3: Plot time series
  twlz_entries %>%
    sample_n(1000) %>% # takes too long to plot all
    arrange(n_members) %>%
    ggplot(aes(x = twlz_entry, y = n_members_perc)) +
    geom_point()

  edchatde_entries %>%
    sample_n(1000) %>% # takes too long to plot all
    arrange(n_members) %>%
    ggplot(aes(x = edchatde_entry, y = n_members_perc)) +
    geom_point()
}

mosaic <- function(d) {
  d_user <- d %>%
    distinct(user_id, .keep_all = TRUE)

  xtabs(~ twlz_member_group + edchatde_member_group, d_user) %>%
    mosaicplot(shade = TRUE)

  xtabs(~ twlz_member_group + edchatde_member_group, d_user)

  chisq.test(d_user$twlz_member_group, d_user$edchatde_member_group)
}

explanatory_model_plot <- function(d) {
  d_user <- d %>%
    distinct(user_id, .keep_all = TRUE) %>%
    filter(!is.na(user_switched))

  # Standardize time to unix time, then Z standardize
  d_user["user_switch_time_standard"] <- d_user$user_switch_time %>%
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC") %>%
    as.numeric() %>%
    scale()

  # d_user %>%
  #  ggplot(aes(edchatde_member_group, user_switch_time_standard, color=user_lifespan_days)) +
  #    geom_point()

  d_user["above_median_lifespan"] <- ifelse(d_user$user_lifespan_days >= median(d_user$user_lifespan_days), "high", "low")
  d_user %>%
    ggplot(aes(factor(edchatde_member_group, level = c("innovators", "early adopters", "early majority", "late majority", "laggards")), user_switch_time_standard, fill = above_median_lifespan)) +
    geom_boxplot()
}

binary_switch_modeling <- function(d) {
  # d=tar_read(d_tagged_switch)

  d_user <- d %>%
    distinct(user_id, .keep_all = TRUE) %>%
    filter(!is.na(user_switched))

  var_space <- c(
    "user_switched",
    "edchatde_member_group",
    "user_total_tweet_count",
    "n_interactions_edchat",
    "user_lifespan_days",
    "user_following_count",
    "user_followers_count"
  )

  d_model <- d_user %>%
    select(!!var_space) %>%
    drop_na()

  # Standardize predictors to ease interpretation
  d_model <- d_model %>%
    mutate_if(is.numeric, scale)

  m0 <- glm(user_switched ~ 1, d_model, family = "binomial")

  m1 <- glm(user_switched ~ edchatde_member_group, d_model, family = "binomial")

  m2 <- glm(user_switched ~ edchatde_member_group + user_total_tweet_count + n_interactions_edchat, d_model, family = "binomial")

  m3 <- glm(user_switched ~ edchatde_member_group + user_total_tweet_count + n_interactions_edchat + user_lifespan_days + user_following_count + user_followers_count, d_model, family = "binomial")

  anova(m0, m1, m2, m3, test = "Chisq")

  summary(m3)

  sjPlot::tab_model(m3)

  m4 <- glm(user_switched ~ edchatde_member_group * user_lifespan_days + user_total_tweet_count + n_interactions_edchat + user_lifespan_days + user_following_count + user_followers_count, d_model, family = "binomial")

  anova(m3, m4, test = "Chisq")

  summary(m4)

  sjPlot::tab_model(m4)

  # Robustness
  brant::brant(m4) # Fails: Hayden to look for more?

  plot(m4)

  plot(m3)

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

time_point_inference <- function(d) {

  # d=tar_read(d_tagged_switch)

  d_user <- d %>%
    distinct(user_id, .keep_all = TRUE) %>%
    filter(!is.na(user_switch_time))

  var_space <- c(
    "user_switch_time",
    "edchatde_member_group",
    "user_total_tweet_count",
    "n_interactions_edchat",
    "user_lifespan_days",
    "user_following_count",
    "user_followers_count"
  )

  d_model <- d_user %>%
    select(!!var_space) %>%
    drop_na()

  # Standardize predictors to ease interpretation
  d_model <- d_model %>%
    mutate_if(is.numeric, scale)

  # Standardize time to unix time, then Z standardize
  d_model["user_switch_time_standard"] <- d_model$user_switch_time %>%
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC") %>%
    as.numeric() %>%
    scale()

  m0 <- lm(user_switch_time_standard ~ 1, d_model)

  m1 <- lm(user_switch_time_standard ~ edchatde_member_group, d_model)

  m2 <- lm(user_switch_time_standard ~ edchatde_member_group + user_total_tweet_count + n_interactions_edchat, d_model)

  m3 <- lm(user_switch_time_standard ~ edchatde_member_group + user_total_tweet_count + n_interactions_edchat + user_lifespan_days + user_following_count + user_followers_count, d_model)

  anova(m0, m1, m2, m3, test = "Chisq")

  summary(m3)

  sjPlot::tab_model(m3)

  af <- anova(m3)
  afss <- af$"Sum Sq"
  print(cbind(af, PctExp = round(afss / sum(afss) * 100, 2)))

  m4 <- lm(user_switch_time_standard ~ edchatde_member_group * user_lifespan_days + user_total_tweet_count + n_interactions_edchat + user_lifespan_days + user_following_count + user_followers_count, d_model)

  anova(m3, m4, test = "Chisq")

  summary(m4)

  sjPlot::tab_model(m4)

  af <- anova(m4)
  afss <- af$"Sum Sq"
  print(cbind(af, PctExp = round(afss / sum(afss) * 100, 2)))

  plot(m4)

  return(d)
}
