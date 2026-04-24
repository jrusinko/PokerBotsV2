# MadeForTV.R
#
# Purpose:
#   Experimental utilities for identifying tournament hands that are
#   especially replay-worthy. These helpers operate on the existing
#   tournament replay / hand_log structure without changing engine logic.

`%||%` <- get0("%||%", ifnotfound = function(x, y) if (is.null(x)) y else x)

nz_scalar_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
  as.character(x[1])
}

nz_scalar_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
  as.numeric(x[1])
}

winner_ids_from_hand <- function(hand) {
  winners <- hand$winners %||% list()
  starters <- hand$starting_stack_summary %||% list()
  enders <- hand$ending_stack_summary %||% hand$stack_summary %||% list()

  seat_to_pid <- setNames(
    vapply(c(starters, enders), function(p) nz_scalar_chr(p$player_id, ""), character(1)),
    vapply(c(starters, enders), function(p) nz_scalar_chr(p$seat, NA_character_), character(1))
  )

  out <- character(0)
  for (w in winners) {
    if (identical(nz_scalar_chr(w$win_type, ""), "win_by_folds")) {
      out <- c(out, nz_scalar_chr(w$player_id, ""))
    }
    if (identical(nz_scalar_chr(w$win_type, ""), "showdown_pot_award")) {
      seats <- as.character(w$winner_seats %||% integer(0))
      out <- c(out, unname(seat_to_pid[seats]))
    }
  }

  unique(out[nzchar(out)])
}

all_previous_winner_ids <- function(replay, hand_index) {
  if (hand_index <= 1L) {
    return(character(0))
  }

  unique(unlist(
    lapply(replay$hand_log[seq_len(hand_index - 1L)], winner_ids_from_hand),
    use.names = FALSE
  ))
}

compute_total_pot_awarded <- function(hand) {
  winners <- hand$winners %||% list()
  total <- 0

  for (w in winners) {
    if (identical(nz_scalar_chr(w$win_type, ""), "win_by_folds")) {
      total <- total + nz_scalar_num(w$amount, 0)
    }
    if (identical(nz_scalar_chr(w$win_type, ""), "showdown_pot_award")) {
      total <- total + nz_scalar_num(w$pot_amount, 0)
    }
  }

  total
}

compute_chip_leader_change <- function(hand) {
  starters <- hand$starting_stack_summary %||% list()
  enders <- hand$ending_stack_summary %||% hand$stack_summary %||% list()

  if (length(starters) == 0 || length(enders) == 0) {
    return(list(changed = FALSE, start_leader_id = NA_character_, end_leader_id = NA_character_))
  }

  start_stack <- setNames(
    vapply(starters, function(p) nz_scalar_num(p$stack, 0), numeric(1)),
    vapply(starters, function(p) nz_scalar_chr(p$player_id, ""), character(1))
  )
  end_stack <- setNames(
    vapply(enders, function(p) nz_scalar_num(p$stack, 0), numeric(1)),
    vapply(enders, function(p) nz_scalar_chr(p$player_id, ""), character(1))
  )

  start_leader_id <- names(which.max(start_stack))[1]
  end_leader_id <- names(which.max(end_stack))[1]

  list(
    changed = !is.na(start_leader_id) && !is.na(end_leader_id) && !identical(start_leader_id, end_leader_id),
    start_leader_id = start_leader_id,
    end_leader_id = end_leader_id
  )
}

compute_short_stack_double_ups <- function(hand, short_stack_bb = 10, double_up_factor = 2) {
  starters <- hand$starting_stack_summary %||% list()
  enders <- hand$ending_stack_summary %||% hand$stack_summary %||% list()
  bb <- nz_scalar_num(hand$big_blind, NA_real_)

  if (length(starters) == 0 || length(enders) == 0 || is.na(bb) || bb <= 0) {
    return(character(0))
  }

  start_stack <- setNames(
    vapply(starters, function(p) nz_scalar_num(p$stack, 0), numeric(1)),
    vapply(starters, function(p) nz_scalar_chr(p$player_id, ""), character(1))
  )
  end_stack <- setNames(
    vapply(enders, function(p) nz_scalar_num(p$stack, 0), numeric(1)),
    vapply(enders, function(p) nz_scalar_chr(p$player_id, ""), character(1))
  )

  short_ids <- names(start_stack)[start_stack <= short_stack_bb * bb]
  short_ids[
    short_ids %in% names(end_stack) &
      end_stack[short_ids] >= double_up_factor * start_stack[short_ids] &
      end_stack[short_ids] > start_stack[short_ids]
  ]
}

classify_showdown_strengths <- function(hand, strong_hand_category_min = 4L) {
  showdown <- hand$showdown_summary
  if (is.null(showdown) || length(showdown$hands %||% list()) == 0) {
    return(list(
      showdown_player_ids = character(0),
      showdown_winner_ids = character(0),
      strong_losing_hand_ids = character(0),
      strongest_losing_hand_category = NA_integer_
    ))
  }

  if (!exists("card_labels_to_df", mode = "function") ||
      !exists("holdem_hand_value", mode = "function") ||
      !exists("hand_category_code_5", mode = "function")) {
    return(list(
      showdown_player_ids = unique(vapply(showdown$hands, function(h) nz_scalar_chr(h$player_id, ""), character(1))),
      showdown_winner_ids = winner_ids_from_hand(hand),
      strong_losing_hand_ids = character(0),
      strongest_losing_hand_category = NA_integer_
    ))
  }

  shown_hands <- showdown$hands
  showdown_player_ids <- unique(vapply(shown_hands, function(h) nz_scalar_chr(h$player_id, ""), character(1)))
  showdown_winner_ids <- winner_ids_from_hand(hand)

  strong_losing_hand_ids <- character(0)
  strongest_losing_hand_category <- NA_integer_

  for (h in shown_hands) {
    pid <- nz_scalar_chr(h$player_id, "")
    if (!nzchar(pid) || pid %in% showdown_winner_ids) next

    hv <- holdem_hand_value(
      hole_df = card_labels_to_df(h$hole_cards %||% character(0)),
      board_df = card_labels_to_df(h$board %||% character(0))
    )
    cat_code <- hand_category_code_5(hv$best_hand)

    if (!is.na(cat_code) && cat_code >= strong_hand_category_min) {
      strong_losing_hand_ids <- c(strong_losing_hand_ids, pid)
      if (is.na(strongest_losing_hand_category)) {
        strongest_losing_hand_category <- cat_code
      } else {
        strongest_losing_hand_category <- max(strongest_losing_hand_category, cat_code)
      }
    }
  }

  list(
    showdown_player_ids = showdown_player_ids[nzchar(showdown_player_ids)],
    showdown_winner_ids = showdown_winner_ids[nzchar(showdown_winner_ids)],
    strong_losing_hand_ids = unique(strong_losing_hand_ids),
    strongest_losing_hand_category = strongest_losing_hand_category
  )
}

estimate_low_equity_winner <- function(hand,
                                       equity_sims = 2000,
                                       low_equity_threshold = 0.35,
                                       include_equity = TRUE) {
  showdown <- hand$showdown_summary
  action_history <- hand$action_history %||% list()

  if (is.null(showdown) || length(showdown$hands %||% list()) < 2) {
    return(list(
      low_equity_winner = FALSE,
      lowest_winner_equity = NA_real_,
      equity_context = "no_showdown"
    ))
  }

  all_in_actions <- action_history[
    vapply(action_history, function(a) grepl("all_in", nz_scalar_chr(a$type, ""), fixed = TRUE), logical(1))
  ]
  all_in_streets <- unique(vapply(all_in_actions, function(a) nz_scalar_chr(a$street, ""), character(1)))
  preflop_all_in_showdown <- length(all_in_actions) > 0 &&
    length(all_in_streets) == 1 &&
    identical(all_in_streets, "preflop")

  if (!isTRUE(include_equity)) {
    return(list(
      low_equity_winner = FALSE,
      lowest_winner_equity = NA_real_,
      equity_context = "equity_disabled"
    ))
  }

  if (!preflop_all_in_showdown ||
      !exists("holdem_equity_mc_fast", mode = "function") ||
      !exists("card_labels_to_df", mode = "function")) {
    return(list(
      low_equity_winner = FALSE,
      lowest_winner_equity = NA_real_,
      equity_context = if (preflop_all_in_showdown) "missing_equity_tools" else "not_preflop_all_in"
    ))
  }

  shown_hands <- showdown$hands
  hole_list <- lapply(shown_hands, function(h) card_labels_to_df(h$hole_cards %||% character(0)))
  eq <- tryCatch(
    holdem_equity_mc_fast(hole_list = hole_list, n_sims = equity_sims),
    error = function(e) NULL
  )

  if (is.null(eq) || is.null(eq$equity)) {
    return(list(
      low_equity_winner = FALSE,
      lowest_winner_equity = NA_real_,
      equity_context = "equity_failed"
    ))
  }

  shown_ids <- vapply(shown_hands, function(h) nz_scalar_chr(h$player_id, ""), character(1))
  eq_by_pid <- setNames(as.numeric(eq$equity), shown_ids)
  winner_eq <- unname(eq_by_pid[intersect(names(eq_by_pid), winner_ids_from_hand(hand))])

  if (length(winner_eq) == 0) {
    return(list(
      low_equity_winner = FALSE,
      lowest_winner_equity = NA_real_,
      equity_context = "winner_not_in_equity_set"
    ))
  }

  lowest_winner_equity <- min(winner_eq, na.rm = TRUE)

  list(
    low_equity_winner = is.finite(lowest_winner_equity) && lowest_winner_equity < low_equity_threshold,
    lowest_winner_equity = lowest_winner_equity,
    equity_context = "preflop_all_in"
  )
}

extract_hand_features <- function(replay, hand_index,
                                  short_stack_bb = 10,
                                  double_up_factor = 2,
                                  strong_hand_category_min = 4L,
                                  equity_sims = 2000,
                                  low_equity_threshold = 0.35,
                                  include_equity = TRUE) {
  if (is.null(replay$hand_log) || length(replay$hand_log) == 0) {
    stop("replay$hand_log is empty.")
  }
  if (!is.numeric(hand_index) || length(hand_index) != 1 || is.na(hand_index)) {
    stop("hand_index must be a single numeric value.")
  }
  hand_index <- as.integer(hand_index)
  if (hand_index < 1L || hand_index > length(replay$hand_log)) {
    stop("hand_index out of range.")
  }

  hand <- replay$hand_log[[hand_index]]
  starters <- hand$starting_stack_summary %||% list()
  action_history <- hand$action_history %||% list()
  showdown <- hand$showdown_summary
  eliminations_this_hand <- hand$eliminations_this_hand %||% character(0)

  chip_leader <- compute_chip_leader_change(hand)
  short_stack_double_up_ids <- compute_short_stack_double_ups(
    hand = hand,
    short_stack_bb = short_stack_bb,
    double_up_factor = double_up_factor
  )
  showdown_strengths <- classify_showdown_strengths(
    hand = hand,
    strong_hand_category_min = strong_hand_category_min
  )
  low_equity <- estimate_low_equity_winner(
    hand = hand,
    equity_sims = equity_sims,
    low_equity_threshold = low_equity_threshold,
    include_equity = include_equity
  )

  current_winner_ids <- winner_ids_from_hand(hand)
  previous_winner_ids <- all_previous_winner_ids(replay, hand_index)
  first_time_winner_ids <- setdiff(current_winner_ids, previous_winner_ids)

  active_starters <- starters[
    vapply(starters, function(p) !identical(nz_scalar_chr(p$status, "active"), "eliminated"), logical(1))
  ]

  all_in_actions <- action_history[
    vapply(action_history, function(a) grepl("all_in", nz_scalar_chr(a$type, ""), fixed = TRUE), logical(1))
  ]
  all_in_streets <- unique(vapply(all_in_actions, function(a) nz_scalar_chr(a$street, ""), character(1)))
  preflop_all_in_showdown <- length(all_in_actions) > 0 &&
    length(all_in_streets) == 1 &&
    identical(all_in_streets, "preflop") &&
    !is.null(showdown)

  data.frame(
    hand_index = hand_index,
    hand_number = as.integer(hand$hand_number %||% hand_index),
    hand_id = nz_scalar_chr(hand$hand_id, ""),
    level = as.integer(hand$level %||% NA_integer_),
    small_blind = nz_scalar_num(hand$small_blind, NA_real_),
    big_blind = nz_scalar_num(hand$big_blind, NA_real_),
    ante = nz_scalar_num(hand$ante, NA_real_),
    players_remaining_start = length(active_starters),
    action_count = length(action_history),
    all_in_count = length(all_in_actions),
    preflop_all_in_showdown = preflop_all_in_showdown,
    reached_showdown = !is.null(showdown),
    showdown_players = length(showdown_strengths$showdown_player_ids),
    multiway_showdown = length(showdown_strengths$showdown_player_ids) >= 3L,
    total_pot_awarded = compute_total_pot_awarded(hand),
    elimination_count = length(eliminations_this_hand),
    had_elimination = length(eliminations_this_hand) > 0L,
    chip_leader_changed = isTRUE(chip_leader$changed),
    first_time_winner_count = length(first_time_winner_ids),
    had_first_time_winner = length(first_time_winner_ids) > 0L,
    short_stack_double_up_count = length(short_stack_double_up_ids),
    had_short_stack_double_up = length(short_stack_double_up_ids) > 0L,
    strong_losing_hand_count = length(showdown_strengths$strong_losing_hand_ids),
    had_strong_hand_lose = length(showdown_strengths$strong_losing_hand_ids) > 0L,
    strongest_losing_hand_category = if (is.na(showdown_strengths$strongest_losing_hand_category)) NA_integer_ else as.integer(showdown_strengths$strongest_losing_hand_category),
    low_equity_winner = isTRUE(low_equity$low_equity_winner),
    lowest_winner_equity = as.numeric(low_equity$lowest_winner_equity),
    equity_context = nz_scalar_chr(low_equity$equity_context, ""),
    stringsAsFactors = FALSE
  )
}

extract_all_hand_features <- function(replay, ...) {
  if (is.null(replay$hand_log) || length(replay$hand_log) == 0) {
    return(data.frame())
  }

  out <- lapply(seq_along(replay$hand_log), function(i) {
    extract_hand_features(replay, i, ...)
  })

  do.call(rbind, out)
}

score_hand_interest <- function(features,
                                elimination_score = 10,
                                stage_weight = TRUE) {
  if (is.data.frame(features)) {
    if (nrow(features) != 1) {
      stop("score_hand_interest() expects a single-row data.frame or a named list.")
    }
    features <- as.list(features[1, , drop = FALSE])
  }

  score <- 1
  reasons <- character(0)
  players_remaining <- suppressWarnings(as.numeric(features$players_remaining_start))
  all_in_count <- suppressWarnings(as.numeric(features$all_in_count))
  total_pot_awarded <- suppressWarnings(as.numeric(features$total_pot_awarded))
  big_blind <- suppressWarnings(as.numeric(features$big_blind))
  lowest_winner_equity <- suppressWarnings(as.numeric(features$lowest_winner_equity))
  strongest_losing_hand_category <- suppressWarnings(as.numeric(features$strongest_losing_hand_category))
  showdown_players <- suppressWarnings(as.numeric(features$showdown_players))
  elimination_count <- suppressWarnings(as.numeric(features$elimination_count))
  first_time_winner_count <- suppressWarnings(as.numeric(features$first_time_winner_count))
  short_stack_double_up_count <- suppressWarnings(as.numeric(features$short_stack_double_up_count))

  if (isTRUE(features$had_elimination)) {
    if (!is.finite(elimination_count)) {
      elimination_count <- 1
    }
    elimination_bonus <- min(7, 4 + 2 * elimination_count)
    score <- max(score, elimination_bonus)
    if (elimination_count >= 2) {
      score <- max(score, elimination_score)
    }
    reasons <- c(reasons, "elimination")
  }

  if (isTRUE(features$chip_leader_changed)) {
    score <- score + 1.5
    reasons <- c(reasons, "chip leader changed")
  }

  if (isTRUE(features$had_first_time_winner)) {
    if (!is.finite(first_time_winner_count)) {
      first_time_winner_count <- 1
    }
    score <- score + min(1.5, 0.75 * first_time_winner_count)
    reasons <- c(reasons, "first-time winner")
  }

  if (isTRUE(features$had_short_stack_double_up)) {
    if (!is.finite(short_stack_double_up_count)) {
      short_stack_double_up_count <- 1
    }
    score <- score + min(2.5, 1.5 + 0.5 * (short_stack_double_up_count - 1))
    reasons <- c(reasons, "short-stack double-up")
  }

  if (isTRUE(features$low_equity_winner)) {
    if (is.finite(lowest_winner_equity)) {
      equity_bonus <- if (lowest_winner_equity < 0.20) {
        4
      } else if (lowest_winner_equity < 0.30) {
        3
      } else {
        2
      }
    } else {
      equity_bonus <- 2
    }
    score <- score + equity_bonus
    reasons <- c(reasons, "low-equity winner")
  }

  if (isTRUE(features$had_strong_hand_lose)) {
    hand_loss_bonus <- if (!is.finite(strongest_losing_hand_category)) {
      2
    } else if (strongest_losing_hand_category >= 6) {
      4
    } else if (strongest_losing_hand_category >= 5) {
      3
    } else {
      2
    }
    score <- score + hand_loss_bonus
    reasons <- c(reasons, "strong hand lost at showdown")
  }

  if (isTRUE(features$multiway_showdown)) {
    if (!is.finite(showdown_players)) {
      showdown_players <- 3
    }
    score <- score + min(2, 0.5 + 0.5 * (showdown_players - 2))
    reasons <- c(reasons, "multiway showdown")
  }

  if (is.finite(all_in_count)) {
    bonus <- min(2.5, 0.75 * all_in_count)
    if (bonus > 0) {
      score <- score + bonus
      reasons <- c(reasons, "all-in action")
    }
  }

  if (is.finite(total_pot_awarded) && is.finite(big_blind) && big_blind > 0) {
    pot_in_bb <- total_pot_awarded / big_blind
    pot_bonus <- if (pot_in_bb >= 120) {
      2.5
    } else if (pot_in_bb >= 80) {
      2
    } else if (pot_in_bb >= 40) {
      1.25
    } else if (pot_in_bb >= 20) {
      0.5
    } else {
      0
    }

    if (pot_bonus > 0) {
      score <- score + pot_bonus
      reasons <- c(reasons, "big pot")
    }
  }

  if (isTRUE(stage_weight) && is.finite(players_remaining) && players_remaining > 0) {
    stage_bonus <- if (players_remaining <= 3) {
      1.5
    } else if (players_remaining <= 5) {
      0.75
    } else {
      0
    }

    if (stage_bonus > 0) {
      score <- score + stage_bonus
      reasons <- c(reasons, "late tournament")
    }
  }

  score <- max(1, min(10, round(score * 2) / 2))

  list(
    score = score,
    reasons = unique(reasons)
  )
}

rank_interesting_hands <- function(replay, ...) {
  features <- extract_all_hand_features(replay, ...)
  if (nrow(features) == 0) {
    return(data.frame())
  }

  scored <- lapply(seq_len(nrow(features)), function(i) {
    s <- score_hand_interest(features[i, , drop = FALSE])
    data.frame(
      hand_index = features$hand_index[i],
      hand_number = features$hand_number[i],
      interest_score = s$score,
      interest_reasons = paste(s$reasons, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })

  score_df <- do.call(rbind, scored)
  out <- cbind(score_df, features[, setdiff(names(features), c("hand_index", "hand_number")), drop = FALSE])
  out <- out[order(-out$interest_score, -out$total_pot_awarded, out$hand_number), , drop = FALSE]
  rownames(out) <- NULL
  out
}

annotate_replay_for_tv <- function(replay, tv_threshold = 3, ...) {
  if (is.null(replay$hand_log) || length(replay$hand_log) == 0) {
    return(replay)
  }

  ranked <- rank_interesting_hands(replay, ...)
  if (nrow(ranked) == 0) {
    return(replay)
  }

  score_by_hand <- setNames(ranked$interest_score, as.character(ranked$hand_index))
  reasons_by_hand <- setNames(ranked$interest_reasons, as.character(ranked$hand_index))

  for (i in seq_along(replay$hand_log)) {
    replay$hand_log[[i]]$interest_score <- as.numeric(score_by_hand[as.character(i)] %||% NA_real_)
    replay$hand_log[[i]]$interest_reasons <- as.character(reasons_by_hand[as.character(i)] %||% "")
    replay$hand_log[[i]]$for_tv <- is.finite(replay$hand_log[[i]]$interest_score) &&
      replay$hand_log[[i]]$interest_score >= tv_threshold
  }

  replay
}

simulate_interest_scores <- function(n_tournaments = 10,
                                     tournament_fun = NULL,
                                     tournament_args = list(),
                                     seed = NA_integer_,
                                     progress = TRUE,
                                     ...) {
  if (!is.numeric(n_tournaments) || length(n_tournaments) != 1 || is.na(n_tournaments) || n_tournaments < 1) {
    stop("n_tournaments must be a positive integer.")
  }
  n_tournaments <- as.integer(n_tournaments)

  if (is.null(tournament_fun)) {
    if (!exists("run_tournament", mode = "function")) {
      stop("Default tournament function `run_tournament()` was not found.")
    }
    tournament_fun <- get("run_tournament", mode = "function")
  }

  if (!is.function(tournament_fun)) {
    stop("tournament_fun must be a function.")
  }

  if (!is.list(tournament_args)) {
    stop("tournament_args must be a list of arguments passed to tournament_fun.")
  }

  out <- vector("list", n_tournaments)

  for (i in seq_len(n_tournaments)) {
    if (!is.na(seed)) {
      tournament_args$rng_seed <- as.integer(seed) + i - 1L
    }

    if (isTRUE(progress)) {
      cat(sprintf("Running tournament %d of %d...\n", i, n_tournaments))
    }

    replay <- do.call(tournament_fun, tournament_args)
    ranked <- rank_interesting_hands(replay, ...)

    if (nrow(ranked) == 0) {
      out[[i]] <- data.frame(
        tournament_run = integer(0),
        tournament_id = character(0),
        hand_number = integer(0),
        interest_score = numeric(0),
        interest_reasons = character(0),
        stringsAsFactors = FALSE
      )
      next
    }

    ranked$tournament_run <- i
    ranked$tournament_id <- nz_scalar_chr(replay$tournament_id, paste0("tournament_", i))

    front_cols <- c("tournament_run", "tournament_id", "hand_index", "hand_number", "interest_score", "interest_reasons")
    ranked <- ranked[, c(front_cols, setdiff(names(ranked), front_cols)), drop = FALSE]
    out[[i]] <- ranked
  }

  combined <- do.call(rbind, out)
  rownames(combined) <- NULL
  combined
}

plot_interest_score_histogram <- function(x,
                                          breaks = seq(0.5, 10.5, by = 1),
                                          main = "Histogram of Hand Interest Scores",
                                          xlab = "Interest score",
                                          col = "#2f8f57",
                                          border = "white",
                                          ...) {
  if (is.data.frame(x)) {
    if (!("interest_score" %in% names(x))) {
      stop("If x is a data.frame, it must contain an `interest_score` column.")
    }
    scores <- as.numeric(x$interest_score)
  } else {
    scores <- as.numeric(x)
  }

  scores <- scores[is.finite(scores)]

  if (length(scores) == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No interest scores available.")
    return(invisible(scores))
  }

  graphics::hist(
    scores,
    breaks = breaks,
    main = main,
    xlab = xlab,
    col = col,
    border = border,
    xaxt = "n",
    ...
  )
  graphics::axis(1, at = 1:10)
  graphics::grid(col = "gray85", lty = "dotted")

  invisible(scores)
}
