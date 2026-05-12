############################################################
# Mathematics of Poker — Demo and Testing Script
# File: poker_demos.R
#
# Purpose:
#   Central home for manual demos, smoke tests, and example runs.
#   This file is safe to source: no demos or loaders run automatically.
#   Call the functions below explicitly when you want test behavior.
############################################################

ensure_demo_dependencies_loaded <- function(verbose = FALSE) {
  if (!exists("poker_load_all", mode = "function")) {
    source("poker_load_all.R")
  }

  if (!exists("initialize_tournament", mode = "function")) {
    poker_load_all(include_demos = FALSE, verbose = verbose)
  }
}

demo_cards_and_hands_holdem <- function(n_players = 2) {
  ensure_demo_dependencies_loaded()
  showdown <- play_holdem_hand(n_players = n_players)
  print_holdem_hand(showdown)
  invisible(showdown)
}


demo_engine_hand_setup <- function() {
  ensure_demo_dependencies_loaded()
  bot_fns <- list(
    "Random Bot 1" = random_bot,
    "Random Bot 2" = random_bot,
    "Random Bot 3" = random_bot
  )

  tourn <- initialize_tournament(
    bot_fns = bot_fns,
    starting_stack = 10000,
    player_names = names(bot_fns)
  )

  tourn <- initialize_hand(tourn)
  tourn <- post_blinds_and_antes(tourn)
  tourn
}

demo_engine_single_hand <- function() {
  ensure_demo_dependencies_loaded()
  bot_fns <- list(
    "Random Bot 1" = random_bot,
    "Random Bot 2" = random_bot,
    "Random Bot 3" = random_bot
  )

  tourn <- initialize_tournament(
    bot_fns = bot_fns,
    starting_stack = 10000,
    player_names = names(bot_fns)
  )

  play_current_hand(tourn)
}

demo_equity_holdem <- function(n_sims = 5000) {
  ensure_demo_dependencies_loaded()
  hole_list <- list(
    data.frame(rank = c("A", "A"), suit = c("h", "s"), card = c("Ah", "As"), stringsAsFactors = FALSE),
    data.frame(rank = c("K", "K"), suit = c("h", "s"), card = c("Kh", "Ks"), stringsAsFactors = FALSE)
  )

  holdem_equity_mc_fast(hole_list = hole_list, n_sims = n_sims)
}
format_card_vec <- function(cards) {
  if (length(cards) == 0) return("(none)")
  paste(cards, collapse = " ")
}

print_demo_player_stacks <- function(tournament_state) {
  cat("\nSTACKS:\n")
  for (p in tournament_state$players) {
    if (inherits(p, "player_state")) {
      status_text <- if (isTRUE(p$folded)) " [folded]" else if (isTRUE(p$all_in)) " [all-in]" else ""
      cat(sprintf(
        "Seat %d | %s | Stack: %s | In pot this street: %s | In pot this hand: %s%s\n",
        p$seat, p$name, p$stack, p$committed_this_round, p$committed_this_hand, status_text
      ))
    }
  }
}

print_demo_board <- function(board) {
  cat("\nBOARD:\n")
  if (length(board) == 0) {
    cat("(preflop: no board cards yet)\n")
    return(invisible(NULL))
  }

  labels <- c("Flop 1", "Flop 2", "Flop 3", "Turn", "River")[seq_along(board)]
  for (i in seq_along(board)) {
    cat(sprintf("%-6s: %s\n", labels[i], board[i]))
  }
}

print_demo_hole_cards <- function(tournament_state) {
  cat("\nPLAYERS:\n")
  for (p in tournament_state$players) {
    if (inherits(p, "player_state") && identical(p$status, "active")) {
      cat(sprintf(
        "Seat %d | %s | Hole cards: %s\n",
        p$seat, p$name, format_card_vec(p$hole_cards)
      ))
    }
  }
}

print_demo_last_action <- function(tournament_state) {
  hand_state <- tournament_state$current_hand
  hist <- hand_state$action_history

  if (length(hist) == 0) return(invisible(NULL))

  a <- hist[[length(hist)]]
  a_type <- a$type

  cat("\nACTION:\n")

  if (a_type %in% c("post_ante", "post_sb", "post_bb")) {
    cat(sprintf(
      "%s (Seat %d) %s %s\n",
      a$player_name, a$seat, gsub("_", " ", a_type), a$amount
    ))
  } else if (a_type %in% c("fold", "check")) {
    cat(sprintf("%s (Seat %d) %s\n", a$player_name, a$seat, a_type))
  } else if (a_type %in% c("call", "bet", "raise")) {
    cat(sprintf("%s (Seat %d) %s %s\n", a$player_name, a$seat, a_type, a$amount))
  } else if (a_type %in% c("all_in_bet", "all_in_raise", "all_in_call", "all_in_short")) {
    cat(sprintf("%s (Seat %d) %s %s\n", a$player_name, a$seat, gsub("_", " ", a_type), a$amount))
  } else if (a_type == "win_by_folds") {
    cat(sprintf(
      "Seat %d wins the pot uncontested for %s\n",
      a$winner_seat, a$amount
    ))
  } else if (a_type == "showdown_reveal") {
    cat("Showdown: hands revealed.\n")
  } else if (a_type == "showdown_pot_award") {
    winners <- paste(a$winner_seats, collapse = ", ")
    cat(sprintf(
      "Side/Main pot %d awarded. Amount: %s | Winner seat(s): %s\n",
      a$pot_index, a$pot_amount, winners
    ))
  } else {
    print(a)
  }

  cat(sprintf("Current pot: %s\n", hand_state$pot))
}

get_demo_bot_names <- function(bot_fns, player_names) {
  bot_names <- names(bot_fns)
  if (is.null(bot_names)) {
    bot_names <- rep("", length(bot_fns))
  }

  registered_names <- vapply(bot_fns, function(bot_fn) {
    if (!is.function(bot_fn)) return("")
    registered_name <- attr(bot_fn, "bot_name", exact = TRUE)
    if (is.null(registered_name)) "" else as.character(registered_name)[1]
  }, character(1))

  ifelse(
    nzchar(bot_names),
    bot_names,
    ifelse(nzchar(registered_names), registered_names, player_names)
  )
}

get_demo_remaining_players <- function(tournament_state, bot_fns, player_names) {
  active_idx <- which(vapply(
    tournament_state$players,
    function(p) inherits(p, "player_state") && identical(p$status, "active"),
    logical(1)
  ))

  bot_names <- get_demo_bot_names(bot_fns, player_names)

  remaining <- do.call(
    rbind,
    lapply(active_idx, function(idx) {
      p <- tournament_state$players[[idx]]
      data.frame(
        bot_index = idx,
        bot_name = bot_names[[idx]],
        player_id = p$player_id,
        name = p$name,
        seat = p$seat,
        stack = p$stack,
        stringsAsFactors = FALSE
      )
    })
  )

  if (is.null(remaining)) {
    remaining <- data.frame(
      bot_index = integer(0),
      bot_name = character(0),
      player_id = character(0),
      name = character(0),
      seat = integer(0),
      stack = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  remaining <- remaining[order(-remaining$stack, remaining$seat), ]
  rownames(remaining) <- NULL
  remaining
}

thin_demo_hand_log <- function(
    tournament_state,
    snapshot_mode = c("full", "key", "final", "none"),
    preserve_tv_hands = TRUE,
    tv_threshold = 3,
    major_stack_change_pct = 0.30,
    require_major_stack_change = TRUE
) {
  snapshot_mode <- match.arg(snapshot_mode)

  if (identical(snapshot_mode, "full")) {
    return(tournament_state)
  }

  if (isTRUE(preserve_tv_hands) &&
      exists("annotate_replay_for_tv", mode = "function") &&
      length(tournament_state$hand_log %||% list()) > 0) {
    has_tv_tags <- all(vapply(
      tournament_state$hand_log,
      function(hand) !is.null(hand$for_tv),
      logical(1)
    ))

    if (!has_tv_tags) {
      tournament_state <- tryCatch(
        annotate_replay_for_tv(
          tournament_state,
          tv_threshold = tv_threshold,
          major_stack_change_pct = major_stack_change_pct,
          require_major_stack_change = require_major_stack_change,
          include_equity = FALSE
        ),
        error = function(e) tournament_state
      )
    }
  }

  keep_snapshots <- function(snaps) {
    if (is.null(snaps) || length(snaps) == 0) {
      return(list())
    }

    if (identical(snapshot_mode, "none")) {
      return(list())
    }

    snap_types <- vapply(
      snaps,
      function(s) as.character(s$snapshot_type %||% "")[1],
      character(1)
    )

    if (identical(snapshot_mode, "final")) {
      keep_idx <- which(snap_types %in% c("showdown", "hand_end", "elimination"))
      if (length(keep_idx) == 0) {
        keep_idx <- length(snaps)
      }
      return(snaps[keep_idx])
    }

    keep_idx <- which(snap_types %in% c(
      "hand_start",
      "forced_bets",
      "street",
      "showdown",
      "hand_end",
      "elimination"
    ))

    if (length(keep_idx) == 0) {
      keep_idx <- unique(c(1L, length(snaps)))
    }

    snaps[keep_idx]
  }

  tournament_state$hand_log <- lapply(tournament_state$hand_log %||% list(), function(hand) {
    if (length(hand$eliminations_this_hand %||% character(0)) > 0) {
      hand$had_elimination <- TRUE
      hand$for_tv <- TRUE
      if (length(hand$action_history %||% list()) > 0) {
        hand$broadcast_action_history <- hand$action_history
      } else if (length(hand$broadcast_action_history %||% list()) > 0) {
        hand$action_history <- hand$broadcast_action_history
      }
      reasons <- trimws(as.character(hand$interest_reasons %||% ""))
      if (!grepl("elimination", reasons, fixed = TRUE)) {
        hand$interest_reasons <- if (nzchar(reasons)) paste(reasons, "elimination", sep = "; ") else "elimination"
      }
      if (is.na(as.numeric(hand$interest_score %||% NA_real_))) {
        hand$interest_score <- 10
      }
    }

    if (isTRUE(preserve_tv_hands) && isTRUE(hand$for_tv)) {
      if (length(hand$action_history %||% list()) > 0) {
        hand$broadcast_action_history <- hand$action_history
      } else if (length(hand$broadcast_action_history %||% list()) > 0) {
        hand$action_history <- hand$broadcast_action_history
      }
      return(hand)
    }

    hand$state_snapshots <- keep_snapshots(hand$state_snapshots %||% list())
    hand
  })

  if (!is.null(tournament_state$current_hand) && inherits(tournament_state$current_hand, "hand_state")) {
    current_hand_number <- tournament_state$current_hand$hand_number %||% NA_integer_
    current_hand_is_tv <- isTRUE(preserve_tv_hands) && any(vapply(
      tournament_state$hand_log %||% list(),
      function(hand) {
        isTRUE(hand$for_tv) &&
          identical(as.integer(hand$hand_number %||% NA_integer_), as.integer(current_hand_number))
      },
      logical(1)
    ))

    if (!current_hand_is_tv) {
      tournament_state$current_hand$state_snapshots <- keep_snapshots(tournament_state$current_hand$state_snapshots %||% list())
    }
  }

  tournament_state
}
demo_engine_single_hand_verbose <- function(
    bot_fns = list(random_bot, simple_preflop_strength_bot, always_call_bot),
    player_names = c("Bot A", "Bot B", "Bot C"),
    starting_stack = 1000,
    rng_seed = NA_integer_,
    pause_mode = c("none", "street", "action"),
    tournament_state = NULL
) {
  ensure_demo_dependencies_loaded()
  pause_mode <- match.arg(pause_mode)

  if (is.null(tournament_state)) {
    if (!is.na(rng_seed)) {
      set.seed(as.integer(rng_seed))
    }

    tourn <- initialize_tournament(
      bot_fns = bot_fns,
      player_names = player_names,
      starting_stack = starting_stack,
      rng_seed = rng_seed
    )
  } else {
    tourn <- tournament_state
  }

  maybe_pause <- function(trigger = c("street", "action"), msg = "Press <Enter> to continue...") {
    trigger <- match.arg(trigger)

    do_pause <- switch(
      pause_mode,
      none = FALSE,
      street = identical(trigger, "street"),
      action = TRUE
    )

    if (do_pause) readline(msg)
  }

  tourn <- initialize_hand(tourn)

  cat("\n============================\n")
  cat("NEW HAND\n")
  cat("============================\n")
  cat(sprintf("Hand ID: %s\n", tourn$current_hand$hand_id))
  cat(sprintf(
    "Button: Seat %d | Small Blind: Seat %d | Big Blind: Seat %d\n",
    tourn$current_hand$button_seat,
    tourn$current_hand$small_blind_seat,
    tourn$current_hand$big_blind_seat
  ))

  print_demo_hole_cards(tourn)
  print_demo_player_stacks(tourn)
  print_demo_board(tourn$current_hand$board)

  tourn <- post_blinds_and_antes(tourn)

  cat("\n--- FORCED BETS POSTED ---\n")
  hist_len <- length(tourn$current_hand$action_history)
  if (hist_len > 0) {
    for (i in seq_len(hist_len)) {
      a <- tourn$current_hand$action_history[[i]]
      if (a$type %in% c("post_ante", "post_sb", "post_bb")) {
        cat(sprintf("%s (Seat %d) %s %s\n",
                    a$player_name, a$seat, gsub("_", " ", a$type), a$amount))
      }
    }
  }
  cat(sprintf("Current pot: %s\n", tourn$current_hand$pot))
  print_demo_player_stacks(tourn)

  maybe_pause("street", "Press <Enter> to continue to preflop action...")

  action_counter <- 0L
  previous_history_length <- length(tourn$current_hand$action_history)

  repeat {
    hand_state <- tourn$current_hand

    if (isTRUE(hand_state$hand_over)) break

    if ((identical(hand_state$street, "showdown") ||
         isTRUE(hand_state$showdown_required)) &&
        length(hand_state$board) == 5) {

      cat("\n============================\n")
      cat("SHOWDOWN\n")
      cat("============================\n")
      print_demo_board(hand_state$board)

      maybe_pause("street", "Press <Enter> to reveal showdown...")

      tourn <- resolve_showdown(tourn)

      new_hist <- tourn$current_hand$action_history
      if (length(new_hist) > previous_history_length) {
        for (i in seq(from = previous_history_length + 1L, to = length(new_hist))) {
          a <- new_hist[[i]]
          if (a$type == "showdown_reveal") {
            cat("\nREVEALED HANDS:\n")
            for (h in a$hands) {
              cat(sprintf(
                "Seat %d | %s | Hole cards: %s | Score: %s\n",
                h$seat, h$player_name, format_card_vec(h$hole_cards), h$score
              ))
            }
          }
          if (a$type == "showdown_pot_award") {
            cat(sprintf(
              "Pot %d awarded: %s chips to seat(s) %s\n",
              a$pot_index, a$pot_amount, paste(a$winner_seats, collapse = ", ")
            ))
          }
        }
      }

      break
    }

    if (is.na(hand_state$acting_seat)) {
      old_street <- hand_state$street
      tourn <- advance_street(tourn)
      new_street <- tourn$current_hand$street

      if (new_street != old_street) {
        cat("\n============================\n")
        cat(sprintf("DEALING %s\n", toupper(new_street)))
        cat("============================\n")
        print_demo_board(tourn$current_hand$board)
        print_demo_player_stacks(tourn)

        maybe_pause("street", paste0("Press <Enter> to continue on the ", new_street, "..."))
      }

      next
    }

    acting_idx <- get_player_index_by_seat(tourn$players, hand_state$acting_seat)
    acting_player <- tourn$players[[acting_idx]]

    cat("\n----------------------------\n")
    cat(sprintf(
      "To act: Seat %d | %s | Street: %s\n",
      acting_player$seat, acting_player$name, hand_state$street
    ))
    cat(sprintf("Hole cards: %s\n", format_card_vec(acting_player$hole_cards)))
    cat(sprintf(
      "Pot: %s | Current bet: %s | Player stack: %s | To call: %s\n",
      hand_state$pot,
      hand_state$current_bet,
      acting_player$stack,
      max(0, hand_state$current_bet - acting_player$committed_this_round)
    ))

    action <- safe_get_bot_action(tourn)
    tourn <- apply_action(tourn, action)

    print_demo_last_action(tourn)
    print_demo_player_stacks(tourn)

    previous_history_length <- length(tourn$current_hand$action_history)
    action_counter <- action_counter + 1L

    maybe_pause("action")

    if (action_counter > 1000L) stop("Too many actions in demo hand.")
  }

  cat("\n============================\n")
  cat("FINAL STACKS AFTER HAND\n")
  cat("============================\n")
  print_demo_player_stacks(tourn)

  invisible(tourn)
}

demo_tournament_run <- function(
    bot_fns = list(random_bot, aggressive_bot, simple_preflop_strength_bot, always_call_bot,strength_by_street_bot,passive_bot,mixed_bot,mixed_bot2),
    player_names = c("Rando", "Aggro", "PrePlanner", "GetAlong","Da streets", "ScardyBot","Confused","MoreConfused"),
    starting_stack = 10000,
    starting_stacks = NULL,
    blind_schedule = NULL,
    tournament_id = "DEMO_TOURNAMENT",
    rng_seed = NA_integer_,
    max_hands = 100,
    verbose = TRUE,
    show_hand_details = FALSE,
    hand_pause_mode = c("none", "street", "action"),
    pause_between_hands = FALSE,
    stop_at_players = 1L,
    max_actions_per_hand = 1000L,
    snapshot_mode = c("full", "key", "final", "none"),
    preserve_tv_hands = TRUE,
    tv_threshold = 3,
    major_stack_change_pct = 0.30,
    require_major_stack_change = TRUE
) {
  ensure_demo_dependencies_loaded()
  hand_pause_mode <- match.arg(hand_pause_mode)
  snapshot_mode <- match.arg(snapshot_mode)

  if (!is.numeric(stop_at_players) || length(stop_at_players) != 1 || is.na(stop_at_players) || stop_at_players < 1) {
    stop("`stop_at_players` must be a positive integer.")
  }
  stop_at_players <- as.integer(stop_at_players)

  if (!is.numeric(max_actions_per_hand) || length(max_actions_per_hand) != 1 || is.na(max_actions_per_hand) || max_actions_per_hand < 1) {
    stop("`max_actions_per_hand` must be a positive integer.")
  }
  max_actions_per_hand <- as.integer(max_actions_per_hand)

  if (!is.na(rng_seed)) {
    set.seed(as.integer(rng_seed))
  }

  tourn <- initialize_tournament(
    bot_fns = bot_fns,
    player_names = player_names,
    starting_stack = starting_stack,
    blind_schedule = blind_schedule,
    tournament_id = tournament_id,
    rng_seed = rng_seed
  )

  if (!is.null(starting_stacks)) {
    if (!is.numeric(starting_stacks) || length(starting_stacks) != length(bot_fns) || any(is.na(starting_stacks) | starting_stacks < 0)) {
      stop("`starting_stacks` must be a nonnegative numeric vector with one stack per bot.")
    }

    for (i in seq_along(tourn$players)) {
      tourn$players[[i]]$stack <- as.numeric(starting_stacks[[i]])
      tourn$players[[i]]$all_in <- isTRUE(tourn$players[[i]]$stack <= 0)
      tourn$players[[i]] <- validate_player_state(tourn$players[[i]])
    }
  }

  if (isTRUE(verbose)) {
    cat("=====================================\n")
    cat("STARTING TOURNAMENT DEMO\n")
    cat("=====================================\n")
    cat(sprintf("Tournament ID: %s\n", tourn$tournament_id))
    cat(sprintf("Players: %s\n", paste(player_names, collapse = ", ")))
    if (is.null(starting_stacks)) {
      cat(sprintf("Starting stack: %s\n", starting_stack))
    } else {
      cat(sprintf("Starting stacks: %s\n", paste(starting_stacks, collapse = ", ")))
    }
  }

  while (TRUE) {
    active_idx <- which(vapply(
      tourn$players,
      function(p) inherits(p, "player_state") && identical(p$status, "active"),
      logical(1)
    ))

    if (length(active_idx) <= stop_at_players) break

    if (tourn$hand_number >= max_hands) {
      warning("Maximum hand limit reached before tournament finished.")
      break
    }

    tourn <- update_blind_level(tourn)

    if (isTRUE(verbose)) {
      cat("\n=====================================\n")
      cat(sprintf(
        "HAND %d | LEVEL %d | BLINDS %s/%s | ANTE %s\n",
        tourn$hand_number + 1L,
        tourn$level,
        tourn$small_blind,
        tourn$big_blind,
        tourn$ante
      ))
      cat("=====================================\n")
    }

    if (show_hand_details) {
      tourn <- demo_engine_single_hand_verbose(
        tournament_state = tourn,
        pause_mode = hand_pause_mode
      )
    } else {
      tourn <- play_current_hand(tourn, max_actions = max_actions_per_hand)
    }

    tourn <- update_blind_level(tourn)

    if (isTRUE(verbose)) {
      cat("\nCHIP COUNTS AFTER HAND:\n")
      for (p in tourn$players) {
        if (inherits(p, "player_state")) {
          cat(sprintf(
            "Seat %d | %s | Stack: %s | Status: %s\n",
            p$seat, p$name, p$stack, p$status
          ))
        }
      }
    }

    if (isTRUE(pause_between_hands)) {
      readline("Press <Enter> to continue to the next hand...")
    }
  }

  tourn <- compute_finishing_places(tourn)

  standings <- do.call(
    rbind,
    lapply(tourn$players, function(p) {
      data.frame(
        player_id = p$player_id,
        name = p$name,
        seat = p$seat,
        stack = p$stack,
        status = p$status,
        finishing_place = p$finishing_place,
        stringsAsFactors = FALSE
      )
    })
  )

  standings <- standings[order(standings$finishing_place, standings$seat), ]
  rownames(standings) <- NULL

  remaining_players <- get_demo_remaining_players(tourn, bot_fns, player_names)
  tourn$standings <- standings
  tourn$remaining_players <- remaining_players
  tourn$remaining_bot_names <- remaining_players$bot_name
  tourn$remaining_player_names <- remaining_players$name
  tourn$remaining_bot_fns <- bot_fns[remaining_players$bot_index]
  tourn$stopped_at_players <- stop_at_players
  tourn <- thin_demo_hand_log(
    tourn,
    snapshot_mode = snapshot_mode,
    preserve_tv_hands = preserve_tv_hands,
    tv_threshold = tv_threshold,
    major_stack_change_pct = major_stack_change_pct,
    require_major_stack_change = require_major_stack_change
  )

  if (isTRUE(verbose)) {
    cat("\n=====================================\n")
    cat("TOURNAMENT COMPLETE\n")
    cat("=====================================\n")
    print(standings, row.names = FALSE)

    cat("\nREMAINING PLAYERS TO PASS FORWARD\n")
    print(remaining_players, row.names = FALSE)
  }

  invisible(tourn)
}
