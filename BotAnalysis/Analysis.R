############################################################
# Mathematics of Poker — Demo and Testing Script
# File: poker_demos.R
#
# Purpose:
#   Central home for manual demos, smoke tests, and example runs.
#   This file is safe to source: nothing runs automatically.
#   Call the functions below explicitly when you want test behavior.
############################################################

# demos -------------------------------------------------------------------


# Ensure core modules are loaded
if (!exists("poker_load_all")) {
  source("poker_load_all.R")
}

poker_load_all()
source("core_internal/viewer_app_no_chatter.R")
source("BotAnalysis/Bots.R")

demo_cards_and_hands_holdem <- function(n_players = 2) {
  showdown <- play_holdem_hand(n_players = n_players)
  print_holdem_hand(showdown)
  invisible(showdown)
}


demo_engine_hand_setup <- function() {
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
demo_engine_single_hand_verbose <- function(
    bot_fns = list(random_bot, simple_preflop_strength_bot, always_call_bot),
    player_names = c("Bot A", "Bot B", "Bot C"),
    starting_stack = 1000,
    rng_seed = NA_integer_,
    pause_mode = c("none", "street", "action"),
    tournament_state = NULL
) {
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

blinds_Main <- data.frame(
  level = 1:26,
  small_blind = c(100, 200, 200, 300, 300,400,500,600,1000,1000,1000,1500,2000,3000,3000,4000,5000,6000,10000,10000,20000,20000,30000,30000,40000,50000),
  big_blind = c(200, 300, 400, 500, 600,800,1000,1200,1500,2000,2500,3000,4000,5000,6000,8000,10000,12000,15000,20000,30000,40000,50000,60000,80000,100000),
  ante = c(20, 30, 40, 50, 60,80,100,120,150,200,250,300,400,500,600,800,1000,1200,1500,2000,3000,4000,5000,6000,8000,10000),
  hands_per_level = rep(75,26))

demo_tournament_run <- function(
    bot_fns = list(random_bot, aggressive_bot, simple_preflop_strength_bot, always_call_bot,
                   strength_by_street_bot, passive_bot, mixed_bot, mixed_bot2),
    player_names = c("Rando", "Aggro", "PrePlanner", "GetAlong",
                     "Da streets", "ScardyBot", "Confused", "MoreConfused"),
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
    max_actions_per_hand = 1000L
) {
  hand_pause_mode <- match.arg(hand_pause_mode)

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

  if (isTRUE(verbose)) {
    cat("\n=====================================\n")
    cat("TOURNAMENT COMPLETE\n")
    cat("=====================================\n")
    print(standings, row.names = FALSE)

    cat("\nREMAINING PLAYERS TO PASS FORWARD\n")
    print(remaining_players, row.names = FALSE)
  }

  return(list(
    tournament_state = tourn,
    standings = standings,
    remaining_players = remaining_players,
    remaining_bot_names = remaining_players$bot_name,
    remaining_player_names = remaining_players$name,
    remaining_bot_fns = bot_fns[remaining_players$bot_index]
  ))
}

play_cash_game <- function(
    bot_fns = list(random_bot, aggressive_bot, simple_preflop_strength_bot, always_call_bot,
                   strength_by_street_bot, passive_bot, mixed_bot, mixed_bot2),
    player_names = c("Rando", "Aggro", "PrePlanner", "GetAlong",
                     "Da streets", "ScardyBot", "Confused", "MoreConfused"),
    n = 100,
    starting_stack = 500,
    small_blind = 1,
    big_blind = 2,
    ante = 0,
    game_id = "DEMO_CASH_GAME",
    rng_seed = NA_integer_,
    verbose = TRUE,
    max_actions_per_hand = 1000L
) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a positive integer.")
  }
  n<-as.integer(n)

  if (!is.numeric(max_actions_per_hand) || length(max_actions_per_hand) != 1 || is.na(max_actions_per_hand) || max_actions_per_hand < 1) {
    stop("`max_actions_per_hand` must be a positive integer.")
  }
  max_actions_per_hand<-as.integer(max_actions_per_hand)

  if (!is.na(rng_seed)) {
    set.seed(as.integer(rng_seed))
  }

  cash_blinds<-data.frame(
    level = 1L,
    small_blind = small_blind,
    big_blind = big_blind,
    ante = ante,
    hands_per_level = max(1L,n + 1L)
  )

  cash_state<-initialize_tournament(
    bot_fns = bot_fns,
    player_names = player_names,
    starting_stack = starting_stack,
    blind_schedule = cash_blinds,
    tournament_id = game_id,
    rng_seed = rng_seed
  )
  cash_state$status<-"running"

  if (isTRUE(verbose)) {
    cat("=====================================\n")
    cat("STARTING CASH GAME DEMO\n")
    cat("=====================================\n")
    cat(sprintf("Game ID: %s\n", cash_state$tournament_id))
    cat(sprintf("Players: %s\n", paste(player_names,collapse = ", ")))
    cat(sprintf("Starting stack: %s\n", starting_stack))
    cat(sprintf("Blinds: %s/%s | Ante: %s\n", small_blind,big_blind,ante))
    cat(sprintf("Hands: %s\n", n))
  }

  for (hand_idx in seq_len(n)) {
    for (i in seq_along(cash_state$players)) {
      p<-cash_state$players[[i]]
      if (!inherits(p,"player_state")) {
        next
      }

      if (p$stack <= 0) {
        p$status<-"eliminated"
        p$folded<-TRUE
        p$all_in<-FALSE
        p$acted_this_round<-FALSE
        p$committed_this_round<-0
        p$committed_this_hand<-0
        cash_state$players[[i]]<-validate_player_state(p)
        next
      }

      p$status<-"active"
      p$folded<-FALSE
      p$all_in<-FALSE
      p$acted_this_round<-FALSE
      p$committed_this_round<-0
      p$committed_this_hand<-0
      p$finishing_place<-NA_integer_
      cash_state$players[[i]]<-validate_player_state(p)
    }

    cash_state$small_blind<-small_blind
    cash_state$big_blind<-big_blind
    cash_state$ante<-ante
    cash_state$level<-1L
    cash_state$status<-"running"

    active_count<-sum(vapply(
      cash_state$players,
      function(p) inherits(p,"player_state") && identical(p$status,"active") && p$stack > 0,
      logical(1)
    ))
    if (active_count < 2) {
      if (isTRUE(verbose)) {
        cat("\nCash game stopped early because fewer than two players have chips.\n")
      }
      break
    }

    if (isTRUE(verbose)) {
      cat("\n=====================================\n")
      cat(sprintf("CASH HAND %d | BLINDS %s/%s | ANTE %s\n",hand_idx,small_blind,big_blind,ante))
      cat("=====================================\n")
    }

    cash_state<-play_current_hand(cash_state,max_actions = max_actions_per_hand)
    cash_state$status<-"running"

    if (isTRUE(verbose)) {
      cat("\nCHIP COUNTS AFTER HAND:\n")
      for (p in cash_state$players) {
        if (inherits(p,"player_state")) {
          cat(sprintf(
            "Seat %d | %s | Stack: %s\n",
            p$seat,p$name,p$stack
          ))
        }
      }
    }
  }

  standings<-do.call(
    rbind,
    lapply(cash_state$players,function(p) {
      data.frame(
        player_id = p$player_id,
        name = p$name,
        seat = p$seat,
        stack = p$stack,
        status = p$status,
        hands_played = length(cash_state$hand_log),
        stringsAsFactors = FALSE
      )
    })
  )

  standings<-standings[order(-standings$stack,standings$seat),]
  rownames(standings)<-NULL

  cash_state$standings<-standings
  cash_state$cash_game<-TRUE
  cash_state$cash_game_hands<-length(cash_state$hand_log)
  cash_state$cash_game_blinds<-cash_blinds

  if (isTRUE(verbose)) {
    cat("\n=====================================\n")
    cat("CASH GAME COMPLETE\n")
    cat("=====================================\n")
    print(standings,row.names = FALSE)
  }

  list(
    tournament_state = cash_state,
    standings = standings,
    hand_log = cash_state$hand_log,
    player_names = player_names,
    bot_fns = bot_fns,
    starting_stack = starting_stack,
    small_blind = small_blind,
    big_blind = big_blind,
    ante = ante,
    hands_played = length(cash_state$hand_log)
  )
}

make_timed_bot_fns <- function(
    bot_fns,
    player_names = NULL,
    slow_action_threshold_sec = 0.25,
    print_slow_actions = TRUE
) {
  if (!is.list(bot_fns)) {
    stop("`bot_fns` must be a list of bot functions.")
  }

  if (is.null(player_names)) {
    player_names <- names(bot_fns)
  }

  if (is.null(player_names) || length(player_names) != length(bot_fns) || any(is.na(player_names) | player_names == "")) {
    player_names <- paste0("Bot_", seq_along(bot_fns))
  }

  timing_env <- new.env(parent = emptyenv())
  timing_env$action_index <- 0L
  timing_env$log <- data.frame(
    action_index = integer(0),
    player_name = character(0),
    player_id = character(0),
    seat = integer(0),
    street = character(0),
    pot = numeric(0),
    elapsed_sec = numeric(0),
    action_type = character(0),
    stringsAsFactors = FALSE
  )

  timed_bot_fns <- Map(function(bot_fn, player_name) {
    force(bot_fn)
    force(player_name)

    function(bot_input) {
      started <- proc.time()[["elapsed"]]
      bot_action <- bot_fn(bot_input)
      elapsed <- proc.time()[["elapsed"]] - started

      action_type <- if (is.list(bot_action) && !is.null(bot_action$type)) {
        as.character(bot_action$type)[1]
      } else {
        NA_character_
      }

      timing_env$action_index <- timing_env$action_index + 1L
      timing_env$log <- rbind(
        timing_env$log,
        data.frame(
          action_index = timing_env$action_index,
          player_name = player_name,
          player_id = as.character(bot_input$player_id %||% NA_character_),
          seat = as.integer(bot_input$seat %||% NA_integer_),
          street = as.character(bot_input$street %||% NA_character_),
          pot = as.numeric(bot_input$pot %||% NA_real_),
          elapsed_sec = as.numeric(elapsed),
          action_type = action_type,
          stringsAsFactors = FALSE
        )
      )

      if (isTRUE(print_slow_actions) && is.finite(elapsed) && elapsed >= slow_action_threshold_sec) {
        cat(sprintf(
          "\nSLOW BOT ACTION: %s | seat %s | street %s | pot %s | %.3f sec\n",
          player_name,
          bot_input$seat %||% NA,
          bot_input$street %||% NA,
          bot_input$pot %||% NA,
          elapsed
        ))
      }

      bot_action
    }
  }, bot_fns, player_names)

  names(timed_bot_fns) <- names(bot_fns)

  list(
    bot_fns = timed_bot_fns,
    timing_env = timing_env
  )
}

summarize_bot_timing <- function(timing_log) {
  if (is.null(timing_log) || nrow(timing_log) == 0) {
    return(data.frame(
      player_name = character(0),
      actions = integer(0),
      total_sec = numeric(0),
      mean_sec = numeric(0),
      median_sec = numeric(0),
      max_sec = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  pieces <- split(timing_log, timing_log$player_name)
  summary <- do.call(
    rbind,
    lapply(names(pieces), function(player_name) {
      x <- pieces[[player_name]]$elapsed_sec
      data.frame(
        player_name = player_name,
        actions = length(x),
        total_sec = sum(x, na.rm = TRUE),
        mean_sec = mean(x, na.rm = TRUE),
        median_sec = median(x, na.rm = TRUE),
        max_sec = max(x, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
  )

  summary <- summary[order(-summary$total_sec, -summary$max_sec), ]
  rownames(summary) <- NULL
  summary
}

timed_demo_tournament_run <- function(
    bot_fns,
    player_names,
    slow_action_threshold_sec = 0.25,
    print_slow_actions = TRUE,
    print_timing_summary = TRUE,
    ...
) {
  timed <- make_timed_bot_fns(
    bot_fns = bot_fns,
    player_names = player_names,
    slow_action_threshold_sec = slow_action_threshold_sec,
    print_slow_actions = print_slow_actions
  )

  result <- demo_tournament_run(
    bot_fns = timed$bot_fns,
    player_names = player_names,
    ...
  )

  result$timing_log <- timed$timing_env$log
  result$timing_summary <- summarize_bot_timing(result$timing_log)

  if (isTRUE(print_timing_summary)) {
    cat("\nBOT TIMING SUMMARY\n")
    print(result$timing_summary, row.names = FALSE)
  }

  result
}



source("poker_load_all.R")
poker_load_all(include_demos = FALSE)

source("core_internal/viewer_app_no_chatter.R")
source("BotAnalysis/Bots.R")

bot_call_amount<-function(bot_input) {
  max(0, as.numeric(bot_input$current_bet %||% 0) - as.numeric(bot_input$committed_this_round %||% 0))
}

# Setup -------------------------------------------------------------------
Botbots <- list(random_bot, aggressive_bot, simple_preflop_strength_bot, always_call_bot,strength_by_street_bot,passive_bot,mixed_bot,mixed_bot2,lab_bot,lab_bot_v2)
Bot_names = c("Rando", "Aggro", "PrePlanner", "GetAlong","Da streets", "ScardyBot","Confused","MoreConfused","LabBot","LabBot2")

studentBots <- list(jaymon_bot, joel_bot, Nikola_bot, mehdi_bot,nate_bot,mady_bot,tara_bot,lucy_bot,Siena_bot,ruth_bot)
StudentNames<-c("Jaymon","Joel","Nikola","Mehdi","Nate","Mady","Tara","Lucy","Siena","Ruth")

guestBots<- list(king_bot,hatch_bot,gearan_bot,hu_bot,talmage_bot,spector_bot,khan_bot,forde_bot,hawkins_bot,biermann_bot)
guestNames<- list("King Rikki","Hatch Bot","Gearan up to beat you","Sir Hu McBluff","Talmage Bot","Bot inSpector","Khan you fold?","Fordeing Ahead","Maurice Hawkins","Biermann")

allBots<-c(Botbots,studentBots,guestBots)
allNames<-as.character(c(Bot_names,StudentNames,unlist(guestNames)))

# Bot Feature Simulation --------------------------------------------------

FeatureBotFunctions<-list(
  Siena_bot = Siena_bot,
  mehdi_bot = mehdi_bot,
  nate_bot = nate_bot,
  ruth_bot = ruth_bot,
  mady_bot = mady_bot,
  lucy_bot = lucy_bot,
  jaymon_bot = jaymon_bot,
  tara_bot = tara_bot,
  joel_bot = joel_bot,
  Nikola_bot = Nikola_bot,
  king_bot = king_bot,
  hatch_bot = hatch_bot,
  gearan_bot = gearan_bot,
  hu_bot = hu_bot,
  talmage_bot = talmage_bot,
  spector_bot = spector_bot,
  khan_bot = khan_bot,
  forde_bot = forde_bot,
  hawkins_bot = hawkins_bot,
  biermann_bot = biermann_bot,
  random_bot = random_bot,
  always_call_bot = always_call_bot,
  simple_preflop_strength_bot = simple_preflop_strength_bot,
  aggressive_bot = aggressive_bot,
  strength_by_street_bot = strength_by_street_bot,
  passive_bot = passive_bot,
  mixed_bot = mixed_bot,
  mixed_bot2 = mixed_bot2,
  student_bot_template = student_bot_template,
  lab_bot = lab_bot,
  lab_bot_v2 = lab_bot_v2
)
FeatureBotNames<-names(FeatureBotFunctions)
FeatureBotAliases<-c(
  "Siena" = "Siena_bot",
  "Mehdi" = "mehdi_bot",
  "Nate" = "nate_bot",
  "Ruth" = "ruth_bot",
  "Mady" = "mady_bot",
  "Lucy" = "lucy_bot",
  "Jaymon" = "jaymon_bot",
  "Tara" = "tara_bot",
  "Joel" = "joel_bot",
  "Nikola" = "Nikola_bot",
  "King Rikki" = "king_bot",
  "Hatch Bot" = "hatch_bot",
  "Gearan up to beat you" = "gearan_bot",
  "Sir Hu McBluff" = "hu_bot",
  "Talmage Bot" = "talmage_bot",
  "Bot inSpector" = "spector_bot",
  "Khan you fold?" = "khan_bot",
  "Fordeing Ahead" = "forde_bot",
  "Maurice Hawkins" = "hawkins_bot",
  "Biermann" = "biermann_bot",
  "Rando" = "random_bot",
  "Aggro" = "aggressive_bot",
  "PrePlanner" = "simple_preflop_strength_bot",
  "GetAlong" = "always_call_bot",
  "Da streets" = "strength_by_street_bot",
  "ScardyBot" = "passive_bot",
  "Confused" = "mixed_bot",
  "MoreConfused" = "mixed_bot2",
  "LabBot" = "lab_bot",
  "LabBot2" = "lab_bot_v2"
)

feature_bot_row_name<-function(player_name,valid_names = FeatureBotNames) {
  player_name<-as.character(player_name %||% "")
  if (!nzchar(player_name)) {
    return(NA_character_)
  }
  if (player_name %in% valid_names) {
    return(player_name)
  }
  alias<-if (player_name %in% names(FeatureBotAliases)) FeatureBotAliases[[player_name]] else NA_character_
  if (!is.na(alias) && alias %in% valid_names) {
    return(alias)
  }
  NA_character_
}

FeatureNames<-c("VPIP","PFR","Aggretion_ratio","all_in_rate","showdown_rate","tournaments Played")
new_bot_feature_counts<-function(bot_names = FeatureBotNames) {
  matrix(
    0,
    nrow = length(bot_names),
    ncol = 11,
    dimnames = list(
      bot_names,
      c(
        "hands",
        "vpip_hands",
        "pfr_hands",
        "aggressive_actions",
        "call_actions",
        "all_in_hands",
        "showdown_hands",
        "tournaments Played",
        "VPIP",
        "PFR",
        "Aggretion_ratio"
      )
    )
  )
}

BotFeatureCounts<-new_bot_feature_counts()

BotFeatures<-matrix(
  0,
  nrow = length(FeatureBotNames),
  ncol = length(FeatureNames),
  dimnames = list(FeatureBotNames,FeatureNames)
)

normalize_tournament_state<-function(tourn) {
  if (inherits(tourn,"tournament_state")) {
    return(tourn)
  }
  if (is.list(tourn) && inherits(tourn$tournament_state,"tournament_state")) {
    return(tourn$tournament_state)
  }
  stop("Expected a tournament_state or a demo_tournament_run result.")
}

hand_starting_players<-function(hand) {
  starters<-hand$starting_stack_summary %||% hand$player_start_summary %||% list()
  if (!is.list(starters)) {
    return(data.frame(
      player_id = character(0),
      player_name = character(0),
      stack = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  rows<-lapply(starters,function(p) {
    data.frame(
      player_id = as.character(p$player_id %||% ""),
      player_name = as.character(p$player_name %||% p$name %||% p$player_id %||% ""),
      stack = as.numeric(p$stack %||% 0),
      status = as.character(p$status %||% ""),
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0) {
    return(data.frame(
      player_id = character(0),
      player_name = character(0),
      stack = numeric(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out<-do.call(rbind,rows)
  out$stack[is.na(out$stack)]<-0
  out[nzchar(out$player_name) & identical_or_empty_active(out$status) & out$stack > 0,,drop = FALSE]
}

identical_or_empty_active<-function(status) {
  status<-tolower(trimws(as.character(status %||% "")))
  status[is.na(status)]<-""
  status == "" | status == "active"
}

hand_showdown_player_ids<-function(hand) {
  showdown<-hand$showdown_summary
  hands<-showdown$hands %||% list()
  ids<-vapply(hands,function(h) as.character(h$player_id %||% ""),character(1))
  unique(ids[nzchar(ids)])
}

update_bot_features_from_tournament<-function(tourn,bot_feature_counts = BotFeatureCounts) {
  tourn<-normalize_tournament_state(tourn)
  hand_log<-tourn$hand_log %||% list()

  table_names<-vapply(tourn$players,function(p) as.character(p$name %||% p$player_name %||% p$player_id %||% ""),character(1))
  table_names<-vapply(table_names,feature_bot_row_name,character(1),valid_names = rownames(bot_feature_counts))
  table_names<-unique(intersect(table_names[!is.na(table_names) & nzchar(table_names)],rownames(bot_feature_counts)))
  bot_feature_counts[table_names,"tournaments Played"]<-bot_feature_counts[table_names,"tournaments Played"] + 1

  tournament_player_ids<-vapply(tourn$players,function(p) as.character(p$player_id %||% ""),character(1))
  tournament_player_names<-vapply(tourn$players,function(p) as.character(p$name %||% p$player_name %||% p$player_id %||% ""),character(1))
  tournament_feature_names<-vapply(tournament_player_names,feature_bot_row_name,character(1),valid_names = rownames(bot_feature_counts))
  tournament_id_to_feature_name<-setNames(tournament_feature_names,tournament_player_ids)

  for (hand in hand_log) {
    starters<-hand_starting_players(hand)
    if (nrow(starters) == 0) {
      next
    }

    starters$feature_name<-ifelse(
      starters$player_id %in% names(tournament_id_to_feature_name),
      tournament_id_to_feature_name[starters$player_id],
      vapply(starters$player_name,feature_bot_row_name,character(1),valid_names = rownames(bot_feature_counts))
    )
    starter_names<-intersect(starters$feature_name[!is.na(starters$feature_name) & nzchar(starters$feature_name)],rownames(bot_feature_counts))
    player_id_to_feature_name<-setNames(starters$feature_name,starters$player_id)
    bot_feature_counts[starter_names,"hands"]<-bot_feature_counts[starter_names,"hands"] + 1

    action_history<-hand$action_history %||% list()
    voluntary_by_player<-list()
    pfr_by_player<-list()
    all_in_by_player<-list()
    aggressive_count<-setNames(numeric(0),character(0))
    call_count<-setNames(numeric(0),character(0))

    for (a in action_history) {
      player_id<-as.character(a$player_id %||% "")
      player_name<-if (nzchar(player_id) && player_id %in% names(player_id_to_feature_name)) {
        player_id_to_feature_name[[player_id]]
      } else if (nzchar(player_id) && player_id %in% names(tournament_id_to_feature_name)) {
        tournament_id_to_feature_name[[player_id]]
      } else {
        feature_bot_row_name(a$player_name %||% "",valid_names = rownames(bot_feature_counts))
      }

      if (is.na(player_name) || !nzchar(player_name) || !(player_name %in% rownames(bot_feature_counts))) {
        next
      }

      action_type<-tolower(trimws(as.character(a$type %||% "")))
      action_street<-tolower(trimws(as.character(a$street %||% "")))
      is_preflop_action<-identical(action_street,"preflop")

      if (is_preflop_action &&
          action_type %in% c("call","bet","raise","all_in","all_in_call","all_in_bet","all_in_raise","all_in_short")) {
        voluntary_by_player[[player_name]]<-TRUE
      }
      if (is_preflop_action &&
          (action_type %in% c("bet","raise") || grepl("all_in",action_type,fixed = TRUE))) {
        pfr_by_player[[player_name]]<-TRUE
      }
      if (grepl("all_in",action_type,fixed = TRUE)) {
        all_in_by_player[[player_name]]<-TRUE
      }
      if (action_type %in% c("bet","raise","all_in_bet","all_in_raise")) {
        current_aggressive_count<-if (player_name %in% names(aggressive_count)) aggressive_count[[player_name]] else 0
        aggressive_count[player_name]<-current_aggressive_count + 1
      }
      if (action_type %in% c("call","all_in_call")) {
        current_call_count<-if (player_name %in% names(call_count)) call_count[[player_name]] else 0
        call_count[player_name]<-current_call_count + 1
      }
    }

    vpip_names<-intersect(names(voluntary_by_player),rownames(bot_feature_counts))
    pfr_names<-intersect(names(pfr_by_player),rownames(bot_feature_counts))
    all_in_names<-intersect(names(all_in_by_player),rownames(bot_feature_counts))
    showdown_ids<-hand_showdown_player_ids(hand)
    showdown_names<-intersect(starters$feature_name[starters$player_id %in% showdown_ids],rownames(bot_feature_counts))

    bot_feature_counts[vpip_names,"vpip_hands"]<-bot_feature_counts[vpip_names,"vpip_hands"] + 1
    bot_feature_counts[pfr_names,"pfr_hands"]<-bot_feature_counts[pfr_names,"pfr_hands"] + 1
    bot_feature_counts[all_in_names,"all_in_hands"]<-bot_feature_counts[all_in_names,"all_in_hands"] + 1
    bot_feature_counts[showdown_names,"showdown_hands"]<-bot_feature_counts[showdown_names,"showdown_hands"] + 1

    for (nm in intersect(names(aggressive_count),rownames(bot_feature_counts))) {
      bot_feature_counts[nm,"aggressive_actions"]<-bot_feature_counts[nm,"aggressive_actions"] + aggressive_count[[nm]]
    }
    for (nm in intersect(names(call_count),rownames(bot_feature_counts))) {
      bot_feature_counts[nm,"call_actions"]<-bot_feature_counts[nm,"call_actions"] + call_count[[nm]]
    }
  }

  bot_feature_counts
}

compute_bot_features<-function(bot_feature_counts = BotFeatureCounts) {
  out<-matrix(
    0,
    nrow = nrow(bot_feature_counts),
    ncol = length(FeatureNames),
    dimnames = list(rownames(bot_feature_counts),FeatureNames)
  )

  hands<-bot_feature_counts[,"hands"]
  calls<-bot_feature_counts[,"call_actions"]

  out[,"VPIP"]<-ifelse(hands > 0,bot_feature_counts[,"vpip_hands"] / hands,NA_real_)
  out[,"PFR"]<-ifelse(hands > 0,bot_feature_counts[,"pfr_hands"] / hands,NA_real_)
  out[,"Aggretion_ratio"]<-bot_feature_counts[,"aggressive_actions"] / (calls + 1)
  out[,"all_in_rate"]<-ifelse(hands > 0,bot_feature_counts[,"all_in_hands"] / hands,NA_real_)
  out[,"showdown_rate"]<-ifelse(hands > 0,bot_feature_counts[,"showdown_hands"] / hands,NA_real_)
  out[,"tournaments Played"]<-bot_feature_counts[,"tournaments Played"]

  out
}

bot_preflop_actions<-function(tourn,bot_name = "Siena_bot") {
  tourn<-normalize_tournament_state(tourn)
  rows<-list()

  tournament_player_ids<-vapply(tourn$players,function(p) as.character(p$player_id %||% ""),character(1))
  tournament_player_names<-vapply(tourn$players,function(p) as.character(p$name %||% p$player_name %||% p$player_id %||% ""),character(1))
  tournament_feature_names<-vapply(tournament_player_names,feature_bot_row_name,character(1),valid_names = FeatureBotNames)
  tournament_id_to_feature_name<-setNames(tournament_feature_names,tournament_player_ids)

  for (hand in tourn$hand_log %||% list()) {
    starters<-hand_starting_players(hand)
    if (nrow(starters) == 0) {
      next
    }

    starters$feature_name<-ifelse(
      starters$player_id %in% names(tournament_id_to_feature_name),
      tournament_id_to_feature_name[starters$player_id],
      vapply(starters$player_name,feature_bot_row_name,character(1),valid_names = FeatureBotNames)
    )
    player_id_to_feature_name<-setNames(starters$feature_name,starters$player_id)

    for (a in hand$action_history %||% list()) {
      action_street<-tolower(trimws(as.character(a$street %||% "")))
      if (!identical(action_street,"preflop")) {
        next
      }

      player_id<-as.character(a$player_id %||% "")
      feature_name<-if (nzchar(player_id) && player_id %in% names(player_id_to_feature_name)) {
        player_id_to_feature_name[[player_id]]
      } else if (nzchar(player_id) && player_id %in% names(tournament_id_to_feature_name)) {
        tournament_id_to_feature_name[[player_id]]
      } else {
        feature_bot_row_name(a$player_name %||% "",valid_names = FeatureBotNames)
      }

      if (is.na(feature_name) || !identical(feature_name,bot_name)) {
        next
      }

      rows[[length(rows) + 1L]]<-data.frame(
        hand_number = hand$hand_number %||% NA_integer_,
        player_id = player_id,
        player_name = as.character(a$player_name %||% ""),
        feature_name = feature_name,
        type = as.character(a$type %||% ""),
        amount = as.numeric(a$amount %||% 0),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      hand_number = integer(0),
      player_id = character(0),
      player_name = character(0),
      feature_name = character(0),
      type = character(0),
      amount = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind,rows)
}

run_bot_feature_simulations<-function(
    n,
    bot_feature_counts = NULL,
    bot_fns = FeatureBotFunctions,
    bot_names = FeatureBotNames,
    table_size = 10L,
    blind_schedule = blinds_Main,
    max_hands = 2000,
    starting_stack = 1000,
    stop_at_players = 1L,
    verbose = FALSE,
    reset = TRUE
) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a positive integer.")
  }

  n<-as.integer(n)
  if (isTRUE(reset) || is.null(bot_feature_counts)) {
    bot_feature_counts<-new_bot_feature_counts(bot_names)
  }

  tournament_results<-vector("list",n)

  for (i in seq_len(n)) {
    selected_idx<-sample(seq_along(bot_names),size = min(table_size,length(bot_names)))
    participantBots<-bot_fns[selected_idx]
    participantNames<-bot_names[selected_idx]

    tournament_results[[i]]<-demo_tournament_run(
      bot_fns = participantBots,
      player_names = participantNames,
      blind_schedule = blind_schedule,
      max_hands = max_hands,
      starting_stack = starting_stack,
      verbose = verbose,
      stop_at_players = stop_at_players
    )

    bot_feature_counts<-update_bot_features_from_tournament(
      tournament_results[[i]],
      bot_feature_counts = bot_feature_counts
    )
  }

  BotFeatureCounts<<-bot_feature_counts
  BotFeatures<<-compute_bot_features(BotFeatureCounts)

  invisible(list(
    counts = BotFeatureCounts,
    features = BotFeatures,
    tournaments = tournament_results
  ))
}

#
Index<-1:length(allNames)
# Seat Draw
Table1<-sample(Index,10)
Index<-setdiff(Index,Table1)
Table2<-sample(Index,10)
Table3<-setdiff(Index,Table2)


# RUn Tournament ----------------------------------------------------------


participantBots<-allBots[Table1]
participantNames<-allNames[Table1]
participantNames

tournT1<-demo_tournament_run(bot_fns = participantBots,player_names = participantNames,blind_schedule = blinds_Main,max_hands = 2000,starting_stack = 60000,verbose = FALSE,stop_at_players = 6)

participantBots<-allBots[Table2]
participantNames<-allNames[Table2]
tournT2<-demo_tournament_run(bot_fns = participantBots,player_names = participantNames,blind_schedule = blinds_Main,max_hands = 2000,starting_stack = 60000,verbose = FALSE,stop_at_players = 6)
participantBots<-allBots[Table3]
participantNames<-allNames[Table3]
tournT3<-demo_tournament_run(bot_fns = participantBots,player_names = participantNames,blind_schedule = blinds_Main,max_hands = 2000,starting_stack = 60000,verbose = FALSE,stop_at_players = 6)




## Take the 6 remaining players from each first-round table.
## Divide those 18 players into two tables of 9.
## Each Round 2 table plays down to 5 players.

blind_schedule_from_level<-function(blind_schedule,start_level) {
  level_idx<-which(blind_schedule$level >= start_level)[1]
  if (is.na(level_idx)) {
    level_idx<-nrow(blind_schedule)
  }

  blind_schedule[level_idx:nrow(blind_schedule),,drop = FALSE]
}

Round2bots<-c(tournT1$remaining_bot_fns,tournT2$remaining_bot_fns,tournT3$remaining_bot_fns)
Round2Names<-c(tournT1$remaining_player_names,tournT2$remaining_player_names,tournT3$remaining_player_names)
Round2Stacks<-c(tournT1$remaining_players$stack,tournT2$remaining_players$stack,tournT3$remaining_players$stack)

Round2StartLevel<-min(c(tournT1$level,tournT2$level,tournT3$level),na.rm = TRUE)
Round2BlindSchedule<-blind_schedule_from_level(tournT1$blind_schedule,Round2StartLevel)

Round2Draw<-sample(seq_along(Round2Names))
Round2Table1<-Round2Draw[1:ceiling(length(Round2Draw)/2)]
Round2Table2<-Round2Draw[(ceiling(length(Round2Draw)/2)+1):length(Round2Draw)]

participantBots<-Round2bots[Round2Table1]
participantNames<-Round2Names[Round2Table1]
participantStacks<-Round2Stacks[Round2Table1]
tournR2T1<-demo_tournament_run(
  bot_fns = participantBots,
  player_names = participantNames,
  starting_stacks = participantStacks,
  blind_schedule = Round2BlindSchedule,
  max_hands = 2000,
  verbose = FALSE,
  stop_at_players = 5
)

participantBots<-Round2bots[Round2Table2]
participantNames<-Round2Names[Round2Table2]
participantStacks<-Round2Stacks[Round2Table2]
tournR2T2<-demo_tournament_run(
  bot_fns = participantBots,
  player_names = participantNames,
  starting_stacks = participantStacks,
  blind_schedule = Round2BlindSchedule,
  max_hands = 2000,
  verbose = FALSE,
  stop_at_players = 5
)



FinalTablebots<-c(tournR2T1$remaining_bot_fns,tournR2T2$remaining_bot_fns)
FinalTableNames<-c(tournR2T1$remaining_player_names,tournR2T2$remaining_player_names)
FinalTableStacks<-c(tournR2T1$remaining_players$stack,tournR2T2$remaining_players$stack)

FinalTableStartLevel<-min(c(tournR2T1$level,tournR2T2$level),na.rm = TRUE)
FinalTableBlindSchedule<-blind_schedule_from_level(tournR2T1$blind_schedule,FinalTableStartLevel)

## Final Table
participantBots<-FinalTablebots
participantNames<-FinalTableNames
participantStacks<-FinalTableStacks
participantNames
tournFinal<-demo_tournament_run(
  bot_fns = participantBots,
  blind_schedule = FinalTableBlindSchedule,
  player_names = participantNames,
  starting_stacks = participantStacks,
  max_hands = 2000,
  verbose = FALSE,
  snapshot_mode = "key"
)


# Televised Schedule ------------------------------------------------------

estimate_broadcast_runtime(tournT1)
estimate_broadcast_runtime(tournT2)
estimate_broadcast_runtime(tournT3)
estimate_broadcast_runtime(tournR2T1)
estimate_broadcast_runtime(tournR2T2)
estimate_broadcast_runtime(tournFinal)


run_viewer_app(tournT1)
run_viewer_app(tournT2)
run_viewer_app(tournT3)
run_viewer_app(tournR2T1)
run_viewer_app(tournR2T2)
run_viewer_app(tournFinal)



# bot feature Sim ---------------------------------------------------------
results <- run_bot_feature_simulations(10)
BotFeatures


