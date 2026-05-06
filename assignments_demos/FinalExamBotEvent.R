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
source("reference_bots/example_bots.R")
source("reference_bots/GuestBots.R")

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
  level = 1:20,
  small_blind = c(100, 200, 200, 300, 300,400,500,600,1000,1000,1000,1500,2000,3000,3000,4000,5000,6000,10000,10000),
  big_blind = c(200, 300, 400, 500, 600,800,1000,1200,1500,2000,2500,3000,4000,5000,6000,8000,10000,12000,15000,20000),
  ante = c(20, 30, 40, 50, 60,80,100,120,150,200,250,300,400,500,600,800,1000,1200,1500,2000),
  hands_per_level = rep(80,20))

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

source("assignments_demos/poker_demos.R")
bot_call_amount<-function(bot_input) {
  max(0, as.numeric(bot_input$current_bet %||% 0) - as.numeric(bot_input$committed_this_round %||% 0))
}

source_final_student_bot_file<-function(path,envir = .GlobalEnv) {
  exprs<-parse(path)
  for (expr in exprs) {
    if (!is.call(expr) || !as.character(expr[[1]]) %in% c("<-","=")) {
      next
    }

    rhs<-expr[[3]]
    if (is.call(rhs) && identical(as.character(rhs[[1]]),"function")) {
      eval(expr,envir = envir)
    }
  }

  invisible(path)
}

final_student_bot_files<-list.files("FinalStudentBots",pattern = "\\.R$",full.names = TRUE)
for (bot_file in final_student_bot_files) {
  source_final_student_bot_file(bot_file)
}
source("reference_bots/GuestBots.R")
# Setup -------------------------------------------------------------------
Botbots <- list(random_bot, aggressive_bot, simple_preflop_strength_bot, always_call_bot,strength_by_street_bot,passive_bot,mixed_bot,mixed_bot2,lab_bot,lab_bot_v2)
Bot_names = c("Rando", "Aggro", "PrePlanner", "GetAlong","Da streets", "ScardyBot","Confused","MoreConfused","LabBot","LabBot2")

studentBots <- list(jaymon_bot, joel_bot, Nikola_bot, mehdi_bot,nate_bot,mady_bot,tara_bot,lucy_bot,Siena_bot,ruth_bot)
StudentNames<-c("Jaymon","Joel","Nikola","Mehdi","Nate","Mady","Tara","Lucy","Siena","Ruth")

guestBots<- list(king_bot,hatch_bot,gearan_bot,hu_bot,talmage_bot,spector_bot,khan_bot,forde_bot,hawkins_bot,biermann_bot)
guestNames<- list("King Rikki","Hatch Bot","Gearan up to beat you","Sir Hu McBluff","Talmage Bot","Bot inSpector","Khan you fold?","Fordeing Ahead","Maurice Hawkins","Biermann")

allBots<-c(Botbots,studentBots,guestBots)
allNames<-c(Bot_names,StudentNames,guestNames)

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


run_viewer_app(tournT1)
run_viewer_app(tournT2)
run_viewer_app(tournT3)
run_viewer_app(tournR2T1)
run_viewer_app(tournR2T2)
run_viewer_app(tournFinal)
