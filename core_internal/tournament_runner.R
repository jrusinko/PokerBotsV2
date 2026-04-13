############################################################
# Mathematics of Poker — Tournament Runner
# File: tournament_runner.R
#
# Purpose:
#   Match orchestration, repeated simulations, standings, and logging.
#   At present this is scaffold only.
############################################################

new_match_log <- function() {
  list(hands = list(), summary = NULL)
}

play_match <- function(bot_registry, n_hands = 100, game = "holdem", stack_size = 100) {
  # Placeholder for repeated bot-vs-bot or multi-bot matches.
  stop("play_match() has not been implemented yet.")
}

round_robin_tournament <- function(bot_registry, n_hands_per_match = 100, game = "holdem", stack_size = 100) {
  # Placeholder for class round-robin tournaments.
  stop("round_robin_tournament() has not been implemented yet.")
}

summarize_tournament_results <- function(tournament_obj) {
  # Placeholder for standings and ELO-style summaries.
  stop("summarize_tournament_results() has not been implemented yet.")
}

write_match_log_csv <- function(match_obj, path = "match_log.csv") {
  # Placeholder for persistent logging.
  stop("write_match_log_csv() has not been implemented yet.")
}
# =========================
# Tournament runner helpers
# =========================

update_blind_level <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  sched <- tournament_state$blind_schedule

  required_cols <- c("level", "small_blind", "big_blind", "ante", "hands_per_level")
  if (!is.data.frame(sched) || !all(required_cols %in% names(sched))) {
    stop(
      "`blind_schedule` must be a data.frame with columns: ",
      paste(required_cols, collapse = ", ")
    )
  }

  if (nrow(sched) < 1) {
    stop("Blind schedule must have at least one row.")
  }

  # Total hands completed so far
  hands_played <- as.integer(tournament_state$hand_number)

  # Determine current level from cumulative hand counts
  cumulative_hands <- cumsum(as.integer(sched$hands_per_level))
  new_level <- which(hands_played <= cumulative_hands)[1]

  # If past the end of the schedule, stay at the last level
  if (is.na(new_level)) {
    new_level <- nrow(sched)
  }

  tournament_state$level <- as.integer(sched$level[new_level])
  tournament_state$small_blind <- as.numeric(sched$small_blind[new_level])
  tournament_state$big_blind <- as.numeric(sched$big_blind[new_level])
  tournament_state$ante <- as.numeric(sched$ante[new_level])

  validate_tournament_state(tournament_state)
}


compute_finishing_places <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  players <- tournament_state$players
  n_players <- length(players)

  # Players eliminated earlier should have worse finishing places.
  # elimination_order stores bust-outs from earliest to latest.
  elim_order <- tournament_state$elimination_order

  # Winner: remaining active player, if unique
  active_idx <- which(vapply(
    players,
    function(p) inherits(p, "player_state") && identical(p$status, "active"),
    logical(1)
  ))

  # Assign all eliminated players
  # First eliminated gets place n, second gets n-1, ..., last eliminated gets 2
  if (length(elim_order) > 0) {
    for (k in seq_along(elim_order)) {
      pid <- elim_order[k]
      idx <- which(vapply(players, function(p) p$player_id, character(1)) == pid)
      if (length(idx) == 1) {
        players[[idx]]$finishing_place <- as.integer(n_players - k + 1L)
        players[[idx]] <- validate_player_state(players[[idx]])
      }
    }
  }

  # Assign winner, if exactly one remains
  if (length(active_idx) == 1) {
    players[[active_idx]]$finishing_place <- 1L
    players[[active_idx]] <- validate_player_state(players[[active_idx]])
  }

  tournament_state$players <- players
  validate_tournament_state(tournament_state)
}
# =========================
# Run a full tournament
# =========================

run_tournament <- function(
    bot_fns = NULL,
    tournament_state = NULL,
    starting_stack = 10000,
    blind_schedule = NULL,
    tournament_id = NULL,
    player_names = NULL,
    max_seats = 10L,
    initial_button_seat = 1L,
    rng_seed = NA_integer_,
    max_hands = 10000L,
    verbose = TRUE
) {
  # -----------------------------------
  # Initialize or validate tournament
  # -----------------------------------
  if (!is.null(tournament_state) && !is.null(bot_fns)) {
    stop("Provide either `tournament_state` or `bot_fns`, not both.")
  }

  if (is.null(tournament_state) && is.null(bot_fns)) {
    stop("You must provide either `tournament_state` or `bot_fns`.")
  }

  if (is.null(tournament_state)) {
    tournament_state <- initialize_tournament(
      bot_fns = bot_fns,
      starting_stack = starting_stack,
      blind_schedule = blind_schedule,
      tournament_id = tournament_id,
      player_names = player_names,
      max_seats = max_seats,
      initial_button_seat = initial_button_seat,
      rng_seed = rng_seed
    )
  }

  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  if (!is.numeric(max_hands) || length(max_hands) != 1 || max_hands < 1) {
    stop("`max_hands` must be a positive integer.")
  }

  max_hands <- as.integer(max_hands)

  if (!is.na(rng_seed)) {
    set.seed(as.integer(rng_seed))
  }

  # Make sure blind level matches current hand count
  tournament_state <- update_blind_level(tournament_state)

  if (isTRUE(verbose)) {
    cat("Starting tournament:", tournament_state$tournament_id, "\n")
    cat("Players:", length(tournament_state$players), "\n")
    cat(
      "Level", tournament_state$level,
      "- Blinds:", tournament_state$small_blind, "/", tournament_state$big_blind,
      " Ante:", tournament_state$ante, "\n"
    )
  }

  # -----------------------------------
  # Main tournament loop
  # -----------------------------------
  while (TRUE) {
    active_idx <- which(vapply(
      tournament_state$players,
      function(p) inherits(p, "player_state") && identical(p$status, "active"),
      logical(1)
    ))

    if (length(active_idx) <= 1) {
      break
    }

    if (tournament_state$hand_number >= max_hands) {
      warning("Maximum hand limit reached before tournament finished.")
      break
    }

    # Ensure blind level is correct before the next hand starts
    tournament_state <- update_blind_level(tournament_state)

    if (isTRUE(verbose)) {
      active_players <- tournament_state$players[active_idx]
      chip_report <- paste(
        vapply(
          active_players,
          function(p) paste0(p$name, "(", p$stack, ")"),
          character(1)
        ),
        collapse = ", "
      )

      cat(
        "\n--- Hand", tournament_state$hand_number + 1L,
        "| Level", tournament_state$level,
        "| Blinds", tournament_state$small_blind, "/", tournament_state$big_blind,
        "| Active:", length(active_idx), "---\n"
      )
      cat(chip_report, "\n")
    }

    tournament_state <- play_current_hand(tournament_state)

    # Mark players with zero stack as eliminated if not already marked
    zero_active_idx <- which(vapply(
      tournament_state$players,
      function(p) {
        inherits(p, "player_state") &&
          identical(p$status, "active") &&
          isTRUE(p$stack <= 0)
      },
      logical(1)
    ))

    if (length(zero_active_idx) > 0) {
      for (idx in zero_active_idx) {
        tournament_state$players[[idx]]$status <- "eliminated"

        pid <- tournament_state$players[[idx]]$player_id
        if (!(pid %in% tournament_state$elimination_order)) {
          tournament_state$elimination_order <- c(
            tournament_state$elimination_order,
            pid
          )
        }

        tournament_state$players[[idx]] <- validate_player_state(tournament_state$players[[idx]])
      }
    }

    validate_tournament_state(tournament_state)
  }

  # -----------------------------------
  # Finalize tournament
  # -----------------------------------
  tournament_state <- compute_finishing_places(tournament_state)

  active_idx <- which(vapply(
    tournament_state$players,
    function(p) inherits(p, "player_state") && identical(p$status, "active"),
    logical(1)
  ))

  if (length(active_idx) == 1) {
    tournament_state$status <- "finished"
  }

  validate_tournament_state(tournament_state)

  if (isTRUE(verbose)) {
    cat("\nTournament complete.\n")

    standings <- do.call(
      rbind,
      lapply(tournament_state$players, function(p) {
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
    print(standings, row.names = FALSE)
  }

  tournament_state
}

