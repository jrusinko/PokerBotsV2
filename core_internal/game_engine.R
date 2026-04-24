############################################################
# Mathematics of Poker — Game Engine
# File: game_engine.R
#
# Purpose:
#   Central home for game state objects and gameplay progression.
#   The current file contains a minimal scaffold plus a few working
#   demonstration routines. Many functions are intentionally left as
#   placeholders for the next development phase.
############################################################

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


new_player_state <- function(
    player_id,
    name = player_id,
    seat,
    stack,
    bot_fn = NULL,
    is_human = FALSE,
    status = "active",
    hole_cards = character(0),
    folded = FALSE,
    all_in = FALSE,
    acted_this_round = FALSE,
    committed_this_round = 0,
    committed_this_hand = 0,
    finishing_place = NA_integer_
) {
  x <- list(
    player_id = as.character(player_id),
    name = as.character(name),
    seat = as.integer(seat),
    stack = as.numeric(stack),
    bot_fn = bot_fn,
    is_human = isTRUE(is_human),
    status = as.character(status),   # active, eliminated
    hole_cards = as.character(hole_cards),
    folded = isTRUE(folded),
    all_in = isTRUE(all_in),
    acted_this_round = isTRUE(acted_this_round),
    committed_this_round = as.numeric(committed_this_round),
    committed_this_hand = as.numeric(committed_this_hand),
    finishing_place = as.integer(finishing_place)
  )

  class(x) <- "player_state"
  validate_player_state(x)
}
validate_player_state <- function(x) {
  stopifnot(is.list(x))
  stopifnot(is.character(x$player_id), length(x$player_id) == 1)
  stopifnot(is.character(x$name), length(x$name) == 1)
  stopifnot(is.numeric(x$seat), length(x$seat) == 1, !is.na(x$seat))
  stopifnot(is.numeric(x$stack), length(x$stack) == 1, x$stack >= 0)
  stopifnot(is.logical(x$is_human), length(x$is_human) == 1)
  stopifnot(x$status %in% c("active", "eliminated"))
  stopifnot(is.character(x$hole_cards))
  stopifnot(length(x$hole_cards) %in% c(0, 2))
  stopifnot(is.logical(x$folded), length(x$folded) == 1)
  stopifnot(is.logical(x$all_in), length(x$all_in) == 1)
  stopifnot(is.logical(x$acted_this_round), length(x$acted_this_round) == 1)
  stopifnot(is.numeric(x$committed_this_round), length(x$committed_this_round) == 1, x$committed_this_round >= 0)
  stopifnot(is.numeric(x$committed_this_hand), length(x$committed_this_hand) == 1, x$committed_this_hand >= 0)
  x
}

new_hand_state <- function(
    hand_id,
    hand_number,
    street = "preflop",
    button_seat,
    small_blind_seat,
    big_blind_seat,
    acting_seat,
    min_bet,
    current_bet = 0,
    last_full_raise = min_bet,
    pot = 0,
    side_pots = list(),
    board = character(0),
    deck = character(0),
    action_history = list(),
    hand_over = FALSE,
    showdown_required = FALSE
) {
  x <- list(
    hand_id = as.character(hand_id),
    hand_number = as.integer(hand_number),
    street = as.character(street),   # preflop, flop, turn, river, showdown
    button_seat = as.integer(button_seat),
    small_blind_seat = as.integer(small_blind_seat),
    big_blind_seat = as.integer(big_blind_seat),
    acting_seat = as.integer(acting_seat),
    min_bet = as.numeric(min_bet),
    current_bet = as.numeric(current_bet),
    last_full_raise = as.numeric(last_full_raise),
    pot = as.numeric(pot),
    side_pots = side_pots,
    board = as.character(board),
    deck = as.character(deck),
    action_history = action_history,
    hand_over = isTRUE(hand_over),
    showdown_required = isTRUE(showdown_required)
  )

  class(x) <- "hand_state"
  validate_hand_state(x)
}
validate_hand_state <- function(x) {
  stopifnot(is.list(x))
  stopifnot(is.character(x$hand_id), length(x$hand_id) == 1)
  stopifnot(is.numeric(x$hand_number), length(x$hand_number) == 1)
  stopifnot(x$street %in% c("preflop", "flop", "turn", "river", "showdown"))
  stopifnot(is.numeric(x$button_seat), length(x$button_seat) == 1)
  stopifnot(is.numeric(x$small_blind_seat), length(x$small_blind_seat) == 1)
  stopifnot(is.numeric(x$big_blind_seat), length(x$big_blind_seat) == 1)
  stopifnot(is.numeric(x$acting_seat), length(x$acting_seat) == 1)
  stopifnot(is.numeric(x$min_bet), length(x$min_bet) == 1, x$min_bet >= 0)
  stopifnot(is.numeric(x$current_bet), length(x$current_bet) == 1, x$current_bet >= 0)
  stopifnot(is.numeric(x$last_full_raise), length(x$last_full_raise) == 1, x$last_full_raise >= 0)
  stopifnot(is.numeric(x$pot), length(x$pot) == 1, x$pot >= 0)
  stopifnot(is.character(x$board), length(x$board) <= 5)
  stopifnot(is.character(x$deck))
  stopifnot(is.list(x$action_history))
  stopifnot(is.logical(x$hand_over), length(x$hand_over) == 1)
  stopifnot(is.logical(x$showdown_required), length(x$showdown_required) == 1)
  x
}

new_tournament_state <- function(
    tournament_id,
    players,
    max_seats = 10L,
    starting_stack,
    blind_schedule,
    small_blind,
    big_blind,
    ante = 0,
    level = 1L,
    hand_number = 0L,
    button_seat = NA_integer_,
    current_hand = NULL,
    action_log = list(),
    hand_log = list(),
    elimination_order = character(0),
    status = "not_started",
    rng_seed = NA_integer_
) {
  x <- list(
    tournament_id = as.character(tournament_id),
    players = players,
    max_seats = as.integer(max_seats),
    starting_stack = as.numeric(starting_stack),
    blind_schedule = blind_schedule,
    small_blind = as.numeric(small_blind),
    big_blind = as.numeric(big_blind),
    ante = as.numeric(ante),
    level = as.integer(level),
    hand_number = as.integer(hand_number),
    button_seat = as.integer(button_seat),
    current_hand = current_hand,
    action_log = action_log,
    hand_log = hand_log,
    elimination_order = as.character(elimination_order),
    status = as.character(status),   # not_started, running, finished
    rng_seed = as.integer(rng_seed)
  )

  class(x) <- "tournament_state"
  validate_tournament_state(x)
}
validate_tournament_state <- function(x) {
  stopifnot(is.list(x))
  stopifnot(is.character(x$tournament_id), length(x$tournament_id) == 1)
  stopifnot(is.list(x$players))
  stopifnot(length(x$players) >= 2)
  stopifnot(length(x$players) <= x$max_seats)
  stopifnot(all(vapply(x$players, inherits, logical(1), what = "player_state")))
  stopifnot(is.numeric(x$max_seats), length(x$max_seats) == 1, x$max_seats >= 2)
  stopifnot(is.numeric(x$starting_stack), length(x$starting_stack) == 1, x$starting_stack > 0)
  stopifnot(is.numeric(x$small_blind), length(x$small_blind) == 1, x$small_blind >= 0)
  stopifnot(is.numeric(x$big_blind), length(x$big_blind) == 1, x$big_blind >= x$small_blind)
  stopifnot(is.numeric(x$ante), length(x$ante) == 1, x$ante >= 0)
  stopifnot(is.numeric(x$level), length(x$level) == 1, x$level >= 1)
  stopifnot(is.numeric(x$hand_number), length(x$hand_number) == 1, x$hand_number >= 0)
  stopifnot(x$status %in% c("not_started", "running", "finished"))
  x
}
new_action <- function(
    player_id,
    seat,
    street,
    type,
    amount = 0,
    timestamp = Sys.time()
) {
  x <- list(
    player_id = as.character(player_id),
    seat = as.integer(seat),
    street = as.character(street),
    type = as.character(type),   # fold, check, call, bet, raise, all_in, post_sb, post_bb, post_ante
    amount = as.numeric(amount),
    timestamp = timestamp
  )
  class(x) <- "poker_action"
  validate_action(x)
}

default_blind_schedule <- data.frame(
  level = c(1, 2, 3, 4, 5),
  small_blind = c(50, 100, 150, 200, 300),
  big_blind   = c(100, 200, 300, 400, 600),
  ante        = c(0, 0, 0, 0, 0),
  hands_per_level = c(10, 10, 10, 10, 10)
)






# ==========================
# State reset / init helpers
# ==========================

reset_players_for_new_hand <- function(players) {
  if (!is.list(players)) {
    stop("`players` must be a list of player_state objects.")
  }

  out <- lapply(players, function(p) {
    if (!inherits(p, "player_state")) {
      stop("All entries of `players` must inherit from 'player_state'.")
    }

    # Eliminated players stay out of future hands.
    if (identical(p$status, "eliminated") || isTRUE(p$stack <= 0)) {
      p$status <- "eliminated"
      p$hole_cards <- character(0)
      p$folded <- TRUE
      p$all_in <- FALSE
      p$acted_this_round <- FALSE
      p$committed_this_round <- 0
      p$committed_this_hand <- 0
      return(validate_player_state(p))
    }

    p$hole_cards <- character(0)
    p$folded <- FALSE
    p$all_in <- FALSE
    p$acted_this_round <- FALSE
    p$committed_this_round <- 0
    p$committed_this_hand <- 0

    validate_player_state(p)
  })

  names(out) <- names(players)
  out
}


initialize_tournament <- function(
    bot_fns,
    starting_stack = 10000,
    blind_schedule = NULL,
    tournament_id = NULL,
    player_names = NULL,
    max_seats = 10L,
    initial_button_seat = 1L,
    rng_seed = NA_integer_
) {
  # -------------------------
  # Basic argument processing
  # -------------------------
  if (!is.list(bot_fns) || length(bot_fns) < 2) {
    stop("`bot_fns` must be a list with at least 2 entries.")
  }

  if (length(bot_fns) > max_seats) {
    stop("Number of players cannot exceed `max_seats`.")
  }

  if (!is.numeric(starting_stack) || length(starting_stack) != 1 || starting_stack <= 0) {
    stop("`starting_stack` must be a positive number.")
  }

  if (is.null(player_names)) {
    player_names <- names(bot_fns)
    if (is.null(player_names) || any(player_names == "")) {
      player_names <- paste0("Player ", seq_along(bot_fns))
    }
  }

  if (length(player_names) != length(bot_fns)) {
    stop("`player_names` must have the same length as `bot_fns`.")
  }

  # Default blind schedule if none is supplied
  if (is.null(blind_schedule)) {
    blind_schedule <- data.frame(
      level = c(1, 2, 3, 4, 5, 6),
      small_blind = c(50, 100, 150, 200, 300, 400),
      big_blind = c(100, 200, 300, 400, 600, 800),
      ante = c(0, 0, 0, 0, 0, 0),
      hands_per_level = c(10, 10, 10, 10, 10, 10)
    )
  }

  required_cols <- c("level", "small_blind", "big_blind", "ante", "hands_per_level")
  if (!is.data.frame(blind_schedule) || !all(required_cols %in% names(blind_schedule))) {
    stop(
      "`blind_schedule` must be a data.frame with columns: ",
      paste(required_cols, collapse = ", ")
    )
  }

  if (nrow(blind_schedule) < 1) {
    stop("`blind_schedule` must have at least one row.")
  }

  # -------------------------
  # Build player states
  # -------------------------
  players <- vector("list", length(bot_fns))

  for (i in seq_along(bot_fns)) {
    bot_fn_i <- bot_fns[[i]]

    # Allow NULL for now, but non-human players should generally have a bot function.
    if (!is.null(bot_fn_i) && !is.function(bot_fn_i)) {
      stop(sprintf("bot_fns[[%d]] is not a function (or NULL).", i))
    }

    players[[i]] <- new_player_state(
      player_id = paste0("P", i),
      name = player_names[[i]],
      seat = i,
      stack = starting_stack,
      bot_fn = bot_fn_i,
      is_human = FALSE,
      status = "active",
      hole_cards = character(0),
      folded = FALSE,
      all_in = FALSE,
      acted_this_round = FALSE,
      committed_this_round = 0,
      committed_this_hand = 0,
      finishing_place = NA_integer_
    )
  }

  names(players) <- paste0("seat_", seq_along(players))

  # -------------------------
  # Tournament metadata
  # -------------------------
  if (is.null(tournament_id)) {
    tournament_id <- paste0("T_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }

  if (!is.na(rng_seed)) {
    set.seed(as.integer(rng_seed))
  }

  sb0 <- as.numeric(blind_schedule$small_blind[1])
  bb0 <- as.numeric(blind_schedule$big_blind[1])
  ante0 <- as.numeric(blind_schedule$ante[1])

  # Keep button seat among occupied seats.
  if (!is.numeric(initial_button_seat) || length(initial_button_seat) != 1) {
    stop("`initial_button_seat` must be a single numeric value.")
  }
  initial_button_seat <- as.integer(initial_button_seat)

  if (!(initial_button_seat %in% seq_along(players))) {
    stop("`initial_button_seat` must correspond to one of the occupied seats.")
  }

  # -------------------------
  # Build tournament object
  # -------------------------
  tourn <- new_tournament_state(
    tournament_id = tournament_id,
    players = players,
    max_seats = max_seats,
    starting_stack = starting_stack,
    blind_schedule = blind_schedule,
    small_blind = sb0,
    big_blind = bb0,
    ante = ante0,
    level = 1L,
    hand_number = 0L,
    button_seat = initial_button_seat,
    current_hand = NULL,
    action_log = list(),
    hand_log = list(),
    elimination_order = character(0),
    status = "not_started",
    rng_seed = rng_seed
  )

  validate_tournament_state(tourn)
}



# =====================
# Engine utility helpers
# =====================

get_active_seat_numbers <- function(players) {
  if (!is.list(players)) {
    stop("`players` must be a list.")
  }

  active_seats <- vapply(
    players,
    function(p) {
      inherits(p, "player_state") &&
        identical(p$status, "active") &&
        isTRUE(p$stack > 0)
    },
    logical(1)
  )

  sort(vapply(players[active_seats], function(p) p$seat, integer(1)))
}


get_next_active_seat <- function(players, current_seat) {
  active_seats <- get_active_seat_numbers(players)

  if (length(active_seats) == 0) {
    stop("No active seats found.")
  }

  larger <- active_seats[active_seats > current_seat]

  if (length(larger) > 0) {
    return(larger[1])
  }

  active_seats[1]
}


# ==========================
# Hand initialization helper
# ==========================

initialize_hand <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  players <- tournament_state$players
  active_seats <- get_active_seat_numbers(players)

  if (length(active_seats) < 2) {
    stop("Cannot initialize a hand with fewer than 2 active players.")
  }

  # -----------------------------------
  # Advance hand counter and reset state
  # -----------------------------------
  tournament_state$hand_number <- as.integer(tournament_state$hand_number + 1L)
  players <- reset_players_for_new_hand(players)

  # -----------------------------------
  # Move button to next active seat
  # -----------------------------------
  if (is.na(tournament_state$button_seat)) {
    button_seat <- active_seats[1]
  } else {
    button_seat <- get_next_active_seat(players, tournament_state$button_seat)
  }

  # -----------------------------------
  # Blind positions and first actor
  # -----------------------------------
  if (length(active_seats) == 2L) {
    # Heads-up: button posts the small blind and acts first preflop.
    small_blind_seat <- button_seat
    big_blind_seat <- get_next_active_seat(players, button_seat)
    acting_seat <- small_blind_seat
  } else {
    small_blind_seat <- get_next_active_seat(players, button_seat)
    big_blind_seat <- get_next_active_seat(players, small_blind_seat)
    acting_seat <- get_next_active_seat(players, big_blind_seat)
  }

  # -----------------------------------
  # Build and shuffle deck
  # -----------------------------------
  # Assumes create_deck() exists in cards_and_hands.R and returns 52 card labels.
  if (!exists("create_deck", mode = "function")) {
    stop("`create_deck()` was not found. Please ensure cards_and_hands.R is sourced.")
  }

  deck <- create_deck()
  deck <- sample(deck, length(deck), replace = FALSE)

  # -----------------------------------
  # Deal two hole cards to each active player
  # -----------------------------------
  n_active <- length(active_seats)
  cards_needed <- 2L * n_active

  if (length(deck) < cards_needed) {
    stop("Deck does not contain enough cards to deal this hand.")
  }

  deal_cards <- deck[seq_len(cards_needed)]
  deck <- deck[-seq_len(cards_needed)]

  # Deal in seat order among active players.
  # First pass: one card each. Second pass: one card each.
  first_round <- deal_cards[seq(1, by = 2, length.out = n_active)]
  second_round <- deal_cards[seq(2, by = 2, length.out = n_active)]

  for (i in seq_along(active_seats)) {
    seat_i <- active_seats[i]

    player_index <- which(vapply(players, function(p) p$seat, integer(1)) == seat_i)
    if (length(player_index) != 1) {
      stop("Could not uniquely identify player at seat ", seat_i, ".")
    }

    players[[player_index]]$hole_cards <- c(first_round[i], second_round[i])
    players[[player_index]]$folded <- FALSE
    players[[player_index]]$all_in <- isTRUE(players[[player_index]]$stack <= 0)

    players[[player_index]] <- validate_player_state(players[[player_index]])
  }

  # -----------------------------------
  # Create initial hand state
  # -----------------------------------
  hand_id <- paste0(tournament_state$tournament_id, "_H", tournament_state$hand_number)

  hand_state <- new_hand_state(
    hand_id = hand_id,
    hand_number = tournament_state$hand_number,
    street = "preflop",
    button_seat = button_seat,
    small_blind_seat = small_blind_seat,
    big_blind_seat = big_blind_seat,
    acting_seat = acting_seat,
    min_bet = as.numeric(tournament_state$big_blind),
    current_bet = 0,
    last_full_raise = as.numeric(tournament_state$big_blind),
    pot = 0,
    side_pots = list(),
    board = character(0),
    deck = deck,
    action_history = list(),
    hand_over = FALSE,
    showdown_required = FALSE
  )

  # -----------------------------------
  # Update tournament state
  # -----------------------------------
  tournament_state$players <- players
  tournament_state$button_seat <- button_seat
  tournament_state$current_hand <- hand_state
  tournament_state$status <- "running"

  validate_tournament_state(tournament_state)
}

create_game_state <- function(players, stack_size = 100, game = "holdem") {
  if (!is.character(players) || length(players) < 2) stop("players must be a character vector of length at least 2.")
  if (!is.numeric(stack_size) || length(stack_size) != 1 || is.na(stack_size) || stack_size <= 0) {
    stop("stack_size must be a positive number.")
  }

  list(
    game = game,
    players = players,
    stacks = rep(stack_size, length(players)),
    pot = 0,
    current_bet = 0,
    community_cards = data.frame(rank = character(), suit = character(), card = character()),
    hole_cards = vector("list", length(players)),
    folded = rep(FALSE, length(players)),
    all_in = rep(FALSE, length(players)),
    dealer_button = 1L,
    action_on = 1L,
    street = "preflop",
    deck = shuffle_deck(create_deck()),
    history = list()
  )
}

record_action <- function(game_state, player_index, action) {
  event <- list(player = player_index, action = action, street = game_state$street)
  game_state$history[[length(game_state$history) + 1]] <- event
  game_state
}

get_legal_actions <- function(game_state, player_index) {
  # Minimal temporary version.
  # This should later depend on stack sizes, current bet, prior raises,
  # all-in status, betting-round structure, and game format.
  if (game_state$current_bet > 0) return(c("fold", "call", "raise"))
  c("check", "bet")
}

apply_action_to_state <- function(game_state, player_index, action) {
  # Placeholder logic: records the action but does not yet enforce full betting rules.
  game_state <- record_action(game_state, player_index, action)
  game_state$last_action <- list(player = player_index, action = action)
  game_state
}

advance_action_pointer <- function(game_state) {
  game_state$action_on <- if (game_state$action_on == length(game_state$players)) 1L else game_state$action_on + 1L
  game_state
}

deal_private_cards <- function(game_state) {
  if (game_state$game == "holdem") {
    dealt <- deal_cards(game_state$deck, 2 * length(game_state$players))
    game_state$deck <- dealt$remaining
    game_state$hole_cards <- lapply(split(dealt$dealt, rep(seq_along(game_state$players), each = 2)), function(df) {
      df <- as.data.frame(df)
      rownames(df) <- NULL
      df
    })
    return(game_state)
  }

  if (game_state$game == "omaha") {
    dealt <- deal_cards(game_state$deck, 4 * length(game_state$players))
    game_state$deck <- dealt$remaining
    game_state$hole_cards <- lapply(split(dealt$dealt, rep(seq_along(game_state$players), each = 4)), function(df) {
      df <- as.data.frame(df)
      rownames(df) <- NULL
      df
    })
    return(game_state)
  }

  stop("Unsupported game type in deal_private_cards().")
}

deal_next_street <- function(game_state) {
  # Minimal board dealing helper; does not yet model burn cards.
  n_to_deal <- switch(game_state$street, preflop = 3L, flop = 1L, turn = 1L, river = 0L, 0L)
  if (n_to_deal == 0L) return(game_state)
  dealt <- deal_cards(game_state$deck, n_to_deal)
  game_state$deck <- dealt$remaining
  game_state$community_cards <- rbind(game_state$community_cards, dealt$dealt)
  game_state$street <- switch(game_state$street, preflop = "flop", flop = "turn", turn = "river", river = "showdown", "showdown")
  game_state
}

resolve_showdown <- function(game_state) {
  if (!(game_state$game %in% c("holdem", "omaha"))) stop("Unsupported game type in resolve_showdown().")
  if (nrow(game_state$community_cards) != 5) stop("resolve_showdown() currently expects a full 5-card board.")

  results <- vector("list", length(game_state$players))
  for (i in seq_along(game_state$players)) {
    if (isTRUE(game_state$folded[i])) {
      results[[i]] <- list(player = i, score = -Inf, best_hand = NULL)
      next
    }
    hv <- if (game_state$game == "holdem") {
      holdem_hand_value(game_state$hole_cards[[i]], game_state$community_cards)
    } else {
      omaha_hand_value(game_state$hole_cards[[i]], game_state$community_cards)
    }
    results[[i]] <- list(player = i, score = hv$score, best_hand = hv$best_hand)
  }

  scores <- vapply(results, function(x) x$score, numeric(1))
  winners <- which(scores == max(scores))
  list(results = results, winners = winners, scores = scores)
}

play_random_hand_demo <- function(game = "holdem", players = c("Bot1", "Bot2"), stack_size = 100) {
  # This is a simple demonstration harness, not the final betting engine.
  state <- create_game_state(players = players, stack_size = stack_size, game = game)
  state <- deal_private_cards(state)
  state <- deal_next_street(state)
  state <- deal_next_street(state)
  state <- deal_next_street(state)
  list(state = state, showdown = resolve_showdown(state))
}

run_betting_round <- function(game_state, bots) {
  # Placeholder for real betting-round logic.
  stop("run_betting_round() has not been implemented yet.")
}

award_pot <- function(game_state, winners) {
  # Placeholder for split pots and side pots.
  stop("award_pot() has not been implemented yet.")
}

play_hand <- function(game_state, bots) {
  # Placeholder for the full hand lifecycle.
  stop("play_hand() has not been implemented yet.")
}
# =========================
# Hand action log helper
# =========================

append_action_to_hand <- function(hand_state, action) {
  if (!inherits(hand_state, "hand_state")) {
    stop("`hand_state` must inherit from 'hand_state'.")
  }

  hand_state$action_history[[length(hand_state$action_history) + 1L]] <- action
  validate_hand_state(hand_state)
}


# =========================
# Forced bet posting helper
# =========================

post_blinds_and_antes <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  if (is.null(tournament_state$current_hand)) {
    stop("No current hand found. Call `initialize_hand()` first.")
  }

  hand_state <- tournament_state$current_hand
  players <- tournament_state$players

  if (!inherits(hand_state, "hand_state")) {
    stop("`tournament_state$current_hand` must inherit from 'hand_state'.")
  }

  # (uses global `get_player_index_by_seat()` defined below)
  # -----------------------------------
  # Local helper: post chips from stack
  # -----------------------------------
  post_amount <- function(player, amount) {
    amount <- as.numeric(amount)

    if (amount < 0) {
      stop("Cannot post a negative amount.")
    }

    posted <- min(player$stack, amount)

    player$stack <- player$stack - posted
    player$committed_this_round <- player$committed_this_round + posted
    player$committed_this_hand <- player$committed_this_hand + posted

    if (player$stack <= 0) {
      player$stack <- 0
      player$all_in <- TRUE
    }

    list(player = validate_player_state(player), posted = posted)
  }

  # -----------------------------------
  # Post antes
  # -----------------------------------
  ante <- as.numeric(tournament_state$ante)

  if (ante > 0) {
    active_player_indices <- which(vapply(
      players,
      function(p) inherits(p, "player_state") &&
        identical(p$status, "active") &&
        isTRUE(p$stack > 0),
      logical(1)
    ))

    for (idx in active_player_indices) {
      res <- post_amount(players[[idx]], ante)
      players[[idx]] <- res$player
      posted <- res$posted

      hand_state$pot <- hand_state$pot + posted

      action <- list(
        type = "post_ante",
        player_id = players[[idx]]$player_id,
        player_name = players[[idx]]$name,
        seat = players[[idx]]$seat,
        street = hand_state$street,
        amount = posted
      )

      hand_state <- append_action_to_hand(hand_state, action)
    }
  }

  # -----------------------------------
  # Post small blind
  # -----------------------------------
  sb_seat <- hand_state$small_blind_seat
  sb_idx <- get_player_index_by_seat(players, sb_seat)

  sb_res <- post_amount(players[[sb_idx]], tournament_state$small_blind)
  players[[sb_idx]] <- sb_res$player
  sb_posted <- sb_res$posted

  hand_state$pot <- hand_state$pot + sb_posted

  sb_action <- list(
    type = "post_sb",
    player_id = players[[sb_idx]]$player_id,
    player_name = players[[sb_idx]]$name,
    seat = players[[sb_idx]]$seat,
    street = hand_state$street,
    amount = sb_posted
  )

  hand_state <- append_action_to_hand(hand_state, sb_action)

  # -----------------------------------
  # Post big blind
  # -----------------------------------
  bb_seat <- hand_state$big_blind_seat
  bb_idx <- get_player_index_by_seat(players, bb_seat)

  bb_res <- post_amount(players[[bb_idx]], tournament_state$big_blind)
  players[[bb_idx]] <- bb_res$player
  bb_posted <- bb_res$posted

  hand_state$pot <- hand_state$pot + bb_posted

  bb_action <- list(
    type = "post_bb",
    player_id = players[[bb_idx]]$player_id,
    player_name = players[[bb_idx]]$name,
    seat = players[[bb_idx]]$seat,
    street = hand_state$street,
    amount = bb_posted
  )

  hand_state <- append_action_to_hand(hand_state, bb_action)

  # -----------------------------------
  # Set preflop betting targets
  # -----------------------------------
  # current_bet is what players must match on this street.
  # If the big blind is short, current_bet becomes the amount actually posted.
  hand_state$current_bet <- bb_posted

  # last_full_raise should represent the size of the last full raise increment.
  # At the start of preflop, we use the nominal big blind as the default unit.
  hand_state$last_full_raise <- as.numeric(tournament_state$big_blind)

  # min_bet also begins as the big blind size.
  hand_state$min_bet <- as.numeric(tournament_state$big_blind)

  # Forced bets can make the scheduled first actor ineligible (for example,
  # a heads-up small blind who is all-in after posting). If only one
  # non-all-in player remains, there is no further betting action.
  non_allin_live <- count_live_non_allin_players_in_hand(players)

  if (non_allin_live <= 1) {
    hand_state$acting_seat <- NA_integer_
    hand_state$showdown_required <- TRUE
  } else if (!is.na(hand_state$acting_seat)) {
    acting_idx <- get_player_index_by_seat(players, hand_state$acting_seat)
    acting_player <- players[[acting_idx]]

    acting_player_can_act <-
      identical(acting_player$status, "active") &&
      !isTRUE(acting_player$folded) &&
      !isTRUE(acting_player$all_in) &&
      isTRUE(acting_player$stack > 0)

    if (!acting_player_can_act) {
      hand_state$acting_seat <- get_next_eligible_acting_seat(players, hand_state$acting_seat)
    }
  }

  # -----------------------------------
  # Update tournament state
  # -----------------------------------
  tournament_state$players <- players
  tournament_state$current_hand <- validate_hand_state(hand_state)

  validate_tournament_state(tournament_state)
}



# =========================
# Legal action helper
# =========================

get_legal_actions <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  if (is.null(tournament_state$current_hand)) {
    stop("No current hand found. Call `initialize_hand()` first.")
  }

  hand_state <- tournament_state$current_hand
  players <- tournament_state$players

  if (!inherits(hand_state, "hand_state")) {
    stop("`tournament_state$current_hand` must inherit from 'hand_state'.")
  }

  if (isTRUE(hand_state$hand_over)) {
    stop("This hand is already over.")
  }

  acting_seat <- hand_state$acting_seat

  # -----------------------------------
  # Find acting player
  # -----------------------------------
  idx <- which(vapply(players, function(p) p$seat, integer(1)) == as.integer(acting_seat))
  if (length(idx) != 1) {
    stop("Could not uniquely identify acting player at seat ", acting_seat, ".")
  }

  player <- players[[idx]]

  if (!inherits(player, "player_state")) {
    stop("Acting player does not inherit from 'player_state'.")
  }

  if (!identical(player$status, "active")) {
    stop("Acting player is not active in the tournament.")
  }

  if (isTRUE(player$folded)) {
    stop("Acting player has already folded this hand.")
  }

  if (isTRUE(player$all_in)) {
    stop("Acting player is already all-in.")
  }

  # -----------------------------------
  # Core quantities
  # -----------------------------------
  stack <- as.numeric(player$stack)
  current_bet <- as.numeric(hand_state$current_bet)
  already_committed <- as.numeric(player$committed_this_round)

  to_call <- max(0, current_bet - already_committed)

  # Amount needed to make a full minimum raise:
  # player must first call to current_bet, then increase by last_full_raise
  min_raise_increment <- as.numeric(hand_state$last_full_raise)
  if (is.na(min_raise_increment) || min_raise_increment < 0) {
    stop("Invalid `last_full_raise` in hand state.")
  }

  min_total_to <- current_bet + min_raise_increment
  min_additional_to_raise <- max(0, min_total_to - already_committed)

  max_additional <- stack
  max_total_to <- already_committed + stack

  can_check <- (to_call == 0)
  can_call <- (to_call > 0 && stack > 0)
  can_fold <- (to_call > 0)
  can_bet <- (to_call == 0 && stack > 0)
  can_raise <- (to_call > 0 && stack > to_call)

  # A full legal opening bet on a street where current_bet == 0
  min_open_bet <- as.numeric(hand_state$min_bet)
  if (is.na(min_open_bet) || min_open_bet < 0) {
    stop("Invalid `min_bet` in hand state.")
  }

  # -----------------------------------
  # Build legal action set
  # -----------------------------------
  legal_types <- character(0)
  action_specs <- list()

  # CHECK
  if (can_check) {
    legal_types <- c(legal_types, "check")
    action_specs$check <- list(type = "check")
  }

  # FOLD
  if (can_fold) {
    legal_types <- c(legal_types, "fold")
    action_specs$fold <- list(type = "fold")
  }

  # CALL
  if (can_call) {
    call_amount <- min(stack, to_call)
    legal_types <- c(legal_types, "call")
    action_specs$call <- list(
      type = "call",
      amount = call_amount
    )
  }

  # BET / ALL-IN BET
  if (can_bet) {
    # If stack is smaller than the nominal minimum bet,
    # only all-in is possible.
    if (stack >= min_open_bet && min_open_bet > 0) {
      legal_types <- c(legal_types, "bet")
      action_specs$bet <- list(
        type = "bet",
        min_amount = min_open_bet,
        max_amount = stack
      )
    }

    if (stack > 0) {
      legal_types <- c(legal_types, "all_in")
      action_specs$all_in <- list(
        type = "all_in",
        amount = stack
      )
    }
  }

  # RAISE / ALL-IN RAISE
  if (can_raise) {
    # A full raise is only available if the player has enough chips
    # to reach at least min_total_to.
    if (stack >= min_additional_to_raise) {
      legal_types <- c(legal_types, "raise")
      action_specs$raise <- list(
        type = "raise",
        min_amount = min_total_to,
        max_amount = max_total_to
      )
    }

    # All-in raise attempt is always available whenever player has chips beyond call,
    # even if it does not constitute a full raise.
    legal_types <- c(legal_types, "all_in")
    action_specs$all_in <- list(
      type = "all_in",
      amount = stack
    )
  }

  # Special case:
  # if player cannot fully call, their "call" is effectively all-in for less.
  # We still represent it as "call" with amount = stack.
  if (to_call > 0 && stack > 0 && stack <= to_call) {
    legal_types <- c(setdiff(legal_types, "call"), "call")
    action_specs$call <- list(
      type = "call",
      amount = stack
    )
  }

  # Remove duplicates while preserving first appearance
  legal_types <- unique(legal_types)

  list(
    player_id = player$player_id,
    player_name = player$name,
    seat = player$seat,
    street = hand_state$street,
    stack = stack,
    already_committed = already_committed,
    current_bet = current_bet,
    to_call = to_call,
    min_bet = min_open_bet,
    last_full_raise = min_raise_increment,
    min_total_to_raise = if (can_raise && stack >= min_additional_to_raise) min_total_to else NA_real_,
    max_total_to_raise = if (can_raise) max_total_to else NA_real_,
    legal_action_types = legal_types,
    actions = action_specs
  )
}

# =========================
# Apply one player action
# =========================

apply_action <- function(tournament_state, action) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  if (is.null(tournament_state$current_hand)) {
    stop("No current hand found.")
  }

  hand_state <- tournament_state$current_hand
  players <- tournament_state$players

  if (!inherits(hand_state, "hand_state")) {
    stop("`tournament_state$current_hand` must inherit from 'hand_state'.")
  }

  if (isTRUE(hand_state$hand_over)) {
    stop("Cannot apply an action: this hand is already over.")
  }

  if (!is.list(action) || is.null(action$type)) {
    stop("`action` must be a list with at least a `type` field.")
  }

  legal <- get_legal_actions(tournament_state)
  acting_seat <- hand_state$acting_seat
  acting_idx <- get_player_index_by_seat(players, acting_seat)
  player <- players[[acting_idx]]

  action_type <- as.character(action$type)[1]

  if (!(action_type %in% legal$legal_action_types)) {
    stop(
      "Illegal action '", action_type,
      "' for seat ", acting_seat,
      ". Legal actions are: ",
      paste(legal$legal_action_types, collapse = ", ")
    )
  }

  add_to_pot_from_player <- function(player, add_amount) {
    add_amount <- as.numeric(add_amount)

    if (length(add_amount) != 1 || is.na(add_amount) || add_amount < 0) {
      stop("Invalid chip amount.")
    }

    if (add_amount > player$stack) {
      stop("Player does not have enough chips for this action.")
    }

    player$stack <- player$stack - add_amount
    player$committed_this_round <- player$committed_this_round + add_amount
    player$committed_this_hand <- player$committed_this_hand + add_amount
    player$acted_this_round <- TRUE

    if (player$stack <= 0) {
      player$stack <- 0
      player$all_in <- TRUE
    }

    list(player = validate_player_state(player), add_amount = add_amount)
  }

  reset_other_players_acted_flags_after_aggression <- function(players, aggressor_seat) {
    for (i in seq_along(players)) {
      p <- players[[i]]

      if (!inherits(p, "player_state")) {
        next
      }

      if (!identical(p$status, "active") || isTRUE(p$folded) || isTRUE(p$all_in)) {
        next
      }

      if (p$seat == aggressor_seat) {
        p$acted_this_round <- TRUE
      } else {
        p$acted_this_round <- FALSE
      }

      players[[i]] <- validate_player_state(p)
    }

    players
  }

  # -----------------------------------
  # Resolve by action type
  # -----------------------------------

  if (action_type == "fold") {
    player$folded <- TRUE
    player$acted_this_round <- TRUE
    players[[acting_idx]] <- validate_player_state(player)

    hand_state <- append_action_to_hand(hand_state, list(
      type = "fold",
      player_id = player$player_id,
      player_name = player$name,
      seat = player$seat,
      street = hand_state$street,
      amount = 0
    ))
  }

  else if (action_type == "check") {
    if (legal$to_call != 0) {
      stop("Cannot check when facing a bet.")
    }

    player$acted_this_round <- TRUE
    players[[acting_idx]] <- validate_player_state(player)

    hand_state <- append_action_to_hand(hand_state, list(
      type = "check",
      player_id = player$player_id,
      player_name = player$name,
      seat = player$seat,
      street = hand_state$street,
      amount = 0
    ))
  }

  else if (action_type == "call") {
    call_amount <- legal$actions$call$amount

    res <- add_to_pot_from_player(player, call_amount)
    player <- res$player

    players[[acting_idx]] <- player
    hand_state$pot <- hand_state$pot + call_amount

    hand_state <- append_action_to_hand(hand_state, list(
      type = "call",
      player_id = player$player_id,
      player_name = player$name,
      seat = player$seat,
      street = hand_state$street,
      amount = call_amount
    ))
  }

  else if (action_type == "bet") {
    if (is.null(action$amount)) {
      stop("A `bet` action must include `amount`.")
    }

    bet_total <- as.numeric(action$amount)[1]

    if (is.na(bet_total) ||
        bet_total < legal$actions$bet$min_amount ||
        bet_total > legal$actions$bet$max_amount) {
      stop(
        "Illegal bet amount. Allowed range: ",
        legal$actions$bet$min_amount, " to ", legal$actions$bet$max_amount, "."
      )
    }

    res <- add_to_pot_from_player(player, bet_total)
    player <- res$player

    players[[acting_idx]] <- player
    hand_state$pot <- hand_state$pot + bet_total
    hand_state$current_bet <- player$committed_this_round
    hand_state$last_full_raise <- bet_total

    players <- reset_other_players_acted_flags_after_aggression(players, player$seat)

    hand_state <- append_action_to_hand(hand_state, list(
      type = "bet",
      player_id = player$player_id,
      player_name = player$name,
      seat = player$seat,
      street = hand_state$street,
      amount = bet_total
    ))
  }

  else if (action_type == "raise") {
    if (is.null(action$amount)) {
      stop("A `raise` action must include `amount`.")
    }

    raise_total_to <- as.numeric(action$amount)[1]

    if (is.na(raise_total_to) ||
        raise_total_to < legal$actions$raise$min_amount ||
        raise_total_to > legal$actions$raise$max_amount) {
      stop(
        "Illegal raise amount. Allowed total-to range: ",
        legal$actions$raise$min_amount, " to ", legal$actions$raise$max_amount, "."
      )
    }

    add_amount <- raise_total_to - player$committed_this_round
    old_current_bet <- hand_state$current_bet
    raise_increment <- raise_total_to - old_current_bet

    res <- add_to_pot_from_player(player, add_amount)
    player <- res$player

    players[[acting_idx]] <- player
    hand_state$pot <- hand_state$pot + add_amount
    hand_state$current_bet <- raise_total_to
    hand_state$last_full_raise <- raise_increment

    players <- reset_other_players_acted_flags_after_aggression(players, player$seat)

    hand_state <- append_action_to_hand(hand_state, list(
      type = "raise",
      player_id = player$player_id,
      player_name = player$name,
      seat = player$seat,
      street = hand_state$street,
      amount = raise_total_to
    ))
  }

  else if (action_type == "all_in") {
    all_in_additional <- player$stack
    total_to <- player$committed_this_round + all_in_additional
    old_current_bet <- hand_state$current_bet
    old_last_full_raise <- hand_state$last_full_raise

    res <- add_to_pot_from_player(player, all_in_additional)
    player <- res$player

    players[[acting_idx]] <- player
    hand_state$pot <- hand_state$pot + all_in_additional

    # Case 1: all-in when checking is available -> treat as an aggressive bet
    if (legal$to_call == 0) {
      hand_state$current_bet <- total_to

      # Opening all-in counts as a bet. We use the all-in size as the increment.
      hand_state$last_full_raise <- total_to

      players <- reset_other_players_acted_flags_after_aggression(players, player$seat)

      hand_state <- append_action_to_hand(hand_state, list(
        type = "all_in_bet",
        player_id = player$player_id,
        player_name = player$name,
        seat = player$seat,
        street = hand_state$street,
        amount = total_to
      ))
    }

    # Case 2: all-in facing a bet
    else {
      if (total_to > old_current_bet) {
        raise_increment <- total_to - old_current_bet

        # Full raise only if increment >= previous full raise amount
        if (raise_increment >= old_last_full_raise) {
          hand_state$current_bet <- total_to
          hand_state$last_full_raise <- raise_increment

          players <- reset_other_players_acted_flags_after_aggression(players, player$seat)

          hand_state <- append_action_to_hand(hand_state, list(
            type = "all_in_raise",
            player_id = player$player_id,
            player_name = player$name,
            seat = player$seat,
            street = hand_state$street,
            amount = total_to
          ))
        } else {
          # Short all-in: increases current bet for matching purposes,
          # but does not reopen action under standard no-limit rules.
          hand_state$current_bet <- total_to

          # Do not reset others' acted flags
          hand_state <- append_action_to_hand(hand_state, list(
            type = "all_in_short",
            player_id = player$player_id,
            player_name = player$name,
            seat = player$seat,
            street = hand_state$street,
            amount = total_to
          ))
        }
      } else {
        # All-in for less than or equal to call amount: effectively a call for less
        hand_state <- append_action_to_hand(hand_state, list(
          type = "all_in_call",
          player_id = player$player_id,
          player_name = player$name,
          seat = player$seat,
          street = hand_state$street,
          amount = all_in_additional
        ))
      }
    }
  }

  else {
    stop("Unsupported action type: ", action_type)
  }

  # -----------------------------------
  # Write back intermediate state
  # -----------------------------------
  tournament_state$players <- players
  tournament_state$current_hand <- validate_hand_state(hand_state)

  # -----------------------------------
  # Check whether hand ends immediately
  # -----------------------------------
  if (count_live_players_in_hand(players) == 1) {
    return(mark_hand_winner_by_folds(tournament_state))
  }

  # -----------------------------------
  # Determine whether street action is complete
  # -----------------------------------
  street_complete <-
    all_remaining_players_have_acted(players) &&
    all_bets_are_matched_or_allin(players, tournament_state$current_hand$current_bet)

  if (street_complete) {
    hand_state <- tournament_state$current_hand
    hand_state$acting_seat <- NA_integer_
    tournament_state$current_hand <- validate_hand_state(hand_state)
    return(validate_tournament_state(tournament_state))
  }

  # -----------------------------------
  # Otherwise advance to next acting seat
  # -----------------------------------
  next_seat <- get_next_eligible_acting_seat(players, acting_seat)

  if (is.na(next_seat)) {
    hand_state <- tournament_state$current_hand
    hand_state$acting_seat <- NA_integer_
    tournament_state$current_hand <- validate_hand_state(hand_state)
    return(validate_tournament_state(tournament_state))
  }

  hand_state <- tournament_state$current_hand
  hand_state$acting_seat <- next_seat
  tournament_state$current_hand <- validate_hand_state(hand_state)

  validate_tournament_state(tournament_state)
}

# =========================
# Street advancement helpers
# =========================

get_first_postflop_acting_seat <- function(players, button_seat) {
  if (!is.list(players)) {
    stop("`players` must be a list.")
  }

  eligible_seats <- sort(vapply(
    players[vapply(
      players,
      function(p) {
        inherits(p, "player_state") &&
          identical(p$status, "active") &&
          !isTRUE(p$folded) &&
          !isTRUE(p$all_in) &&
          isTRUE(p$stack > 0)
      },
      logical(1)
    )],
    function(p) p$seat,
    integer(1)
  ))

  if (length(eligible_seats) == 0) {
    return(NA_integer_)
  }

  larger <- eligible_seats[eligible_seats > button_seat]
  if (length(larger) > 0) {
    return(as.integer(larger[1]))
  }

  as.integer(eligible_seats[1])
}
# =========================
# Advance to the next street
# =========================

advance_street <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  if (is.null(tournament_state$current_hand)) {
    stop("No current hand found.")
  }

  hand_state <- tournament_state$current_hand
  players <- tournament_state$players

  if (!inherits(hand_state, "hand_state")) {
    stop("`tournament_state$current_hand` must inherit from 'hand_state'.")
  }

  if (isTRUE(hand_state$hand_over)) {
    stop("Cannot advance street: hand is already over.")
  }

  # -----------------------------------
  # Quick hand-end checks
  # -----------------------------------
  live_players <- which(vapply(
    players,
    function(p) {
      inherits(p, "player_state") &&
        identical(p$status, "active") &&
        !isTRUE(p$folded)
    },
    logical(1)
  ))

  if (length(live_players) <= 1) {
    hand_state$hand_over <- TRUE
    hand_state$acting_seat <- NA_integer_
    tournament_state$current_hand <- validate_hand_state(hand_state)
    return(validate_tournament_state(tournament_state))
  }

  non_allin_live_players <- which(vapply(
    players,
    function(p) {
      inherits(p, "player_state") &&
        identical(p$status, "active") &&
        !isTRUE(p$folded) &&
        !isTRUE(p$all_in) &&
        isTRUE(p$stack > 0)
    },
    logical(1)
  ))

  # If nobody can still act, we can keep running board cards until showdown.
  no_further_action <- (length(non_allin_live_players) <= 1)

  current_street <- as.character(hand_state$street)

  # -----------------------------------
  # Determine next street and deal cards
  # -----------------------------------
  if (current_street == "preflop") {
    if (length(hand_state$deck) < 3) {
      stop("Not enough cards in deck to deal flop.")
    }

    flop_cards <- hand_state$deck[1:3]
    hand_state$deck <- hand_state$deck[-(1:3)]
    hand_state$board <- c(hand_state$board, flop_cards)
    hand_state$street <- "flop"
  }

  else if (current_street == "flop") {
    if (length(hand_state$deck) < 1) {
      stop("Not enough cards in deck to deal turn.")
    }

    turn_card <- hand_state$deck[1]
    hand_state$deck <- hand_state$deck[-1]
    hand_state$board <- c(hand_state$board, turn_card)
    hand_state$street <- "turn"
  }

  else if (current_street == "turn") {
    if (length(hand_state$deck) < 1) {
      stop("Not enough cards in deck to deal river.")
    }

    river_card <- hand_state$deck[1]
    hand_state$deck <- hand_state$deck[-1]
    hand_state$board <- c(hand_state$board, river_card)
    hand_state$street <- "river"
  }

  else if (current_street == "river") {
    hand_state$street <- "showdown"
    hand_state$showdown_required <- TRUE
    hand_state$acting_seat <- NA_integer_
    tournament_state$current_hand <- validate_hand_state(hand_state)
    return(validate_tournament_state(tournament_state))
  }

  else if (current_street == "showdown") {
    stop("Hand is already at showdown.")
  }

  else {
    stop("Unknown street: ", current_street)
  }

  # -----------------------------------
  # Reset round-specific player state
  # -----------------------------------
  for (i in seq_along(players)) {
    p <- players[[i]]

    if (!inherits(p, "player_state")) {
      stop("All players must inherit from 'player_state'.")
    }

    if (identical(p$status, "active") && !isTRUE(p$folded)) {
      p$committed_this_round <- 0
      p$acted_this_round <- FALSE
    }

    if (isTRUE(p$all_in) || isTRUE(p$stack <= 0)) {
      p$all_in <- TRUE
      p$acted_this_round <- TRUE
    }

    players[[i]] <- validate_player_state(p)
  }

  # -----------------------------------
  # Reset street-level betting state
  # -----------------------------------
  hand_state$current_bet <- 0
  hand_state$last_full_raise <- hand_state$min_bet

  # -----------------------------------
  # Set next acting seat
  # -----------------------------------
  if (no_further_action) {
    hand_state$acting_seat <- NA_integer_
  } else {
    hand_state$acting_seat <- get_first_postflop_acting_seat(
      players = players,
      button_seat = hand_state$button_seat
    )
  }

  # -----------------------------------
  # If we somehow advanced to a street with nobody able to act,
  # continue later by calling advance_street() again until showdown.
  # -----------------------------------
  if (is.na(hand_state$acting_seat)) {
    hand_state$showdown_required <- TRUE
  } else {
    hand_state$showdown_required <- FALSE
  }

  tournament_state$players <- players
  tournament_state$current_hand <- validate_hand_state(hand_state)

  validate_tournament_state(tournament_state)
}

# =========================
# Showdown helpers
# =========================

evaluate_holdem_showdown_hand <- function(hole_cards, board) {
  # This wrapper tries a few plausible evaluator names.
  # If none exist in your current project, edit this function so that it calls
  # your actual Hold'em best-hand scoring function.
  #
  # The function must return a value where larger means stronger.

  if (exists("score_best_holdem_hand", mode = "function")) {
    return(score_best_holdem_hand(hole_cards, board))
  }

  if (exists("evaluate_holdem_hand", mode = "function")) {
    return(evaluate_holdem_hand(hole_cards, board))
  }

  if (exists("best_hand_value_holdem", mode = "function")) {
    return(best_hand_value_holdem(hole_cards, board))
  }

  if (exists("holdem_best_hand_score", mode = "function")) {
    return(holdem_best_hand_score(hole_cards, board))
  }

  stop(
    "No recognized Hold'em evaluator was found. ",
    "Please edit `evaluate_holdem_showdown_hand()` so it calls your actual evaluator."
  )
}


seat_order_from_button <- function(seats, button_seat) {
  seats <- sort(unique(as.integer(seats)))

  larger <- seats[seats > button_seat]
  smaller_equal <- seats[seats <= button_seat]

  c(larger, smaller_equal)
}


split_pot_among_winners <- function(pot_amount, winner_seats, button_seat) {
  pot_amount <- as.integer(round(pot_amount))
  winner_seats <- sort(unique(as.integer(winner_seats)))

  n <- length(winner_seats)
  if (n < 1) {
    stop("Cannot split a pot among zero winners.")
  }

  base_share <- pot_amount %/% n
  remainder <- pot_amount %% n

  payouts <- rep(base_share, n)
  names(payouts) <- as.character(winner_seats)

  if (remainder > 0) {
    ordered <- seat_order_from_button(winner_seats, button_seat)
    for (s in ordered[seq_len(remainder)]) {
      payouts[as.character(s)] <- payouts[as.character(s)] + 1L
    }
  }

  payouts
}


build_side_pots <- function(players) {
  # Build pots from committed_this_hand.
  # Returns a list of pots of the form:
  # list(amount = ..., eligible_seats = c(...))

  contrib <- vapply(
    players,
    function(p) {
      if (!inherits(p, "player_state")) stop("All players must be player_state objects.")
      as.numeric(p$committed_this_hand)
    },
    numeric(1)
  )

  seats <- vapply(players, function(p) as.integer(p$seat), integer(1))
  active_for_pot <- vapply(
    players,
    function(p) {
      identical(p$status, "active") && !isTRUE(p$folded)
    },
    logical(1)
  )

  positive_levels <- sort(unique(contrib[contrib > 0]))

  if (length(positive_levels) == 0) {
    return(list())
  }

  pots <- list()
  prev_level <- 0

  for (lvl in positive_levels) {
    contributors <- which(contrib >= lvl)
    layer_size <- lvl - prev_level
    pot_amount <- layer_size * length(contributors)

    eligible <- which(contrib >= lvl & active_for_pot)
    eligible_seats <- seats[eligible]

    pots[[length(pots) + 1L]] <- list(
      amount = as.numeric(pot_amount),
      eligible_seats = as.integer(sort(eligible_seats)),
      level = as.numeric(lvl)
    )

    prev_level <- lvl
  }

  pots
}

# =========================
# Resolve showdown
# =========================

resolve_showdown <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  if (is.null(tournament_state$current_hand)) {
    stop("No current hand found.")
  }

  hand_state <- tournament_state$current_hand
  players <- tournament_state$players

  if (!inherits(hand_state, "hand_state")) {
    stop("`tournament_state$current_hand` must inherit from 'hand_state'.")
  }

  if (isTRUE(hand_state$hand_over)) {
    stop("This hand is already over.")
  }

  live_idx <- which(vapply(
    players,
    function(p) {
      inherits(p, "player_state") &&
        identical(p$status, "active") &&
        !isTRUE(p$folded)
    },
    logical(1)
  ))

  if (length(live_idx) == 0) {
    stop("No live players remain in the hand.")
  }

  # If only one live player remains, this hand should already have been awarded.
  if (length(live_idx) == 1) {
    stop("Only one live player remains; this is not a showdown.")
  }

  if (!(identical(hand_state$street, "showdown") || isTRUE(hand_state$showdown_required))) {
    stop("Hand is not yet ready for showdown.")
  }

  if (length(hand_state$board) != 5) {
    stop("Showdown requires a complete 5-card board.")
  }

  # -----------------------------------
  # Build side pots
  # -----------------------------------
  side_pots <- build_side_pots(players)

  if (length(side_pots) == 0) {
    stop("No pot contributions found in `committed_this_hand`.")
  }

  # -----------------------------------
  # Evaluate live hands
  # -----------------------------------
  hand_scores <- list()

  for (idx in live_idx) {
    p <- players[[idx]]

    if (length(p$hole_cards) != 2) {
      stop("Player ", p$player_id, " does not have exactly 2 hole cards at showdown.")
    }

    score <- evaluate_holdem_showdown_hand(
      hole_cards = p$hole_cards,
      board = hand_state$board
    )

    hand_scores[[as.character(p$seat)]] <- score
  }

  # -----------------------------------
  # Award each side pot
  # -----------------------------------
  payout_by_seat <- numeric(0)
  pot_results <- list()

  for (pot_i in seq_along(side_pots)) {
    pot <- side_pots[[pot_i]]
    eligible_seats <- pot$eligible_seats

    if (length(eligible_seats) == 0) {
      next
    }

    # Extract scores only for showdown-eligible players
    eligible_scores <- hand_scores[intersect(names(hand_scores), as.character(eligible_seats))]

    if (length(eligible_scores) == 0) {
      stop("A side pot has no eligible live showdown hands.")
    }

    score_vec <- unlist(eligible_scores, recursive = FALSE, use.names = TRUE)

    best_score <- max(score_vec)
    winner_seats <- as.integer(names(score_vec)[score_vec == best_score])

    payouts <- split_pot_among_winners(
      pot_amount = pot$amount,
      winner_seats = winner_seats,
      button_seat = hand_state$button_seat
    )

    for (s in names(payouts)) {
      if (!(s %in% names(payout_by_seat))) {
        payout_by_seat[s] <- 0
      }
      payout_by_seat[s] <- payout_by_seat[s] + payouts[s]
    }

    pot_results[[length(pot_results) + 1L]] <- list(
      pot_index = pot_i,
      pot_amount = pot$amount,
      eligible_seats = eligible_seats,
      winner_seats = winner_seats,
      payouts = payouts
    )
  }

  # -----------------------------------
  # Apply payouts
  # -----------------------------------
  for (i in seq_along(players)) {
    seat_i <- as.character(players[[i]]$seat)

    if (seat_i %in% names(payout_by_seat)) {
      players[[i]]$stack <- players[[i]]$stack + as.numeric(payout_by_seat[seat_i])
    }

    players[[i]] <- validate_player_state(players[[i]])
  }

  # -----------------------------------
  # Record showdown results
  # -----------------------------------
  for (res in pot_results) {
    hand_state$action_history[[length(hand_state$action_history) + 1L]] <- list(
      type = "showdown_pot_award",
      street = "showdown",
      pot_index = res$pot_index,
      pot_amount = res$pot_amount,
      eligible_seats = res$eligible_seats,
      winner_seats = res$winner_seats,
      payouts = res$payouts
    )
  }

  # Optional: record revealed hand strengths
  shown_hands <- lapply(names(hand_scores), function(seat_chr) {
    idx <- get_player_index_by_seat(players, as.integer(seat_chr))
    list(
      seat = players[[idx]]$seat,
      player_id = players[[idx]]$player_id,
      player_name = players[[idx]]$name,
      hole_cards = players[[idx]]$hole_cards,
      board = hand_state$board,
      score = hand_scores[[seat_chr]]
    )
  })

  hand_state$action_history[[length(hand_state$action_history) + 1L]] <- list(
    type = "showdown_reveal",
    street = "showdown",
    board = hand_state$board,
    hands = shown_hands
  )

  # -----------------------------------
  # Close the hand
  # -----------------------------------
  hand_state$pot <- 0
  hand_state$side_pots <- side_pots
  hand_state$hand_over <- TRUE
  hand_state$showdown_required <- FALSE
  hand_state$acting_seat <- NA_integer_

  # -----------------------------------
  # Mark eliminations
  # -----------------------------------
  busted_idx <- which(vapply(
    players,
    function(p) {
      inherits(p, "player_state") &&
        identical(p$status, "active") &&
        isTRUE(p$stack <= 0)
    },
    logical(1)
  ))

  if (length(busted_idx) > 0) {
    # Simple convention: append busted players in seat order.
    # If you later want finer placement logic for simultaneous bustouts,
    # this is the place to refine it.
    busted_idx <- busted_idx[order(vapply(players[busted_idx], function(p) p$seat, integer(1)))]

    for (idx in busted_idx) {
      players[[idx]]$status <- "eliminated"

      if (!(players[[idx]]$player_id %in% tournament_state$elimination_order)) {
        tournament_state$elimination_order <- c(
          tournament_state$elimination_order,
          players[[idx]]$player_id
        )
      }

      players[[idx]] <- validate_player_state(players[[idx]])
    }
  }

  # -----------------------------------
  # Assign finishing places if tournament ends
  # -----------------------------------
  active_idx_after <- which(vapply(
    players,
    function(p) inherits(p, "player_state") && identical(p$status, "active"),
    logical(1)
  ))

  if (length(active_idx_after) == 1) {
    winner_idx <- active_idx_after[1]
    players[[winner_idx]]$finishing_place <- 1L
    players[[winner_idx]] <- validate_player_state(players[[winner_idx]])
    tournament_state$status <- "finished"
  }

  # -----------------------------------
  # Final write-back
  # -----------------------------------
  tournament_state$players <- players
  tournament_state$current_hand <- validate_hand_state(hand_state)

  validate_tournament_state(tournament_state)
}



# =========================
# Bot-input helpers
# =========================

build_bot_input <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  hand_state <- tournament_state$current_hand
  if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
    stop("No valid current hand found.")
  }

  legal <- get_legal_actions(tournament_state)
  acting_seat <- hand_state$acting_seat
  acting_idx <- get_player_index_by_seat(tournament_state$players, acting_seat)
  player <- tournament_state$players[[acting_idx]]

  list(
    player_id = player$player_id,
    player_name = player$name,
    seat = player$seat,
    hole_cards = player$hole_cards,
    board = hand_state$board,
    street = hand_state$street,
    pot = hand_state$pot,
    current_bet = hand_state$current_bet,
    committed_this_round = player$committed_this_round,
    committed_this_hand = player$committed_this_hand,
    stack = player$stack,
    small_blind = tournament_state$small_blind,
    big_blind = tournament_state$big_blind,
    ante = tournament_state$ante,
    legal_actions = legal,
    public_players = lapply(tournament_state$players, function(p) {
      list(
        player_id = p$player_id,
        player_name = p$name,
        seat = p$seat,
        stack = p$stack,
        status = p$status,
        folded = p$folded,
        all_in = p$all_in,
        committed_this_round = p$committed_this_round,
        committed_this_hand = p$committed_this_hand
      )
    }),
    action_history = hand_state$action_history
  )
}

bot_input_to_dataframe <- function(bot_input) {
  if (!is.list(bot_input)) {
    stop("`bot_input` must be a list.")
  }

  legal_types <- NA_character_
  if (!is.null(bot_input$legal_actions$legal_action_types)) {
    legal_types <- paste(bot_input$legal_actions$legal_action_types, collapse = ", ")
  }

  bet_min <- NA_real_
  bet_max <- NA_real_
  raise_min <- NA_real_
  raise_max <- NA_real_

  if (!is.null(bot_input$legal_actions$actions$bet)) {
    bet_min <- bot_input$legal_actions$actions$bet$min_amount
    bet_max <- bot_input$legal_actions$actions$bet$max_amount
  }

  if (!is.null(bot_input$legal_actions$actions$raise)) {
    raise_min <- bot_input$legal_actions$actions$raise$min_amount
    raise_max <- bot_input$legal_actions$actions$raise$max_amount
  }

  public_summary <- paste(
    vapply(bot_input$public_players, function(p) {
      paste0(
        p$player_name,
        "[seat=", p$seat,
        ",stack=", p$stack,
        ",status=", p$status,
        ",folded=", p$folded,
        ",all_in=", p$all_in,
        ",ctr=", p$committed_this_round,
        ",cth=", p$committed_this_hand,
        "]"
      )
    }, character(1)),
    collapse = " | "
  )

  action_history_summary <- if (length(bot_input$action_history) == 0) {
    ""
  } else {
    paste(
      vapply(bot_input$action_history, function(a) {
        type_val <- if (!is.null(a$type)) as.character(a$type) else "NA"
        seat_val <- if (!is.null(a$seat)) as.character(a$seat) else "NA"
        amount_val <- if (!is.null(a$amount)) as.character(a$amount) else "NA"
        paste0("type=", type_val, ";seat=", seat_val, ";amount=", amount_val)
      }, character(1)),
      collapse = " | "
    )
  }

  data.frame(
    player_id = bot_input$player_id,
    player_name = bot_input$player_name,
    seat = bot_input$seat,
    hole_cards = paste(bot_input$hole_cards, collapse = ", "),
    board = paste(bot_input$board, collapse = ", "),
    street = bot_input$street,
    pot = bot_input$pot,
    current_bet = bot_input$current_bet,
    committed_this_round = bot_input$committed_this_round,
    committed_this_hand = bot_input$committed_this_hand,
    stack = bot_input$stack,
    small_blind = bot_input$small_blind,
    big_blind = bot_input$big_blind,
    ante = bot_input$ante,
    legal_action_types = legal_types,
    bet_min = bet_min,
    bet_max = bet_max,
    raise_min = raise_min,
    raise_max = raise_max,
    public_players = public_summary,
    action_history = action_history_summary,
    stringsAsFactors = FALSE
  )
}

demo_show_bot_input <- function(tournament_state, as_dataframe = TRUE) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  bot_input <- build_bot_input(tournament_state)

  if (isTRUE(as_dataframe)) {
    out <- bot_input_to_dataframe(bot_input)
    print(out)
    return(invisible(out))
  }

  print(bot_input)
  invisible(bot_input)
}

# =========================
# Safe bot action helper
# =========================

safe_get_bot_action <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  hand_state <- tournament_state$current_hand
  if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
    stop("No valid current hand found.")
  }

  legal <- get_legal_actions(tournament_state)
  acting_seat <- hand_state$acting_seat
  acting_idx <- get_player_index_by_seat(tournament_state$players, acting_seat)
  player <- tournament_state$players[[acting_idx]]

  if (!is.function(player$bot_fn)) {
    # Fallback policy if no bot function is present:
    # check if possible, otherwise call if possible, otherwise fold.
    if ("check" %in% legal$legal_action_types) {
      return(list(type = "check"))
    }
    if ("call" %in% legal$legal_action_types) {
      return(list(type = "call"))
    }
    return(list(type = "fold"))
  }

  bot_input <- build_bot_input(tournament_state)

  bot_action <- tryCatch(
    player$bot_fn(bot_input),
    error = function(e) {
      NULL
    }
  )

  # Defensive fallback if bot output is malformed
  if (!is.list(bot_action) || is.null(bot_action$type)) {
    if ("check" %in% legal$legal_action_types) {
      return(list(type = "check"))
    }
    if ("call" %in% legal$legal_action_types) {
      return(list(type = "call"))
    }
    return(list(type = "fold"))
  }

  action_type <- as.character(bot_action$type)[1]

  # Fallback if illegal type
  if (!(action_type %in% legal$legal_action_types)) {
    if ("check" %in% legal$legal_action_types) {
      return(list(type = "check"))
    }
    if ("call" %in% legal$legal_action_types) {
      return(list(type = "call"))
    }
    return(list(type = "fold"))
  }

  # Normalize numeric amounts for bet/raise if missing or malformed
  if (action_type == "bet") {
    if (is.null(bot_action$amount) || !is.numeric(bot_action$amount) || length(bot_action$amount) != 1 || is.na(bot_action$amount)) {
      bot_action$amount <- legal$actions$bet$min_amount
    }
  }

  if (action_type == "raise") {
    if (is.null(bot_action$amount) || !is.numeric(bot_action$amount) || length(bot_action$amount) != 1 || is.na(bot_action$amount)) {
      bot_action$amount <- legal$actions$raise$min_amount
    }
  }

  bot_action
}
# =========================
# Play one full hand
# =========================

play_current_hand <- function(tournament_state, max_actions = 1000L) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  active_count <- sum(vapply(
    tournament_state$players,
    function(p) inherits(p, "player_state") && identical(p$status, "active") && p$stack > 0,
    logical(1)
  ))

  if (active_count < 2) {
    stop("Cannot play a hand with fewer than 2 active players.")
  }

  starting_stack_summary <- build_hand_stack_summary(tournament_state$players)
  elimination_order_before_hand <- tournament_state$elimination_order %||% character(0)

  # -----------------------------------
  # Initialize and post forced bets
  # -----------------------------------
  tournament_state <- initialize_hand(tournament_state)
  tournament_state <- append_hand_snapshot(
    tournament_state,
    message = "Hand initialized and hole cards dealt.",
    snapshot_type = "hand_start"
  )
  tournament_state <- post_blinds_and_antes(tournament_state)
  tournament_state <- append_hand_snapshot(
    tournament_state,
    message = "Forced bets posted.",
    snapshot_type = "forced_bets"
  )

  action_counter <- 0L

  # -----------------------------------
  # Main hand loop
  # -----------------------------------
  repeat {
    hand_state <- tournament_state$current_hand

    if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
      stop("Current hand is missing or invalid during play.")
    }

    if (isTRUE(hand_state$hand_over)) {
      break
    }

    if (action_counter >= max_actions) {
      stop("Maximum action count exceeded while playing hand.")
    }

    # If showdown is required, resolve it
    if (identical(hand_state$street, "showdown") || isTRUE(hand_state$showdown_required) && length(hand_state$board) == 5) {
      tournament_state <- resolve_showdown(tournament_state)
      tournament_state <- append_hand_snapshot(
        tournament_state,
        message = "Showdown resolved.",
        snapshot_type = "showdown"
      )
      break
    }

    # If no acting seat, street is complete and should advance
    if (is.na(hand_state$acting_seat)) {
      tournament_state <- advance_street(tournament_state)
      tournament_state <- append_hand_snapshot(
        tournament_state,
        message = build_street_snapshot_message(tournament_state$current_hand),
        snapshot_type = "street"
      )
      action_counter <- action_counter + 1L
      next
    }

    # Otherwise get bot action and apply it
    action <- safe_get_bot_action(tournament_state)
    tournament_state <- apply_action(tournament_state, action)
    tournament_state <- append_hand_snapshot(
      tournament_state,
      message = build_last_action_snapshot_message(tournament_state$current_hand),
      snapshot_type = "action"
    )
    action_counter <- action_counter + 1L
  }

  tournament_state <- append_hand_snapshot(
    tournament_state,
    message = "Hand complete.",
    snapshot_type = "hand_end"
  )

  # -----------------------------------
  # Build a simple hand summary
  # -----------------------------------
  hand_state <- tournament_state$current_hand
  players <- tournament_state$players

  stack_summary <- build_hand_stack_summary(players)
  eliminations_this_hand <- setdiff(
    tournament_state$elimination_order %||% character(0),
    elimination_order_before_hand
  )

  hand_summary <- list(
    hand_id = hand_state$hand_id,
    hand_number = hand_state$hand_number,
    level = tournament_state$level,
    small_blind = tournament_state$small_blind,
    big_blind = tournament_state$big_blind,
    ante = tournament_state$ante,
    board = hand_state$board,
    button_seat = hand_state$button_seat,
    small_blind_seat = hand_state$small_blind_seat,
    big_blind_seat = hand_state$big_blind_seat,
    hand_over = hand_state$hand_over,
    final_street = hand_state$street,
    pot = hand_state$pot,
    side_pots = hand_state$side_pots %||% list(),
    action_count = length(hand_state$action_history),
    action_history = hand_state$action_history,
    state_snapshots = hand_state$state_snapshots %||% list(),
    starting_stack_summary = starting_stack_summary,
    ending_stack_summary = stack_summary,
    stack_summary = stack_summary,
    stack_deltas = compute_stack_deltas(starting_stack_summary, stack_summary),
    winners = extract_hand_winner_summary(hand_state$action_history),
    showdown_summary = extract_showdown_summary(hand_state$action_history),
    elimination_order_before_hand = elimination_order_before_hand,
    eliminations_this_hand = eliminations_this_hand
  )

  tournament_state$hand_log[[length(tournament_state$hand_log) + 1L]] <- hand_summary

  # -----------------------------------
  # Mark tournament finished if one player remains
  # -----------------------------------
  active_idx <- which(vapply(
    tournament_state$players,
    function(p) inherits(p, "player_state") && identical(p$status, "active"),
    logical(1)
  ))

  if (length(active_idx) == 1) {
    winner_idx <- active_idx[1]
    tournament_state$players[[winner_idx]]$finishing_place <- 1L
    tournament_state$players[[winner_idx]] <- validate_player_state(tournament_state$players[[winner_idx]])
    tournament_state$status <- "finished"
  }

  validate_tournament_state(tournament_state)
}

# =========================
# Compatibility patch layer
# =========================

# This section harmonizes game_engine.R with cards_and_hands.R.
# cards_and_hands.R uses data.frame cards (columns rank, suit, card)
# for evaluation helpers, while this engine stores cards internally
# as character labels like "Ah" and "Td" in player_state and hand_state.

card_labels_to_df <- function(cards) {
  if (is.null(cards)) {
    return(data.frame(rank = character(0), suit = character(0), card = character(0),
                      stringsAsFactors = FALSE))
  }

  if (is.data.frame(cards)) {
    out <- cards
    if (!all(c("rank", "suit") %in% names(out))) {
      stop("Card data.frame must contain columns `rank` and `suit`.")
    }
    if (!("card" %in% names(out))) {
      out$card <- paste0(out$rank, out$suit)
    }
    rownames(out) <- NULL
    return(out[, c("rank", "suit", "card"), drop = FALSE])
  }

  if (!is.character(cards)) {
    stop("Cards must be supplied as a character vector or card data.frame.")
  }

  if (length(cards) == 0) {
    return(data.frame(rank = character(0), suit = character(0), card = character(0),
                      stringsAsFactors = FALSE))
  }

  cards <- trimws(cards)
  ok <- grepl("^(10|[2-9TJQKA])[hscd]$", cards)
  if (!all(ok)) {
    bad <- unique(cards[!ok])
    stop("Invalid card labels: ", paste(bad, collapse = ", "))
  }

  # Normalize 10 -> T to match cards_and_hands.R rank conventions
  norm_cards <- sub("^10", "T", cards)
  ranks <- substr(norm_cards, 1, nchar(norm_cards) - 1)
  suits <- substr(norm_cards, nchar(norm_cards), nchar(norm_cards))

  out <- data.frame(
    rank = ranks,
    suit = suits,
    card = paste0(ranks, suits),
    stringsAsFactors = FALSE
  )

  if (anyDuplicated(out$card)) {
    stop("Duplicate card detected.")
  }

  out
}

card_df_to_labels <- function(cards_df) {
  df <- card_labels_to_df(cards_df)
  df$card
}

fresh_shuffled_card_labels <- function() {
  if (!exists("create_deck", mode = "function")) {
    stop("`create_deck()` was not found. Please ensure cards_and_hands.R is sourced.")
  }
  deck_df <- create_deck()
  if (!is.data.frame(deck_df) || !all(c("rank", "suit") %in% names(deck_df))) {
    stop("`create_deck()` from cards_and_hands.R must return a card data.frame.")
  }
  sample(card_df_to_labels(deck_df), size = nrow(deck_df), replace = FALSE)
}

validate_action <- function(x) {
  stopifnot(is.list(x))
  stopifnot(is.character(x$player_id), length(x$player_id) == 1)
  stopifnot(is.numeric(x$seat), length(x$seat) == 1, !is.na(x$seat))
  stopifnot(is.character(x$street), length(x$street) == 1)
  stopifnot(x$street %in% c("preflop", "flop", "turn", "river", "showdown"))
  stopifnot(is.character(x$type), length(x$type) == 1)
  stopifnot(x$type %in% c("fold", "check", "call", "bet", "raise", "all_in",
                          "post_sb", "post_bb", "post_ante"))
  stopifnot(is.numeric(x$amount), length(x$amount) == 1, !is.na(x$amount), x$amount >= 0)
  x
}

get_player_index_by_seat <- function(players, seat) {
  idx <- which(vapply(players, function(p) p$seat, integer(1)) == as.integer(seat))
  if (length(idx) != 1) {
    stop("Could not uniquely identify player at seat ", seat, ".")
  }
  idx
}

count_live_players_in_hand <- function(players) {
  sum(vapply(
    players,
    function(p) inherits(p, "player_state") &&
      identical(p$status, "active") &&
      !isTRUE(p$folded),
    logical(1)
  ))
}

count_live_non_allin_players_in_hand <- function(players) {
  sum(vapply(
    players,
    function(p) inherits(p, "player_state") &&
      identical(p$status, "active") &&
      !isTRUE(p$folded) &&
      !isTRUE(p$all_in) &&
      isTRUE(p$stack > 0),
    logical(1)
  ))
}

all_remaining_players_have_acted <- function(players) {
  all(vapply(
    players,
    function(p) {
      if (!inherits(p, "player_state")) return(TRUE)
      if (!identical(p$status, "active") || isTRUE(p$folded) || isTRUE(p$all_in)) return(TRUE)
      isTRUE(p$acted_this_round)
    },
    logical(1)
  ))
}

all_bets_are_matched_or_allin <- function(players, current_bet) {
  current_bet <- as.numeric(current_bet)
  all(vapply(
    players,
    function(p) {
      if (!inherits(p, "player_state")) return(TRUE)
      if (!identical(p$status, "active") || isTRUE(p$folded)) return(TRUE)
      isTRUE(p$all_in) || as.numeric(p$committed_this_round) >= current_bet
    },
    logical(1)
  ))
}

get_next_eligible_acting_seat <- function(players, current_seat) {
  eligible_seats <- sort(vapply(
    players[vapply(
      players,
      function(p) inherits(p, "player_state") &&
        identical(p$status, "active") &&
        !isTRUE(p$folded) &&
        !isTRUE(p$all_in) &&
        isTRUE(p$stack > 0),
      logical(1)
    )],
    function(p) p$seat,
    integer(1)
  ))

  if (length(eligible_seats) == 0) {
    return(NA_integer_)
  }

  larger <- eligible_seats[eligible_seats > current_seat]
  if (length(larger) > 0) {
    return(as.integer(larger[1]))
  }

  as.integer(eligible_seats[1])
}

mark_hand_winner_by_folds <- function(tournament_state) {
  hand_state <- tournament_state$current_hand
  players <- tournament_state$players

  live_idx <- which(vapply(
    players,
    function(p) inherits(p, "player_state") &&
      identical(p$status, "active") &&
      !isTRUE(p$folded),
    logical(1)
  ))

  if (length(live_idx) != 1) {
    stop("mark_hand_winner_by_folds() requires exactly one live player.")
  }

  winner_idx <- live_idx[1]
  players[[winner_idx]]$stack <- players[[winner_idx]]$stack + hand_state$pot
  players[[winner_idx]] <- validate_player_state(players[[winner_idx]])

  hand_state$action_history[[length(hand_state$action_history) + 1L]] <- list(
    type = "win_by_folds",
    street = hand_state$street,
    winner_seat = players[[winner_idx]]$seat,
    winner_player_id = players[[winner_idx]]$player_id,
    amount = hand_state$pot
  )

  hand_state$pot <- 0
  hand_state$hand_over <- TRUE
  hand_state$showdown_required <- FALSE
  hand_state$acting_seat <- NA_integer_

  busted_idx <- which(vapply(
    players,
    function(p) inherits(p, "player_state") &&
      identical(p$status, "active") &&
      isTRUE(p$stack <= 0),
    logical(1)
  ))

  if (length(busted_idx) > 0) {
    busted_idx <- busted_idx[order(vapply(players[busted_idx], function(p) p$seat, integer(1)))]
    for (idx in busted_idx) {
      players[[idx]]$status <- "eliminated"
      if (!(players[[idx]]$player_id %in% tournament_state$elimination_order)) {
        tournament_state$elimination_order <- c(tournament_state$elimination_order, players[[idx]]$player_id)
      }
      players[[idx]] <- validate_player_state(players[[idx]])
    }
  }

  active_idx_after <- which(vapply(
    players,
    function(p) inherits(p, "player_state") && identical(p$status, "active"),
    logical(1)
  ))
  if (length(active_idx_after) == 1) {
    players[[active_idx_after[1]]]$finishing_place <- 1L
    players[[active_idx_after[1]]] <- validate_player_state(players[[active_idx_after[1]]])
    tournament_state$status <- "finished"
  }

  tournament_state$players <- players
  tournament_state$current_hand <- validate_hand_state(hand_state)
  validate_tournament_state(tournament_state)
}

# Override initialize_hand so the engine always stores deck / board / hole cards
# as character card labels compatible with player_state and hand_state.
initialize_hand <- function(tournament_state) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  validate_tournament_state(tournament_state)

  players <- tournament_state$players
  active_seats <- get_active_seat_numbers(players)

  if (length(active_seats) < 2) {
    stop("Cannot initialize a hand with fewer than 2 active players.")
  }

  tournament_state$hand_number <- as.integer(tournament_state$hand_number + 1L)
  players <- reset_players_for_new_hand(players)

  button_seat <- if (is.na(tournament_state$button_seat)) active_seats[1] else get_next_active_seat(players, tournament_state$button_seat)

  if (length(active_seats) == 2L) {
    small_blind_seat <- button_seat
    big_blind_seat <- get_next_active_seat(players, button_seat)
    acting_seat <- small_blind_seat
  } else {
    small_blind_seat <- get_next_active_seat(players, button_seat)
    big_blind_seat <- get_next_active_seat(players, small_blind_seat)
    acting_seat <- get_next_active_seat(players, big_blind_seat)
  }

  deck <- fresh_shuffled_card_labels()

  n_active <- length(active_seats)
  cards_needed <- 2L * n_active
  if (length(deck) < cards_needed) {
    stop("Deck does not contain enough cards to deal this hand.")
  }

  dealt <- deck[seq_len(cards_needed)]
  deck <- deck[-seq_len(cards_needed)]

  first_round <- dealt[seq(1, by = 2, length.out = n_active)]
  second_round <- dealt[seq(2, by = 2, length.out = n_active)]

  for (i in seq_along(active_seats)) {
    seat_i <- active_seats[i]
    player_index <- get_player_index_by_seat(players, seat_i)
    players[[player_index]]$hole_cards <- c(first_round[i], second_round[i])
    players[[player_index]]$folded <- FALSE
    players[[player_index]]$all_in <- isTRUE(players[[player_index]]$stack <= 0)
    players[[player_index]] <- validate_player_state(players[[player_index]])
  }

  hand_id <- paste0(tournament_state$tournament_id, "_H", tournament_state$hand_number)

  hand_state <- new_hand_state(
    hand_id = hand_id,
    hand_number = tournament_state$hand_number,
    street = "preflop",
    button_seat = button_seat,
    small_blind_seat = small_blind_seat,
    big_blind_seat = big_blind_seat,
    acting_seat = acting_seat,
    min_bet = as.numeric(tournament_state$big_blind),
    current_bet = 0,
    last_full_raise = as.numeric(tournament_state$big_blind),
    pot = 0,
    side_pots = list(),
    board = character(0),
    deck = deck,
    action_history = list(),
    hand_over = FALSE,
    showdown_required = FALSE
  )

  tournament_state$players <- players
  tournament_state$button_seat <- button_seat
  tournament_state$current_hand <- hand_state
  tournament_state$status <- "running"

  validate_tournament_state(tournament_state)
}

# Override evaluation wrapper to call the evaluator from cards_and_hands.R
# after converting character labels to the expected data.frame format.
evaluate_holdem_showdown_hand <- function(hole_cards, board) {
  if (!exists("holdem_hand_value", mode = "function")) {
    stop("`holdem_hand_value()` was not found. Please ensure cards_and_hands.R is sourced.")
  }

  hole_df <- card_labels_to_df(hole_cards)
  board_df <- card_labels_to_df(board)

  hv <- holdem_hand_value(hole_df, board_df)
  hv$score
}

# Override demo constructor so it uses card-label vectors consistently.
create_game_state <- function(players, stack_size = 100, game = "holdem") {
  if (!is.character(players) || length(players) < 2) stop("players must be a character vector of length at least 2.")
  if (!is.numeric(stack_size) || length(stack_size) != 1 || is.na(stack_size) || stack_size <= 0) {
    stop("stack_size must be a positive number.")
  }

  list(
    game = game,
    players = players,
    stacks = rep(stack_size, length(players)),
    pot = 0,
    current_bet = 0,
    community_cards = character(0),
    hole_cards = vector("list", length(players)),
    folded = rep(FALSE, length(players)),
    all_in = rep(FALSE, length(players)),
    dealer_button = 1L,
    action_on = 1L,
    street = "preflop",
    deck = fresh_shuffled_card_labels(),
    history = list()
  )
}

deal_private_cards <- function(game_state) {
  if (!(game_state$game %in% c("holdem", "omaha"))) {
    stop("Unsupported game type in deal_private_cards().")
  }

  cards_per_player <- if (game_state$game == "holdem") 2L else 4L
  dealt <- deal_cards(game_state$deck, cards_per_player * length(game_state$players))
  game_state$deck <- dealt$remaining

  raw_cards <- dealt$dealt
  game_state$hole_cards <- split(raw_cards, rep(seq_along(game_state$players), each = cards_per_player))
  game_state
}

deal_next_street <- function(game_state) {
  n_to_deal <- switch(game_state$street, preflop = 3L, flop = 1L, turn = 1L, river = 0L, 0L)
  if (n_to_deal == 0L) return(game_state)
  dealt <- deal_cards(game_state$deck, n_to_deal)
  game_state$deck <- dealt$remaining
  game_state$community_cards <- c(game_state$community_cards, dealt$dealt)
  game_state$street <- switch(game_state$street, preflop = "flop", flop = "turn", turn = "river", river = "showdown", "showdown")
  game_state
}

resolve_showdown <- local({
  old_resolve_showdown <- resolve_showdown
  function(tournament_state) {
    # The body below is the same as before, but it will now use the
    # patched evaluate_holdem_showdown_hand().
    old_resolve_showdown(tournament_state)
  }
})
############################################################
# Mathematics of Poker — Replay Logging Helpers
# Suggested location: game_engine.R
############################################################

capture_hand_snapshot <- function(tournament_state,
                                  message = NULL,
                                  snapshot_type = "state",
                                  include_hole_cards = TRUE) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  hand_state <- tournament_state$current_hand
  if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
    stop("No valid current hand found in `tournament_state$current_hand`.")
  }

  players <- tournament_state$players
  if (is.null(players) || !is.list(players)) {
    stop("`tournament_state$players` must be a list.")
  }

  # Snapshot the player information needed for replay.
  player_snapshots <- lapply(players, function(p) {
    list(
      player_id = p$player_id %||% NA_character_,
      name = p$name %||% p$player_id %||% NA_character_,
      seat = p$seat %||% NA_integer_,
      stack = p$stack %||% NA_real_,
      folded = isTRUE(p$folded),
      all_in = isTRUE(p$all_in),
      eliminated = isTRUE(p$eliminated),
      status = p$status %||% NA_character_,
      committed_this_round = p$committed_this_round %||% 0,
      committed_this_hand = p$committed_this_hand %||% 0,
      hole_cards = if (isTRUE(include_hole_cards)) {
        p$hole_cards %||% character(0)
      } else {
        character(0)
      }
    )
  })

  # Try to identify the next acting player name for convenience in the viewer.
  acting_seat <- hand_state$acting_seat %||% NA_integer_
  acting_player_name <- NA_character_
  if (!is.na(acting_seat)) {
    acting_match <- vapply(
      player_snapshots,
      function(p) identical(p$seat, acting_seat),
      logical(1)
    )
    if (any(acting_match)) {
      acting_player_name <- player_snapshots[[which(acting_match)[1]]]$name
    }
  }

  # Use existing action history length as a natural step counter when available.
  action_count <- length(hand_state$action_history %||% list())

  snapshot <- list(
    step = action_count,
    snapshot_type = snapshot_type,
    message = message %||% "",
    hand_id = hand_state$hand_id %||% NA_character_,
    hand_number = hand_state$hand_number %||% NA_integer_,
    street = hand_state$street %||% NA_character_,
    pot = hand_state$pot %||% 0,
    current_bet = hand_state$current_bet %||% 0,
    min_raise_to = hand_state$min_raise_to %||% NA_real_,
    acting_seat = acting_seat,
    acting_player_name = acting_player_name,
    button_seat = hand_state$button_seat %||% NA_integer_,
    small_blind_seat = hand_state$small_blind_seat %||% NA_integer_,
    big_blind_seat = hand_state$big_blind_seat %||% NA_integer_,
    board = hand_state$board %||% character(0),
    action_count = action_count,
    players = player_snapshots
  )

  snapshot
}


append_hand_snapshot <- function(tournament_state,
                                 message = NULL,
                                 snapshot_type = "state",
                                 include_hole_cards = TRUE) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  hand_state <- tournament_state$current_hand
  if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
    stop("No valid current hand found in `tournament_state$current_hand`.")
  }

  if (is.null(hand_state$state_snapshots)) {
    hand_state$state_snapshots <- list()
  }

  snap <- capture_hand_snapshot(
    tournament_state = tournament_state,
    message = message,
    snapshot_type = snapshot_type,
    include_hole_cards = include_hole_cards
  )

  hand_state$state_snapshots[[length(hand_state$state_snapshots) + 1L]] <- snap
  tournament_state$current_hand <- hand_state
  tournament_state
}


build_hand_stack_summary <- function(players) {
  lapply(players, function(p) {
    list(
      player_id = p$player_id,
      player_name = p$name,
      seat = p$seat,
      stack = p$stack,
      status = p$status,
      finishing_place = p$finishing_place
    )
  })
}


compute_stack_deltas <- function(starting_stack_summary, ending_stack_summary) {
  start_by_player <- setNames(
    vapply(starting_stack_summary, function(p) as.numeric(p$stack %||% 0), numeric(1)),
    vapply(starting_stack_summary, function(p) as.character(p$player_id %||% ""), character(1))
  )

  lapply(ending_stack_summary, function(p) {
    player_id <- as.character(p$player_id %||% "")
    ending_stack <- as.numeric(p$stack %||% 0)
    starting_stack <- unname(start_by_player[player_id])
    if (length(starting_stack) == 0 || is.na(starting_stack)) {
      starting_stack <- ending_stack
    }

    list(
      player_id = player_id,
      player_name = p$player_name %||% p$name %||% player_id,
      seat = p$seat %||% NA_integer_,
      starting_stack = starting_stack,
      ending_stack = ending_stack,
      delta = ending_stack - starting_stack,
      status = p$status %||% NA_character_
    )
  })
}


extract_hand_winner_summary <- function(action_history) {
  if (!is.list(action_history) || length(action_history) == 0) {
    return(list())
  }

  winners <- list()

  for (a in action_history) {
    if (identical(a$type, "win_by_folds")) {
      winners[[length(winners) + 1L]] <- list(
        win_type = "win_by_folds",
        seat = a$winner_seat %||% NA_integer_,
        player_id = a$winner_player_id %||% NA_character_,
        amount = a$amount %||% NA_real_
      )
    }

    if (identical(a$type, "showdown_pot_award")) {
      winners[[length(winners) + 1L]] <- list(
        win_type = "showdown_pot_award",
        pot_index = a$pot_index %||% NA_integer_,
        pot_amount = a$pot_amount %||% NA_real_,
        winner_seats = a$winner_seats %||% integer(0),
        payouts = a$payouts %||% numeric(0)
      )
    }
  }

  winners
}


extract_showdown_summary <- function(action_history) {
  if (!is.list(action_history) || length(action_history) == 0) {
    return(NULL)
  }

  reveal_idx <- which(vapply(action_history, function(a) identical(a$type, "showdown_reveal"), logical(1)))
  if (length(reveal_idx) == 0) {
    return(NULL)
  }

  reveal_action <- action_history[[reveal_idx[length(reveal_idx)]]]
  list(
    board = reveal_action$board %||% character(0),
    hands = reveal_action$hands %||% list()
  )
}


build_street_snapshot_message <- function(hand_state) {
  if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
    return("Street advanced.")
  }

  paste0(
    "Advanced to ",
    toupper(as.character(hand_state$street %||% "next street")),
    "."
  )
}


build_last_action_snapshot_message <- function(hand_state) {
  if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
    return("Action applied.")
  }

  ah <- hand_state$action_history %||% list()
  if (length(ah) == 0) {
    return("Action applied.")
  }

  a <- ah[[length(ah)]]
  actor <- a$player_name %||% a$winner_player_id %||% paste0("Seat ", a$seat %||% a$winner_seat %||% "?")
  action_type <- gsub("_", " ", as.character(a$type %||% "action"))

  if (!is.null(a$winner_seat)) {
    return(paste0(actor, " ", action_type, " for ", a$amount %||% 0, "."))
  }

  if (!is.null(a$amount) && is.numeric(a$amount) && !is.na(a$amount) && a$amount > 0) {
    return(paste0(actor, " ", action_type, " ", a$amount, "."))
  }

  paste0(actor, " ", action_type, ".")
}
