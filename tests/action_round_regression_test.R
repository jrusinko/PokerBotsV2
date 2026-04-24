source("poker_load_all.R")
poker_load_all(include_demos = FALSE, verbose = FALSE)

players <- list(
  new_player_state("P1", "Rando", 1, 0, status = "active", all_in = TRUE, acted_this_round = TRUE, committed_this_round = 10000, committed_this_hand = 10000),
  new_player_state("P2", "Aggro", 2, 0, status = "active", all_in = TRUE, acted_this_round = TRUE, committed_this_round = 10000, committed_this_hand = 10000),
  new_player_state("P3", "PrePlanner", 3, 0, status = "active", all_in = TRUE, acted_this_round = TRUE, committed_this_round = 10000, committed_this_hand = 10000),
  new_player_state("P4", "GetAlong", 4, 0, status = "active", all_in = TRUE, acted_this_round = TRUE, committed_this_round = 10000, committed_this_hand = 10000),
  new_player_state("P5", "Da streets", 5, 0, status = "active", all_in = TRUE, acted_this_round = TRUE, committed_this_round = 10000, committed_this_hand = 10000),
  new_player_state("P7", "Confused", 7, 9800, status = "active", acted_this_round = FALSE, committed_this_round = 200, committed_this_hand = 200),
  new_player_state("P8", "MoreConfused", 8, 9700, status = "active", acted_this_round = FALSE, committed_this_round = 300, committed_this_hand = 300)
)

names(players) <- paste0("seat_", vapply(players, function(p) p$seat, integer(1)))

hand_state <- new_hand_state(
  hand_id = "REGRESSION_H1",
  hand_number = 1L,
  street = "preflop",
  button_seat = 1L,
  small_blind_seat = 3L,
  big_blind_seat = 4L,
  acting_seat = 7L,
  min_bet = 100,
  current_bet = 10000,
  last_full_raise = 9700,
  pot = sum(vapply(players, function(p) p$committed_this_hand, numeric(1))),
  side_pots = list(),
  board = character(0),
  deck = character(0),
  action_history = list(
    list(type = "raise", seat = 8L, street = "preflop", amount = 300),
    list(type = "all_in_raise", seat = 1L, street = "preflop", amount = 10000),
    list(type = "call", seat = 2L, street = "preflop", amount = 9800),
    list(type = "call", seat = 3L, street = "preflop", amount = 9800),
    list(type = "call", seat = 4L, street = "preflop", amount = 9800),
    list(type = "call", seat = 5L, street = "preflop", amount = 9800)
  ),
  hand_over = FALSE,
  showdown_required = FALSE
)

tournament_state <- new_tournament_state(
  tournament_id = "REGRESSION_T",
  players = players,
  max_seats = 10L,
  starting_stack = 10000,
  blind_schedule = default_blind_schedule,
  small_blind = 50,
  big_blind = 100,
  ante = 0,
  level = 1L,
  hand_number = 1L,
  button_seat = 1L,
  current_hand = hand_state,
  action_log = list(),
  hand_log = list(),
  elimination_order = character(0),
  status = "running",
  rng_seed = NA_integer_
)

updated <- apply_action(tournament_state, list(type = "fold"))

stopifnot(
  identical(updated$current_hand$showdown_required, FALSE),
  identical(updated$current_hand$acting_seat, 8L),
  identical(updated$players[[which(vapply(updated$players, function(p) p$seat, integer(1)) == 8L)]]$folded, FALSE)
)

cat("Regression test passed: unmatched non-all-in player still receives action after intervening fold.\n")
