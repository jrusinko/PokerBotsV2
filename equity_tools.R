############################################################
# Mathematics of Poker — Equity Tools
# File: equity_tools.R
#
# Purpose:
#   Monte Carlo equity for Hold'em and Omaha, built on the
#   integer-based evaluator from cards_and_hands.R.
#
# Dependencies:
#   source("cards_and_hands.R")
############################################################

holdem_equity_mc_fast <- function(hole_list, board_df = data.frame(rank = character(), suit = character()), n_sims = 10000) {
  if (!is.list(hole_list) || length(hole_list) < 2) stop("hole_list must be a list of at least two player hands.")
  if (!is.numeric(n_sims) || length(n_sims) != 1 || is.na(n_sims) || n_sims <= 0) stop("n_sims must be a positive integer.")
  n_sims <- as.integer(n_sims)

  hole_ids_list <- lapply(hole_list, df_to_ids)
  board_ids_known <- if (nrow(board_df) == 0) integer(0) else df_to_ids(board_df)
  used_ids <- c(unlist(hole_ids_list, use.names = FALSE), board_ids_known)
  if (anyDuplicated(used_ids)) stop("Duplicate known cards detected in equity input.")

  full_deck <- seq_len(52)
  unseen <- setdiff(full_deck, used_ids)
  n_board_needed <- 5 - length(board_ids_known)
  n_players <- length(hole_ids_list)
  wins <- numeric(n_players)

  for (sim in seq_len(n_sims)) {
    drawn <- if (n_board_needed > 0) sample(unseen, n_board_needed, replace = FALSE) else integer(0)
    board_ids <- c(board_ids_known, drawn)
    scores <- vapply(hole_ids_list, function(h) holdem_best_score_ids(h, board_ids)$score, numeric(1))
    winners <- which(scores == max(scores))
    wins[winners] <- wins[winners] + 1 / length(winners)
  }

  data.frame(player = seq_len(n_players), equity = wins / n_sims)
}

omaha_equity_mc_fast <- function(hole_list, board_df = data.frame(rank = character(), suit = character()), n_sims = 10000) {
  if (!is.list(hole_list) || length(hole_list) < 2) stop("hole_list must be a list of at least two player hands.")
  if (!is.numeric(n_sims) || length(n_sims) != 1 || is.na(n_sims) || n_sims <= 0) stop("n_sims must be a positive integer.")
  n_sims <- as.integer(n_sims)

  hole_ids_list <- lapply(hole_list, df_to_ids)
  board_ids_known <- if (nrow(board_df) == 0) integer(0) else df_to_ids(board_df)
  used_ids <- c(unlist(hole_ids_list, use.names = FALSE), board_ids_known)
  if (anyDuplicated(used_ids)) stop("Duplicate known cards detected in equity input.")

  full_deck <- seq_len(52)
  unseen <- setdiff(full_deck, used_ids)
  n_board_needed <- 5 - length(board_ids_known)
  n_players <- length(hole_ids_list)
  wins <- numeric(n_players)

  for (sim in seq_len(n_sims)) {
    drawn <- if (n_board_needed > 0) sample(unseen, n_board_needed, replace = FALSE) else integer(0)
    board_ids <- c(board_ids_known, drawn)
    scores <- vapply(hole_ids_list, function(h) omaha_best_score_ids(h, board_ids)$score, numeric(1))
    winners <- which(scores == max(scores))
    wins[winners] <- wins[winners] + 1 / length(winners)
  }

  data.frame(player = seq_len(n_players), equity = wins / n_sims)
}
