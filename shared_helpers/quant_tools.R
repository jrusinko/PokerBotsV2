############################################################
# Mathematics of Poker — Quantitative Tools
# File: quant_tools.R
#
# Purpose:
#   Derived utilities for uncertainty, outs, ranges, and board texture.
#
# Dependencies:
#   source("poker_math.R")
############################################################

mc_se <- function(samples) {
  x <- as.numeric(samples)
  n <- length(x)
  if (n < 2) return(NA_real_)
  sd(x) / sqrt(n)
}

mc_ci_normal <- function(samples, level = 0.95) {
  x <- as.numeric(samples)
  n <- length(x)
  if (n < 2) stop("Need at least 2 samples for a CI.")
  m <- mean(x)
  se <- mc_se(x)
  alpha <- 1 - level
  z <- qnorm(1 - alpha / 2)
  c(mean = m, se = se, level = level, lo = m - z * se, hi = m + z * se)
}

mc_sims_needed <- function(target_halfwidth = 0.005, level = 0.95, worst_case = TRUE, pilot_se = NULL) {
  assert_scalar_numeric(target_halfwidth, "target_halfwidth", positive = TRUE)
  alpha <- 1 - level
  z <- qnorm(1 - alpha / 2)
  if (worst_case) {
    sd0 <- 0.5
  } else {
    if (is.null(pilot_se)) stop("Provide pilot_se if worst_case = FALSE.")
    stop("If worst_case = FALSE, use mc_sims_needed_from_sd() with pilot_sd.")
  }
  ceiling((z * sd0 / target_halfwidth)^2)
}

mc_sims_needed_from_sd <- function(target_halfwidth = 0.005, level = 0.95, pilot_sd) {
  assert_scalar_numeric(target_halfwidth, "target_halfwidth", positive = TRUE)
  assert_scalar_numeric(pilot_sd, "pilot_sd", nonneg = TRUE)
  alpha <- 1 - level
  z <- qnorm(1 - alpha / 2)
  ceiling((z * pilot_sd / target_halfwidth)^2)
}

outs_to_prob <- function(outs, unseen, cards_to_come = 1) {
  assert_scalar_numeric(outs, "outs", nonneg = TRUE)
  assert_scalar_numeric(unseen, "unseen", positive = TRUE)
  assert_scalar_numeric(cards_to_come, "cards_to_come", positive = TRUE)
  outs <- as.integer(outs)
  unseen <- as.integer(unseen)
  k <- as.integer(cards_to_come)
  if (outs > unseen) stop("outs cannot exceed unseen.")
  if (k > unseen) stop("cards_to_come cannot exceed unseen.")
  if (outs == 0) return(0)
  1 - (choose(unseen - outs, k) / choose(unseen, k))
}

holdem_unseen_cards <- function(n_known_cards) {
  assert_scalar_numeric(n_known_cards, "n_known_cards", nonneg = TRUE)
  n_known_cards <- as.integer(n_known_cards)
  if (n_known_cards > 52) stop("n_known_cards cannot exceed 52.")
  52 - n_known_cards
}

pot_fraction <- function(bet, pot_before) {
  assert_scalar_numeric(bet, "bet", nonneg = TRUE)
  assert_scalar_numeric(pot_before, "pot_before", nonneg = TRUE)
  if (pot_before == 0) return(Inf)
  bet / pot_before
}

bet_from_pot_fraction <- function(fraction, pot_before) {
  assert_scalar_numeric(fraction, "fraction", nonneg = TRUE)
  assert_scalar_numeric(pot_before, "pot_before", nonneg = TRUE)
  fraction * pot_before
}

stack_fraction <- function(bet, stack) {
  assert_scalar_numeric(bet, "bet", nonneg = TRUE)
  assert_scalar_numeric(stack, "stack", positive = TRUE)
  bet / stack
}

ev_best_action <- function(ev_named) {
  if (!is.numeric(ev_named) || is.null(names(ev_named))) stop("ev_named must be a named numeric vector.")
  best <- which.max(ev_named)
  list(action = names(ev_named)[best], ev = ev_named[best], all = ev_named)
}

ev_regret <- function(ev_named, chosen_action) {
  if (!is.numeric(ev_named) || is.null(names(ev_named))) stop("ev_named must be a named numeric vector.")
  if (!(chosen_action %in% names(ev_named))) stop("chosen_action must be a name in ev_named.")
  max(ev_named) - ev_named[chosen_action]
}

risk_adjusted_utility <- function(mean_ev, sd_ev, lambda = 0) {
  assert_scalar_numeric(mean_ev, "mean_ev")
  assert_scalar_numeric(sd_ev, "sd_ev", nonneg = TRUE)
  assert_scalar_numeric(lambda, "lambda", nonneg = TRUE)
  mean_ev - lambda * sd_ev
}

new_range_holdem <- function(combos_df, label = NULL, normalize = TRUE) {
  needed <- c("c1", "c2", "w")
  if (!all(needed %in% names(combos_df))) stop("Hold'em range needs columns c1, c2, w.")
  out <- list(game = "holdem", combos = combos_df, weights = combos_df$w, label = label)
  class(out) <- "poker_range"
  if (normalize) out <- range_normalize(out)
  out
}


range_normalize <- function(range_obj) {
  s <- sum(range_obj$weights)
  if (s <= 0) stop("Range weights must sum to a positive value.")
  range_obj$weights <- range_obj$weights / s
  range_obj$combos$w <- range_obj$weights
  range_obj
}

range_size <- function(range_obj) {
  nrow(range_obj$combos)
}

print.poker_range <- function(x, ...) {
  cat("<poker_range>", x$game, "| combos:", range_size(x), "| label:", ifelse(is.null(x$label), "(none)", x$label), "\n")
  invisible(x)
}

board_rank_values <- function(board_df) {
  rank_value(board_df$rank)
}

board_is_paired <- function(board_df) {
  any(table(board_df$rank) >= 2)
}

board_is_trips_or_more <- function(board_df) {
  any(table(board_df$rank) >= 3)
}

board_suit_counts <- function(board_df) {
  table(board_df$suit)
}

board_is_monotone <- function(board_df) {
  length(unique(board_df$suit)) == 1
}

board_is_two_tone <- function(board_df) {
  length(unique(board_df$suit)) == 2
}

board_high_card <- function(board_df) {
  max(rank_value(board_df$rank))
}

board_connectivity <- function(board_df) {
  vals <- sort(unique(rank_value(board_df$rank)))
  if (length(vals) <= 1) return(0)
  sum(diff(vals) <= 2)
}

board_features <- function(board_df) {
  list(
    paired = board_is_paired(board_df),
    trips_or_more = board_is_trips_or_more(board_df),
    monotone = board_is_monotone(board_df),
    two_tone = board_is_two_tone(board_df),
    high_card = board_high_card(board_df),
    connectivity = board_connectivity(board_df)
  )
}
