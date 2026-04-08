############################################################
# Mathematics of Poker — Poker Math Utilities
# File: poker_math.R
#
# Purpose:
#   One-street EV, pot-odds, MDF, bluff:value, and geometric sizing tools.
############################################################

assert_scalar_numeric <- function(x, name = "x", nonneg = FALSE, positive = FALSE) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) stop(sprintf("%s must be a single numeric value.", name))
  if (nonneg && x < 0) stop(sprintf("%s must be nonnegative.", name))
  if (positive && x <= 0) stop(sprintf("%s must be positive.", name))
  invisible(TRUE)
}

clamp01 <- function(p) {
  p <- as.numeric(p)
  p[p < 0] <- 0
  p[p > 1] <- 1
  p
}

pct <- function(p, digits = 1) {
  paste0(round(100 * p, digits), "%")
}

pot_odds <- function(call_amount, pot_before_call) {
  assert_scalar_numeric(call_amount, "call_amount", nonneg = TRUE)
  assert_scalar_numeric(pot_before_call, "pot_before_call", nonneg = TRUE)
  denom <- pot_before_call + call_amount
  if (denom == 0) return(0)
  call_amount / denom
}

break_even_equity_call <- function(call_amount, pot_before_call) {
  pot_odds(call_amount, pot_before_call)
}

pot_odds_as_fraction <- function(call_amount, pot_before_call) {
  x <- pot_odds(call_amount, pot_before_call)
  c(numerator = call_amount, denominator = pot_before_call + call_amount, value = x)
}

ev_discrete <- function(outcomes, probs) {
  outcomes <- as.numeric(outcomes)
  probs <- as.numeric(probs)
  if (length(outcomes) != length(probs)) stop("outcomes and probs must have the same length.")
  if (any(is.na(outcomes)) || any(is.na(probs))) stop("Missing values are not allowed.")
  if (any(probs < 0)) stop("Probabilities must be nonnegative.")
  if (abs(sum(probs) - 1) > 1e-8) stop("Probabilities must sum to 1.")
  sum(outcomes * probs)
}

ev_binary <- function(p_success, gain_if_success, loss_if_fail = 0) {
  assert_scalar_numeric(p_success, "p_success", nonneg = TRUE)
  if (p_success > 1) stop("p_success must be at most 1.")
  assert_scalar_numeric(gain_if_success, "gain_if_success")
  assert_scalar_numeric(loss_if_fail, "loss_if_fail")
  p_success * gain_if_success + (1 - p_success) * loss_if_fail
}

ev_call <- function(win_prob, pot_before_call, call_amount) {
  assert_scalar_numeric(win_prob, "win_prob", nonneg = TRUE)
  if (win_prob > 1) stop("win_prob must be at most 1.")
  assert_scalar_numeric(pot_before_call, "pot_before_call", nonneg = TRUE)
  assert_scalar_numeric(call_amount, "call_amount", nonneg = TRUE)
  win_prob * pot_before_call - (1 - win_prob) * call_amount
}

ev_fold <- function() 0

ev_bet_or_bluff <- function(fold_prob, pot_before_bet, bet_amount) {
  assert_scalar_numeric(fold_prob, "fold_prob", nonneg = TRUE)
  if (fold_prob > 1) stop("fold_prob must be at most 1.")
  assert_scalar_numeric(pot_before_bet, "pot_before_bet", nonneg = TRUE)
  assert_scalar_numeric(bet_amount, "bet_amount", nonneg = TRUE)
  fold_prob * pot_before_bet + (1 - fold_prob) * (-bet_amount)
}

break_even_fold_prob_bluff <- function(pot_before_bet, bet_amount) {
  assert_scalar_numeric(pot_before_bet, "pot_before_bet", nonneg = TRUE)
  assert_scalar_numeric(bet_amount, "bet_amount", nonneg = TRUE)
  denom <- pot_before_bet + bet_amount
  if (denom == 0) return(0)
  bet_amount / denom
}

ev_bet_with_equity <- function(call_prob, equity_when_called, pot_before_bet, bet_amount) {
  assert_scalar_numeric(call_prob, "call_prob", nonneg = TRUE)
  if (call_prob > 1) stop("call_prob must be at most 1.")
  assert_scalar_numeric(equity_when_called, "equity_when_called", nonneg = TRUE)
  if (equity_when_called > 1) stop("equity_when_called must be at most 1.")
  assert_scalar_numeric(pot_before_bet, "pot_before_bet", nonneg = TRUE)
  assert_scalar_numeric(bet_amount, "bet_amount", nonneg = TRUE)

  fold_prob <- 1 - call_prob
  fold_prob * pot_before_bet +
    call_prob * (equity_when_called * (pot_before_bet + bet_amount) - (1 - equity_when_called) * bet_amount)
}

bluff_to_value_ratio <- function(pot_before, bet) {
  assert_scalar_numeric(pot_before, "pot_before", nonneg = TRUE)
  assert_scalar_numeric(bet, "bet", nonneg = TRUE)
  denom <- pot_before + bet
  if (denom == 0) return(0)
  bet / denom
}

bluff_fraction_in_betting_range <- function(pot_before, bet) {
  r <- bluff_to_value_ratio(pot_before, bet)
  r / (1 + r)
}

minimum_defense_frequency <- function(pot_before, bet) {
  assert_scalar_numeric(pot_before, "pot_before", nonneg = TRUE)
  assert_scalar_numeric(bet, "bet", nonneg = TRUE)
  denom <- pot_before + bet
  if (denom == 0) return(1)
  pot_before / denom
}

geometric_bet_size <- function(pot, target_multiplier) {
  assert_scalar_numeric(pot, "pot", nonneg = TRUE)
  assert_scalar_numeric(target_multiplier, "target_multiplier", positive = TRUE)
  (target_multiplier - 1) * pot
}

geometric_pot_ladder <- function(pot0, multipliers) {
  assert_scalar_numeric(pot0, "pot0", nonneg = TRUE)
  multipliers <- as.numeric(multipliers)
  if (any(is.na(multipliers)) || any(multipliers <= 0)) stop("multipliers must be positive numbers.")

  pot <- pot0
  out <- vector("list", length(multipliers))
  for (i in seq_along(multipliers)) {
    m <- multipliers[i]
    b <- (m - 1) * pot
    pot_next <- m * pot
    out[[i]] <- data.frame(street = i, pot_before = pot, bet = b, multiplier = m, pot_after = pot_next)
    pot <- pot_next
  }
  do.call(rbind, out)
}

geometric_solve_multiplier <- function(pot, stack, streets_left) {
  assert_scalar_numeric(pot, "pot", positive = TRUE)
  assert_scalar_numeric(stack, "stack", nonneg = TRUE)
  assert_scalar_numeric(streets_left, "streets_left", positive = TRUE)
  streets_left <- as.integer(streets_left)
  ((pot + 2 * stack) / pot)^(1 / streets_left)
}

effective_stack <- function(stack_hero, stack_villain) {
  assert_scalar_numeric(stack_hero, "stack_hero", nonneg = TRUE)
  assert_scalar_numeric(stack_villain, "stack_villain", nonneg = TRUE)
  min(stack_hero, stack_villain)
}

spr <- function(effective_stack_amt, pot) {
  assert_scalar_numeric(effective_stack_amt, "effective_stack_amt", nonneg = TRUE)
  assert_scalar_numeric(pot, "pot", positive = TRUE)
  effective_stack_amt / pot
}

report_call_threshold <- function(call_amount, pot_before_call, digits = 1) {
  be <- break_even_equity_call(call_amount, pot_before_call)
  cat("Break-even equity to call:", pct(be, digits), "\n")
  invisible(be)
}

report_bluff_threshold <- function(pot_before_bet, bet_amount, digits = 1) {
  be <- break_even_fold_prob_bluff(pot_before_bet, bet_amount)
  cat("Break-even fold probability for bluff:", pct(be, digits), "\n")
  invisible(be)
}
