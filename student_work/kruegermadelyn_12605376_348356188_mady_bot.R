############################################################
# GTO-Inspired Advanced Poker Bot
# File: gto_bot.R
#
# Theory basis:
#   - Pot-odds / MDF (minimum defense frequency) from poker_math.R
#   - Geometric bet sizing across streets
#   - Equity-based decisions via Monte Carlo (equity_tools.R)
#   - ICM chip-equity pressure (from icm_bluff_bot.R)
#   - Board texture awareness (quant_tools.R)
#   - Polarized betting: bet strong hands + balanced bluffs
#   - Continuation bet theory: bet when range advantage exists
#   - SPR-based commitment thresholds
#   - Position-aware aggression
#   - Hand-category-driven sizing (thin value vs. strong value vs. bluff)
#
# Dependencies (load via poker_load_all() before sourcing):
#   poker_math.R, equity_tools.R, quant_tools.R, example_bots.R
############################################################


# ============================================================
# SECTION 1: ICM TOOL (carried over from icm_bluff_bot.R)
# ============================================================

icm_chip_equity <- function(stacks, payouts = NULL, hero_idx = 1L) {
  stacks    <- as.numeric(stacks)
  hero_idx  <- as.integer(hero_idx)
  if (any(is.na(stacks)) || any(stacks < 0)) stop("Invalid stacks.")
  if (sum(stacks) == 0) stop("Total chips must be positive.")
  if (hero_idx < 1L || hero_idx > length(stacks)) stop("hero_idx out of range.")

  total_chips   <- sum(stacks)
  chip_fraction <- stacks[hero_idx] / total_chips
  pressure      <- 1 - chip_fraction

  if (is.null(payouts)) {
    return(list(chip_fraction = chip_fraction, icm_equity = NA_real_,
                icm_all = NA_real_, pressure_factor = pressure))
  }

  payouts  <- as.numeric(payouts)
  n_places <- min(length(payouts), length(stacks))
  active   <- which(stacks > 0)

  mh_prob <- function(pidx, place, active_idxs, sv) {
    if (place == 1L) {
      s <- sv[active_idxs]; if (sum(s) == 0) return(0)
      return(sv[pidx] / sum(s))
    }
    tot <- 0
    for (w in active_idxs) {
      if (w == pidx) next
      s   <- sv[active_idxs]
      p_w <- sv[w] / sum(s)
      tot <- tot + p_w * mh_prob(pidx, place - 1L, setdiff(active_idxs, w), sv)
    }
    tot
  }

  icm_all <- numeric(length(stacks))
  for (p in active) {
    ev <- 0
    for (pl in seq_len(n_places)) ev <- ev + mh_prob(p, pl, active, stacks) * payouts[pl]
    icm_all[p] <- ev
  }

  list(chip_fraction = chip_fraction, icm_equity = icm_all[hero_idx],
       icm_all = icm_all, pressure_factor = pressure)
}


# ============================================================
# SECTION 2: HAND STRENGTH & DRAW EVALUATION
# ============================================================

# Map card labels to rank values
.rank_val <- function(cards) {
  r <- substring(cards, 1, nchar(cards) - 1)
  v <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
         "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)
  unname(v[r])
}

# Preflop hand tier
# Returns: "premium" | "strong" | "medium" | "weak"
.preflop_tier <- function(hole_cards) {
  if (length(hole_cards) != 2) return("weak")
  vals   <- sort(.rank_val(hole_cards), decreasing = TRUE)
  suits  <- substring(hole_cards, nchar(hole_cards), nchar(hole_cards))
  suited <- length(unique(suits)) == 1
  paired <- vals[1] == vals[2]

  # Premium: AA/KK/QQ/JJ/TT, AKo, AQo+, AJs+, KQs
  if (paired && vals[1] >= 10)                          return("premium")
  if (!paired && vals[1] == 14 && vals[2] >= 13)        return("premium")  # AK
  if (!paired && vals[1] == 14 && vals[2] >= 12)        return("premium")  # AQ
  if (!paired && suited && vals[1] == 14 && vals[2] >= 11) return("premium") # AJs
  if (!paired && suited && vals[1] == 13 && vals[2] == 12) return("premium") # KQs

  # Strong: 77-99, KQo, KJs, QJs, ATo-A9o, A8s-A2s, KTs
  if (paired && vals[1] >= 7)                            return("strong")
  if (!paired && vals[1] == 13 && vals[2] >= 12)         return("strong")  # KQ
  if (!paired && suited && vals[1] == 13 && vals[2] >= 10) return("strong") # KTs+
  if (!paired && suited && vals[1] == 12 && vals[2] >= 10) return("strong") # QTs+
  if (!paired && vals[1] == 14 && vals[2] >= 9)          return("strong")  # A9+
  if (!paired && suited && vals[1] == 14 && vals[2] >= 2) return("strong") # Axs

  # Medium: 22-66, connectors 67-JT suited, broadway combos
  if (paired)                                             return("medium")
  if (suited && vals[1] - vals[2] <= 2 && vals[2] >= 5)  return("medium") # suited connectors/gappers
  if (vals[1] >= 11 && vals[2] >= 10)                    return("medium") # JTo

  "weak"
}

# Count flush draw outs for hero
.flush_draw_outs <- function(hole_cards, board) {
  all_cards <- c(hole_cards, board)
  suits     <- substring(all_cards, nchar(all_cards), nchar(all_cards))
  hole_suit <- substring(hole_cards, nchar(hole_cards), nchar(hole_cards))
  # Need 2 hole cards of same suit AND 2 board cards of same suit
  if (length(unique(hole_suit)) == 1) {
    s <- hole_suit[1]
    if (sum(suits == s) == 4) return(9L)  # flush draw
  }
  0L
}

# Count open-ended straight draw outs
.straight_draw_outs <- function(hole_cards, board) {
  all_vals  <- sort(unique(.rank_val(c(hole_cards, board))))
  if (14 %in% all_vals) all_vals <- sort(unique(c(1L, all_vals)))
  max_consec <- 0; cur <- 1
  for (i in seq_along(all_vals)[-1]) {
    if (all_vals[i] - all_vals[i-1] == 1) { cur <- cur + 1; max_consec <- max(max_consec, cur) }
    else cur <- 1
  }
  if (max_consec >= 4) return(8L)   # OESD
  if (max_consec == 3) return(4L)   # gutshot
  0L
}

# Total draw outs (deduped with made hand consideration)
.draw_outs <- function(hole_cards, board, hand_cat) {
  if (hand_cat %in% c("flush","straight","full_house","quads","straight_flush")) return(0L)
  fd <- .flush_draw_outs(hole_cards, board)
  sd <- .straight_draw_outs(hole_cards, board)
  # Approximate dedup
  as.integer(max(fd, sd) + floor(min(fd, sd) * 0.5))
}

# Cards to come based on street
.cards_to_come <- function(street) {
  switch(street, preflop = 5L, flop = 2L, turn = 1L, river = 0L, 0L)
}

# ============================================================
# SECTION 3: VILLAIN RANGES BY AGGRESSION PROFILE
# ============================================================

# We maintain three prebuilt villain ranges.
# In a real engine you'd update based on observed betting history;
# here we select based on the opponent's stack-to-average ratio
# as a rough proxy for tight/loose tendencies.

.villain_range_tight <- function() {
  new_range_holdem(
    data.frame(
      c1 = c("Ah","As","Kh","Ks","Qh","Ad","Ac","Kd","Kc","Qd"),
      c2 = c("Ac","Kd","Kd","Qd","Qd","Kh","Qs","Qh","Jh","Jd"),
      w  = c(6,   5,   4,   3,   3,   5,   4,   3,   2,   2),
      stringsAsFactors = FALSE
    ), label = "Tight villain"
  )
}

.villain_range_medium <- function() {
  new_range_holdem(
    data.frame(
      c1 = c("Ah","As","Kh","Qh","Jh","Th","Ad","9h","8h","7h"),
      c2 = c("Kd","Kd","Qd","Jd","Td","9d","Qd","8d","7d","6d"),
      w  = c(4,   4,   3,   3,   2,   2,   3,   1,   1,   1),
      stringsAsFactors = FALSE
    ), label = "Medium villain"
  )
}

.villain_range_loose <- function() {
  new_range_holdem(
    data.frame(
      c1 = c("Ah","Kh","Qh","Jh","Th","9h","8h","7h","6h","5h","2h","3h"),
      c2 = c("2d","3d","4d","5d","6d","7d","8d","9d","5d","4d","3d","4d"),
      w  = c(3,   3,   2,   2,   2,   2,   1,   1,   1,   1,   1,   1),
      stringsAsFactors = FALSE
    ), label = "Loose villain"
  )
}

# Pick villain range based on average-stack ratio
.pick_villain_range <- function(villain_stack, avg_stack) {
  ratio <- if (avg_stack > 0) villain_stack / avg_stack else 1
  if (ratio > 1.3) return(.villain_range_tight())   # big stack = tight/solid player
  if (ratio < 0.6) return(.villain_range_loose())   # short stack = desperate/loose
  .villain_range_medium()
}


# ============================================================
# SECTION 4: MC EQUITY WITH CACHING
# ============================================================

# Simple session cache: key = paste(sorted hole cards + board cards)
.equity_cache <- new.env(hash = TRUE, parent = emptyenv())

.mc_equity <- function(hole_cards, board, villain_range, n_sims = 600) {
  cache_key <- paste(sort(c(hole_cards, board)), collapse = "_")
  cached    <- tryCatch(get(cache_key, envir = .equity_cache), error = function(e) NULL)
  if (!is.null(cached)) return(cached)

  board_df <- tryCatch({
    if (length(board) > 0) parse_cards(board)
    else data.frame(rank = character(), suit = character(), stringsAsFactors = FALSE)
  }, error = function(e)
    data.frame(rank = character(), suit = character(), stringsAsFactors = FALSE))

  hero_df <- tryCatch(parse_cards(hole_cards), error = function(e) NULL)
  if (is.null(hero_df) || nrow(hero_df) != 2) return(0.5)

  eq_result <- tryCatch(
    holdem_equity_mc_fast(list(hero_df, villain_range),
                          board_df = board_df, n_sims = n_sims),
    error = function(e) NULL
  )

  eq <- if (!is.null(eq_result)) eq_result$equity[1] else 0.5
  assign(cache_key, eq, envir = .equity_cache)
  eq
}


# ============================================================
# SECTION 5: BET SIZING ENGINE
# ============================================================
# GTO theory: size bets to put villain in tough spots.
# - Thin value / marginal: ~33% pot
# - Standard value:         ~66% pot
# - Strong value / polar:   ~100% pot
# - Bluff:                  match the value size you represent

.target_bet <- function(sizing_class, pot, min_bet, max_bet) {
  fraction <- switch(sizing_class,
    thin_value = 0.33,
    standard   = 0.60,
    strong     = 1.00,
    overbet    = 1.50,
    bluff      = 0.66,
    0.60
  )
  raw <- round(fraction * pot)
  as.integer(max(min_bet, min(max_bet, raw)))
}


# ============================================================
# SECTION 6: POSITION DETECTION
# ============================================================
# In position (IP) = acting last = more aggression warranted.
# We approximate via seat order relative to the dealer button.
# bot_input$players is ordered; the last non-folded active player
# to act is IP.

.is_in_position <- function(bot_input) {
  players  <- bot_input$players
  my_seat  <- as.integer(bot_input$seat)
  active   <- Filter(function(p) !isTRUE(p$folded) && !isTRUE(p$all_in), players)
  if (length(active) == 0) return(TRUE)
  last_seat <- as.integer(active[[length(active)]]$seat)
  my_seat == last_seat
}


# ============================================================
# SECTION 7: CONTINUATION BET LOGIC
# ============================================================
# C-bet when:
#   (a) we were the preflop aggressor (approximated by: we raised preflop),
#   (b) board hits our range more than villain's range (range advantage),
#   (c) equity > 45% on the flop,
#   (d) board is dry (low connectivity, rainbow/monotone in our favor).
#
# This is approximated with board features + equity threshold.

.should_cbet <- function(equity, board_feats, in_position, street) {
  if (!identical(street, "flop")) return(FALSE)
  # Avoid c-betting into very connected / two-tone boards out of position
  if (!in_position && isTRUE(board_feats$two_tone) && board_feats$connectivity >= 2) return(FALSE)
  equity >= 0.45
}


# ============================================================
# SECTION 8: BLUFF SELECTION — POLARIZED RANGE
# ============================================================
# GTO bluffing: only bluff with hands that have:
#   (a) blockers to villain's strong hands (e.g. Ax blocks nut flush),
#   (b) some draw equity (semi-bluffs preferred over pure air),
#   (c) correct bluff frequency given MDF.
#
# mdf = minimum_defense_frequency = pot / (pot + bet)
# If we bluff too much, villain exploits by calling.
# If we bluff too little, villain exploits by folding.
# Optimal bluff ratio ≈ bet / (pot + bet) of our betting range.

.bluff_ev_positive <- function(pot, bet_amount, fold_prob) {
  # EV of bluff = fold_prob * pot - (1-fold_prob) * bet
  ev_bet_or_bluff(fold_prob, pot, bet_amount) > 0
}

.select_bluff_sizing <- function(pot, min_bet, max_bet) {
  # Use ~2/3 pot bluff sizing — balances fold equity and price paid
  .target_bet("bluff", pot, min_bet, max_bet)
}

# Has a blocker to nuts? (simplified: holds an Ace or a card matching board suit count)
.has_blocker <- function(hole_cards, board) {
  hole_ranks <- substring(hole_cards, 1, nchar(hole_cards) - 1)
  "A" %in% hole_ranks  # Ace blocks many strong hands
}

# Semi-bluff: has draw equity
.is_semi_bluff_candidate <- function(draw_outs, street) {
  ctc <- .cards_to_come(street)
  if (ctc == 0 || draw_outs == 0) return(FALSE)
  # Need meaningful equity from draw
  if (draw_outs == 0) return(FALSE)
  unseen <- holdem_unseen_cards(2 + if (street == "flop") 3 else if (street == "turn") 4 else 5)
  draw_prob <- tryCatch(outs_to_prob(draw_outs, unseen, ctc), error = function(e) 0)
  draw_prob >= 0.12  # at least 12% draw equity
}


# ============================================================
# SECTION 9: SPR-BASED COMMITMENT THRESHOLD
# ============================================================
# SPR (Stack-to-Pot Ratio) tells us how committed we are.
# Low SPR (< 3):  commit with top pair or better
# Mid SPR (3-10): commit with two pair or better
# High SPR (>10): commit with straights, flushes, or better

.commitment_threshold <- function(spr_val) {
  if (spr_val < 3)  return("pair")
  if (spr_val < 6)  return("two_pair")
  if (spr_val < 10) return("trips")
  "straight"
}

.hand_rank_int <- function(cat) {
  r <- c(high_card=0, pair=1, two_pair=2, trips=3,
         straight=4, flush=5, full_house=6, quads=7, straight_flush=8)
  if (cat %in% names(r)) r[[cat]] else 0
}

.meets_commitment <- function(hand_cat, spr_val) {
  threshold <- .commitment_threshold(spr_val)
  .hand_rank_int(hand_cat) >= .hand_rank_int(threshold)
}


# ============================================================
# SECTION 10: MAIN GTO BOT
# ============================================================

mady_bot <- function(bot_input) {

  # ---- Unpack ----
  hole_cards  <- bot_input$hole_cards
  board       <- bot_input$board
  street      <- bot_input$street
  pot         <- bot_input$pot
  stack       <- bot_input$stack
  big_blind   <- bot_input$big_blind
  current_bet <- bot_input$current_bet
  committed   <- bot_input$committed_this_round

  legal_types <- bot_input$legal_actions$legal_action_types
  call_amount <- max(0, current_bet - committed)

  can_bet    <- bot_has_action(bot_input, "bet")
  can_raise  <- bot_has_action(bot_input, "raise")
  can_call   <- bot_has_action(bot_input, "call")
  can_check  <- bot_has_action(bot_input, "check")
  can_all_in <- bot_has_action(bot_input, "all_in")

  min_bet   <- if (can_bet)   bot_min_bet(bot_input)   else NA_integer_
  max_bet   <- if (can_bet)   bot_max_bet(bot_input)   else NA_integer_
  min_raise <- if (can_raise) bot_min_raise(bot_input) else NA_integer_
  max_raise <- if (can_raise) bot_max_raise(bot_input) else NA_integer_

  # ---- ICM pressure ----
  player_info <- bot_input$players
  all_stacks  <- vapply(player_info, function(p) as.numeric(p$stack), numeric(1))
  hero_seat   <- bot_input$seat
  hero_idx    <- which(vapply(player_info,
                              function(p) as.integer(p$seat) == as.integer(hero_seat),
                              logical(1)))
  if (length(hero_idx) == 0) hero_idx <- 1L

  icm          <- icm_chip_equity(all_stacks, payouts = NULL, hero_idx = hero_idx)
  icm_pressure <- icm$pressure_factor   # 0 = chip leader, ~1 = very short

  # ---- Position ----
  in_position <- .is_in_position(bot_input)
  pos_bonus   <- if (in_position) 0.03 else -0.03  # equity bonus for IP

  # ---- SPR ----
  spr_val <- tryCatch(spr(stack, max(pot, 1)), error = function(e) 10)

  # ---- Active villain count (for multi-way adjustments) ----
  active_players <- Filter(function(p)
    !isTRUE(p$folded) && !isTRUE(p$all_in) &&
    as.integer(p$seat) != as.integer(hero_seat), player_info)
  n_villains <- length(active_players)

  # Multi-way: be tighter (need stronger hand to bet into multiple opponents)
  multiway_penalty <- if (n_villains > 1) 0.08 * (n_villains - 1) else 0

  # ---- Villain range selection (use first active villain's stack) ----
  avg_stack <- if (length(all_stacks) > 0) mean(all_stacks[all_stacks > 0]) else stack
  villain_stack <- if (length(active_players) > 0)
    as.numeric(active_players[[1]]$stack) else avg_stack
  villain_range <- .pick_villain_range(villain_stack, avg_stack)

  # ===========================================================
  # PREFLOP LOGIC
  # ===========================================================
  if (identical(street, "preflop")) {
    tier <- .preflop_tier(hole_cards)

    # --- Raise sizing: 2.5-3x BB standard, 4x when many limpers ---
    n_limpers  <- max(0, n_villains - 1)
    raise_mult <- 2.5 + 0.5 * n_limpers
    target_raise_amt <- round(raise_mult * big_blind)

    # --- Action by tier ---
    if (tier == "premium") {
      # Always raise/re-raise premiums; go all-in if SPR very low or huge hand
      if (can_raise) {
        amt <- max(min_raise, min(max_raise, target_raise_amt))
        return(list(type = "raise", amount = amt))
      }
      if (can_bet) {
        amt <- max(min_bet, min(max_bet, target_raise_amt))
        return(list(type = "bet", amount = amt))
      }
      if (can_all_in) return(list(type = "all_in"))
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }

    if (tier == "strong") {
      # Raise strong hands but fold to heavy 3-bet pressure (ICM-gated)
      if (can_raise && icm_pressure < 0.70) {
        amt <- max(min_raise, min(max_raise, target_raise_amt))
        return(list(type = "raise", amount = amt))
      }
      if (can_bet) {
        amt <- max(min_bet, min(max_bet, target_raise_amt))
        return(list(type = "bet", amount = amt))
      }
      # Call reasonable open raises
      if (can_call) {
        required_eq <- pot_odds(call_amount, pot)
        if (required_eq <= 0.35) return(list(type = "call"))
      }
      if (can_check) return(list(type = "check"))
      return(list(type = "fold"))
    }

    if (tier == "medium") {
      # Speculative: limp/call small; fold to big raises
      if (can_check) return(list(type = "check"))
      if (can_call && call_amount <= 2.5 * big_blind && icm_pressure < 0.65) {
        return(list(type = "call"))
      }
      return(list(type = "fold"))
    }

    # Weak: check if free, else fold
    if (can_check) return(list(type = "check"))
    return(list(type = "fold"))
  }

  # ===========================================================
  # POSTFLOP LOGIC
  # ===========================================================

  # ---- Board features ----
  board_df <- tryCatch({
    if (length(board) > 0) parse_cards(board)
    else data.frame(rank = character(), suit = character(), stringsAsFactors = FALSE)
  }, error = function(e)
    data.frame(rank = character(), suit = character(), stringsAsFactors = FALSE))

  board_feats <- if (nrow(board_df) >= 3) {
    tryCatch(board_features(board_df),
             error = function(e)
               list(paired=FALSE, trips_or_more=FALSE, monotone=FALSE,
                    two_tone=FALSE, high_card=14, connectivity=0))
  } else {
    list(paired=FALSE, trips_or_more=FALSE, monotone=FALSE,
         two_tone=FALSE, high_card=14, connectivity=0)
  }

  wet_board <- isTRUE(board_feats$two_tone) || board_feats$connectivity >= 2
  dry_board <- !wet_board

  # ---- Made hand strength ----
  hand_cat <- tryCatch(
    made_hand_category(hole_cards, board),
    error = function(e) "high_card"
  )
  hand_rank <- .hand_rank_int(hand_cat)

  strong_hands  <- c("two_pair","trips","straight","flush",
                     "full_house","quads","straight_flush")
  medium_hands  <- c("pair")
  is_strong     <- hand_cat %in% strong_hands
  is_medium     <- hand_cat %in% medium_hands
  is_weak       <- !is_strong && !is_medium

  # ---- Equity (MC) ----
  # Use more sims on river (no draws to account for; equity is exact)
  n_sims_by_street <- switch(street, river = 800, turn = 600, flop = 500, 400)
  mc_eq <- .mc_equity(hole_cards, board, villain_range, n_sims = n_sims_by_street)

  # Position and multiway adjustments
  adj_eq <- clamp01(mc_eq + pos_bonus - multiway_penalty)

  # ICM adjustment: demand more equity when under pressure
  icm_adj <- icm_pressure * 0.12

  # ---- Draw analysis ----
  draw_outs <- .draw_outs(hole_cards, board, hand_cat)
  ctc       <- .cards_to_come(street)
  draw_prob <- if (ctc > 0 && draw_outs > 0) {
    unseen <- holdem_unseen_cards(2 + length(board))
    tryCatch(outs_to_prob(draw_outs, unseen, ctc), error = function(e) 0)
  } else 0

  total_equity <- clamp01(adj_eq + draw_prob * 0.5)  # weight draw equity at 50%

  # ---- Pot odds required to call ----
  required_eq_call <- if (can_call && call_amount > 0)
    pot_odds(call_amount, pot) else 0

  # ---- MDF check (are we over-folding?) ----
  # minimum_defense_frequency: how often we must continue to deny auto-profit bluffs
  mdf <- if (can_call && call_amount > 0)
    tryCatch(minimum_defense_frequency(pot, call_amount), error = function(e) 0.5)
  else 0.5

  # If villain bets and our equity > MDF → calling/raising is mandatory
  must_defend <- (can_call || can_raise) && total_equity >= mdf

  # ============================================================
  # DECISION TREE
  # ============================================================

  # ---- 1. VERY STRONG HAND → Value bet / raise aggressively ----
  is_monster <- hand_rank >= 6  # full_house, quads, straight_flush
  is_very_strong <- hand_rank >= 4  # straight, flush, full_house+

  if (is_monster && spr_val < 5) {
    # Near-committed — just go all in
    if (can_all_in) return(list(type = "all_in"))
    if (can_raise)  return(list(type = "raise", amount = max_raise))
  }

  if (is_very_strong && total_equity >= (0.68 - icm_adj)) {
    if (can_raise) {
      amt <- .target_bet("strong", pot, min_raise, max_raise)
      return(list(type = "raise", amount = amt))
    }
    if (can_bet) {
      amt <- .target_bet("strong", pot, min_bet, max_bet)
      return(list(type = "bet", amount = amt))
    }
    # Can't bet: check-raise opportunity lost, just call
    if (can_call)  return(list(type = "call"))
    if (can_check) return(list(type = "check"))
  }

  # ---- 2. STRONG MADE HAND → Standard value bet ----
  if (is_strong && total_equity >= (0.58 - icm_adj)) {
    sizing <- if (n_villains > 1) "standard" else "strong"
    if (can_raise) {
      amt <- .target_bet(sizing, pot, min_raise, max_raise)
      return(list(type = "raise", amount = amt))
    }
    if (can_bet) {
      amt <- .target_bet(sizing, pot, min_bet, max_bet)
      return(list(type = "bet", amount = amt))
    }
    if (can_call) return(list(type = "call"))
    if (can_check) return(list(type = "check"))
  }

  # ---- 3. MEDIUM HAND (pair) → Thin value in position, check/call OOP ----
  if (is_medium && total_equity >= (0.50 - icm_adj)) {
    if (in_position && dry_board && can_bet) {
      amt <- .target_bet("thin_value", pot, min_bet, max_bet)
      return(list(type = "bet", amount = amt))
    }
    # Check-call in position or OOP
    if (can_check) return(list(type = "check"))
    if (can_call) {
      # Only call if equity beats required threshold with margin
      if (total_equity >= required_eq_call + 0.05) return(list(type = "call"))
    }
  }

  # ---- 4. SEMI-BLUFF (draw with equity) ----
  is_semi <- .is_semi_bluff_candidate(draw_outs, street)
  if (is_semi && !identical(street, "river")) {
    # Semi-bluff with draws: bet/raise to fold out better hands or improve
    fold_needed <- break_even_fold_prob_bluff(pot,
                     if (can_bet) as.numeric(.select_bluff_sizing(pot, min_bet, max_bet))
                     else as.numeric(min_raise))
    # Only semi-bluff if fold equity is achievable (< 60% fold needed on wet boards)
    if (fold_needed < 0.55 && icm_pressure < 0.75) {
      if (in_position || dry_board) {
        if (can_raise) {
          amt <- .target_bet("bluff", pot, min_raise, max_raise)
          return(list(type = "raise", amount = amt))
        }
        if (can_bet) {
          amt <- .select_bluff_sizing(pot, min_bet, max_bet)
          return(list(type = "bet", amount = amt))
        }
      }
    }
    # Even if not betting, call with a draw if pot odds justify it
    if (can_call) {
      pot_eq_needed <- pot_odds(call_amount, pot)
      combined_eq   <- clamp01(adj_eq + draw_prob)  # full draw credit for calls
      if (combined_eq >= pot_eq_needed) return(list(type = "call"))
    }
    if (can_check) return(list(type = "check"))
  }

  # ---- 5. CONTINUATION BET ----
  if (.should_cbet(total_equity, board_feats, in_position, street)) {
    if (can_bet) {
      # Smaller c-bet on wet boards (protection), larger on dry
      sizing <- if (dry_board) "standard" else "thin_value"
      amt    <- .target_bet(sizing, pot, min_bet, max_bet)
      return(list(type = "bet", amount = amt))
    }
  }

  # ---- 6. BLUFF (pure, polarized, balanced) ----
  # GTO bluffing: frequency controlled by MDF math.
  # We bluff with blockers or when ICM permits and board is dry.
  # Bluff frequency ≈ bet/(pot+bet) to keep villain indifferent.
  bluff_sizing_amt <- if (can_bet)
    as.numeric(.select_bluff_sizing(pot, min_bet, max_bet))
  else if (can_raise) as.numeric(min_raise) else 0

  optimal_bluff_freq <- if ((pot + bluff_sizing_amt) > 0)
    bluff_sizing_amt / (pot + bluff_sizing_amt) else 0.33

  should_bluff <- (
    is_weak &&
    runif(1) < optimal_bluff_freq &&
    dry_board &&
    .has_blocker(hole_cards, board) &&
    icm_pressure < 0.65 &&
    (in_position || runif(1) < 0.40) &&  # bluff more in position
    n_villains == 1  # never bluff multi-way (someone always calls)
  )

  if (should_bluff) {
    if (can_bet) {
      amt <- .select_bluff_sizing(pot, min_bet, max_bet)
      return(list(type = "bet", amount = amt))
    }
    if (can_raise && call_amount == 0) {
      # Only bluff-raise if we weren't already facing a bet
      amt <- .target_bet("bluff", pot, min_raise, max_raise)
      return(list(type = "raise", amount = amt))
    }
  }

  # ---- 7. DEFEND vs VILLAIN BET (MDF logic) ----
  if (must_defend && can_call) {
    # Call if equity exceeds pot odds requirement
    if (total_equity >= required_eq_call) return(list(type = "call"))
  }

  # ---- 8. SPR COMMITMENT (pot committed → call even weak) ----
  if (can_call && .meets_commitment(hand_cat, spr_val)) {
    if (total_equity >= required_eq_call - 0.05) return(list(type = "call"))
  }

  # ---- 9. DEFAULT: Check/fold ----
  if (can_check) return(list(type = "check"))
  if (can_call && total_equity >= required_eq_call && total_equity >= 0.35) {
    return(list(type = "call"))
  }
  list(type = "fold")
}


############################################################
# USAGE EXAMPLE (runs only if this block is un-commented)
############################################################
if (FALSE) {
  source("poker_load_all.R")
  poker_load_all(include_demos = TRUE, verbose = FALSE)

  result <- run_tournament(
    bot_fns = list(
      gto_bot,
      random_bot,
      always_call_bot,
      passive_bot,
      aggressive_bot,
      strength_by_street_bot
    ),
    player_names = c(
      "GTO Bot",
      "Random Bot",
      "Caller Bot",
      "Passive Bot",
      "Aggro Bot",
      "Strength Bot"
    ),
    starting_stack = 5000,
    rng_seed       = 42,
    max_hands      = 300,
    verbose        = FALSE
  )

  standings <- data.frame(
    player = vapply(result$players, function(p) p$name,            character(1)),
    chips  = vapply(result$players, function(p) p$stack,           numeric(1)),
    place  = vapply(result$players, function(p) p$finishing_place, integer(1))
  )
  print(standings[order(standings$place), ])
}
