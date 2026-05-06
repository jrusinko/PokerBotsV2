# ============================================================
# MADY BOT v3 — GTO-ADAPTIVE MTT BOT WITH OPPONENT MODELING
# ============================================================
# Key improvements over v2:
#   1. Opponent profile system (aggression, fold freq, call freq)
#   2. MTT stack pressure: M-ratio, push/fold zones, ICM awareness
#   3. Exploitative sizing adjustments based on opponent tendencies
#   4. Street-aware continuation betting with opponent fold history
#   5. Dynamic bluff frequency gated by opponent calling tendencies
#   6. Position-weighted decision making throughout
# ============================================================


# ============================================================
# SECTION 1: OPPONENT MODELING
# ============================================================

# Initialize a blank opponent profile (call once per new villain)
.new_opponent_profile <- function() {
  list(
    hands_seen       = 0,
    vpip             = 0.0,   # Voluntarily Put $ In Pot (estimated)
    aggression_freq  = 0.0,   # (Bets + Raises) / (Bets + Raises + Calls + Checks)
    fold_to_cbet     = 0.5,   # Prior: folds to cbet 50% of time
    fold_to_3bet     = 0.6,   # Prior: folds to 3bet 60% of time
    went_to_sd       = 0.3,   # Went to showdown with a hand
    cbet_count       = 0,
    cbet_fold_count  = 0,
    bet_count        = 0,
    raise_count      = 0,
    call_count       = 0,
    check_fold_count = 0
  )
}

# Update profile after observing villain action
# action: "bet", "raise", "call", "check", "fold_to_cbet", "fold_to_3bet", "showdown"
.update_opponent <- function(profile, action) {
  profile$hands_seen <- profile$hands_seen + 1

  if (action %in% c("bet", "raise")) {
    profile$bet_count   <- profile$bet_count + (action == "bet")
    profile$raise_count <- profile$raise_count + (action == "raise")
  } else if (action == "call") {
    profile$call_count  <- profile$call_count + 1
  } else if (action == "check") {
    # neutral
  } else if (action == "fold_to_cbet") {
    profile$cbet_fold_count <- profile$cbet_fold_count + 1
    profile$cbet_count      <- profile$cbet_count + 1
  } else if (action == "fold_to_3bet") {
    profile$check_fold_count <- profile$check_fold_count + 1
  }

  # Recompute derived stats (Bayesian update with priors)
  total_actions <- profile$bet_count + profile$raise_count +
    profile$call_count + profile$check_fold_count + 1
  agg_actions   <- profile$bet_count + profile$raise_count

  # Weighted toward priors when sample is small (shrink toward 0.5)
  weight <- min(profile$hands_seen / 20, 1.0)

  profile$aggression_freq <- weight * (agg_actions / total_actions) + (1 - weight) * 0.33

  if (profile$cbet_count > 0) {
    raw_ftc <- profile$cbet_fold_count / profile$cbet_count
    profile$fold_to_cbet <- weight * raw_ftc + (1 - weight) * 0.50
  }

  profile
}

# Classify opponent into archetype for easy strategy switching
.opponent_archetype <- function(profile) {
  af  <- profile$aggression_freq
  ftc <- profile$fold_to_cbet

  if (af > 0.55 && ftc < 0.35) return("LAG")         # Loose-Aggressive: tough, fights back
  if (af < 0.25 && ftc > 0.65) return("WEAK_TIGHT")  # Exploitable: folds too much
  if (af < 0.30 && ftc < 0.40) return("CALLING_STATION") # Never folds, rarely bets
  if (af > 0.45 && ftc > 0.55) return("BLUFFY_AGGRO") # Bluffs a lot, also folds a lot
  return("UNKNOWN")                                   # Not enough data / balanced
}


# ============================================================
# SECTION 2: MTT STACK PRESSURE
# ============================================================

# M-ratio: how many full orbits can we survive before blinding out
.m_ratio <- function(stack, big_blind, small_blind, antes = 0, players = 9) {
  orbit_cost <- big_blind + small_blind + (antes * players)
  stack / orbit_cost
}

# Stack-to-Pot Ratio (SPR): governs commitment decisions
.spr <- function(stack, pot) {
  if (pot <= 0) return(Inf)
  stack / pot
}

# Push-or-fold zone: M < 10 should strongly consider shove-or-fold
.push_fold_mode <- function(m) m < 10

# Bubble factor adjustment: tighten ranges when ICM pressure is high
# bubble_proximity: 0 (not near bubble) to 1.0 (on the bubble)
.icm_fold_premium <- function(bubble_proximity, stack_vs_avg) {
  # Short stacks near bubble fold more; big stacks can apply pressure
  base_premium <- bubble_proximity * 0.15
  if (stack_vs_avg < 0.5) base_premium <- base_premium * 1.5  # Desperate short stack tightens
  if (stack_vs_avg > 2.0) base_premium <- base_premium * (-0.5) # Big stack loosens
  base_premium
}


# ============================================================
# SECTION 3: BOARD TEXTURE (IMPROVED)
# ============================================================

.analyze_board_texture <- function(board) {
  if (length(board) < 3) return(list(dry = TRUE, wet = FALSE, coordination = 0, paired = FALSE))

  vals  <- sort(.rank_val(board))
  suits <- substring(board, nchar(board), nchar(board))

  # Connectivity
  diffs     <- diff(vals)
  gap_count <- sum(diffs <= 2)

  # Flush potential
  max_suit  <- max(table(suits))

  # Board pairing (paired boards kill flush/straight outs, favour value)
  paired <- any(table(vals) >= 2)

  list(
    dry         = (max_suit < 3 && gap_count < 1),
    wet         = (max_suit >= 3 || gap_count >= 2),
    coordination = gap_count + (max_suit - 1),
    paired      = paired
  )
}


# ============================================================
# SECTION 4: HAND CATEGORIZATION (IMPROVED)
# ============================================================

.categorize_v3 <- function(total_equity, hole_cards, board, street) {
  if (street == "river") {
    if (total_equity > 0.85) return("NUT_VALUE")
    if (total_equity > 0.60) return("STRONG_VALUE")
    if (total_equity > 0.42) return("MARGINAL_SDV")
    return("AIR")
  }

  if (total_equity > 0.85) return("NUT_VALUE")
  if (total_equity > 0.65) return("STRONG_VALUE")
  if (total_equity > 0.45) return("MARGINAL_SDV")

  outs <- .draw_outs(hole_cards, board, "high_card")
  if (outs >= 12) return("SEMI_BLUFF_PREMIUM")  # Combo draw / OESD + FD
  if (outs >= 8)  return("SEMI_BLUFF")
  if (outs >= 4)  return("WEAK_DRAW")

  return("AIR")
}


# ============================================================
# SECTION 5: EXPLOITATIVE SIZING
# ============================================================

# Adjust bet size based on who we're against
.exploit_size <- function(base_size, archetype, hand_type) {
  # vs. Calling Station: size UP for value, size DOWN for bluffs (don't bluff them)
  if (archetype == "CALLING_STATION") {
    if (hand_type %in% c("NUT_VALUE", "STRONG_VALUE")) return(min(base_size * 1.4, 1.2))
    return(0)  # Signal: don't bluff stations
  }

  # vs. Weak Tight: bluff more / value bet normal
  if (archetype == "WEAK_TIGHT") {
    if (hand_type %in% c("AIR", "SEMI_BLUFF")) return(base_size * 1.1)
    return(base_size)
  }

  # vs. LAG: trap more, check-raise instead of leading
  if (archetype == "LAG") {
    if (hand_type %in% c("NUT_VALUE", "STRONG_VALUE")) return(base_size * 0.65)  # Induce
    return(base_size * 0.8)  # Smaller / don't spew
  }

  # vs. Bluffy Aggro: check-call more with strong hands, bluff less
  if (archetype == "BLUFFY_AGGRO") {
    if (hand_type %in% c("NUT_VALUE", "STRONG_VALUE")) return(base_size * 0.70)  # Induce
    return(0)  # Don't bluff someone who's already bluffing
  }

  base_size  # UNKNOWN: stay GTO
}


# ============================================================
# SECTION 6: MAIN BOT — v3
# ============================================================

mady_bot <- function(bot_input, opponent_profile = NULL, mtt_context = NULL) {

  # --- 1. Unpack ---
  pot      <- bot_input$pot
  stack    <- bot_input$stack
  street   <- bot_input$street
  board    <- bot_input$board
  cards    <- bot_input$hole_cards
  call_amt <- max(0, bot_input$current_bet - bot_input$committed_this_round)
  rng      <- runif(1)

  # Default opponent profile if not provided
  if (is.null(opponent_profile)) opponent_profile <- .new_opponent_profile()
  archetype <- .opponent_archetype(opponent_profile)

  # Default MTT context
  if (is.null(mtt_context)) {
    mtt_context <- list(
      big_blind        = max(1, pot * 0.05),  # rough estimate
      small_blind      = max(1, pot * 0.025),
      antes            = 0,
      players          = 9,
      bubble_proximity = 0,
      avg_stack        = stack
    )
  }

  # --- 2. MTT Stack Pressure ---
  bb  <- mtt_context$big_blind
  m   <- .m_ratio(stack, bb, mtt_context$small_blind, mtt_context$antes, mtt_context$players)
  spr <- .spr(stack, pot)
  icm_premium <- .icm_fold_premium(mtt_context$bubble_proximity,
                                   stack / max(mtt_context$avg_stack, 1))

  # --- 3. PREFLOP ---
  if (street == "preflop") {
    tier <- .preflop_tier(cards)
    in_pos <- .is_in_position(bot_input)

    # Push-or-fold when short (M < 10)
    if (.push_fold_mode(m)) {
      # Shove with top ~30% of hands when M < 10; tighten near bubble
      shove_threshold <- if (mtt_context$bubble_proximity > 0.7) 2 else if (m < 5) 4 else 3
      if (tier <= shove_threshold) {
        return(list(type = "raise", amount = stack))  # All-in shove
      }
      return(list(type = if (call_amt == 0) "check" else "fold"))
    }

    # Normal preflop: tighten near bubble by skipping marginal opens
    if (mtt_context$bubble_proximity > 0.6 && tier >= 4) {
      return(list(type = if (call_amt == 0) "check" else "fold"))
    }

    return(.preflop_decision(bot_input, tier, in_pos, rng))
  }

  # --- 4. POSTFLOP ---
  texture  <- .analyze_board_texture(board)
  v_info   <- .get_villain_stack(bot_input)
  v_range  <- .pick_villain_range(v_info$villain_stack, v_info$avg_stack)

  raw_eq   <- .mc_equity(cards, board, v_range, n_sims = 1000)
  outs     <- .draw_outs(cards, board, "high_card")

  # Equity realization: draws worth less in MTT (can't always see cheap cards)
  # Also discount draws on paired boards (outs may be counterfeited)
  out_mult <- ifelse(street == "flop", 0.035, 0.018)
  if (texture$paired) out_mult <- out_mult * 0.7
  equity_adj <- raw_eq + (outs * out_mult)

  hand_type <- .categorize_v3(equity_adj, cards, board, street)

  # --- 5. Base Sizing (texture-aware) ---
  base_size <- dplyr::case_when(
    texture$dry    ~ 0.33,
    texture$wet    ~ 0.70,
    texture$paired ~ 0.50,
    TRUE           ~ 0.50
  )

  # Exploitative size override
  exploit_size <- .exploit_size(base_size, archetype, hand_type)

  # ICM tightening: near bubble, prefer smaller sizes to keep pots manageable
  if (mtt_context$bubble_proximity > 0.5) exploit_size <- exploit_size * 0.85

  # --- 6. SPR-based commitment check ---
  # If SPR <= 3 we're pot-committed with any value hand — don't slowplay
  committed <- spr <= 3 && hand_type %in% c("NUT_VALUE", "STRONG_VALUE", "MARGINAL_SDV")

  # --- 7. Decision Matrix ---

  # --- NUT VALUE ---
  if (hand_type == "NUT_VALUE") {
    if (committed) {
      # Jam: get it in
      if (bot_has_action(bot_input, "raise"))
        return(list(type = "raise", amount = bot_max_raise(bot_input)))
      return(list(type = "call"))
    }

    # vs. LAG / BLUFFY_AGGRO: check-raise trap
    if (archetype %in% c("LAG", "BLUFFY_AGGRO") && call_amt == 0 && rng < 0.45) {
      return(list(type = "check"))  # Let them bet into us
    }

    size <- .geo_sizing(pot, stack, .streets_left(street))
    amt  <- as.integer(clamp(pot * size, bot_min_raise(bot_input), bot_max_raise(bot_input)))
    if (bot_has_action(bot_input, "raise")) return(list(type = "raise", amount = amt))
    if (bot_has_action(bot_input, "bet"))   return(list(type = "bet",   amount = amt))
    return(list(type = "call"))
  }

  # --- STRONG VALUE ---
  if (hand_type == "STRONG_VALUE") {
    # vs. Calling Station: bet big for value
    amt <- as.integer(pot * max(exploit_size, 0.33))
    if (call_amt > 0) {
      # Facing a bet: raise for value unless vs. LAG (then call to control)
      if (archetype == "LAG" && !committed) return(list(type = "call"))
      if (bot_has_action(bot_input, "raise"))
        return(list(type = "raise", amount = clamp(amt * 2.5, bot_min_raise(bot_input), bot_max_raise(bot_input))))
      return(list(type = "call"))
    }
    if (exploit_size > 0 && bot_has_action(bot_input, "bet"))
      return(list(type = "bet", amount = amt))
    return(list(type = "check"))
  }

  # --- SEMI BLUFF PREMIUM (Combo draw / OESD+FD) ---
  if (hand_type == "SEMI_BLUFF_PREMIUM") {
    # Always semi-bluff in position; bluff ~60% OOP
    bluff_freq <- if (.is_in_position(bot_input)) 0.70 else 0.55
    # Don't bluff stations
    if (archetype == "CALLING_STATION") bluff_freq <- bluff_freq * 0.3

    if (rng < bluff_freq && call_amt == 0 && bot_has_action(bot_input, "bet")) {
      # Size larger — we have equity even if called
      return(list(type = "bet", amount = as.integer(pot * 0.75)))
    }
    if (call_amt > 0) {
      # Call if we're getting good odds (MDF-based)
      mdf <- pot / (pot + call_amt)
      if (equity_adj > (1 - mdf - 0.05)) return(list(type = "call"))
    }
  }

  # --- SEMI BLUFF (8-11 outs) ---
  if (hand_type == "SEMI_BLUFF") {
    bluff_freq <- if (.is_in_position(bot_input)) 0.40 else 0.22
    if (archetype == "WEAK_TIGHT")       bluff_freq <- bluff_freq * 1.4
    if (archetype == "CALLING_STATION")  bluff_freq <- 0.05
    if (archetype == "LAG")              bluff_freq <- bluff_freq * 0.6

    if (rng < bluff_freq && call_amt == 0 && bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = as.integer(pot * exploit_size)))
    }
    # Facing a bet with a draw: call if price is right
    if (call_amt > 0) {
      pot_odds <- call_amt / (pot + call_amt)
      if (equity_adj > pot_odds + 0.05) return(list(type = "call"))
    }
  }

  # --- WEAK DRAW (4-7 outs) ---
  if (hand_type == "WEAK_DRAW") {
    if (call_amt > 0) {
      pot_odds <- call_amt / (pot + call_amt)
      # Only call implied odds on flop; fold turn unless very cheap
      if (street == "flop" && equity_adj > pot_odds) return(list(type = "call"))
      if (street == "turn"  && call_amt < (pot * 0.15)) return(list(type = "call"))
    }
    if (bot_has_action(bot_input, "check")) return(list(type = "check"))
  }

  # --- MARGINAL SHOWDOWN VALUE ---
  if (hand_type == "MARGINAL_SDV") {
    if (call_amt > 0) {
      mdf <- pot / (pot + call_amt)
      # Tighten call threshold vs. aggro players; loosen vs. bluffy ones
      buffer <- dplyr::case_when(
        archetype == "LAG"         ~ 0.15,
        archetype == "BLUFFY_AGGRO"~ 0.05,
        archetype == "WEAK_TIGHT"  ~ 0.12,
        TRUE                       ~ 0.10
      )
      # Also factor in ICM: near bubble, fold more marginal spots
      buffer <- buffer + icm_premium

      if (equity_adj > (1 - mdf + buffer)) return(list(type = "call"))
      return(list(type = "fold"))
    }
    # In position with SDV: check back to control pot & see free card
    return(list(type = "check"))
  }

  # --- AIR: Selective Pure Bluffs ---
  if (hand_type == "AIR") {
    # Only bluff when: (a) opponent folds enough, (b) we have some blocker, (c) board didn't brick run-out
    fold_equity_good <- opponent_profile$fold_to_cbet > 0.55
    has_blocker      <- .has_blocker(cards, board)

    if (street == "river" && texture$dry && has_blocker && fold_equity_good && rng < 0.18) {
      # Blocker bluff on dry river — small sample gating via rng
      if (archetype != "CALLING_STATION" && bot_has_action(bot_input, "bet")) {
        return(list(type = "bet", amount = as.integer(pot * 0.70)))
      }
    }

    # Flop/Turn: probe bet vs. Weak Tight opponents if checked to us
    if (street %in% c("flop", "turn") && call_amt == 0 && archetype == "WEAK_TIGHT" && rng < 0.30) {
      if (bot_has_action(bot_input, "bet"))
        return(list(type = "bet", amount = as.integer(pot * 0.40)))
    }
  }

  # --- FALLBACK ---
  if (bot_has_action(bot_input, "check")) return(list(type = "check"))
  # Don't fold to micro-stabs (<2% of stack); too exploitable
  if (call_amt > 0 && call_amt <= (stack * 0.02)) return(list(type = "call"))
  return(list(type = "fold"))
}


# ============================================================
# SECTION 7: SESSION MANAGER (tracks opponents across hands)
# ============================================================

# Maintains opponent profiles across the session
mady_session <- local({
  profiles <- list()

  list(
    # Register a new opponent by seat ID
    register = function(seat_id) {
      profiles[[as.character(seat_id)]] <<- .new_opponent_profile()
    },

    # Record an observed action for a villain
    observe = function(seat_id, action) {
      id <- as.character(seat_id)
      if (is.null(profiles[[id]])) profiles[[id]] <<- .new_opponent_profile()
      profiles[[id]] <<- .update_opponent(profiles[[id]], action)
    },

    # Get a profile for use in bot decision
    get_profile = function(seat_id) {
      id <- as.character(seat_id)
      if (is.null(profiles[[id]])) return(.new_opponent_profile())
      profiles[[id]]
    },

    # Summary of all known opponents (useful for debugging)
    summary = function() {
      lapply(profiles, function(p) {
        list(
          hands      = p$hands_seen,
          archetype  = .opponent_archetype(p),
          fold_cbet  = round(p$fold_to_cbet, 2),
          aggr_freq  = round(p$aggression_freq, 2)
        )
      })
    }
  )
})


