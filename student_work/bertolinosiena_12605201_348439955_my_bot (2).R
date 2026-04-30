############################################################
# TOURNAMENT BOT — v4
# Mathematics of Poker
#
# CRITICAL FORMAT NOTE (learned from BotTemplate.R + example_bots.R):
#   hole_cards  = character vector, e.g. c("Ah", "Kd")
#   board       = character vector, e.g. c("7c", "2h", "Ts")
#   street      = string: "preflop", "flop", "turn", "river"
#   All example bots use hole_cards as strings — NOT a data.frame.
#
# HOW TO USE:
#   source("poker_load_all.R")
#   poker_load_all(include_demos = TRUE, verbose = FALSE)
#   source("my_bot.R")
#   # Change "my_bot" below to your tournament registration name
#
# KNOWN BOT COUNTERS (from example_bots.R):
#   passive_bot         = check/fold only          → bluff every street
#   always_call_bot     = check/call/fold           → never bluff, value bomb
#   random_bot          = picks random legal action → play solid, wait it out
#   aggressive_bot      = min-raise every action    → trap with strong hands
#   simple_preflop_bot  = raises premium preflop,   → steal every postflop pot
#                         check/call/fold postflop    they check to us
#   strength_by_street  = same as above but also    → same steal strategy
#                         bets two_pair+ postflop
#   mixed_bot           = 10% passive, 30% call,   → live tracker identifies
#                         40% random, 20% aggressive   blend within 15 hands
############################################################


# ═══════════════════════════════════════════════════════════
# SECTION 1 — PERSISTENT OPPONENT TRACKER
# Lives outside the bot function so it persists across hands.
# Keyed by seat number (always available, unlike player name).
# ═══════════════════════════════════════════════════════════

.opp_env <- new.env(parent = emptyenv())
.opp_env$seats <- list()

.opp_log <- function(seat, action) {
  k <- as.character(seat)
  if (is.null(.opp_env$seats[[k]]))
    .opp_env$seats[[k]] <- list(f=0L, c=0L, r=0L, n=0L)
  d <- .opp_env$seats[[k]]
  d$n <- d$n + 1L
  if      (action == "fold")                           d$f <- d$f + 1L
  else if (action %in% c("call", "check"))             d$c <- d$c + 1L
  else if (action %in% c("raise","bet","all_in"))      d$r <- d$r + 1L
  .opp_env$seats[[k]] <- d
}

.opp_stats <- function(seat) {
  d <- .opp_env$seats[[as.character(seat)]]
  if (is.null(d) || d$n < 8L)
    return(list(fold_rate=0.42, raise_rate=0.28, n=0L, type="unknown"))
  fr <- d$f / d$n
  rr <- d$r / d$n
  cr <- d$c / d$n
  # Classify into known archetypes
  type <-
    if      (fr >= 0.68)              "nit"        # passive_bot: check/fold
    else if (rr >= 0.60)              "maniac"     # aggressive_bot: raise everything
    else if (cr >= 0.65 && rr < 0.15) "station"   # always_call_bot
    else if (fr >= 0.45 && rr < 0.22) "weak-tight" # simple_preflop / strength_by_street
    else                              "unknown"
  list(fold_rate=fr, raise_rate=rr, n=d$n, type=type)
}


# ═══════════════════════════════════════════════════════════
# SECTION 2 — CARD HELPERS
# hole_cards and board are CHARACTER VECTORS like c("Ah","Kd")
# ═══════════════════════════════════════════════════════════

# Rank map — same as example_bots.R hole_rank_values()
.RANK_MAP <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
               "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)

# Extract numeric rank values from a character card vector
.card_ranks <- function(cards) {
  r <- substring(cards, 1, nchar(cards) - 1)
  unname(.RANK_MAP[r])
}

# Extract suit characters
.card_suits <- function(cards) {
  substring(cards, nchar(cards), nchar(cards))
}

# ═══════════════════════════════════════════════════════════
# SECTION 3 — PREFLOP TIER CLASSIFIER
# Returns: "monster" | "premium" | "strong" | "playable" |
#          "marginal" | "weak"
# ═══════════════════════════════════════════════════════════

.preflop_tier <- function(hole_cards) {
  vals  <- sort(.card_ranks(hole_cards), decreasing = TRUE)
  suits <- .card_suits(hole_cards)
  if (length(vals) < 2) return("weak")

  hi    <- vals[1]
  lo    <- vals[2]
  pair  <- hi == lo
  s     <- suits[1] == suits[2]   # suited
  gap   <- hi - lo

  # Tier 1 MONSTER — 3-bet/shove, never fold preflop
  if (pair && hi >= 12)          return("monster")  # QQ, KK, AA
  if (hi == 14 && lo == 13)      return("monster")  # AK

  # Tier 2 PREMIUM — open raise, 3-bet vs aggression
  if (pair && hi >= 9)           return("premium")  # 99, TT, JJ
  if (hi == 14 && lo >= 11)      return("premium")  # AQ, AJ
  if (s && hi == 14 && lo >= 10) return("premium")  # ATs

  # Tier 3 STRONG — open raise, call one raise
  if (pair && hi >= 6)           return("strong")   # 66, 77, 88
  if (hi == 14 && lo >= 9)       return("strong")   # AT, A9
  if (hi == 13 && lo >= 11)      return("strong")   # KQ, KJ
  if (s && hi == 14 && lo >= 5)  return("strong")   # A2s–A9s

  # Tier 4 PLAYABLE — enter cheap, fold to raises
  if (pair)                       return("playable") # 22–55
  if (s && gap <= 2 && hi >= 8)   return("playable") # JTs, T9s, 98s, 87s
  if (s && hi == 13 && lo >= 9)   return("playable") # K9s+
  if (hi == 14 && lo >= 7)        return("playable") # A7o+

  # Tier 5 MARGINAL — late position steal only
  if (s && gap <= 3 && hi >= 6)   return("marginal")
  if (hi >= 10 && lo >= 8)        return("marginal")

  "weak"
}


# ═══════════════════════════════════════════════════════════
# SECTION 4 — POSTFLOP HAND STRENGTH
# Uses the same made_hand_category() logic as example_bots.R
# so the result is consistent with how other bots think.
# Returns a numeric 0–1 strength score.
# ═══════════════════════════════════════════════════════════

.hand_strength <- function(hole_cards, board) {
  tryCatch({
    all_cards <- c(hole_cards, board)
    if (length(all_cards) < 5) {
      # On flop with only 3 board cards + 2 hole = 5 total, fine.
      # Return heuristic from hole cards alone if board empty.
      if (length(board) == 0) return(0.30)
    }

    vals  <- .card_ranks(all_cards)
    suits <- .card_suits(all_cards)

    rank_counts <- sort(table(vals),  decreasing = TRUE)
    suit_counts <- sort(table(suits), decreasing = TRUE)

    has_flush <- max(suit_counts) >= 5

    uv <- sort(unique(vals))
    if (14 %in% uv) uv <- sort(unique(c(1L, uv)))
    has_straight <- FALSE
    if (length(uv) >= 5)
      for (i in seq_len(length(uv) - 4))
        if (all(diff(uv[i:(i+4)]) == 1)) { has_straight <- TRUE; break }

    # Straight flush
    if (has_flush) {
      flush_suit <- names(suit_counts)[suit_counts >= 5][1]
      fv <- sort(unique(vals[suits == flush_suit]))
      if (14 %in% fv) fv <- sort(unique(c(1L, fv)))
      if (length(fv) >= 5)
        for (i in seq_len(length(fv) - 4))
          if (all(diff(fv[i:(i+4)]) == 1)) return(0.99)
    }

    mx <- max(rank_counts)
    if (mx == 4)                          return(0.97)  # quads
    if (mx == 3 && sum(rank_counts>=2)>=2) return(0.94) # full house
    if (has_flush)                         return(0.91)
    if (has_straight)                      return(0.88)
    if (mx == 3)                           return(0.76)  # trips
    if (sum(rank_counts >= 2) >= 2)        return(0.67)  # two pair

    if (mx == 2) {
      # One pair — quality check
      pair_val  <- as.numeric(names(rank_counts)[rank_counts == 2][1])
      board_vals <- .card_ranks(board)
      top_board  <- if (length(board_vals) > 0) max(board_vals) else 0

      # Kicker strength (higher hole card that isn't the pair)
      hole_vals  <- .card_ranks(hole_cards)
      kicker     <- suppressWarnings(max(hole_vals[hole_vals != pair_val]))
      kb         <- if (is.finite(kicker) && kicker > 7) (kicker - 7) * 0.009 else 0

      base <-
        if (pair_val > top_board)      0.63   # overpair
        else if (pair_val == top_board) 0.54  # top pair
        else if (pair_val >= 9)         0.43  # middle pair
        else                            0.32  # weak pair

      return(min(0.70, base + kb))
    }

    # No made hand — estimate draw equity
    # (outs-based; cards to come depends on street)
    0.14   # high card placeholder; draw equity added in main bot

  }, error = function(e) 0.25)
}

# Estimate draw equity (outs method) for incomplete hands
.draw_equity <- function(hole_cards, board, street) {
  tryCatch({
    ctc <- switch(street, flop=2L, turn=1L, river=0L, 0L)
    if (ctc == 0L) return(0.0)

    all_c <- c(hole_cards, board)
    vals  <- .card_ranks(all_c)
    suits <- .card_suits(all_c)
    n     <- length(all_c)
    unseen <- 52L - n

    # Flush draw: 4 cards of same suit → 9 outs
    suit_counts <- table(suits)
    fd_outs <- if (max(suit_counts) == 4L) 9L else 0L

    # Straight draw
    uv <- sort(unique(vals))
    if (14L %in% uv) uv <- sort(unique(c(1L, uv)))
    best <- 0L
    if (length(uv) >= 4L)
      for (i in seq_len(max(1L, length(uv) - 3L))) {
        win <- uv[i:min(i+4L, length(uv))]
        if (length(win) >= 4L && (max(win)-min(win)) <= 4L) {
          h <- sum(vals %in% win)
          if (h > best) best <- h
        }
      }
    sd_outs <- if (best >= 4L) {
      # OESD = 8 outs, gutshot = 4 outs
      if (sum(.card_ranks(hole_cards) %in% uv) >= 2L) 8L else 4L
    } else 0L

    # Overcards to board (3 outs each to make top pair)
    bv      <- .card_ranks(board)
    top_b   <- if (length(bv) > 0) max(bv) else 0L
    oc_outs <- sum(.card_ranks(hole_cards) > top_b) * 3L

    total_outs <- min(fd_outs + sd_outs + oc_outs, 21L)
    if (total_outs == 0L) return(0.0)

    # Use engine's outs_to_prob if available, otherwise rule-of-4/2
    tryCatch(
      outs_to_prob(total_outs, unseen, ctc),
      error = function(e) total_outs * (if (ctc == 2L) 0.04 else 0.02)
    )
  }, error = function(e) 0.0)
}


# ═══════════════════════════════════════════════════════════
# SECTION 5 — SAFE BET/RAISE SIZING
# All amounts clamped to legal min/max and stack.
# ═══════════════════════════════════════════════════════════

.safe_bet <- function(bot_input, pot_frac) {
  pot   <- bot_input$pot
  stack <- bot_input$stack
  mn    <- tryCatch(bot_min_bet(bot_input),   error=function(e) 1L)
  mx    <- tryCatch(bot_max_bet(bot_input),   error=function(e) stack)
  as.integer(max(mn, min(round(pot * pot_frac), mx, stack)))
}

.safe_raise <- function(bot_input, pot_frac) {
  pot   <- bot_input$pot
  stack <- bot_input$stack
  mn    <- tryCatch(bot_min_raise(bot_input), error=function(e) 1L)
  mx    <- tryCatch(bot_max_raise(bot_input), error=function(e) stack)
  as.integer(max(mn, min(round(pot * pot_frac), mx, stack)))
}

.bb_raise <- function(bot_input, mult, bb) {
  stack <- bot_input$stack
  mn    <- tryCatch(bot_min_raise(bot_input), error=function(e) bb)
  mx    <- tryCatch(bot_max_raise(bot_input), error=function(e) stack)
  as.integer(max(mn, min(round(mult * bb), mx, stack)))
}


# ═══════════════════════════════════════════════════════════
# MAIN BOT FUNCTION
# Rename to match your tournament registration name.
# ═══════════════════════════════════════════════════════════

my_bot <- function(bot_input) {

  # ── Unpack inputs ────────────────────────────────────────
  hole_cards  <- bot_input$hole_cards          # character vector: c("Ah","Kd")
  board       <- bot_input$board               # character vector: c("7c","2h","Ts")
  street      <- bot_input$street              # "preflop","flop","turn","river"
  pot         <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed   <- bot_input$committed_this_round
  stack       <- bot_input$stack
  bb_raw      <- bot_input$big_blind
  legal_types <- bot_input$legal_actions$legal_action_types
  pub         <- bot_input$public_players      # data.frame of all players
  hist        <- bot_input$action_history      # data.frame of past actions

  bb          <- if (!is.null(bb_raw) && bb_raw > 0) bb_raw else 10
  call_amount <- max(0, current_bet - committed)

  # Legal action flags
  can_raise <- "raise"  %in% legal_types
  can_bet   <- "bet"    %in% legal_types
  can_call  <- "call"   %in% legal_types
  can_check <- "check"  %in% legal_types
  can_allin <- "all_in" %in% legal_types

  # ── Log action history into opponent model ───────────────
  tryCatch({
    if (!is.null(hist) && is.data.frame(hist) && nrow(hist) > 0) {
      seats   <- if ("seat"   %in% names(hist)) hist$seat   else rep(NA, nrow(hist))
      actions <- if ("action" %in% names(hist)) hist$action else rep(NA, nrow(hist))
      for (i in seq_len(nrow(hist)))
        if (!is.na(seats[i]) && !is.na(actions[i]))
          .opp_log(seats[i], actions[i])
    }
  }, error = function(e) NULL)

  # ── My seat & opponents ──────────────────────────────────
  my_seat <- tryCatch(bot_input$seat, error = function(e) -1L)

  villain_seat <- tryCatch({
    others <- pub[pub$stack > 0 & pub$seat != my_seat, , drop = FALSE]
    if (nrow(others) > 0) others$seat[1] else 1L
  }, error = function(e) 1L)

  n_active <- tryCatch(sum(pub$stack > 0), error = function(e) 2L)
  n_opp    <- max(1L, n_active - 1L)

  vs     <- .opp_stats(villain_seat)
  v_fold <- vs$fold_rate
  v_rr   <- vs$raise_rate
  v_type <- vs$type

  # ── Position (am I last to act?) ────────────────────────
  late_pos <- tryCatch({
    others <- pub[pub$stack > 0 &
                    !isTRUE(pub$folded) &
                    pub$seat != my_seat, , drop = FALSE]
    nrow(others) == 0 || my_seat > max(others$seat)
  }, error = function(e) FALSE)

  # ── Short stack (< 15 BB → push/fold mode) ──────────────
  short_stack <- (stack / bb) < 15

  # ── Did we raise preflop? (c-bet trigger) ───────────────
  we_raised_pf <- tryCatch({
    if (is.null(hist) || !is.data.frame(hist) || nrow(hist) == 0) FALSE
    else {
      pf <- hist[!is.na(hist$street) & hist$street == "preflop" &
                   !is.na(hist$seat)   & hist$seat   == my_seat, ]
      nrow(pf) > 0 && any(pf$action %in% c("raise","bet","all_in"))
    }
  }, error = function(e) FALSE)


  ################################################################
  # PREFLOP
  ################################################################

  if (street == "preflop") {

    tier <- .preflop_tier(hole_cards)

    # ── Short stack: shove or fold ─────────────────────────
    if (short_stack) {
      if (tier %in% c("monster","premium","strong")) {
        if (can_allin) return(list(type = "all_in"))
        if (can_raise) return(list(type = "raise", amount = stack))
        if (can_bet)   return(list(type = "bet",   amount = stack))
        if (can_call)  return(list(type = "call"))
      }
      if (tier == "playable" && can_call && call_amount <= bb * 1.5)
        return(list(type = "call"))
      if (can_check) return(list(type = "check"))
      return(list(type = "fold"))
    }

    # ── MONSTER: AA, KK, QQ, AK ────────────────────────────
    # Always raise — NEVER limp or flat-call with these.
    # Open small (2.2x) to attract callers when first in.
    # 3-bet to ~3x facing a raise — build the pot, price out draws.
    if (tier == "monster") {
      if (current_bet > bb) {
        # Facing a raise — 3-bet for value
        rr_to <- as.integer(min(stack, max(
          tryCatch(bot_min_raise(bot_input), error = function(e) bb),
          round(current_bet * 3.0)
        )))
        if (can_raise) return(list(type = "raise", amount = rr_to))
        if (can_call)  return(list(type = "call"))
      }
      # First in: small open looks like a steal → gets called by weaker hands
      if (can_raise) return(list(type = "raise", amount = .bb_raise(bot_input, 2.2, bb)))
      if (can_bet)   return(list(type = "bet",   amount = .bb_raise(bot_input, 2.2, bb)))
      if (can_call)  return(list(type = "call"))
    }

    # ── PREMIUM: 99–JJ, AQ, AJ, ATs ───────────────────────
    # Open 2.5x. 3-bet vs aggressive openers; call tight ones once.
    if (tier == "premium") {
      if (current_bet > bb) {
        po <- pot_odds(call_amount, pot)
        # vs known nit, we're often behind their 3-bet range → just call
        if (v_type == "nit" && po > 0.30) return(list(type = "fold"))
        # vs everyone else: 3-bet to isolate
        rr_to <- as.integer(min(stack, max(
          tryCatch(bot_min_raise(bot_input), error = function(e) bb),
          round(current_bet * 2.8)
        )))
        if (can_raise) return(list(type = "raise", amount = rr_to))
        if (can_call && po <= 0.33) return(list(type = "call"))
        return(list(type = "fold"))
      }
      if (can_raise) return(list(type = "raise", amount = .bb_raise(bot_input, 2.5, bb)))
      if (can_bet)   return(list(type = "bet",   amount = .bb_raise(bot_input, 2.5, bb)))
      if (can_call)  return(list(type = "call"))
    }

    # ── STRONG: 66–88, AT, KQ/KJ, suited aces ─────────────
    # Open 2.5x. Call single raise with good pot odds.
    if (tier == "strong") {
      if (current_bet > bb) {
        po <- pot_odds(call_amount, pot)
        if (can_call && po <= 0.28) return(list(type = "call"))
        if (can_check) return(list(type = "check"))
        return(list(type = "fold"))
      }
      if (can_raise) return(list(type = "raise", amount = .bb_raise(bot_input, 2.5, bb)))
      if (can_bet)   return(list(type = "bet",   amount = .bb_raise(bot_input, 2.5, bb)))
      if (can_check) return(list(type = "check"))
      return(list(type = "fold"))
    }

    # ── PLAYABLE: small pairs, suited connectors ────────────
    # Cheap entry only. Late-position steal with big sizing.
    if (tier == "playable") {
      if (current_bet > bb) {
        limit <- if (late_pos) 2.5 * bb else 1.5 * bb
        if (can_call && call_amount <= limit) return(list(type = "call"))
        if (can_check) return(list(type = "check"))
        return(list(type = "fold"))
      }
      # Steal from late position — size big so it looks premium
      if (late_pos && n_opp <= 2 && can_raise)
        return(list(type = "raise", amount = .bb_raise(bot_input, 3.0, bb)))
      if (can_check) return(list(type = "check"))
      if (can_call && call_amount <= bb) return(list(type = "call"))
      return(list(type = "fold"))
    }

    # ── MARGINAL: late position steal only ─────────────────
    if (tier == "marginal" && late_pos && n_opp <= 2 && current_bet <= bb) {
      if (can_raise) return(list(type = "raise", amount = .bb_raise(bot_input, 3.5, bb)))
      if (can_check) return(list(type = "check"))
    }

    # ── WEAK ────────────────────────────────────────────────
    if (can_check) return(list(type = "check"))
    return(list(type = "fold"))
  }


  ################################################################
  # POSTFLOP — flop / turn / river
  ################################################################

  # ── Board texture ────────────────────────────────────────
  board_df <- tryCatch({
    if (length(board) == 0) NULL
    else data.frame(
      rank = substring(board, 1, nchar(board)-1),
      suit = substring(board, nchar(board), nchar(board)),
      card = board,
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)

  feats <- tryCatch(
    if (!is.null(board_df) && nrow(board_df) >= 3) board_features(board_df)
    else list(paired=FALSE, monotone=FALSE, two_tone=FALSE, connectivity=0),
    error = function(e) list(paired=FALSE, two_tone=FALSE,
                             monotone=FALSE, connectivity=0)
  )

  is_dry <- !isTRUE(feats$two_tone) && !isTRUE(feats$monotone) &&
            (is.null(feats$connectivity) || feats$connectivity <= 1)
  is_wet <- isTRUE(feats$two_tone) || isTRUE(feats$monotone) ||
            (!is.null(feats$connectivity) && feats$connectivity >= 3)

  # ── Hand strength ────────────────────────────────────────
  made_str   <- .hand_strength(hole_cards, board)
  draw_str   <- .draw_equity(hole_cards, board, street)
  # Blend: use the better of made hand or draw equity
  # For hands with nothing made, draw equity takes over
  equity_raw <- if (made_str >= 0.40) made_str
                else max(made_str, made_str + draw_str)
  equity_raw <- max(0.05, min(0.97, equity_raw))

  # Monte Carlo refinement (runs fast at 400 sims)
  equity <- tryCatch({
    if (!is.null(board_df) && nrow(board_df) >= 3) {
      # Build villain range from observed aggression
      vr_str <- if      (v_rr >= 0.55) "22+, A2s+, A4o+, K6s+, K9o+, Q8s+, QTo+, J8s+, JTo, T8s+, 97s+, 87s, 76s"
                else if (v_rr >= 0.30) "44+, A8s+, ATo+, KTs+, KJo+, QJs, JTs, T9s, 98s"
                else                   "77+, AJs+, AQo+, KQs, KQo"
      vr <- new_range_holdem_from_string(vr_str)
      mc <- holdem_equity_mc_fast(
        hole_list = list(hole_cards, vr),
        board_df  = board_df,
        n_sims    = 400
      )
      mc$equity[1]
    } else equity_raw
  }, error = function(e) equity_raw)

  equity <- max(0.05, min(0.97, equity - max(0, (n_opp - 1L) * 0.06)))

  # Pot odds and EV
  po <- if (call_amount > 0) pot_odds(call_amount, pot) else 0.0
  ev <- tryCatch(
    if (call_amount > 0) ev_call(equity=equity, pot_before_call=pot, call_amount=call_amount)
    else 0.0,
    error = function(e) equity - po   # fallback: simple equity minus required equity
  )

  # Bluff math: is a bet likely to be profitable via folds alone?
  bluff_sz <- .safe_bet(bot_input, 0.50)
  bluff_be <- tryCatch(
    break_even_fold_prob_bluff(pot, bluff_sz),
    error = function(e) bluff_sz / (pot + bluff_sz)
  )
  # Bluff fires on: dry board, heads-up, fold rate sufficient, not river
  bluff_ok <- v_fold >= (bluff_be + 0.07) &&
              is_dry &&
              n_opp == 1 &&
              street %in% c("flop","turn")


  # ════════════════════════════════════════════════════════
  # KNOWN BOT COUNTERS — fire BEFORE generic logic
  # These are calibrated to the exact strategies in example_bots.R
  # ════════════════════════════════════════════════════════

  # ── PASSIVE BOT (check/fold always) ──────────────────────
  # Their strategy: never bet, check or fold to any bet.
  # Counter: bet EVERY street. They fold to anything.
  # Bet even air — they can't call without a hand (they fold anyway).
  # Value-bet is bigger; bluff is 50% pot.
  if (v_type == "nit" && n_opp == 1) {
    if (equity >= 0.50) {
      vb <- .safe_bet(bot_input, 0.70)
      if (can_bet)   return(list(type = "bet",   amount = vb))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, 0.70)))
      if (can_call)  return(list(type = "call"))
    }
    # Bluff: they fold to almost anything
    if (v_fold >= bluff_be + 0.05) {
      if (can_bet)   return(list(type = "bet",   amount = bluff_sz))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, 0.50)))
    }
    if (can_check) return(list(type = "check"))
    if (can_call && ev > 0) return(list(type = "call"))
    return(list(type = "fold"))
  }

  # ── ALWAYS-CALL BOT (check/call/fold) ────────────────────
  # Their strategy: call anything, never raise.
  # Counter: NEVER bluff (they call). Value-bet thin — even one pair.
  # Bet big with strong hands; they pay it off.
  if (v_type == "station") {
    if (equity >= 0.50) {
      frac <- if (equity >= 0.80) 0.90 else if (equity >= 0.65) 0.75 else 0.58
      if (can_bet)   return(list(type = "bet",   amount = .safe_bet(bot_input, frac)))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, frac)))
      if (can_call)  return(list(type = "call"))
    }
    # Mediocre hand: check back, don't call big bets
    if (can_check) return(list(type = "check"))
    if (can_call && ev > 0 && call_amount <= pot * 0.35)
      return(list(type = "call"))
    return(list(type = "fold"))
  }

  # ── AGGRESSIVE BOT (min-raise every action) ──────────────
  # Their strategy: min-raise always, regardless of hand.
  # Counter: TRAP. Check strong hands to induce their raise,
  # then raise back. Fold weak hands to their constant pressure.
  if (v_type == "maniac") {
    if (equity >= 0.62) {
      # Check → let them raise → then raise back
      if (can_check) return(list(type = "check"))
      # Or if they've bet into us: raise
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, 0.75)))
      if (can_call)  return(list(type = "call"))
    }
    if (equity >= 0.42) {
      # Medium hand: call once, fold to big pressure
      if (can_check) return(list(type = "check"))
      if (can_call && ev > 0 && call_amount <= pot * 0.40)
        return(list(type = "call"))
    }
    if (can_check) return(list(type = "check"))
    return(list(type = "fold"))
  }

  # ── SIMPLE PREFLOP / STRENGTH-BY-STREET BOT ─────────────
  # Their strategy: only raises preflop with premium; postflop
  # checks/calls/folds (simple) or bets made hands (strength_by_street).
  # Counter: steal EVERY pot they check to us on any board.
  # They rarely have the postflop courage to fight back.
  if (v_type == "weak-tight" && n_opp == 1) {
    if (equity < 0.58 && can_bet)
      return(list(type = "bet", amount = .safe_bet(bot_input, 0.55)))
    if (equity >= 0.58) {
      vb <- .safe_bet(bot_input, 0.65)
      if (can_bet)   return(list(type = "bet",   amount = vb))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, 0.65)))
    }
    if (can_check) return(list(type = "check"))
    if (can_call && ev > 0 && call_amount <= pot * 0.45)
      return(list(type = "call"))
    return(list(type = "fold"))
  }


  # ════════════════════════════════════════════════════════
  # GENERIC POSTFLOP — for unknown / AI bots
  # Philosophy: solid fundamentals, EV-driven, no spew
  # ════════════════════════════════════════════════════════

  # ── CONTINUATION BET ─────────────────────────────────────
  # If we raised preflop, bet the flop. We have range advantage
  # as the aggressor. Size: 55% dry / 65% wet (to protect).
  if (street == "flop" && we_raised_pf && n_opp <= 3) {
    cbet_frac <- if (is_wet) 0.65 else 0.55
    if (equity >= 0.30) {
      if (can_bet)   return(list(type = "bet",   amount = .safe_bet(bot_input, cbet_frac)))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, cbet_frac)))
    }
    # Pure bluff c-bet on dry board, heads up, fold math works
    if (is_dry && n_opp == 1 && v_fold >= bluff_be + 0.08) {
      if (can_bet)   return(list(type = "bet",   amount = .safe_bet(bot_input, cbet_frac)))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, cbet_frac)))
    }
  }

  # ── RIVER: value bet or fold, almost never bluff ─────────
  if (street == "river") {
    if (equity >= 0.62) {
      frac <- if (equity >= 0.85) 0.85 else 0.68
      if (can_bet)   return(list(type = "bet",   amount = .safe_bet(bot_input, frac)))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, frac)))
      if (can_call)  return(list(type = "call"))
      if (can_check) return(list(type = "check"))
    }
    if (equity >= 0.48) {
      if (can_check) return(list(type = "check"))
      if (can_call && ev > 0) return(list(type = "call"))
      return(list(type = "fold"))
    }
    if (can_check) return(list(type = "check"))
    # Only call very cheap river bets with genuine equity
    if (can_call && equity >= po + 0.12 && call_amount <= pot * 0.25)
      return(list(type = "call"))
    return(list(type = "fold"))
  }

  # ── STRONG hand (equity >= 0.58): bet for value ──────────
  if (equity >= 0.58) {
    frac <- if      (equity >= 0.88) 0.85
             else if (equity >= 0.75) 0.72
             else                     0.60
    if (is_wet) frac <- min(0.85, frac + 0.10)  # bigger on wet boards to charge draws
    if (can_bet)   return(list(type = "bet",   amount = .safe_bet(bot_input, frac)))
    if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, frac)))
    if (can_call)  return(list(type = "call"))
    if (can_check) return(list(type = "check"))
  }

  # ── DECENT hand (equity 0.40–0.58) ───────────────────────
  if (equity >= 0.40) {
    if (can_check) {
      # Probe bet in position on dry boards
      if (late_pos && is_dry) {
        if (can_bet)   return(list(type = "bet",   amount = .safe_bet(bot_input, 0.45)))
        if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, 0.45)))
      }
      return(list(type = "check"))
    }
    # Call: needs positive EV and can't be calling off too much stack
    if (can_call) {
      stack_risk <- call_amount / max(1, stack)
      if (stack_risk > 0.35 && equity < 0.55) return(list(type = "fold"))
      if (ev > 0) return(list(type = "call"))
    }
    return(list(type = "fold"))
  }

  # ── DRAW / MEDIUM (equity 0.26–0.40) ─────────────────────
  if (equity >= 0.26) {
    if (can_check) {
      # Semi-bluff on flop only, dry board, in position, fold math ok
      if (street == "flop" && bluff_ok && late_pos) {
        semi <- .safe_bet(bot_input, 0.40)
        if (can_bet)   return(list(type = "bet",   amount = semi))
        if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, 0.40)))
      }
      return(list(type = "check"))
    }
    # Only call draws with clear positive EV and not too large
    if (can_call && ev > 0 && equity >= po + 0.04) {
      if (call_amount / max(1, stack) > 0.22) return(list(type = "fold"))
      return(list(type = "call"))
    }
    return(list(type = "fold"))
  }

  # ── WEAK hand (equity < 0.26) ────────────────────────────
  if (can_check) {
    # Bluff only when the math genuinely supports it
    if (bluff_ok) {
      if (can_bet)   return(list(type = "bet",   amount = bluff_sz))
      if (can_raise) return(list(type = "raise", amount = .safe_raise(bot_input, 0.50)))
    }
    return(list(type = "check"))
  }

  # Trivially cheap call
  if (can_call && call_amount <= bb) return(list(type = "call"))

  return(list(type = "fold"))
}
