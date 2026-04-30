############################################################
# TOURNAMENT BOT — v4
# Mathematics of Poker

# siena -------------------------------------------------------------------

# ── Opponent tracker ──────────────────────────────────────────────────
.e <- new.env(parent = emptyenv())
.e$s <- list()

.log_action <- function(seat, action) {
  k <- as.character(seat)
  if (is.null(.e$s[[k]])) .e$s[[k]] <- list(f=0L,c=0L,r=0L,n=0L)
  d <- .e$s[[k]]
  d$n <- d$n + 1L
  if      (action == "fold")                         d$f <- d$f + 1L
  else if (action %in% c("call","check"))            d$c <- d$c + 1L
  else if (action %in% c("raise","bet","all_in"))    d$r <- d$r + 1L
  .e$s[[k]] <- d
}

.fold_rate <- function(seat) {
  d <- .e$s[[as.character(seat)]]
  if (is.null(d) || d$n < 8L) return(0.40)
  d$f / d$n
}


# ── Card helpers ──────────────────────────────────────────────────────
# hole_cards and board are CHARACTER VECTORS e.g. c("Ah","Kd")

.RANKS <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
            "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)

.ranks <- function(cards) unname(.RANKS[ substring(cards,1,nchar(cards)-1) ])
.suits <- function(cards) substring(cards,nchar(cards),nchar(cards))


# ── Preflop tier ──────────────────────────────────────────────────────
# Only 3 tiers — simpler and harder to make mistakes with.
#
#  PREMIUM  = hands strong enough to raise and play for multiple streets
#             AA, KK, QQ, JJ, TT, AK, AQ, AJ, KQ
#  SPECULATIVE = hands that need to flop very well to continue
#             Small pairs (22-99), suited connectors, Axs
#             Enter FREE only. Fold to any raise.
#  FOLD     = everything else

.preflop_tier <- function(hole_cards) {
  v <- sort(.ranks(hole_cards), decreasing=TRUE)
  s <- .suits(hole_cards)
  if (length(v) < 2) return("fold")

  hi <- v[1]; lo <- v[2]
  pair   <- hi == lo
  suited <- s[1] == s[2]

  # PREMIUM: always raise these, play all streets
  if (pair && hi >= 10)           return("premium")   # TT, JJ, QQ, KK, AA
  if (hi==14 && lo>=11)           return("premium")   # AK, AQ, AJ
  if (hi==13 && lo==12)           return("premium")   # KQ

  # SPECULATIVE: enter free, fold to raises, need to flop two pair+
  if (pair)                        return("speculative") # 22-99
  if (suited && (hi-lo)<=2 && hi>=7) return("speculative") # suited connectors
  if (suited && hi==14 && lo>=4)  return("speculative") # suited aces

  "fold"
}


# ── Postflop hand score (0–1) ─────────────────────────────────────────
# Returns how strong our made hand is RIGHT NOW.
# Does NOT include draw equity — we do not chase draws against callers.

.hand_score <- function(hole_cards, board) {
  tryCatch({
    cards <- c(hole_cards, board)
    if (length(cards) < 5) return(0.20)

    v  <- .ranks(cards)
    s  <- .suits(cards)
    rc <- sort(table(v), decreasing=TRUE)
    sc <- sort(table(s), decreasing=TRUE)

    has_flush <- max(sc) >= 5

    uv <- sort(unique(v))
    if (14 %in% uv) uv <- sort(unique(c(1L,uv)))
    has_straight <- FALSE
    if (length(uv) >= 5)
      for (i in seq_len(length(uv)-4))
        if (all(diff(uv[i:(i+4)])==1)) { has_straight <- TRUE; break }

    # Straight flush
    if (has_flush) {
      fs <- names(sc)[sc>=5][1]
      fv <- sort(unique(v[s==fs]))
      if (14 %in% fv) fv <- sort(unique(c(1L,fv)))
      if (length(fv)>=5)
        for (i in seq_len(length(fv)-4))
          if (all(diff(fv[i:(i+4)])==1)) return(1.00)
    }

    mx <- max(rc)
    if (mx==4)                  return(0.97)  # quads
    if (mx==3 && sum(rc>=2)>=2) return(0.94)  # full house
    if (has_flush)              return(0.91)  # flush
    if (has_straight)           return(0.88)  # straight
    if (mx==3)                  return(0.76)  # trips
    if (sum(rc>=2)>=2)          return(0.67)  # two pair

    if (mx==2) {
      # One pair — score reflects quality but stays well below 0.67
      pv  <- as.numeric(names(rc)[rc==2][1])
      btop <- if (length(board)>0) max(.ranks(board)) else 0
      hv   <- .ranks(hole_cards)
      kick <- suppressWarnings(max(hv[hv!=pv], na.rm=TRUE))
      kb   <- if (is.finite(kick) && kick>7) (kick-7)*0.01 else 0

      base <-
        if (pv > btop)      0.58   # overpair
      else if (pv==btop)  0.50   # top pair
      else if (pv>=9)     0.40   # middle pair
      else                0.28   # weak pair

      return(min(0.64, base + kb))
    }

    0.12  # high card / no hand

  }, error=function(e) 0.20)
}


# ── Safe bet/raise sizing ─────────────────────────────────────────────

.bet <- function(bot_input, frac) {
  p  <- bot_input$pot; st <- bot_input$stack
  mn <- tryCatch(bot_min_bet(bot_input),   error=function(e) 1L)
  mx <- tryCatch(bot_max_bet(bot_input),   error=function(e) st)
  as.integer(max(mn, min(round(p*frac), mx, st)))
}

.raise_to <- function(bot_input, frac) {
  p  <- bot_input$pot; st <- bot_input$stack
  mn <- tryCatch(bot_min_raise(bot_input), error=function(e) 1L)
  mx <- tryCatch(bot_max_raise(bot_input), error=function(e) st)
  as.integer(max(mn, min(round(p*frac), mx, st)))
}

.bb_raise <- function(bot_input, mult, bb) {
  st <- bot_input$stack
  mn <- tryCatch(bot_min_raise(bot_input), error=function(e) bb)
  mx <- tryCatch(bot_max_raise(bot_input), error=function(e) st)
  as.integer(max(mn, min(round(mult*bb), mx, st)))
}


# ══════════════════════════════════════════════════════════════════════
# MAIN BOT — rename to your tournament registration name
# ══════════════════════════════════════════════════════════════════════

siena_bot <- function(bot_input) {

  # Unpack inputs
  hole_cards  <- bot_input$hole_cards
  board       <- bot_input$board
  street      <- bot_input$street
  pot         <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed   <- bot_input$committed_this_round
  stack       <- bot_input$stack
  bb_raw      <- bot_input$big_blind
  legal       <- bot_input$legal_actions$legal_action_types
  pub         <- bot_input$public_players
  hist        <- bot_input$action_history

  bb          <- if (!is.null(bb_raw) && bb_raw>0) bb_raw else 10
  call_amount <- max(0, current_bet - committed)

  can_raise <- "raise"  %in% legal
  can_bet   <- "bet"    %in% legal
  can_call  <- "call"   %in% legal
  can_check <- "check"  %in% legal
  can_allin <- "all_in" %in% legal

  # Log opponent actions
  tryCatch({
    if (!is.null(hist) && is.data.frame(hist) && nrow(hist)>0)
      for (i in seq_len(nrow(hist))) {
        sc <- if ("seat"   %in% names(hist)) hist$seat[i]   else NA
        ac <- if ("action" %in% names(hist)) hist$action[i] else NA
        if (!is.na(sc) && !is.na(ac)) .log_action(sc, ac)
      }
  }, error=function(e) NULL)

  my_seat <- tryCatch(bot_input$seat, error=function(e) -1L)
  n_active <- tryCatch(sum(pub$stack>0), error=function(e) 2L)
  n_opp    <- max(1L, n_active - 1L)


  ##################################################################
  # PREFLOP
  ##################################################################

  if (street == "preflop") {

    tier <- .preflop_tier(hole_cards)

    # ── FOLD tier: immediately fold to any cost, check if free ────────
    if (tier == "fold") {
      if (can_check) return(list(type="check"))
      return(list(type="fold"))
    }

    # ── SPECULATIVE: small pairs, suited connectors, suited aces ──────
    # Only see the flop for FREE. If someone has raised even 1 BB extra,
    # fold. These hands need to make two pair or better on the flop to
    # continue — and even then, we just check/call small or bet ourselves.
    if (tier == "speculative") {
      if (can_check) return(list(type="check"))
      # Call ONLY if it costs at most 1 BB and we are not already raised
      if (can_call && call_amount <= bb && current_bet <= bb)
        return(list(type="call"))
      return(list(type="fold"))
    }

    # ── PREMIUM: TT+, AK, AQ, AJ, KQ ────────────────────────────────
    # These are the only hands we invest real chips with preflop.
    #
    # Sizing philosophy:
    #   - Open to 2.5x BB when first in — small enough to get called
    #     by weaker hands, big enough to deny free flops to speculative hands
    #   - When facing a raise: call if it's < 20% of stack
    #   - Never go all-in preflop — we want to see the board first
    if (tier == "premium") {

      if (current_bet > bb) {
        # Facing a raise — call if cheap, fold if expensive
        stack_cost <- call_amount / max(1, stack)
        po         <- pot_odds(call_amount, pot)

        # Too expensive: fold and preserve chips
        if (stack_cost > 0.20) return(list(type="fold"))

        # Reasonable price: call (don't re-raise — keep pot manageable)
        if (can_call && po <= 0.33) return(list(type="call"))
        if (can_check) return(list(type="check"))
        return(list(type="fold"))
      }

      # First in: open raise to 2.5x BB
      if (can_raise) return(list(type="raise",
                                 amount=.bb_raise(bot_input, 2.5, bb)))
      if (can_bet)   return(list(type="bet",
                                 amount=.bb_raise(bot_input, 2.5, bb)))
      if (can_call)  return(list(type="call"))
    }

    # Safety net
    if (can_check) return(list(type="check"))
    return(list(type="fold"))
  }


  ##################################################################
  # POSTFLOP — flop, turn, river
  ##################################################################

  # Get current made-hand score (no draw equity — we don't chase)
  score <- .hand_score(hole_cards, board)

  # Multiway discount: each extra opponent lowers our effective equity
  score <- score - max(0, (n_opp - 1L) * 0.06)
  score <- max(0.05, min(0.97, score))

  # What fraction of our stack would a call cost?
  stack_pct <- if (call_amount > 0) call_amount / max(1, stack) else 0.0

  # Pot odds
  po <- if (call_amount > 0) pot_odds(call_amount, pot) else 0.0

  # EV of calling (from poker_math.R — win_prob * pot - (1-win_prob) * call)
  ev <- if (call_amount > 0)
    ev_call(equity=score, pot_before_call=pot, call_amount=call_amount)
  else 0.0

  # ════════════════════════════════════════════════════════════════════
  # POSTFLOP DECISION FRAMEWORK
  #
  # Three clean tiers based on made-hand score:
  #
  # TIER A — Two pair or better (score >= 0.67):
  #   Bet for value every time. This is the only situation where we
  #   voluntarily put significant chips in. Size to extract max value
  #   from the callers. On turn/river with near-nuts, go all-in.
  #
  # TIER B — Overpair / top pair with strong kicker (score 0.55–0.67):
  #   Check when possible. Call ONLY if the price is small (< 8% stack)
  #   and EV is clearly positive. No betting — one pair loses too often
  #   against the callers who will have hit something.
  #
  # TIER C — Everything else (score < 0.55):
  #   Check and fold. Do not call. Do not bet. Wait for the next hand.
  #   Every chip saved here is a chip that keeps us alive.
  #
  # NO bluffing at all — caller bots and random bots don't fold enough.
  # NO c-bets — we don't have range advantage worth betting without a hand.
  # NO chasing draws — pot odds rarely justify it with multiple callers.
  # ════════════════════════════════════════════════════════════════════

  # ── TIER A: Two pair or better ────────────────────────────────────
  if (score >= 0.67) {

    # ALL-IN CONDITIONS (end of hand, near-nuts only):
    #   - River with score >= 0.88  (straight/flush/boat/quads)
    #   - Turn with score >= 0.88 AND already pot-committed (>= 25% stack)
    near_nuts    <- score >= 0.88
    on_river     <- street == "river"
    on_turn      <- street == "turn"
    committed_pct <- committed / max(1, stack + committed)

    if (near_nuts && on_river && can_allin)
      return(list(type="all_in"))

    if (near_nuts && on_turn && committed_pct >= 0.25 && can_allin)
      return(list(type="all_in"))

    # Normal value bet: size scales with hand strength
    # Bigger bets with stronger hands — callers will pay us off
    frac <-
      if      (score >= 0.90) 0.85   # quads/SF/boat/flush: near-pot
    else if (score >= 0.80) 0.75   # flush/straight/trips
    else if (score >= 0.70) 0.65   # two pair
    else                    0.55   # two pair (borderline)

    if (can_bet)   return(list(type="bet",   amount=.bet(bot_input, frac)))
    if (can_raise) return(list(type="raise", amount=.raise_to(bot_input, frac)))
    if (can_call)  return(list(type="call"))   # facing a bet with strong hand
    if (can_check) return(list(type="check"))
  }

  # ── TIER B: Strong one pair (overpair / top pair good kicker) ────────
  # score 0.55–0.67: we have something real but not two pair.
  # Strategy: check behind to keep pot small, call ONLY tiny bets.
  # Fold to any significant bet — one pair loses to two pair or better
  # and the callers will have that frequently.
  if (score >= 0.55) {
    if (can_check) return(list(type="check"))

    # Call only if ALL of:
    #   - EV is positive
    #   - Call costs < 8% of remaining stack
    #   - Our equity clears pot odds by at least 10% margin
    #   - We're not in a multiway pot (3+ opponents = fold one pair)
    if (can_call &&
        ev > 0 &&
        stack_pct <= 0.08 &&
        score >= po + 0.10 &&
        n_opp <= 2) {
      return(list(type="call"))
    }

    return(list(type="fold"))
  }

  # ── TIER C: Weak hand / mediocre pair / high card ────────────────────
  # score < 0.55: check or fold. Full stop.
  # This includes middle pair, weak pair, all draws, and high card.
  # We do NOT call, bet, or raise in this tier under any circumstances.
  if (can_check) return(list(type="check"))
  return(list(type="fold"))
}


# mehdi -------------------------------------------------------------------


mehdi_bot <- function(bot_input) {

  # Get the main information from the current hand
  # I use these values later to decide whether to bet, call, check, or fold
  legal <- bot_input$legal_actions$legal_action_types
  street <- bot_input$street
  pot <- bot_input$pot
  facing_bet <- bot_input$current_bet > bot_input$committed_this_round

  # Convert my two private cards into numbers so they are easier to compare (aces count as high cards)
  hole_vals <- sort(hole_rank_values(bot_input$hole_cards), decreasing = TRUE)

  # Strategy 1:
  # ========================================
  # Preflop strategy:
  # Before any board cards are shown, the bot only plays safer starting hands
  # It plays pocket pairs or two high cards, and folds weaker hands
  if (street == "preflop") {

    # A hand is treated as strong if it is a pair or both cards are 10 or higher
    is_strong_preflop <- (hole_vals[1] == hole_vals[2]) || (hole_vals[1] >= 10 && hole_vals[2] >= 10)

    if (is_strong_preflop) {

      # With a strong starting hand, the bot tries to raise or bet
      # If that is not possible, it calls
      if ("raise" %in% legal) return(list(type = "raise", amount = bot_min_raise(bot_input)))
      if ("bet" %in% legal) return(list(type = "bet", amount = bot_min_bet(bot_input)))
      if ("call" %in% legal) return(list(type = "call"))

    } else {

      if ("check" %in% legal && !facing_bet) return(list(type = "check"))
      return(list(type = "fold"))
    }
  }

  # Strategy 2:
  # ========================================================
  # Postflop strategy:
  # After the flop, turn, or river, the bot checks whether it has made a pair
  # This is one way to see if the hand is worth continuing with
  board_vals <- sort(hole_rank_values(bot_input$board), decreasing = TRUE)

  # The bot counts the hand as "made" (has actual value) if it has a pocket pair,
  # or if one of its private cards matches a card on the board
  is_made_hand <- (hole_vals[1] == hole_vals[2]) ||
    (hole_vals[1] %in% board_vals) ||
    (hole_vals[2] %in% board_vals)

  # This helper function chooses a bet close to the size of the pot
  # It also makes sure the bet stays between the minimum and maximum legal bet
  calc_pot_bet <- function() {
    target <- as.integer(pot) # target a bet equal to the pot size
    min_b <- bot_min_bet(bot_input)
    max_b <- bot_max_bet(bot_input)
    if (!is.null(min_b) && !is.null(max_b)) {
      return(max(as.integer(min_b), min(target, as.integer(max_b))))
    }
    return(min_b)
  }

  # If we have at least a pair (made hand), we play more confidently
  if (is_made_hand) {
    if (facing_bet) {

      # If another bot has already bet, we call with our pair, by calling instead of trying to compute a raise (for simplicity)
      if ("call" %in% legal) return(list(type = "call"))
    } else {

      # If nobody has bet yet, we make a bet about the size of the pot
      # We try to get value when the hand is at least a pair
      if ("bet" %in% legal) return(list(type = "bet", amount = calc_pot_bet()))
    }

    # If we do not have a made hand, we play more carefully
  } else {

    if (facing_bet) {

      # It's safer to fold (with no pair and a bet in front of us)
      # The check line is only for backup if the engine lists it as legal
      if ("check" %in% legal) return(list(type = "check"))
      return(list(type = "fold"))

    } else {

      # If nobody has bet, we can still try a bet about the size of the pot
      # We assume checking with no pair would usually give up the chance to win the pot right away
      if ("bet" %in% legal) return(list(type = "bet", amount = calc_pot_bet()))
    }
  }

  # Strategy 3:
  # ========================================================
  # Backup actions:
  # If none of the main rules returned an action, choose the safest legal option
  # This keeps the bot from failing in an unusual game state
  if ("check" %in% legal) return(list(type = "check"))
  if ("call" %in% legal) return(list(type = "call"))
  return(list(type = "fold"))
}

# nate --------------------------------------------------------------------

nate_bot <- function(bot_input) {
  street      <- bot_input$street
  pot         <- bot_input$pot
  stack       <- bot_input$stack
  hole_cards  <- bot_input$hole_cards
  board       <- bot_input$board

  category <- made_hand_category(hole_cards, board)
  vals     <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

  # --- 1. PREFLOP: THE GATEKEEPER ---
  # We stop "bleeding" chips on mediocre hands.
  if (identical(street, "preflop")) {
    # Premium: AA, KK, QQ, JJ, TT, AK, AQ
    is_premium <- (length(unique(vals)) == 1 && vals[1] >= 10) || (vals[1] == 14 && vals[2] >= 12)
    # Strong: Any Pair, AJ, KQ
    is_strong  <- (length(unique(vals)) == 1) || (vals[1] >= 13 && vals[2] >= 11)

    if (is_premium) {
      cat("Bill: Found a monster. Starting the war.\n")
      if (bot_has_action(bot_input, "raise")) return(list(type = "raise", amount = bot_min_raise(bot_input) * 3.5))
    }
    if (is_strong) {
      return(choose_preferred_action(bot_input, c("call", "check")))
    }
    # FOLD EVERYTHING ELSE (The "Cautious" Filter)
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  # --- 2. POSTFLOP: AGGRESSION TIERS ---

  # Tier 1: The "War" Hands (Trips, Straights, Flushes, Full House)
  is_war_hand <- category %in% c("trips", "straight", "flush", "full_house", "quads")

  # Tier 2: The "Caution" Hands (Top Pair, Two Pair)
  # We only go to war with Top Pair if our kicker (the second card) is high (A, K, or Q).
  is_cautious_hand <- (category == "two_pair") || (category == "pair" && max(vals) >= 12)

  # EXECUTION
  if (is_war_hand) {
    cat("Bill: THE WAR IS ON. Overbetting for max value.\n")
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = clamp_bet(floor(pot * 1.25), bot_input)))
    }
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_max_raise(bot_input)))
    }
  }

  if (is_cautious_hand) {
    # If the pot is small, we bet to protect our hand.
    # If the pot is already huge (someone else is pushing), we just CALL.
    if (pot < (stack * 0.3)) {
      cat("Bill: Controlling the pot with a solid hand.\n")
      if (bot_has_action(bot_input, "bet")) return(list(type = "bet", amount = clamp_bet(floor(pot * 0.6), bot_input)))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  # TIER 3: NOTHING
  cat("Bill: No war today. Folding.\n")
  return(choose_preferred_action(bot_input, c("check", "fold")))
}

# (Keep the clamp_bet helper from before)


# ruth --------------------------------------------------------------------


ruth_bot <- function(bot_input) {

  ##### --- Basic info --- #####

  seat <- bot_input$seat
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street

  pot <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed_this_round <- bot_input$committed_this_round
  stack <- bot_input$stack

  legal_types <- bot_input$legal_actions$legal_action_types
  public_players <- bot_input$public_players

  ##### --- Core calculations --- #####

  to_call <- current_bet - committed_this_round
  pot_odds <- ifelse(to_call > 0, to_call / (pot + to_call), 0)

  num_players <- length(public_players)
  is_late <- seat >= (num_players - 1)

  ##### --- Opponent modeling --- #####

  total_committed <- sum(sapply(public_players, function(p) p$committed_this_hand))
  avg_commit <- total_committed / max(1, num_players)

  passive_table <- avg_commit < pot * 0.3
  aggressive_table <- avg_commit > pot * 0.7

  ##### --- Stack awareness --- #####

  avg_stack <- mean(sapply(public_players, function(p) p$stack))
  short_stack <- stack < avg_stack * 0.5
  big_stack <- stack > avg_stack * 1.5

  ##### --- Helpers --- #####

  sanitize_amount <- function(amount, type) {
    amount <- round(amount)

    if (type == "bet") {
      min_amt <- bot_min_bet(bot_input)
      max_amt <- bot_max_bet(bot_input)
    } else if (type == "raise") {
      min_amt <- bot_min_raise(bot_input)
      max_amt <- bot_max_raise(bot_input)
    } else {
      return(NULL)
    }

    amount <- max(min_amt, amount)
    amount <- min(max_amt, amount)

    return(amount)
  }

  bet_mult <- function(strength = "medium") {
    if (strength == "strong") {
      return(runif(1, 0.6, 1.0))
    } else {
      return(runif(1, 0.4, 0.7))
    }
  }

  ##### --- Preflop --- #####

  if (identical(street, "preflop") && length(hole_cards) == 2) {

    vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

    paired <- length(unique(vals)) == 1
    strong_broadway <- min(vals) >= 12
    strong_ace <- max(vals) == 14 && min(vals) >= 10

    strong_hand <- paired || strong_broadway || strong_ace

    # SHORT STACK: shove wider
    if (short_stack && strong_hand && "all_in" %in% legal_types) {
      return(list(type = "all_in"))
    }

    # Raise strong hands
    if (strong_hand && "raise" %in% legal_types) {
      raw_amt <- pot * 0.6
      amt <- sanitize_amount(raw_amt, "raise")
      return(list(type = "raise", amount = amt))
    }

    # Late position steal (more aggressive if passive table)
    if (is_late && to_call == 0 && "raise" %in% legal_types) {
      steal_prob <- ifelse(passive_table, 0.7, 0.4)

      if (runif(1) < steal_prob) {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
    }

    # Call small bets
    if (to_call > 0 && to_call <= pot * 0.25) {
      return(list(type = "call"))
    }

    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  ##### --- Postflop --- #####

  category <- made_hand_category(hole_cards, board)

  ##### --- Board texture --- #####

  flush_draw <- FALSE
  connected <- FALSE

  if (length(board) >= 3) {
    suits <- substring(board, 2, 2)
    flush_draw <- any(table(suits) >= 2)

    vals <- sort(rank_to_numeric(substring(board, 1, 1)))
    connected <- (max(vals) - min(vals) <= 4)
  }

  draw_board <- flush_draw || connected

  strong_hands <- c("two_pair", "trips", "straight", "flush",
                    "full_house", "quads", "straight_flush")

  ##### --- Strong hands --- #####

  if (category %in% strong_hands) {

    # Big stack = apply pressure
    if (big_stack && "raise" %in% legal_types && runif(1) < 0.5) {
      raw_amt <- pot * 1.0
      amt <- sanitize_amount(raw_amt, "raise")
      return(list(type = "raise", amount = amt))
    }

    if ("raise" %in% legal_types) {
      raw_amt <- pot * bet_mult("strong")
      amt <- sanitize_amount(raw_amt, "raise")
      return(list(type = "raise", amount = amt))
    }

    if ("bet" %in% legal_types) {
      raw_amt <- pot * bet_mult("strong")
      amt <- sanitize_amount(raw_amt, "bet")
      return(list(type = "bet", amount = amt))
    }

    if ("all_in" %in% legal_types) {
      return(list(type = "all_in"))
    }
  }

  ##### --- Pair --- #####

  if (category == "pair") {

    equity_estimate <- ifelse(street == "flop", 0.5,
                              ifelse(street == "turn", 0.55, 0.6))

    if (aggressive_table) {
      equity_estimate <- equity_estimate + 0.1
    }

    # Never call a large bet (>40% of stack) with just one pair
    if (to_call > stack * 0.4) {
      return(choose_preferred_action(bot_input, c("check", "fold")))
    }

    if (to_call > 0 && pot_odds < equity_estimate) {
      return(list(type = "call"))
    }

    if (to_call == 0 && "bet" %in% legal_types && runif(1) < 0.6) {
      raw_amt <- pot * bet_mult("medium")
      amt <- sanitize_amount(raw_amt, "bet")
      return(list(type = "bet", amount = amt))
    }

    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  ##### --- Bluffing (adaptive) --- #####

  if (category == "high_card" && to_call == 0 && "bet" %in% legal_types) {

    bluff_prob <- 0.15

    if (passive_table) bluff_prob <- bluff_prob + 0.2
    if (draw_board) bluff_prob <- bluff_prob - 0.1

    if (runif(1) < bluff_prob) {
      raw_amt <- pot * 0.5
      amt <- sanitize_amount(raw_amt, "bet")
      return(list(type = "bet", amount = amt))
    }
  }

  ##### --- Pot-odds defense --- #####

  if (to_call > 0) {

    # Detect if this is effectively an all-in call
    is_allin_call <- to_call >= stack * 0.8 || to_call >= stack

    equity_estimate <- ifelse(category == "high_card", 0.2,
                              ifelse(category == "pair", 0.5, 0.75))

    if (aggressive_table) {
      equity_estimate <- equity_estimate + 0.05
    }

    # Be much tighter facing a near/full all-in — require strong hands only
    if (is_allin_call) {
      strong_hands <- c("two_pair", "trips", "straight", "flush",
                        "full_house", "quads", "straight_flush")
      if (!(category %in% strong_hands)) {
        return(list(type = "fold"))
      }
      # Even with a strong hand, demand good pot odds
      if (pot_odds >= 0.4) {
        return(list(type = "fold"))
      }
    }

    if (pot_odds < equity_estimate) {
      return(list(type = "call"))
    } else {
      return(list(type = "fold"))
    }
  }

  ##### --- Default --- #####

  return(list(type = "check"))
}


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

.geo_sizing <- function(pot, stack, streets_left) {
  if (streets_left <= 0) return(0.75)
  # Calculates geometric growth to be all-in by river
  # Formula: Pot * (1 + 2x)^n = Pot + Stack
  ratio <- ( (pot + stack) / pot )^(1/streets_left)
  x <- (ratio - 1) / 2
  return(as.numeric(clamp(x, 0.33, 1.5)))
}

.categorize_sophisticated <- function(total_equity, hole_cards, board) {
  # Advanced hand categorization beyond just equity
  if (total_equity > 0.85) return("NUT_VALUE")
  if (total_equity > 0.65) return("STRONG_VALUE")
  if (total_equity > 0.40) return("MARGINAL_SDV")
  if (total_equity > 0.20 || .straight_draw_outs(hole_cards, board) >= 8) return("SEMI_BLUFF")
  return("AIR")
}


# ============================================================
# SECTION 10: MAIN GTO BOT
# ============================================================

mady_bot <- function(bot_input) {

  # ---- A. Unpack & Setup (Your Logic) ----
  pot         <- bot_input$pot
  stack       <- bot_input$stack
  current_bet <- bot_input$current_bet
  committed   <- bot_input$committed_this_round
  call_amount <- max(0, current_bet - committed)

  can_bet   <- bot_has_action(bot_input, "bet")
  can_raise <- bot_has_action(bot_input, "raise")
  can_call  <- bot_has_action(bot_input, "call")
  can_check <- bot_has_action(bot_input, "check")

  # ---- B. Equity & Hands (Your Logic + Enhanced MC) ----
  mc_eq <- .mc_equity(bot_input$hole_cards, bot_input$board, .pick_villain_range(stack, stack), n_sims = 600)
  outs  <- .draw_outs(bot_input$hole_cards, bot_input$board, "high_card")
  total_equity <- clamp01(mc_eq + (outs * 0.02))
  hand_type    <- .categorize_sophisticated(total_equity, "high_card")

  # ---- C. Strategic Layers (Tournament Logic) ----
  rng    <- runif(1) # Mixer for Unpredictability
  mdf    <- pot / (pot + call_amount)
  in_pos <- .is_in_position(bot_input)
  ctc    <- .cards_to_come(bot_input$street)

  # Tournament Survival: Added risk buffer for high stack commitment
  risk_premium <- if (call_amount > (stack * 0.20)) 0.08 else 0.03

  # ---- D. Decision Engine (The Merge) ----

  # 1. THE NUTS (AA, Sets, etc.)
  if (hand_type == "NUT_VALUE") {
    if (rng < 0.15 && can_check) return(list(type = "check")) # Slowplay trap
    size_mult <- .geo_sizing(pot, stack, ctc)
    if (can_raise) return(list(type = "raise", amount = as.integer(clamp(pot * size_mult, bot_min_raise(bot_input), bot_max_raise(bot_input)))))
    if (can_bet)   return(list(type = "bet",   amount = as.integer(clamp(pot * size_mult, bot_min_bet(bot_input), bot_max_bet(bot_input)))))
  }

  # 2. STRONG VALUE (Your "is_strong" logic)
  if (hand_type == "STRONG_VALUE") {
    if (rng < 0.70) {
      if (can_bet) return(list(type = "bet", amount = .target_bet("standard", pot, bot_min_bet(bot_input), bot_max_bet(bot_input))))
    }
    if (can_call && total_equity >= (1 - mdf) + risk_premium) return(list(type = "call"))
  }

  # 3. MARGINAL / SDV (Your "is_medium" logic)
  if (hand_type == "MARGINAL_SDV") {
    if (call_amount == 0) return(list(type = "check"))
    if (can_call && total_equity >= (1 - mdf) + risk_premium + 0.05) return(list(type = "call"))
  }

  # 4. BLUFFS (Your "should_bluff" logic)
  if (hand_type == "SEMI_BLUFF" || (hand_type == "AIR" && .has_blocker(bot_input$hole_cards, bot_input$board))) {
    # Bluff more often in position (using your position logic)
    bluff_freq <- if (in_pos) 0.22 else 0.12
    if (rng < bluff_freq) {
      if (can_bet) return(list(type = "bet", amount = .target_bet("bluff", pot, bot_min_bet(bot_input), bot_max_bet(bot_input))))
    }
  }

  # ---- E. Final Fallbacks ----
  if (can_check) return(list(type = "check"))
  # One last look at equity vs pot odds (Your original call logic)
  if (can_call && total_equity > (1 - mdf) + 0.12) return(list(type = "call"))

  return(list(type = "fold"))
}

lucy_bot <- function(bot_input) {

  ##########################################################
  # INFORMATION AVAILABLE TO YOUR BOT
  ##########################################################

  # Your identity
  player_id <- bot_input$player_id
  player_name <- bot_input$player_name
  seat <- bot_input$seat

  # Your private cards
  hole_cards <- bot_input$hole_cards

  # Board and street
  board <- bot_input$board
  street <- bot_input$street

  # Betting information
  pot <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed_this_round <- bot_input$committed_this_round
  committed_this_hand <- bot_input$committed_this_hand
  stack <- bot_input$stack

  # Blind / ante information
  small_blind <- bot_input$small_blind
  big_blind <- bot_input$big_blind
  ante <- bot_input$ante

  # Legal actions
  legal_types <- bot_input$legal_actions$legal_action_types

  # Public information about all players
  public_players <- bot_input$public_players

  # Previous actions in the hand
  action_history <- bot_input$action_history

  ##########################################################
  # YOUR STRATEGY GOES BELOW
  ##########################################################

  #Preflop

  if (street == "preflop" && length(hole_cards) == 2) {

    vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

    paired <- length(unique(vals)) == 1
    strong_ace <- max(vals) == 14 && min(vals) >= 10
    high_cards <- min(vals) >= 11

    premium <- paired && vals[1] >= 10      # TT+
    strong <- strong_ace || high_cards      # AQ, AJ, KQ,...

    # Premium hands:play aggressively
    if (premium) {
      if ("raise" %in% legal_types) {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
      if ("call" %in% legal_types) {
        return(list(type = "call"))
      }
    }

    # Strong Hands: call or raise occasionally
    if (strong) {
      if (runif(1) < 0.3 && "raise" %in% legal_types) {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
      if ("call" %in% legal_types) {
        return(list(type = "call"))
      }
    }

    # Occasional Bluff
    if (runif(1) < 0.1 && "raise" %in% legal_types) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }

    # Weak Hands
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }


  #Post-Flop

  category <- made_hand_category(hole_cards, board)

  strong_hands <- c("two_pair", "trips", "straight", "flush",
                    "full_house", "quads", "straight_flush")

  medium_hands <- c("pair")

  #Strong Hands, Aggressive

  if (category %in% strong_hands) {
    if ("raise" %in% legal_types) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if ("bet" %in% legal_types) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    if ("call" %in% legal_types) {
      return(list(type = "call"))
    }
  }

  #Meduim Hands, Pot Odds

  if (category %in% medium_hands) {

    # Check if possible
    if ("check" %in% legal_types) {
      return(list(type = "check"))
    }

    # Only call small bets
    if ("call" %in% legal_types && current_bet < 0.25 * pot) {
      return(list(type = "call"))
    }

    return(list(type = "fold"))
  }

  #Weak Hands, Bluff Sometimes

  if (runif(1) < 0.15) {
    if ("bet" %in% legal_types) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
  }

  #Default

  return(choose_preferred_action(bot_input, c("check", "fold")))
}



# jaymon ------------------------------------------------------------------


jp_hand_strength <- function(hole_cards) {
  if (length(hole_cards) != 2) return(0)

  r1 <- substr(hole_cards[1], 1, nchar(hole_cards[1]) - 1)
  s1 <- substr(hole_cards[1], nchar(hole_cards[1]), nchar(hole_cards[1]))
  r2 <- substr(hole_cards[2], 1, nchar(hole_cards[2]) - 1)
  s2 <- substr(hole_cards[2], nchar(hole_cards[2]), nchar(hole_cards[2]))

  rank_val <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
                "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)

  v1 <- rank_val[r1]; v2 <- rank_val[r2]
  if (v1 < v2) { tmp <- v1; v1 <- v2; v2 <- tmp; tmp_s <- s1; s1 <- s2; s2 <- tmp_s }

  suited <- (s1 == s2)

  if      (v1 == 14) base <- 10
  else if (v1 == 13) base <- 8
  else if (v1 == 12) base <- 7
  else if (v1 == 11) base <- 6
  else               base <- v1 / 2

  if (v1 == v2) {
    score <- max(base * 2, 5)
  } else {
    score <- base
    if (suited) score <- score + 2
    gap <- v1 - v2 - 1
    if (gap == 0)      score <- score + 1
    else if (gap == 2) score <- score - 1
    else if (gap == 3) score <- score - 2
    else if (gap >= 4) score <- score - 4
    if (v1 <= 8 && gap <= 1) score <- score + 1
  }

  ceiling(score)
}

jp_make_villain_range <- function(hole_cards, board) {
  known <- c(hole_cards, board)
  all_ranks <- c("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  all_suits <- c("h","s","c","d")
  remaining  <- setdiff(as.vector(outer(all_ranks, all_suits, paste0)), known)

  n <- length(remaining)
  if (n < 2) return(NULL)

  pairs <- combn(n, 2)
  combos_df <- data.frame(
    c1 = remaining[pairs[1, ]],
    c2 = remaining[pairs[2, ]],
    w  = 1.0,
    stringsAsFactors = FALSE
  )

  tryCatch(new_range_holdem(combos_df, label = "villain"), error = function(e) NULL)
}

jp_equity_estimate <- function(hole_cards, board, n_opponents = 1, n_sims = 700) {
  tryCatch({
    our_df       <- data.frame(rank = extract_rank_from_label(hole_cards), suit = extract_suit_from_label(hole_cards), stringsAsFactors = FALSE)
    board_df     <- data.frame(rank = extract_rank_from_label(board),      suit = extract_suit_from_label(board),      stringsAsFactors = FALSE)
    villain_rng  <- jp_make_villain_range(hole_cards, board)
    if (is.null(villain_rng)) return(0.5)

    n_opp     <- min(n_opponents, 4)
    hole_list <- c(list(our_df), replicate(n_opp, villain_rng, simplify = FALSE))

    result <- holdem_equity_mc_fast(hole_list, board_df, n_sims = n_sims)
    result$equity[1]
  }, error = function(e) jp_equity(hole_cards, board))
}

jp_equity <- function(hole_cards, board) {
  if (length(board) < 3) {
    rv <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
            "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)
    r1 <- substr(hole_cards[1], 1, nchar(hole_cards[1])-1)
    r2 <- substr(hole_cards[2], 1, nchar(hole_cards[2])-1)
    return(clamp01((rv[r1] + rv[r2]) / 28))
  }
  switch(made_hand_category(hole_cards, board),
         "straight_flush" = 0.97,
         "quads"          = 0.95,
         "full_house"     = 0.88,
         "flush"          = 0.78,
         "straight"       = 0.72,
         "trips"          = 0.65,
         "two_pair"       = 0.58,
         "pair"           = 0.45,
         0.28
  )
}

jp_opp_tendencies <- function(action_history, player_id) {
  if (length(action_history) == 0) return(0.4)
  opp <- Filter(function(a) !is.null(a$player_id) && a$player_id != player_id, action_history)
  if (length(opp) == 0) return(0.4)
  agg_types <- c("raise", "bet", "all_in")
  sum(sapply(opp, function(a) !is.null(a$type) && a$type %in% agg_types)) / length(opp)
}

jp_bet <- function(bot_input, pot_fraction) {
  if (!bot_has_action(bot_input, "bet")) return(NULL)
  mn <- bot_min_bet(bot_input); mx <- bot_max_bet(bot_input)
  if (is.null(mn) || is.null(mx)) return(NULL)
  max(mn, min(mx, round(bot_input$pot * pot_fraction)))
}

jp_raise <- function(bot_input, pot_fraction) {
  if (!bot_has_action(bot_input, "raise")) return(NULL)
  mn <- bot_min_raise(bot_input); mx <- bot_max_raise(bot_input)
  if (is.null(mn) || is.null(mx)) return(NULL)
  max(mn, min(mx, round(bot_input$pot * pot_fraction)))
}


############################################################
# STUDENT BOT
#
# Rename this function to your bot name.
# Example:
#   joe_bot <- function(bot_input) { ... }
############################################################

jaymon_bot <- function(bot_input) {

  ##########################################################
  # INFORMATION AVAILABLE TO YOUR BOT
  ##########################################################

  # Your identity
  player_id <- bot_input$player_id
  player_name <- bot_input$player_name
  seat <- bot_input$seat

  # Your private cards
  hole_cards <- bot_input$hole_cards

  # Board and street
  board <- bot_input$board
  street <- bot_input$street

  # Betting information
  pot <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed_this_round <- bot_input$committed_this_round
  committed_this_hand <- bot_input$committed_this_hand
  stack <- bot_input$stack

  # Blind / ante information
  small_blind <- bot_input$small_blind
  big_blind <- bot_input$big_blind
  ante <- bot_input$ante

  # Legal actions
  legal_types <- bot_input$legal_actions$legal_action_types

  # Public information about all players
  public_players <- bot_input$public_players

  # Previous actions in the hand
  action_history <- bot_input$action_history

  ##########################################################
  # YOUR STRATEGY GOES BELOW
  ##########################################################

  to_call <- max(0, current_bet - committed_this_round)

  n_active <- 1L
  if (!is.null(public_players) && length(public_players) > 0) {
    n_active <- max(1L, sum(sapply(public_players, function(p) {
      !is.null(p$player_id) && p$player_id != player_id && !isTRUE(p$folded)
    })))
  }

  opp_agg <- jp_opp_tendencies(action_history, player_id)
  is_short_stack <- (stack <= 8 * big_blind)

  if (street == "preflop") {

    strength <- jp_hand_strength(hole_cards)

    t1 <- if (opp_agg > 0.65) 11 else 9
    t2 <- if (opp_agg > 0.65) 8  else 7
    t3 <- if (opp_agg > 0.65) 6  else 5

    if (is_short_stack && strength >= t2) {
      if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      return(choose_preferred_action(bot_input, c("call", "raise", "check", "fold")))
    }

    if (strength >= t1) {
      target <- round(3 * big_blind + to_call)
      size <- jp_raise(bot_input, NA)
      if (bot_has_action(bot_input, "raise")) {
        mn <- bot_min_raise(bot_input); mx <- bot_max_raise(bot_input)
        size <- max(mn, min(mx, target))
        return(list(type = "raise", amount = size))
      }
      if (bot_has_action(bot_input, "bet")) {
        mn <- bot_min_bet(bot_input); mx <- bot_max_bet(bot_input)
        size <- max(mn, min(mx, round(3 * big_blind)))
        return(list(type = "bet", amount = size))
      }
      if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }

    if (strength >= t2) {
      if (to_call <= big_blind) {
        if (bot_has_action(bot_input, "raise")) {
          mn <- bot_min_raise(bot_input); mx <- bot_max_raise(bot_input)
          size <- max(mn, min(mx, round(2.5 * big_blind)))
          return(list(type = "raise", amount = size))
        }
        if (bot_has_action(bot_input, "bet")) {
          mn <- bot_min_bet(bot_input); mx <- bot_max_bet(bot_input)
          size <- max(mn, min(mx, round(2.5 * big_blind)))
          return(list(type = "bet", amount = size))
        }
      } else if (to_call <= 4 * big_blind) {
        return(choose_preferred_action(bot_input, c("call", "check", "fold")))
      }
      return(choose_preferred_action(bot_input, c("check", "fold")))
    }

    if (strength >= t3) {
      if (to_call <= 2 * big_blind) {
        return(choose_preferred_action(bot_input, c("check", "call", "fold")))
      }
      return(choose_preferred_action(bot_input, c("check", "fold")))
    }

    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  equity <- jp_equity_estimate(hole_cards, board, n_active, n_sims = 700)

  po_needed <- if (to_call > 0) pot_odds(to_call, pot) else 0

  curr_spr <- if (pot > 0) stack / pot else Inf

  board_df <- data.frame(rank = extract_rank_from_label(board), suit = extract_suit_from_label(board), stringsAsFactors = FALSE)
  bf <- if (nrow(board_df) >= 3) board_features(board_df) else NULL

  risk <- 0
  if (!is.null(bf)) {
    if (isTRUE(bf$monotone))  risk <- risk + 0.1
    if (isTRUE(bf$paired))    risk <- risk + 0.06
    if (bf$connectivity >= 2) risk <- risk + 0.06
  }
  eff_eq <- equity - risk * (1 - equity)

  streets_left <- switch(street, "flop" = 2L, "turn" = 1L, "river" = 0L, 0L)

  implied_threshold <- if (streets_left > 0 && to_call > 0)
    pot_odds(to_call, pot + to_call * 2.5) else po_needed

  should_call <- eff_eq >= po_needed || (eff_eq >= implied_threshold && equity >= 0.28)

  if (is_short_stack && eff_eq >= 0.45) {
    if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
  }

  if (eff_eq >= 0.62) {
    if (to_call > 0) {
      if (bot_has_action(bot_input, "raise")) {
        frac <- if (curr_spr < 3) 1.0 else 0.8
        size <- jp_raise(bot_input, frac)
        if (!is.null(size)) return(list(type = "raise", amount = size))
      }
      if (should_call) return(choose_preferred_action(bot_input, c("call", "all_in")))
      return(choose_preferred_action(bot_input, c("call", "fold")))
    } else {
      frac <- if (!is.null(bf) && (isTRUE(bf$monotone) || isTRUE(bf$paired))) 0.75 else 0.65
      if (curr_spr < 2 && bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      size <- jp_bet(bot_input, frac)
      if (!is.null(size)) return(list(type = "bet", amount = size))
      return(choose_preferred_action(bot_input, c("check", "call")))
    }
  }

  if (eff_eq >= 0.47) {
    if (to_call > 0) {
      if (should_call) return(choose_preferred_action(bot_input, c("call", "check", "fold")))
      return(choose_preferred_action(bot_input, c("check", "fold")))
    } else {
      if (equity >= 0.65) {
        size <- jp_bet(bot_input, 0.50)
        if (!is.null(size)) return(list(type = "bet", amount = size))
      }
      return(choose_preferred_action(bot_input, c("check", "call", "fold")))
    }
  }

  if (eff_eq >= 0.30 && streets_left > 0) {
    if (to_call > 0) {
      if (should_call) return(choose_preferred_action(bot_input, c("call", "check", "fold")))
      return(choose_preferred_action(bot_input, c("check", "fold")))
    } else {
      if (equity >= 0.36 && runif(1) < 0.35) {
        size <- jp_bet(bot_input, 0.40)
        if (!is.null(size)) return(list(type = "bet", amount = size))
      }
      return(choose_preferred_action(bot_input, c("check", "fold")))
    }
  }

  if (to_call > 0) {
    if (should_call && to_call <= big_blind) {
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  return(choose_preferred_action(bot_input, c("check", "fold")))
}


tara_bot <- function(bot_input) {

  ##########################################################
  # INFORMATION AVAILABLE TO YOUR BOT
  ##########################################################

  # Your identity
  player_id <- bot_input$player_id
  player_name <- bot_input$player_name
  seat <- bot_input$seat

  # Your private cards
  hole_cards <- bot_input$hole_cards

  # Board and street
  board <- bot_input$board
  street <- bot_input$street

  # Betting information
  pot <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed_this_round <- bot_input$committed_this_round
  committed_this_hand <- bot_input$committed_this_hand
  stack <- bot_input$stack

  # Blind / ante information
  small_blind <- bot_input$small_blind
  big_blind <- bot_input$big_blind
  ante <- bot_input$ante

  # Legal actions
  legal_types <- bot_input$legal_actions$legal_action_types

  # Public information about all players
  public_players <- bot_input$public_players

  # Previous actions in the hand
  action_history <- bot_input$action_history

  # extract rank from string
  get_rank <- function(card) substr(card, 1, 1)
  get_suit <- function(card) substr(card, 2, 2)

  # convert rank to number
  rank_to_value <- function(rank) {
    if (rank %in% c("A")) return(14)
    if (rank %in% c("K")) return(13)
    if (rank %in% c("Q")) return(12)
    if (rank %in% c("J")) return(11)
    if (rank %in% c("T")) return(10)
    return(as.numeric(rank))
  }

  ranks <- sapply(hole_cards, get_rank)
  suits <- sapply(hole_cards, get_suit)
  values <- sapply(ranks, rank_to_value)

  v_1 <- values[1]
  v_2 <- values[2]

  is_pair <- (v_1 == v_2)
  is_suited <- (suits[1] == suits[2])
  gap <- abs(v_1 - v_2)

  hand_strength <- v_1 + v_2


  ##########################################################
  # YOUR STRATEGY GOES BELOW
  ##########################################################

  # Example beginner strategy:
  # check if possible, otherwise call, otherwise fold

  # high cards: always call
  if (hand_strength >= 25) {
    if("call" %in% legal_types) return(list(type = "call"))
    if("check" %in% legal_types) return(list(type = "check"))
  }

  # medium cards: call 20% of the time
  if (hand_strength >= 18) {
    if (runif(1) < 0.5) {
      if("call" %in% legal_types) return(list(type = "call"))
      if("check" %in% legal_types) return(list(type = "check"))
    } else {
      if ("fold" %in% legal_types) return(list(type = "fold"))
    }
  }


  # low cards: fold
  if ("fold" %in% legal_types) {
    return(list(type = "fold"))
  }


  # Fallback
  return(choose_preferred_action(bot_input, c("check", "call", "fold")))
}

joel_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  pot <- bot_input$pot
  call_amount <- bot_call_amount(bot_input)

  # -------------------------
  # HAND STRENGTH
  # -------------------------
  vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

  paired <- length(unique(vals)) == 1
  high_card <- max(vals) >= 13   # K or A

  # -------------------------
  # POT ODDS
  # -------------------------
  odds <- pot_odds(call_amount, pot)

  # -------------------------
  # DECISION LOGIC
  # -------------------------

  # STRONG → aggressive
  if (paired || max(vals) >= 14) {
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    return(choose_preferred_action(bot_input, c("call", "check")))
  }

  # MEDIUM → pot odds
  if (high_card) {
    if (odds < 0.3) {
      return(choose_preferred_action(bot_input, c("call", "check")))
    } else {
      return(list(type = "fold"))
    }
  }

  # WEAK → fold unless free
  if (call_amount == 0) {
    return(list(type = "check"))
  }

  return(list(type = "fold"))
}

# Nikola bot --------------------------------------------------------------


Nikola_bot <- function(bot_input) {

  ##########################################################

  # 1. READ INFORMATION FROM THE GAME

  ##########################################################

  hole_cards <- bot_input$hole_cards

  board <- bot_input$board

  street <- bot_input$street

  pot <- bot_input$pot

  current_bet <- bot_input$current_bet

  committed <- bot_input$committed_this_round

  stack <- bot_input$stack

  big_blind <- bot_input$big_blind

  legal_types <- bot_input$legal_actions$legal_action_types

  amount_to_call <- max(0, current_bet - committed)

  ##########################################################

  # 2. SAFE ACTION HELPERS

  ##########################################################

  safe_action <- function(actions) {

    choose_preferred_action(bot_input, actions)

  }

  max_bet_safe <- function() {

    amount <- bot_max_bet(bot_input)

    if (is.null(amount)) {

      return(bot_min_bet(bot_input))

    }

    amount

  }

  max_raise_safe <- function() {

    amount <- bot_max_raise(bot_input)

    if (is.null(amount)) {

      return(bot_min_raise(bot_input))

    }

    amount

  }

  min_bet_safe <- function() {

    bot_min_bet(bot_input)

  }

  min_raise_safe <- function() {

    bot_min_raise(bot_input)

  }

  ##########################################################

  # 3. CARD HELPERS

  ##########################################################

  get_rank <- function(card) {

    substr(card, 1, 1)

  }

  get_suit <- function(card) {

    substr(card, nchar(card), nchar(card))

  }

  rank_value_local <- function(rank) {

    values <- c(

      "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,

      "7" = 7, "8" = 8, "9" = 9,

      "T" = 10, "J" = 11, "Q" = 12, "K" = 13, "A" = 14

    )

    as.numeric(values[rank])

  }

  ranks <- sapply(hole_cards, get_rank)

  suits <- sapply(hole_cards, get_suit)

  values <- sort(sapply(ranks, rank_value_local), decreasing = TRUE)

  high <- values[1]

  low <- values[2]

  pair <- ranks[1] == ranks[2]

  suited <- suits[1] == suits[2]

  connected <- abs(high - low) <= 1

  one_gap <- abs(high - low) == 2

  any_ace <- high == 14

  strong_ace <- high == 14 && low >= 9

  broadway <- high >= 12 && low >= 10

  king_good <- high == 13 && low >= 9

  queen_good <- high == 12 && low >= 9

  premium_pair <- pair && high >= 10

  medium_pair <- pair && high >= 6

  small_pair <- pair && high < 6

  ##########################################################

  # 4. TABLE SITUATION

  ##########################################################

  active_players <- 0

  for (seat_name in names(bot_input$public_players)) {

    player <- bot_input$public_players[[seat_name]]

    if (player$status == "active" && !player$folded) {

      active_players <- active_players + 1

    }

  }

  is_heads_up <- active_players <= 2

  is_multiway <- active_players >= 4

  stack_bb <- stack / big_blind

  pot_odds_value <- ifelse(

    amount_to_call == 0,

    0,

    amount_to_call / (pot + amount_to_call)

  )

  ##########################################################

  # 5. BLUFF CONTROL

  #

  # 8% bluff rate. Small enough to avoid punting chips,

  # but enough so the bot is not predictable.

  ##########################################################

  bluff_now <- runif(1) < 0.08

  ##########################################################

  # 6. PREFLOP HAND SCORE

  #

  # Bigger score = stronger starting hand.

  ##########################################################

  preflop_score <- 0

  if (premium_pair) preflop_score <- preflop_score + 100

  if (medium_pair) preflop_score <- preflop_score + 75

  if (small_pair) preflop_score <- preflop_score + 50

  if (strong_ace) preflop_score <- preflop_score + 85

  if (any_ace && !strong_ace) preflop_score <- preflop_score + 60

  if (broadway) preflop_score <- preflop_score + 70

  if (king_good) preflop_score <- preflop_score + 60

  if (queen_good) preflop_score <- preflop_score + 50

  if (suited) preflop_score <- preflop_score + 12

  if (connected) preflop_score <- preflop_score + 10

  if (one_gap) preflop_score <- preflop_score + 5

  if (high >= 13) preflop_score <- preflop_score + 12

  if (low >= 10) preflop_score <- preflop_score + 10

  if (low <= 5 && !pair && !any_ace) {

    preflop_score <- preflop_score - 20

  }

  ##########################################################

  # 7. PREFLOP STRATEGY

  ##########################################################

  if (street == "preflop") {

    very_strong <- preflop_score >= 85

    strong <- preflop_score >= 65

    playable <- preflop_score >= 48

    ########################################################

    # VERY STRONG PREFLOP HANDS

    # Examples: TT+, AK, AQ, strong Broadway hands

    ########################################################

    if (very_strong) {

      if ("raise" %in% legal_types) {

        return(list(type = "raise", amount = min_raise_safe()))

      }

      if ("bet" %in% legal_types) {

        return(list(type = "bet", amount = max_bet_safe()))

      }

      return(safe_action(c("call", "check", "fold")))

    }

    ########################################################

    # HEADS-UP PREFLOP

    # Play wider because one opponent means weaker average hand.

    ########################################################

    if (is_heads_up) {

      heads_up_playable <- strong || high >= 10 || suited || connected || any_ace || low >= 7

      if (stack_bb <= 6 && heads_up_playable) {
        if ("raise" %in% legal_types) {
          return(list(type = "raise", amount = min_raise_safe()))
        }

        return(safe_action(c("call", "check", "fold")))
      }

      if (heads_up_playable) {

        if ("raise" %in% legal_types && amount_to_call <= 2 * big_blind) {

          return(list(type = "raise", amount = min_raise_safe()))

        }

        if (amount_to_call <= 4 * big_blind) {

          return(safe_action(c("call", "check", "fold")))

        }

      }

      if (amount_to_call == 0) {

        if (bluff_now && "bet" %in% legal_types) {

          return(list(type = "bet", amount = max_bet_safe()))

        }

        return(safe_action(c("check", "fold")))

      }

      return(safe_action(c("fold", "check")))

    }

    ########################################################

    # MULTIWAY PREFLOP

    # Be tighter because many players can beat medium hands.

    ########################################################

    if (is_multiway) {

      if (strong) {

        if ("raise" %in% legal_types && amount_to_call <= 2 * big_blind) {

          return(list(type = "raise", amount = min_raise_safe()))

        }

        if (amount_to_call <= 2.5 * big_blind) {

          return(safe_action(c("call", "check", "fold")))

        }

      }

      if (playable && amount_to_call <= 0.5 * big_blind) {
        return(safe_action(c("call", "check", "fold")))
      }
      return(safe_action(c("check", "fold")))

    }

    ########################################################

    # NORMAL 3-PLAYER PREFLOP

    ########################################################

    if (strong) {

      if ("raise" %in% legal_types && amount_to_call <= 2 * big_blind) {

        return(list(type = "raise", amount = min_raise_safe()))

      }

      return(safe_action(c("call", "check", "fold")))

    }

    if (playable && amount_to_call <= 1.5 * big_blind) {

      return(safe_action(c("call", "check", "fold")))

    }

    return(safe_action(c("check", "fold")))

  }

  ##########################################################

  # 8. POSTFLOP HAND CATEGORY

  ##########################################################

  category <- tryCatch(

    made_hand_category(hole_cards, board),

    error = function(e) "high_card"

  )

  monster <- category %in% c(

    "straight_flush", "four_kind", "quads",

    "full_house", "flush", "straight"

  )

  strong_made <- category %in% c(

    "three_kind", "trips", "two_pair"

  )

  medium_made <- category %in% c("pair")

  ##########################################################

  # 9. EQUITY ESTIMATE

  #

  # Equity = estimated chance of winning the hand.

  ##########################################################

  equity <- NA

  try({

    hole_df <- card_labels_to_df(hole_cards)

    if (length(board) > 0) {

      board_df <- card_labels_to_df(board)

    } else {

      board_df <- data.frame(rank = character(), suit = character())

    }

    eq_result <- holdem_equity_mc_fast(

      hole_list = list(hole_df),

      board_df = board_df,

      n_sims = 150

    )

    if ("win_prob" %in% names(eq_result)) {

      equity <- eq_result$win_prob[1]

    } else if ("equity" %in% names(eq_result)) {

      equity <- eq_result$equity[1]

    }

  }, silent = TRUE)

  ##########################################################

  # 10. FALLBACK EQUITY

  #

  # If Monte Carlo fails, estimate equity from hand category.

  ##########################################################

  if (is.na(equity)) {
    if (monster) {
      equity <- 0.92
    } else if (strong_made) {
      equity <- 0.75
    } else if (medium_made && street == "flop") {
      equity <- 0.52
    } else if (medium_made && street == "turn") {
      equity <- 0.46
    } else if (medium_made && street == "river") {
      equity <- 0.38
    } else if (street == "flop") {
      equity <- 0.30
    } else if (street == "turn") {
      equity <- 0.24
    } else {
      equity <- 0.18
    }
  }

  call_is_big <- amount_to_call >= 0.35 * stack

  if (call_is_big && equity < 0.65 && !monster && !strong_made) {
    return(safe_action(c("fold", "check")))
  }
  ##########################################################

  # 11. MONSTER HANDS

  #

  # Straight, flush, full house, quads, etc.

  # Goal: build the pot.

  ##########################################################

  if (monster || equity >= 0.75) {

    if ("raise" %in% legal_types) {

      return(list(type = "raise", amount = max_raise_safe()))

    }

    if ("bet" %in% legal_types) {

      return(list(type = "bet", amount = max_bet_safe()))

    }

    return(safe_action(c("call", "check", "fold")))

  }

  ##########################################################

  # 12. STRONG HANDS

  #

  # Trips, two pair, or strong equity.

  # Goal: value bet, but avoid donating on river.

  ##########################################################

  if (strong_made || equity >= 0.62) {

    if ("raise" %in% legal_types && equity >= 0.70) {

      return(list(type = "raise", amount = min_raise_safe()))

    }

    if ("bet" %in% legal_types) {

      if (street == "river") {

        return(list(type = "bet", amount = min_bet_safe()))

      }

      return(list(type = "bet", amount = max_bet_safe()))

    }

    if (equity > pot_odds_value + 0.08) {

      return(safe_action(c("call", "check", "fold")))

    }

    return(safe_action(c("check", "fold")))

  }

  ##########################################################

  # 13. MEDIUM HANDS

  #

  # Usually one pair or okay equity.

  # Goal: call only if the price is good.

  ##########################################################

  if (medium_made || equity >= 0.48) {

    if (amount_to_call == 0) {

      if ("bet" %in% legal_types && equity >= 0.48) {

        return(list(type = "bet", amount = min_bet_safe()))

      }

      return(safe_action(c("check", "fold")))

    }

    if (is_heads_up) {

      if (street == "river") {

        if (equity >= 0.52 && pot_odds_value <= 0.28) {

          return(safe_action(c("call", "fold")))

        }

      } else {

        if (!call_is_big && (equity > pot_odds_value + 0.05 || equity >= 0.48)) {

          return(safe_action(c("call", "fold")))

        }

      }

    } else {

      if (equity > pot_odds_value + 0.12 || equity >= 0.57) {

        return(safe_action(c("call", "fold")))

      }

    }

    return(safe_action(c("fold", "check")))

  }

  ##########################################################

  # 14. SMART BLUFFS

  #

  # Only bluff when nobody has bet into us.

  ##########################################################

  if (bluff_now && amount_to_call == 0 && equity >= 0.28) {

    if ("bet" %in% legal_types) {

      return(list(type = "bet", amount = min_bet_safe()))

    }

    if ("raise" %in% legal_types) {

      return(list(type = "raise", amount = min_raise_safe()))

    }

  }

  ##########################################################

  # 15. WEAK HANDS

  #

  # Mostly check/fold. Only call if pot odds are clearly good.

  ##########################################################

  if (amount_to_call == 0) {

    return(safe_action(c("check", "fold")))

  }

  if (is_heads_up && equity >= 0.43 && pot_odds_value <= 0.32) {

    return(safe_action(c("call", "fold")))

  }

  if (!is_heads_up && equity > pot_odds_value + 0.15) {

    return(safe_action(c("call", "fold")))

  }

  return(safe_action(c("fold", "check")))

}