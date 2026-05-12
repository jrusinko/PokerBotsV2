# Siena_bot ----
# TOURNAMENT BOT â€” v6  "Survive and Strike"
#
# Core insight from results:
#   - Random Bot won by luck/variance
#   - Passive Bot outlasted everyone by folding and bleeding slowly
#   - Our bot was busting BEFORE the always_call_bot â€” meaning
#     we were entering pots and losing them to callers
#
# New philosophy:
#   1. FOLD most hands preflop â€” tighter than passive bot
#   2. ONLY continue postflop with two pair or better (score >= 0.67)
#      One pair is NOT good enough to bet or call against callers
#   3. When we DO have two pair+, bet it clearly for value
#   4. No c-bets, no bluffs, no probe bets â€” callers punish all of these
#   5. Check/fold everything else and let the other bots fight
#   6. All-in only on river or turn with near-nut hand (score >= 0.88)
#   7. Never call more than 8% of stack without two pair or better
#
# HOW TO USE:
#   source("poker_load_all.R")
#   poker_load_all(include_demos = TRUE, verbose = FALSE)
#   source("my_bot.R")
#   Rename "my_bot" to your tournament registration name.


# â”€â”€ Opponent tracker â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
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


# â”€â”€ Card helpers â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# hole_cards and board are CHARACTER VECTORS e.g. c("Ah","Kd")

.RANKS <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
            "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)

.ranks <- function(cards) unname(.RANKS[ substring(cards,1,nchar(cards)-1) ])
.suits <- function(cards) substring(cards,nchar(cards),nchar(cards))


# â”€â”€ Preflop tier â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Only 3 tiers â€” simpler and harder to make mistakes with.
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


# â”€â”€ Postflop hand score (0â€“1) â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Returns how strong our made hand is RIGHT NOW.
# Does NOT include draw equity â€” we do not chase draws against callers.

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
      # One pair â€” score reflects quality but stays well below 0.67
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


# â”€â”€ Safe bet/raise sizing â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

.bet <- function(bot_input, frac) {
  p  <- bot_input$pot; st <- bot_input$stack
  mn <- bot_min_bet(bot_input)
  mx <- bot_max_bet(bot_input)
  as.integer(max(mn, min(round(p*frac), mx, st)))
}

.raise_to <- function(bot_input, frac) {
  p  <- bot_input$pot; st <- bot_input$stack
  mn <- bot_min_raise(bot_input)
  mx <- bot_max_raise(bot_input)
  as.integer(max(mn, min(round(p*frac), mx, st)))
}

.bb_bet <- function(bot_input, mult, bb) {
  st <- bot_input$stack
  mn <- bot_min_bet(bot_input)
  mx <- bot_max_bet(bot_input)
  as.integer(max(mn, min(round(mult*bb), mx, st)))
}

.bb_raise <- function(bot_input, mult, bb) {
  st <- bot_input$stack
  mn <- bot_min_raise(bot_input)
  mx <- bot_max_raise(bot_input)
  as.integer(max(mn, min(round(mult*bb), mx, st)))
}


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# MAIN BOT â€” rename to your tournament registration name
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

Siena_bot <- function(bot_input) {

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


  # PREFLOP

  if (street == "preflop") {

    tier <- .preflop_tier(hole_cards)

    # â”€â”€ FOLD tier: immediately fold to any cost, check if free â”€â”€â”€â”€â”€â”€â”€â”€
    if (tier == "fold") {
      if (can_check) return(list(type="check"))
      return(list(type="fold"))
    }

    # â”€â”€ SPECULATIVE: small pairs, suited connectors, suited aces â”€â”€â”€â”€â”€â”€
    # Only see the flop for FREE. If someone has raised even 1 BB extra,
    # fold. These hands need to make two pair or better on the flop to
    # continue â€” and even then, we just check/call small or bet ourselves.
    if (tier == "speculative") {
      if (can_check) return(list(type="check"))
      # Call ONLY if it costs at most 1 BB and we are not already raised
      if (can_call && call_amount <= bb && current_bet <= bb)
        return(list(type="call"))
      return(list(type="fold"))
    }

    # â”€â”€ PREMIUM: TT+, AK, AQ, AJ, KQ â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
    # These are the only hands we invest real chips with preflop.
    #
    # Sizing philosophy:
    #   - Open to 2.5x BB when first in â€” small enough to get called
    #     by weaker hands, big enough to deny free flops to speculative hands
    #   - When facing a raise: call if it's < 20% of stack
    #   - Never go all-in preflop â€” we want to see the board first
    if (tier == "premium") {


      if (current_bet > bb) {
        # Facing a raise â€” call if cheap, fold if expensive
        stack_cost <- call_amount / max(1, stack)
        po         <- pot_odds(call_amount, pot)

        # Too expensive: fold and preserve chips
        if (stack_cost > 0.20) {
          return(list(type="fold"))
        }

        # Reasonable price: call (don't re-raise â€” keep pot manageable)
        if (can_call && po <= 0.33) {
          return(list(type="call"))
        }
        if (can_check) return(list(type="check"))
        return(list(type="fold"))
      }

      # First in: open raise to 2.5x BB
      if (can_raise) {
        return(list(type="raise",
                                  amount=.bb_raise(bot_input, 2.5, bb)))
      }
      if (can_bet) {
        return(list(type="bet",
                                  amount=.bb_bet(bot_input, 2.5, bb)))
      }
      if (can_call)  return(list(type="call"))
    }

    # Safety net
    if (can_check) return(list(type="check"))
    return(list(type="fold"))
  }


  # POSTFLOP â€” flop, turn, river

  # Get current made-hand score (no draw equity â€” we don't chase)
  score <- .hand_score(hole_cards, board)

  # Multiway discount: each extra opponent lowers our effective equity
  score <- score - max(0, (n_opp - 1L) * 0.06)
  score <- max(0.05, min(0.97, score))

  # What fraction of our stack would a call cost?
  stack_pct <- if (call_amount > 0) call_amount / max(1, stack) else 0.0

  # Pot odds
  po <- if (call_amount > 0) pot_odds(call_amount, pot) else 0.0

  # EV of calling (from poker_math.R â€” win_prob * pot - (1-win_prob) * call)
  ev <- if (call_amount > 0)
    ev_call(equity=score, pot_before_call=pot, call_amount=call_amount)
  else 0.0

  # â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
  # POSTFLOP DECISION FRAMEWORK
  #
  # Three clean tiers based on made-hand score:
  #
  # TIER A â€” Two pair or better (score >= 0.67):
  #   Bet for value every time. This is the only situation where we
  #   voluntarily put significant chips in. Size to extract max value
  #   from the callers. On turn/river with near-nuts, go all-in.
  #
  # TIER B â€” Overpair / top pair with strong kicker (score 0.55â€“0.67):
  #   Check when possible. Call ONLY if the price is small (< 8% stack)
  #   and EV is clearly positive. No betting â€” one pair loses too often
  #   against the callers who will have hit something.
  #
  # TIER C â€” Everything else (score < 0.55):
  #   Check and fold. Do not call. Do not bet. Wait for the next hand.
  #   Every chip saved here is a chip that keeps us alive.
  #
  # NO bluffing at all â€” caller bots and random bots don't fold enough.
  # NO c-bets â€” we don't have range advantage worth betting without a hand.
  # NO chasing draws â€” pot odds rarely justify it with multiple callers.
  # â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

  # â”€â”€ TIER A: Two pair or better â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  if (score >= 0.67) {

    # ALL-IN CONDITIONS (end of hand, near-nuts only):
    #   - River with score >= 0.88  (straight/flush/boat/quads)
    #   - Turn with score >= 0.88 AND already pot-committed (>= 25% stack)
    near_nuts    <- score >= 0.88
    on_river     <- street == "river"
    on_turn      <- street == "turn"
    committed_pct <- committed / max(1, stack + committed)

    if (near_nuts && on_river && can_allin) {
      return(list(type="all_in"))
    }

    if (near_nuts && on_turn && committed_pct >= 0.25 && can_allin) {
      return(list(type="all_in"))
    }

    # Normal value bet: size scales with hand strength
    # Bigger bets with stronger hands â€” callers will pay us off
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

  # â”€â”€ TIER B: Strong one pair (overpair / top pair good kicker) â”€â”€â”€â”€â”€â”€â”€â”€
  # score 0.55â€“0.67: we have something real but not two pair.
  # Strategy: check behind to keep pot small, call ONLY tiny bets.
  # Fold to any significant bet â€” one pair loses to two pair or better
  # and the callers will have that frequently.
  if (score >= 0.55) {
    if (can_check) {
      return(list(type="check"))
    }

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

  # â”€â”€ TIER C: Weak hand / mediocre pair / high card â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
  # score < 0.55: check or fold. Full stop.
  # This includes middle pair, weak pair, all draws, and high card.
  # We do NOT call, bet, or raise in this tier under any circumstances.
  if (can_check) {
    return(list(type="check"))
  }
  return(list(type="fold"))
}

# mehdi_bot ----
source("poker_load_all.R")
poker_load_all(include_demos = TRUE, verbose = FALSE)

mehdi_bot <- function(bot_input) {

  # Read the game state
  legal <- bot_input$legal_actions$legal_action_types
  street <- bot_input$street
  pot <- bot_input$pot
  # Check if we are facing a bet
  facing_bet <- bot_input$current_bet > bot_input$committed_this_round
  # Get our card values (e.g., c(14, 10) for Ace, Ten)
  hole_vals <- sort(hole_rank_values(bot_input$hole_cards), decreasing = TRUE)


  # This helper function places a bet equal to the pot size.
  # It ensures the bet stays within the minimum and maximum legal limits.
  calc_pot_bet <- function() {
    target <- as.integer(pot)
    min_b <- bot_min_bet(bot_input)
    max_b <- bot_max_bet(bot_input)
    if (!is.null(min_b) && !is.null(max_b)) {
      return(max(as.integer(min_b), min(target, as.integer(max_b))))
    }
    return(min_b)
  }

  # Preflop Strategy:
  #
  # Before any board cards are shown, the bot only plays safer starting hands.
  # It plays pocket pairs or two high cards, and folds weaker hands.

  if (street == "preflop") {

    # A hand is treated as strong if it is a pair or both cards are 10 or higher
    is_strong_preflop <- (hole_vals[1] == hole_vals[2]) || (hole_vals[1] >= 10 && hole_vals[2] >= 10)


    if (is_strong_preflop) {
      # With a strong starting hand, the bot tries to raise; if thatâ€™s not legal, it bets;
      # if thatâ€™s not legal, it calls.
      if ("raise" %in% legal) return(list(type = "raise", amount = bot_min_raise(bot_input)))
      if ("bet" %in% legal) return(list(type = "bet", amount = bot_min_bet(bot_input)))
      if ("call" %in% legal) return(list(type = "call"))
    } else {
      # If the hand is not strong, check if it is legal; if it isnâ€™t, fold.
      if ("check" %in% legal && !facing_bet) return(list(type = "check"))
      return(list(type = "fold"))
    }
  }

  # Postflop Strategy
  #
  # If the bot has a made hand, it calls when facing a bet and bets aggressively if no one has bet yet.
  # If the bot has a weak hand, it folds when facing a bet and bluffs if no one has bet yet.

  # Check if we have made a hand (pair or better).
  board_vals <- sort(hole_rank_values(bot_input$board), decreasing = TRUE)
  is_made_hand <- (hole_vals[1] == hole_vals[2]) ||
    (hole_vals[1] %in% board_vals) ||
    (hole_vals[2] %in% board_vals)

  # If we have a made hand, call if facing a bet;
  # if no one has bet yet, make a bet equal to the pot size.
  if (is_made_hand) {
    if (facing_bet) {
      if ("call" %in% legal) {
        return(list(type = "call"))
      }
    } else {
      if ("bet" %in% legal) {
        return(list(type = "bet", amount = calc_pot_bet()))
      }
    }
  }

  # If we have a weak hand: if facing a bet, donâ€™t bluff, fold;
  # if no one has bet, bluff with a bet equal to the pot.
  else {
    if (facing_bet) {
      if ("check" %in% legal) return(list(type = "check"))
      return(list(type = "fold"))
    } else {
      if ("bet" %in% legal) {
        return(list(type = "bet", amount = calc_pot_bet()))
      }
    }
  }


  # Back-up Strategy
  #
  # If our code reaches here due to strange game states, default to safe moves.
  if ("check" %in% legal) return(list(type = "check"))
  if ("call" %in% legal) return(list(type = "call"))
  return(list(type = "fold"))
}

# nate_bot ----
# Bot Name: Bill the Dinosaur (The Caller-Slayer Final)
# Strategy: Delayed Aggression / The Trap / Silent

nate_bot <- function(bot_input) {
  street      <- bot_input$street
  pot         <- bot_input$pot
  stack       <- bot_input$stack
  hole_cards  <- bot_input$hole_cards
  board       <- bot_input$board
  bb          <- bot_input$big_blind

  category    <- made_hand_category(hole_cards, board)
  vals        <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

  # 1. THE SAFETY CLAMP
  legal_bet_amount <- function(amt) {
    if (!bot_has_action(bot_input, "bet")) return(NA_real_)
    as.integer(max(bot_min_bet(bot_input), min(as.numeric(amt), bot_max_bet(bot_input))))
  }

  legal_raise_amount <- function(amt) {
    if (!bot_has_action(bot_input, "raise")) return(NA_real_)
    as.integer(max(bot_min_raise(bot_input), min(as.numeric(amt), bot_max_raise(bot_input))))
  }


  # 2. PREFLOP: THE TAX
  # We make it expensive for them to exist.
  if (identical(street, "preflop")) {
    if (max(vals) >= 13 || length(unique(vals)) == 1) {
      if (bot_has_action(bot_input, "raise")) {
        return(list(type = "raise", amount = legal_raise_amount(bot_min_raise(bot_input) * 5))) # 5x Raise!
      }
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  # 3. POSTFLOP: THE DELAYED TRAP

  has_strong_pair <- (category == "pair" && vals[1] >= 11) || (category == "two_pair")
  is_monster      <- category %in% c("trips", "straight", "flush", "full_house", "quads")

  # --- THE RIVER KILL-SHOT ---
  # If we reach the River and we have a strong hand, we SHOVE.
  if (identical(street, "river")) {
    if (has_strong_pair || is_monster) {
      if (bot_has_action(bot_input, "bet") || bot_has_action(bot_input, "raise")) {
        if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
        if (bot_has_action(bot_input, "raise")) {
          return(list(type = "raise", amount = bot_max_raise(bot_input))) # ALL IN-style raise
        }
        return(list(type = "bet", amount = bot_max_bet(bot_input)))
      }
    }
  }

  # --- THE TURN/FLOP FILTER ---
  # On the Flop and Turn, we are "Sticky." We don't bet big.
  # We just call or check to keep the pot small until we are sure.
  if (has_strong_pair || is_monster) {
    # Small "Probing" bet to build the pot slowly
    if (bot_has_action(bot_input, "bet")) {
      if (is_monster) {
      } else {
      }
      return(list(type = "bet", amount = legal_bet_amount(floor(pot * 0.4))))
    }
    return(choose_preferred_action(bot_input, c("call", "check")))
  }

  # 4. THE NO-BLUFF POLICY
  return(choose_preferred_action(bot_input, c("check", "fold")))
}

# ruth_bot ----
ruth_bot <- function(bot_input) {

  ##### --- 1. DATA EXTRACTION --- #####
  # Pulling current game state from the engine's input object
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


  ##### --- 2. DYNAMIC EQUITY CALCULATOR --- #####
  # Estimates our winning probability based on hand rank, board height, and draws.
  calculate_dynamic_equity <- function() {
    category <- made_hand_category(hole_cards, board)

    # Baseline win-rate estimates for various hand categories
    base_eq <- switch(category,
                      "straight_flush" = 0.99, "quads" = 0.95, "full_house" = 0.90,
                      "flush" = 0.85, "straight" = 0.80, "trips" = 0.75,
                      "two_pair" = 0.65, "pair" = 0.50, "high_card" = 0.15, 0.10
    )

    # REFINEMENT: Not all pairs are equal.
    # Checks if our pair is an Overpair (above the board), Top Pair, or Bottom Pair.
    if (category == "pair" && length(board) >= 1) {
      hole_vals <- hole_rank_values(hole_cards)
      board_vals <- rank_to_numeric(substring(board, 1, 1))
      max_board <- max(board_vals)
      my_pair_val <- ifelse(hole_vals[1] == hole_vals[2], hole_vals[1],
                            hole_vals[hole_vals %in% board_vals][1])

      if (is.na(my_pair_val)) my_pair_val <- 0

      if (my_pair_val > max_board) base_eq <- 0.75      # Very strong: Pocket pair higher than board
      else if (my_pair_val == max_board) base_eq <- 0.60 # Strong: Pair matches highest board card
      else base_eq <- 0.40                               # Vulnerable: Mid/Bottom pair
    }

    # DRAW DETECTION: Implementation of the 'Rule of 2 and 4'
    # Estimates chances of hitting a flush on future streets.
    multiplier <- ifelse(street == "flop", 4, 2)
    if (length(board) >= 3) {
      suits <- c(substring(hole_cards, 2, 2), substring(board, 2, 2))
      # If 4 cards of the same suit are present (hole + board), we have a Flush Draw (9 outs)
      if (any(table(suits) == 4)) base_eq <- max(base_eq, (9 * multiplier) / 100)
    }

    return(base_eq)
  }

  ##### --- 3. HELPER FUNCTIONS --- #####

  # THE SAFETY GATE: Crucial for ensuring all bot actions are engine-legal.
  # Rounds values and clamps them between the current min and max legal limits.
  sanitize_action <- function(action_list) {
    type <- action_list$type
    if (type %in% c("bet", "raise")) {
      min_amt <- if(type == "bet") bot_min_bet(bot_input) else bot_min_raise(bot_input)
      max_amt <- if(type == "bet") bot_max_bet(bot_input) else bot_max_raise(bot_input)
      amount <- round(action_list$amount)
      amount <- max(min_amt, min(max_amt, amount))
      return(list(type = type, amount = amount))
    }
    # Fallback to a check/fold preference if an illegal action type is suggested
    if (!(type %in% legal_types)) return(choose_preferred_action(bot_input, c("check", "fold")))
    return(action_list)
  }

  # EXPLOITATIVE SIZING: Varies bet amounts to maximize value against different bot types.
  bet_mult <- function(strength = "medium") {
    if (strength == "strong") return(runif(1, 0.9, 1.3)) # Push max value with big hands
    return(runif(1, 0.4, 0.6)) # Standard sizing for mid-strength hands
  }

  ##### --- 4. CORE ENGINE & OPPONENT MODELING --- #####
  # Calculate Pot Odds: The cost to call relative to the total pot size.
  to_call <- current_bet - committed_this_round
  pot_odds <- ifelse(to_call > 0, to_call / (pot + to_call), 0)

  # Detect 'Maniac' behavior: High average commitment suggests an aggressive/bluffing table.
  if (is.list(public_players) && length(public_players) > 0) {
    avg_commit <- mean(vapply(public_players, function(p) {
      as.numeric(p$committed_this_hand %||% 0)
    }, numeric(1)), na.rm = TRUE)
  } else {
    avg_commit <- 0
  }
  is_maniac_table <- avg_commit > (pot * 0.7)

  equity <- calculate_dynamic_equity()

  # STRATEGY ADJUSTMENT: If the table is wild (maniacs), our 'relative' equity increases
  # because their betting range is much wider/weaker.
  if (is_maniac_table) equity <- equity + 0.12

  ##### --- 5. STREET LOGIC --- #####

  # --- PREFLOP ---
  if (identical(street, "preflop")) {
    vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
    # Define 'Strong' as High Pairs, AK/AQ, or Big Broadway cards.
    strong_hand <- (length(unique(vals)) == 1) || (min(vals) >= 12) || (vals[1] == 14 && vals[2] >= 10)

    if (strong_hand && "raise" %in% legal_types) {
      return(sanitize_action(list(type = "raise", amount = pot * 0.8)))
    }
    # Defense: Call small raises if we have high-card strength (Jacks or better)
    if (to_call > 0 && pot_odds < 0.3 && vals[1] >= 11) {
      return(list(type = "call"))
    }
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  # --- POSTFLOP ---
  category <- made_hand_category(hole_cards, board)
  strong_made <- category %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")

  # STRATEGY A: High Value (The Nuts)
  # When holding a strong made hand, we play aggressively to extract value from callers.
  if (strong_made) {
    size <- pot * bet_mult("strong")
    if ("raise" %in% legal_types) return(sanitize_action(list(type = "raise", amount = size)))
    if ("bet" %in% legal_types) return(sanitize_action(list(type = "bet", amount = size)))
    return(list(type = "call"))
  }

  # STRATEGY B: Facing a Bet (Defensive)
  if (to_call > 0) {
    # RISK MANAGEMENT: Tighten up against near all-in bets to preserve tournament life.
    if ((to_call >= stack * 0.8) && equity < 0.7) {
      return(list(type = "fold"))
    }

    # POKER MATH: Call if our estimated win probability (equity) exceeds the pot odds.
    if (pot_odds < equity) {
      return(list(type = "call"))
    }
    return(list(type = "fold"))
  }

  # STRATEGY C: Leading Out (Uncontested Pots)
  if (to_call == 0 && "bet" %in% legal_types) {
    bluff_roll <- runif(1)
    # Value bet with pairs 50% of the time to build the pot.
    if (category == "pair" && bluff_roll < 0.5) {
      return(sanitize_action(list(type = "bet", amount = pot * bet_mult("medium"))))
    }
    # RESTRAINED BLUFFING: Only 5% frequency to maintain a tight table image.
    # Never bluff into maniacs who are likely to re-raise.
    if (category == "high_card" && bluff_roll < 0.05 && !is_maniac_table) {
      return(sanitize_action(list(type = "bet", amount = pot * 0.4)))
    }
  }

  ##### --- 6. DEFAULT ACTION --- #####
  # Default to check if possible, otherwise fold.
  return(choose_preferred_action(bot_input, c("check", "fold")))
}

# mady_bot ----
# MADY BOT v3 â€” GTO-ADAPTIVE MTT BOT WITH OPPONENT MODELING
# Key improvements over v2:
#   1. Opponent profile system (aggression, fold freq, call freq)
#   2. MTT stack pressure: M-ratio, push/fold zones, ICM awareness
#   3. Exploitative sizing adjustments based on opponent tendencies
#   4. Street-aware continuation betting with opponent fold history
#   5. Dynamic bluff frequency gated by opponent calling tendencies
#   6. Position-weighted decision making throughout



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



clamp <- function(x, lo, hi) {
  lo <- as.numeric(lo %||% 0)
  hi <- as.numeric(hi %||% lo)
  x <- as.numeric(x %||% lo)
  max(lo, min(hi, x))
}

.rank_val <- function(cards) {
  ranks <- substring(cards, 1, nchar(cards) - 1)
  vals <- c("2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7,
            "8" = 8, "9" = 9, "T" = 10, "J" = 11, "Q" = 12, "K" = 13, "A" = 14)
  unname(vals[ranks])
}

.preflop_tier <- function(hole_cards) {
  vals <- sort(.rank_val(hole_cards), decreasing = TRUE)
  suited <- substring(hole_cards[1], nchar(hole_cards[1]), nchar(hole_cards[1])) ==
    substring(hole_cards[2], nchar(hole_cards[2]), nchar(hole_cards[2]))
  pair <- vals[1] == vals[2]
  gap <- abs(vals[1] - vals[2])
  if (pair && vals[1] >= 10) return(1)
  if (vals[1] == 14 && vals[2] >= 12) return(1)
  if (pair && vals[1] >= 7) return(2)
  if (vals[1] >= 13 && vals[2] >= 10) return(2)
  if (suited && vals[1] == 14 && vals[2] >= 9) return(2)
  if (pair || (suited && gap <= 2 && vals[1] >= 10) || vals[1] + vals[2] >= 22) return(3)
  if (suited && gap <= 3) return(4)
  if (gap <= 2 && vals[1] >= 10) return(4)
  5
}

.draw_outs <- function(hole_cards, board, hand_cat = "high_card") {
  cards <- c(hole_cards, board)
  suits <- substring(cards, nchar(cards), nchar(cards))
  flush_outs <- if (length(suits) > 0 && max(table(suits)) == 4) 9 else 0
  vals <- sort(unique(.rank_val(cards)))
  straight_outs <- 0
  for (start in 2:10) {
    missing <- setdiff(start:(start + 4), vals)
    if (length(missing) == 1) straight_outs <- max(straight_outs, 4)
  }
  max(flush_outs, straight_outs)
}

.pick_villain_range <- function(villain_stack, avg_stack) list()

.mc_equity <- function(hole_cards, board, villain_range, n_sims = 600) {
  category <- made_hand_category(hole_cards, board)
  switch(category,
         straight_flush = 0.98, quads = 0.96, full_house = 0.90, flush = 0.84,
         straight = 0.80, trips = 0.72, two_pair = 0.64, pair = 0.48,
         high_card = if (max(.rank_val(hole_cards), na.rm = TRUE) == 14) 0.34 else 0.24,
         0.25)
}

.get_villain_stack <- function(bot_input) {
  public_players <- bot_input$public_players %||% list()
  stacks <- vapply(public_players, function(p) as.numeric(p$stack %||% 0), numeric(1))
  villain_stacks <- stacks[stacks != as.numeric(bot_input$stack %||% NA_real_)]
  if (length(villain_stacks) == 0) villain_stacks <- stacks
  list(
    villain_stack = if (length(villain_stacks) > 0) max(villain_stacks, na.rm = TRUE) else as.numeric(bot_input$stack %||% 0),
    avg_stack = if (length(stacks) > 0) mean(stacks, na.rm = TRUE) else as.numeric(bot_input$stack %||% 0)
  )
}

.is_in_position <- function(bot_input) {
  players <- bot_input$players %||% bot_input$public_players %||% list()
  my_seat <- as.integer(bot_input$seat %||% NA_integer_)
  active <- Filter(function(p) {
    !isTRUE(p$folded) && !isTRUE(p$all_in) && !identical(as.character(p$status %||% "active"), "eliminated")
  }, players)
  if (length(active) == 0 || is.na(my_seat)) return(TRUE)
  seats <- vapply(active, function(p) as.integer(p$seat %||% NA_integer_), integer(1))
  my_seat == seats[length(seats)]
}

.streets_left <- function(street) {
  switch(street, flop = 2L, turn = 1L, river = 0L, preflop = 3L, 0L)
}

.geo_sizing <- function(pot, stack, streets_left) {
  pot <- max(1, as.numeric(pot %||% 1))
  stack <- max(1, as.numeric(stack %||% 1))
  if (streets_left <= 0) return(0.75)
  max(0.33, min(1.25, ((pot + stack) / pot)^(1 / streets_left) - 1))
}

.legal_bet_amount <- function(bot_input, amount) {
  if (!bot_has_action(bot_input, "bet")) return(NA_real_)
  as.integer(clamp(amount, bot_min_bet(bot_input), bot_max_bet(bot_input)))
}

.legal_raise_amount <- function(bot_input, amount) {
  if (!bot_has_action(bot_input, "raise")) return(NA_real_)
  as.integer(clamp(amount, bot_min_raise(bot_input), bot_max_raise(bot_input)))
}

.has_blocker <- function(hole_cards, board) {
  any(.rank_val(hole_cards) >= 13, na.rm = TRUE)
}

.preflop_decision <- function(bot_input, tier, in_pos, rng) {
  if (tier <= 2 && bot_has_action(bot_input, "raise")) return(list(type = "raise", amount = bot_min_raise(bot_input)))
  if (tier <= 4 && bot_has_action(bot_input, "call")) return(list(type = "call"))
  if (tier <= 3 && bot_has_action(bot_input, "bet")) return(list(type = "bet", amount = bot_min_bet(bot_input)))
  choose_preferred_action(bot_input, c("check", "fold"))
}

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
        if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
        return(list(type = "raise", amount = bot_max_raise(bot_input)))  # All-in-style shove
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
  # If SPR <= 3 we're pot-committed with any value hand â€” don't slowplay
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
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = .legal_raise_amount(bot_input, pot * size)))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = .legal_bet_amount(bot_input, pot * size)))
    }
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
        return(list(type = "raise", amount = .legal_raise_amount(bot_input, amt * 2.5)))
      return(list(type = "call"))
    }
    if (exploit_size > 0 && bot_has_action(bot_input, "bet"))
      return(list(type = "bet", amount = .legal_bet_amount(bot_input, amt)))
    return(list(type = "check"))
  }

  # --- SEMI BLUFF PREMIUM (Combo draw / OESD+FD) ---
  if (hand_type == "SEMI_BLUFF_PREMIUM") {
    # Always semi-bluff in position; bluff ~60% OOP
    bluff_freq <- if (.is_in_position(bot_input)) 0.70 else 0.55
    # Don't bluff stations
    if (archetype == "CALLING_STATION") bluff_freq <- bluff_freq * 0.3

    if (rng < bluff_freq && call_amt == 0 && bot_has_action(bot_input, "bet")) {
      # Size larger â€” we have equity even if called
      return(list(type = "bet", amount = .legal_bet_amount(bot_input, pot * 0.75)))
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
      return(list(type = "bet", amount = .legal_bet_amount(bot_input, pot * exploit_size)))
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
      # Blocker bluff on dry river â€” small sample gating via rng
      if (archetype != "CALLING_STATION" && bot_has_action(bot_input, "bet")) {
        return(list(type = "bet", amount = .legal_bet_amount(bot_input, pot * 0.70)))
      }
    }

    # Flop/Turn: probe bet vs. Weak Tight opponents if checked to us
    if (street %in% c("flop", "turn") && call_amt == 0 && archetype == "WEAK_TIGHT" && rng < 0.30) {
      if (bot_has_action(bot_input, "bet"))
        return(list(type = "bet", amount = .legal_bet_amount(bot_input, pot * 0.40)))
    }
  }

  # --- FALLBACK ---
  if (bot_has_action(bot_input, "check")) return(list(type = "check"))
  # Don't fold to micro-stabs (<2% of stack); too exploitable
  if (call_amt > 0 && call_amt <= (stack * 0.02)) return(list(type = "call"))
  return(list(type = "fold"))
}



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



# lucy_bot ----
# Mathematics of Poker â€” Student Bot Template
#
# INSTRUCTIONS
# 1. Rename the function below to your bot name.
# 2. Write your strategy inside that function.
# 3. Use the testing section at the bottom to inspect bot_input.
#
# Your function name IS your bot name.

source("core_internal/bot_api.R")
source("core_internal/cards_and_hands.R")
source("core_internal/game_engine.R")
source("core_internal/tournament_runner.R")
source("reference_bots/example_bots.R")

lucy_bot <- function(bot_input) {

  # INFORMATION AVAILABLE TO YOUR BOT

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

  # YOUR STRATEGY GOES BELOW


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



#Run tournament
if (FALSE) {
  results <- run_tournament(
    list(lucy_bot, random_bot, aggressive_bot, always_call_bot)
  )

  print(results)
}

# TESTING / DEBUGGING SECTION
#
# This section is for experimenting with the input your bot sees.
#
# To use it, you need a tournament state that is already at a point
# where a player is about to act.
#
# The current engine provides:
#   build_bot_input(tournament_state)
#   bot_input_to_dataframe(bot_input)
#   demo_show_bot_input(tournament_state, as_dataframe = TRUE)

# TEST 1: Build the exact bot_input from a live tournament state
#

# Example usage:
#
#bot_input_example <- build_bot_input(tourn)
#str(bot_input_example)
#print(bot_input_example)
#
# Then try:#
#lucy_bot(bot_input_example)
#


# TEST 2: View bot_input as a data frame
#
# Example usage:
#
#bot_input_example <- build_bot_input(tourn)
#bot_input_df <- bot_input_to_dataframe(bot_input_example)
#print(bot_input_df)
#



# TEST 3: Explore individual pieces of bot_input
#
# Example usage:
#
#bot_input_example <- build_bot_input(tourn)
#
#bot_input_example$hole_cards
#bot_input_example$board
#bot_input_example$street
#bot_input_example$pot
#bot_input_example$stack
#bot_input_example$legal_actions$legal_action_types
#bot_input_example$legal_actions$actions
#bot_input_example$public_players
#bot_input_example$action_history
#


# TEST 5: Run your bot on a real input
#
# Example usage:
#
# bot_input_example <- build_bot_input(tourn)
# action <- lucy_bot(bot_input_example)
# print(action)
#
# Expected formats include:
#   list(type = "fold")
#   list(type = "check")
#   list(type = "call")
#   list(type = "all_in")
#   list(type = "bet", amount = x)
#   list(type = "raise", amount = x)
#


# NOTES
#
# 1. Your bot only receives bot_input, not the full tournament state.
# 2. If you want to understand the input better, use:
#      str(build_bot_input(tourn))
# 3. For bet and raise actions, always make sure the amount is legal.
# 4. The safest helper for beginners is:
#      choose_preferred_action(bot_input, c("check", "call", "fold"))

# jaymon_bot ----
# Mathematics of Poker â€” Student Bot Template
#
# INSTRUCTIONS
# 1. Rename the function below to your bot name.
# 2. Write your strategy inside that function.
# 3. Use the testing section at the bottom to inspect bot_input.
#
# Your function name IS your bot name.


# OPTIONAL HELPERS

## Bot helpers are provided by `bot_api.R` (canonical location).
## Students and example bots can use `bot_has_action()`, `bot_min_bet()`,
## `bot_min_raise()`, and `choose_preferred_action()` from there.



jaymon_hand_strength <- function(hole_cards) {
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

jaymon_make_villain_range <- function(hole_cards, board) {
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

jaymon_equity_estimate <- function(hole_cards, board, n_opponents = 1, n_sims = 700) {
  tryCatch({
    our_df       <- data.frame(rank = extract_rank_from_label(hole_cards), suit = extract_suit_from_label(hole_cards), stringsAsFactors = FALSE)
    board_df     <- data.frame(rank = extract_rank_from_label(board),      suit = extract_suit_from_label(board),      stringsAsFactors = FALSE)
    villain_rng  <- jaymon_make_villain_range(hole_cards, board)
    if (is.null(villain_rng)) return(0.5)

    n_opp     <- min(n_opponents, 4)
    hole_list <- c(list(our_df), replicate(n_opp, villain_rng, simplify = FALSE))

    result <- holdem_equity_mc_fast(hole_list, board_df, n_sims = n_sims)
    result$equity[1]
  }, error = function(e) jaymon_equity(hole_cards, board))
}

jaymon_equity <- function(hole_cards, board) {
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

jaymon_opp_tendencies <- function(action_history, player_id) {
  if (length(action_history) == 0) return(0.4)
  opp <- Filter(function(a) !is.null(a$player_id) && a$player_id != player_id, action_history)
  if (length(opp) == 0) return(0.4)
  agg_types <- c("raise", "bet", "all_in")
  sum(sapply(opp, function(a) !is.null(a$type) && a$type %in% agg_types)) / length(opp)
}

jaymon_bet <- function(bot_input, pot_fraction) {
  if (!bot_has_action(bot_input, "bet")) return(NULL)
  mn <- bot_min_bet(bot_input); mx <- bot_max_bet(bot_input)
  if (is.null(mn) || is.null(mx)) return(NULL)
  max(mn, min(mx, round(bot_input$pot * pot_fraction)))
}

jaymon_raise <- function(bot_input, pot_fraction) {
  if (!bot_has_action(bot_input, "raise")) return(NULL)
  mn <- bot_min_raise(bot_input); mx <- bot_max_raise(bot_input)
  if (is.null(mn) || is.null(mx)) return(NULL)
  max(mn, min(mx, round(bot_input$pot * pot_fraction)))
}


# STUDENT BOT
#
# Rename this function to your bot name.
# Example:
#   joe_bot <- function(bot_input) { ... }

jaymon_bot <- function(bot_input) {

  # INFORMATION AVAILABLE TO YOUR BOT

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

  # YOUR STRATEGY GOES BELOW

  to_call <- max(0, current_bet - committed_this_round)


  n_active <- 1L
  if (!is.null(public_players) && length(public_players) > 0) {
    n_active <- max(1L, sum(sapply(public_players, function(p) {
      !is.null(p$player_id) && p$player_id != player_id && !isTRUE(p$folded)
    })))
  }

  opp_agg <- jaymon_opp_tendencies(action_history, player_id)
  is_short_stack <- (stack <= 8 * big_blind)

  if (street == "preflop") {

    strength <- jaymon_hand_strength(hole_cards)

    t1 <- if (opp_agg > 0.65) 11 else 9
    t2 <- if (opp_agg > 0.65) 8  else 7
    t3 <- if (opp_agg > 0.65) 6  else 5

    if (is_short_stack && strength >= t2) {
      if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      return(choose_preferred_action(bot_input, c("call", "raise", "check", "fold")))
    }

    if (strength >= t1) {
      target <- round(3 * big_blind + to_call)
      size <- jaymon_raise(bot_input, NA)
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

  quick_eq <- jaymon_equity(hole_cards, board)
  if (quick_eq > 0.70 || quick_eq <= 0.28) {
    equity <- quick_eq
  } else {
    equity <- jaymon_equity_estimate(hole_cards, board, n_active, n_sims = 150)
  }

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
        size <- jaymon_raise(bot_input, frac)
        if (!is.null(size)) return(list(type = "raise", amount = size))
      }
      if (should_call) return(choose_preferred_action(bot_input, c("call", "all_in")))
      return(choose_preferred_action(bot_input, c("call", "fold")))
    } else {
      frac <- if (!is.null(bf) && (isTRUE(bf$monotone) || isTRUE(bf$paired))) 0.75 else 0.65
      if (curr_spr < 2 && bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      size <- jaymon_bet(bot_input, frac)
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
        size <- jaymon_bet(bot_input, 0.50)
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
        size <- jaymon_bet(bot_input, 0.40)
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


# TESTING / DEBUGGING SECTION
#
# This section is for experimenting with the input your bot sees.
#
# To use it, you need a tournament state that is already at a point
# where a player is about to act.
#
# The current engine provides:
#   build_bot_input(tournament_state)
#   bot_input_to_dataframe(bot_input)
#   demo_show_bot_input(tournament_state, as_dataframe = TRUE)

if (FALSE) {

# TEST 1: Build the exact bot_input from a live tournament state
#
# Example usage:
#
bot_input_example <- build_bot_input(tourn)
str(bot_input_example)
print(bot_input_example)
#
# Then try:#
jaymon_bot(bot_input_example)
#


# TEST 2: View bot_input as a data frame
#
# Example usage:
#
bot_input_example <- build_bot_input(tourn)
bot_input_df <- bot_input_to_dataframe(bot_input_example)
print(bot_input_df)
#



# TEST 3: Explore individual pieces of bot_input
#
# Example usage:
#
bot_input_example <- build_bot_input(tourn)
#
bot_input_example$hole_cards
bot_input_example$board
bot_input_example$street
bot_input_example$pot
bot_input_example$stack
bot_input_example$legal_actions$legal_action_types
bot_input_example$legal_actions$actions
bot_input_example$public_players
bot_input_example$action_history
#


# TEST 5: Run your bot on a real input
#
# Example usage:
#
 bot_input_example <- build_bot_input(tourn)
 action <- jaymon_bot(bot_input_example)
 print(action)
#
# Expected formats include:
   list(type = "fold")
   list(type = "check")
   list(type = "call")
   list(type = "all_in")
   list(type = "bet", amount = x)
   list(type = "raise", amount = x)
#

   source("poker_load_all.R")
   poker_load_all(include_demos = TRUE, verbose = FALSE)
   demo_result <- run_tournament(
     bot_fns = list(
       jaymon_bot,
       random_bot,
       always_call_bot,
       passive_bot,
       aggressive_bot
     ),
     player_names = c(
       "My Bot",
       "Random Bot",
       "Caller Bot",
       "Passive Bot",
       "Aggro Bot"
     ),
     starting_stack = 5000,
     tournament_id = "LAB_BOT_DEMO",
     rng_seed = 39,
     max_hands = 200,
     verbose = TRUE
   )

   data.frame(
     player = vapply(demo_result$players, function(p) p$name, character(1)),
     chips = vapply(demo_result$players, function(p) p$stack, numeric(1)),
     place = vapply(demo_result$players, function(p) p$finishing_place, integer(1))
   )[order(
   vapply(demo_result$players, function(p) p$finishing_place, integer(1))
   ), ]
}
# NOTES
#
# 1. Your bot only receives bot_input, not the full tournament state.
# 2. If you want to understand the input better, use:
#      str(build_bot_input(tourn))
# 3. For bet and raise actions, always make sure the amount is legal.
# 4. The safest helper for beginners is:
#      choose_preferred_action(bot_input, c("check", "call", "fold"))

# tara_bot ----
# Mathematics of Poker â€” Student Bot Template
#
# INSTRUCTIONS
# 1. Rename the function below to your bot name.
# 2. Write your strategy inside that function.
# 3. Use the testing section at the bottom to inspect bot_input.
#
# Your function name IS your bot name.


# OPTIONAL HELPERS

## Bot helpers are provided by `bot_api.R` (canonical location).
## Students and example bots can use `bot_has_action()`, `bot_min_bet()`,
## `bot_min_raise()`, and `choose_preferred_action()` from there.


# STUDENT BOT
#
# Rename this function to your bot name.
# Example:
#   joe_bot <- function(bot_input) { ... }

tara_bot <- function(bot_input) {

  # INFORMATION AVAILABLE TO YOUR BOT

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


  # YOUR STRATEGY GOES BELOW


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



# TESTING / DEBUGGING SECTION
#
# This section is for experimenting with the input your bot sees.
#
# To use it, you need a tournament state that is already at a point
# where a player is about to act.
#
# The current engine provides:
#   build_bot_input(tournament_state)
#   bot_input_to_dataframe(bot_input)
#   demo_show_bot_input(tournament_state, as_dataframe = TRUE)

if (FALSE) {

# TEST 1: Build the exact bot_input from a live tournament state
#
# Example usage:
#
bot_fns <- list(
  "Random Bot" = random_bot,
  "Caller Bot" = always_call_bot,
  "Passive Bot" = passive_bot
)

tourn <- initialize_tournament(
  bot_fns = bot_fns,
  player_names = names(bot_fns),
  starting_stack = 1000
)

tourn <- initialize_hand(tourn)
tourn <- post_blinds_and_antes(tourn)
#
#
 bot_input_example <- build_bot_input(tourn)
 str(bot_input_example)
 print(bot_input_example)
#
# Then try:#
 tara_bot(bot_input_example)
#


# TEST 2: View bot_input as a data frame
#
# Example usage:
#
 bot_input_example <- build_bot_input(tourn)
 bot_input_df <- bot_input_to_dataframe(bot_input_example)
 print(bot_input_df)
#



# TEST 3: Explore individual pieces of bot_input
#
# Example usage:
#
 bot_input_example <- build_bot_input(tourn)
#
 bot_input_example$hole_cards
 bot_input_example$board
 bot_input_example$street
 bot_input_example$pot
 bot_input_example$stack
 bot_input_example$legal_actions$legal_action_types
bot_input_example$legal_actions$actions
 bot_input_example$public_players
 bot_input_example$action_history
#


# TEST 5: Run your bot on a real input
#
# Example usage:
#
 bot_input_example <- build_bot_input(tourn)
 action <- tara_bot(bot_input_example)
 print(action)
#
# Expected formats include:
#   list(type = "fold")
#   list(type = "check")
#   list(type = "call")
#   list(type = "all_in")
#   list(type = "bet", amount = x)
#   list(type = "raise", amount = x)
#

}

# NOTES
#
# 1. Your bot only receives bot_input, not the full tournament state.
# 2. If you want to understand the input better, use:
#      str(build_bot_input(tourn))
# 3. For bet and raise actions, always make sure the amount is legal.
# 4. The safest helper for beginners is:
#      choose_preferred_action(bot_input, c("check", "call", "fold"))

# joel_bot ----
for (helper_file in c("poker_math.R", "quant_tools.R", "equity_tools.R", "bot_api.R")) {
  if (file.exists(helper_file)) {
    source(helper_file)
  }
}

joel_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  pot <- bot_input$pot
  call_amount <- bot_call_amount(bot_input)



  # HAND STRENGTH
  vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

  paired <- length(unique(vals)) == 1
  high_card <- max(vals) >= 12   # A, K, Q
  ace_high <- max(vals) == 14
  king_high <- max(vals) == 13
  suited <- substr(hole_cards[1], 2, 2) == substr(hole_cards[2], 2, 2)
  connected <- abs(vals[1] - vals[2]) == 1

  # POT ODDS
  odds <- pot_odds(call_amount, pot)

  # DECISION LOGIC

  # STRONG â†’ aggressive
  if (paired || max(vals) >= 13 ) {
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    return(choose_preferred_action(bot_input, c("call", "check")))
  }

  # MEDIUM â†’ more pot odds and card dependent
  if (high_card) {
    # Free check
    if (call_amount == 0) {
      return(list(type = "check"))
    }

    # Stronger medium hands sometimes raise
    if (ace_high && (suited || connected) && odds < 0.2) {

      if (bot_has_action(bot_input, "raise")) {
        return(list(type = "raise",
                    amount = bot_min_raise(bot_input)))
      }
    }

    # Ace-high hands call more often
    if (ace_high && odds < 0.4) {
      return(list(type = "call"))
    }

    # King-high hands are tighter
    if (king_high && odds < 0.25) {
      return(list(type = "call"))
    }

    # Suited hands get extra value
    if (suited && odds < 0.3) {
      return(list(type = "call"))
    }

    # Connected cards can make straights
    if (connected && odds < 0.25) {
      return(list(type = "call"))
    }

    # Otherwise fold
    return(list(type = "fold"))
}

  # WEAK â†’ fold unless free
  if (call_amount == 0) {
    return(list(type = "check"))
  }

  return(list(type = "fold"))
}

# Nikola_bot ----
# Mathematics of Poker â€” Student Bot Template
#
# INSTRUCTIONS
# 1. Rename the function below to your bot name.
# 2. Write your strategy inside that function.
# 3. Use the testing section at the bottom to inspect bot_input.
#
# Your function name IS your bot name.


# OPTIONAL HELPERS

## Bot helpers are provided by `bot_api.R` (canonical location).
## Students and example bots can use `bot_has_action()`, `bot_min_bet()`,
## `bot_min_raise()`, and `choose_preferred_action()` from there.


# STUDENT BOT
#
# Rename this function to your bot name.
# Example:
#   joe_bot <- function(bot_input) { ... }

Nikola_bot <- function(bot_input) {


  # 1. READ INFORMATION FROM THE GAME


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



  # 2. SAFE ACTION HELPERS


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


  # 3. CARD HELPERS


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


  # 4. TABLE SITUATION


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


  # 5. BLUFF CONTROL

  #

  # 8% bluff rate. Small enough to avoid punting chips,

  # but enough so the bot is not predictable.


  bluff_now <- runif(1) < 0.08


  # 6. PREFLOP HAND SCORE

  #

  # Bigger score = stronger starting hand.


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


  # 7. PREFLOP STRATEGY


  if (street == "preflop") {

    very_strong <- preflop_score >= 85

    strong <- preflop_score >= 65

    playable <- preflop_score >= 48


    # VERY STRONG PREFLOP HANDS

    # Examples: TT+, AK, AQ, strong Broadway hands


    if (very_strong) {


      if ("raise" %in% legal_types) {

        return(list(type = "raise", amount = min_raise_safe()))

      }

      if ("bet" %in% legal_types) {

        return(list(type = "bet", amount = max_bet_safe()))

      }

      return(safe_action(c("call", "check", "fold")))

    }


    # HEADS-UP PREFLOP

    # Play wider because one opponent means weaker average hand.


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


    # MULTIWAY PREFLOP

    # Be tighter because many players can beat medium hands.


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


    # NORMAL 3-PLAYER PREFLOP


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


  # 8. POSTFLOP HAND CATEGORY


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


  # 9. EQUITY ESTIMATE

  #

  # Equity = estimated chance of winning the hand.


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


  # 10. FALLBACK EQUITY

  #

  # If Monte Carlo fails, estimate equity from hand category.


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

  # 11. MONSTER HANDS

  #

  # Straight, flush, full house, quads, etc.

  # Goal: build the pot.


  if (monster || equity >= 0.75) {


    if ("raise" %in% legal_types) {

      return(list(type = "raise", amount = max_raise_safe()))

    }

    if ("bet" %in% legal_types) {

      return(list(type = "bet", amount = max_bet_safe()))

    }

    return(safe_action(c("call", "check", "fold")))

  }


  # 12. STRONG HANDS

  #

  # Trips, two pair, or strong equity.

  # Goal: value bet, but avoid donating on river.


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


  # 13. MEDIUM HANDS

  #

  # Usually one pair or okay equity.

  # Goal: call only if the price is good.


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


  # 14. SMART BLUFFS

  #

  # Only bluff when nobody has bet into us.


  if (bluff_now && amount_to_call == 0 && equity >= 0.28) {


    if ("bet" %in% legal_types) {

      return(list(type = "bet", amount = min_bet_safe()))

    }

    if ("raise" %in% legal_types) {

      return(list(type = "raise", amount = min_raise_safe()))

    }

  }


  # 15. WEAK HANDS

  #

  # Mostly check/fold. Only call if pot odds are clearly good.


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

# TESTING / DEBUGGING SECTION


# Use this only after you have created a tournament called `tourn`.

# Keep these lines commented unless you are actively testing.

# bot_input_example <- build_bot_input(tourn)

# str(bot_input_example)

# print(bot_input_example)

# action <- Nikola_bot(bot_input_example)

# print(action)

# bot_input_df <- bot_input_to_dataframe(bot_input_example)

# print(bot_input_df)

# bot_input_example$hole_cards

# bot_input_example$board

# bot_input_example$street

# bot_input_example$pot

# bot_input_example$stack

# bot_input_example$legal_actions$legal_action_types

# bot_input_example$legal_actions$actions

# bot_input_example$public_players

# bot_input_example$action_history

# king_bot ----
# Guest Bots
# These guest bots intentionally use the same behavior as random_bot.





king_bot <- function(bot_input) {

  legal_types <- bot_input$legal_actions$legal_action_types
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(vals) == 2 && vals[1] == vals[2]
  high_card <- length(vals) == 2 && vals[1] >= 12
  ace_anything <- length(vals) == 2 && vals[1] == 14
  premium <- paired || high_card || ace_anything

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  big_pressure <- is.finite(stack) && stack > 0 && to_call > 0.35 * stack

  if ((premium || strong_made || runif(1) < 0.25) && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- if (runif(1) < 0.35) max_raise else min(max_raise, min_raise * sample(2:4, size = 1))
    return(list(type = "raise", amount = amount))
  }

  if ((strong_made || medium_made || runif(1) < 0.35) && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- if (runif(1) < 0.30) max_bet else min(max_bet, min_bet * sample(2:4, size = 1))
    return(list(type = "bet", amount = amount))
  }

  if (!big_pressure && runif(1) < 0.12 && bot_has_action(bot_input, "all_in")) {
    return(list(type = "all_in"))
  }

  if (big_pressure && !premium && !strong_made) {
    return(choose_preferred_action(bot_input, c("fold", "call", "check")))
  }

  if ("call" %in% legal_types && runif(1) < 0.70) {
    return(list(type = "call"))
  }

  choose_preferred_action(bot_input, c("check", "call", "fold"))
}

# hatch_bot ----
hatch_bot <- function(bot_input) {

  legal_types <- bot_input$legal_actions$legal_action_types
  hole_cards <- bot_input$hole_cards
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  public_players <- bot_input$public_players %||% list()
  total_players <- length(public_players)
  active_players <- sum(vapply(
    public_players,
    function(p) identical(p$status, "active") && isTRUE(as.numeric(p$stack %||% 0) > 0),
    logical(1)
  ))
  eliminated_players <- max(0, total_players - active_players)
  field_halved <- total_players > 0 && eliminated_players >= total_players / 2
  aggression <- if (total_players > 0) {
    max(0, min(1, eliminated_players / max(1, total_players - 1)))
  } else {
    0
  }

  ranks <- hole_rank_values(hole_cards)
  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]
  connector <- length(ranks) == 2 && abs(diff(sort(ranks))) == 1
  suited_connector <- suited && connector
  has_ace <- length(ranks) == 2 && any(ranks == 14)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  broadway <- length(ranks) == 2 && min(ranks) >= 10
  playable <- suited_connector || paired || broadway || has_ace

  posted_big_blind <- any(vapply(
    bot_input$action_history %||% list(),
    function(a) {
      identical(a$type, "post_bb") &&
        (identical(a$player_id, bot_input$player_id) || identical(as.integer(a$seat), as.integer(bot_input$seat)))
    },
    logical(1)
  ))

  sized_action <- function(type, target_amount) {
    if (type == "bet") {
      amount <- max(bot_min_bet(bot_input), min(bot_max_bet(bot_input), target_amount))
      return(list(type = "bet", amount = amount))
    }

    amount <- max(bot_min_raise(bot_input), min(bot_max_raise(bot_input), target_amount))
    list(type = "raise", amount = amount)
  }

  if (active_players <= 2 && identical(street, "preflop") && has_ace && bot_has_action(bot_input, "all_in")) {
    return(list(type = "all_in"))
  }

  if (identical(street, "river")) {
    target <- max(1, floor(stack / 2))
    if (bot_has_action(bot_input, "bet")) {
      return(sized_action("bet", target))
    }
    if (bot_has_action(bot_input, "raise")) {
      return(sized_action("raise", target))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (posted_big_blind) {
    target <- max(big_blind * 3, current_bet)
    if (bot_has_action(bot_input, "raise")) {
      return(sized_action("raise", target))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(sized_action("bet", target))
    }
  }

  if (suited_connector && to_call <= max(big_blind * 4, stack * 0.18)) {
    if (field_halved && bot_has_action(bot_input, "raise") && runif(1) < 0.35 + aggression * 0.35) {
      return(sized_action("raise", max(bot_min_raise(bot_input), big_blind * 3)))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (!field_halved) {
    if (playable && to_call <= max(big_blind * 2, stack * 0.10)) {
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }
    return(choose_preferred_action(bot_input, c("check", "fold", "call")))
  }

  if (playable || runif(1) < 0.15 + aggression * 0.35) {
    if (bot_has_action(bot_input, "raise")) {
      return(sized_action("raise", max(bot_min_raise(bot_input), big_blind * (2 + ceiling(aggression * 3)))))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(sized_action("bet", max(bot_min_bet(bot_input), big_blind * (2 + ceiling(aggression * 3)))))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}

# gearan_bot ----
gearan_bot <- function(bot_input) {

  legal_types <- bot_input$legal_actions$legal_action_types
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  broadway <- length(ranks) == 2 && min(ranks) >= 10
  ace_good <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 9
  strong_preflop <- paired || broadway || ace_good

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")
  affordable_call <- to_call <= max(big_blind * 2, stack * 0.12)

  # Executive discretion: a modest random element, but not chaos.
  if (runif(1) < 0.18) {
    return(random_bot(bot_input))
  }

  # Build consensus cheaply; avoid needless escalation with marginal hands.
  if (to_call > 0 && affordable_call && (medium_made || strong_preflop || runif(1) < 0.45)) {
    return(choose_preferred_action(bot_input, c("call", "fold", "check")))
  }

  # Lead when the mandate is clear.
  if (strong_made || strong_preflop) {
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  # Collaborative default: check when possible, call small, fold expensive.
  if ("check" %in% legal_types) {
    return(list(type = "check"))
  }

  if (affordable_call && "call" %in% legal_types) {
    return(list(type = "call"))
  }

  choose_preferred_action(bot_input, c("fold", "call", "check"))
}

# hu_bot ----
hu_bot <- function(bot_input) {

  if (runif(1) < 0.50 && bot_has_action(bot_input, "all_in")) {
    return(list(type = "all_in"))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}

# talmage_bot ----
talmage_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  high_cards <- length(ranks) == 2 && min(ranks) >= 10
  ace <- length(ranks) == 2 && ranks[1] == 14
  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  decent_hand <- strong_made || made == "pair" || paired || high_cards || ace || suited
  cheap_enough <- to_call <= max(big_blind * 5, stack * 0.25)

  if (decent_hand && bot_has_action(bot_input, "raise") && runif(1) < 0.45) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- min(max_raise, max(min_raise, big_blind * sample(3:5, size = 1)))
    return(list(type = "raise", amount = amount))
  }

  if (decent_hand && bot_has_action(bot_input, "bet") && runif(1) < 0.55) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- min(max_bet, max(min_bet, big_blind * sample(2:4, size = 1)))
    return(list(type = "bet", amount = amount))
  }

  if (cheap_enough) {
    return(choose_preferred_action(bot_input, c("call", "check", "raise", "bet", "fold")))
  }

  if (runif(1) < 0.35) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "call", "fold"))
}

# spector_bot ----
spector_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  ace_or_king <- length(ranks) == 2 && ranks[1] >= 13
  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  # Decoherence phase: sometimes sit out to look cautious.
  if (runif(1) < 0.22 && !strong_made && !paired) {
    return(choose_preferred_action(bot_input, c("check", "fold", "call")))
  }

  bluff_signal <- runif(1) < 0.38
  high_risk_signal <- strong_made || paired || ace_or_king || suited || bluff_signal

  if (high_risk_signal && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- if (runif(1) < 0.45) {
      max_raise
    } else {
      min(max_raise, max(min_raise, big_blind * sample(4:8, size = 1)))
    }
    return(list(type = "raise", amount = amount))
  }

  if (high_risk_signal && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- if (runif(1) < 0.40) {
      max_bet
    } else {
      min(max_bet, max(min_bet, big_blind * sample(3:7, size = 1)))
    }
    return(list(type = "bet", amount = amount))
  }

  if ((strong_made || bluff_signal) && to_call < stack * 0.50 && bot_has_action(bot_input, "all_in") && runif(1) < 0.15) {
    return(list(type = "all_in"))
  }

  if ((medium_made || suited || ace_or_king || bluff_signal) && to_call <= max(big_blind * 6, stack * 0.25)) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}

# khan_bot ----
khan_bot <- function(bot_input) {
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  premium_pair <- paired && ranks[1] >= 10
  strong_ace <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 10
  broadway <- length(ranks) == 2 && min(ranks) >= 11

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  strong_hand <- premium_pair || strong_ace || broadway || strong_made
  playable <- strong_hand || medium_made || paired
  cheap_call <- to_call <= max(big_blind * 2, stack * 0.10)
  massive_bluff <- runif(1) < 0.08

  if ((strong_hand || massive_bluff) && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- if (massive_bluff) {
      max_raise
    } else {
      min(max_raise, max(min_raise, big_blind * sample(3:5, size = 1)))
    }
    return(list(type = "raise", amount = amount))
  }

  if ((strong_hand || massive_bluff) && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- if (massive_bluff) {
      max_bet
    } else {
      min(max_bet, max(min_bet, big_blind * sample(2:4, size = 1)))
    }
    return(list(type = "bet", amount = amount))
  }

  if (playable && cheap_call) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (medium_made && to_call <= max(big_blind * 4, stack * 0.18)) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}
kahn_bot <- khan_bot

# forde_bot ----
forde_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  premium_pair <- paired && ranks[1] >= 10
  strong_pair <- paired && ranks[1] >= 8
  strong_ace <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 11
  suited_broadway <- FALSE
  if (length(hole_cards) == 2 && length(ranks) == 2) {
    suits <- substring(hole_cards, nchar(hole_cards), nchar(hole_cards))
    suited_broadway <- suits[1] == suits[2] && min(ranks) >= 10
  }

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  model_approved <- strong_made || premium_pair || strong_ace || suited_broadway
  plausible <- model_approved || strong_pair || medium_made
  cheap_call <- to_call <= max(big_blind * 2, stack * 0.08)
  fair_call <- to_call <= max(big_blind * 4, stack * 0.16)

  if (model_approved && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- min(max_raise, max(min_raise, big_blind * 3))
    return(list(type = "raise", amount = amount))
  }

  if (model_approved && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- min(max_bet, max(min_bet, big_blind * 2))
    return(list(type = "bet", amount = amount))
  }

  if (plausible && (cheap_call || (medium_made && fair_call))) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (identical(street, "preflop") && cheap_call && runif(1) < 0.25) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}

# hawkins_bot ----
hawkins_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  pot <- as.numeric(bot_input$pot %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  premium_pair <- paired && ranks[1] >= 10
  any_pair <- paired
  strong_ace <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 9
  broadway <- length(ranks) == 2 && min(ranks) >= 10
  high_card_pressure <- length(ranks) == 2 && ranks[1] >= 13

  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  monster <- made %in% c("straight", "flush", "full_house", "quads", "straight_flush")
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  strong_hand <- premium_pair || strong_ace || broadway || strong_made
  playable <- strong_hand || any_pair || suited || high_card_pressure || medium_made
  cheap_call <- to_call <= max(big_blind * 3, stack * 0.14)
  pressure_spot <- to_call == 0 || to_call <= max(big_blind * 4, stack * 0.18)
  bluff <- runif(1) < if (identical(street, "preflop")) 0.18 else 0.28

  size_raise <- function(mult) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    min(max_raise, max(min_raise, big_blind * mult, floor(pot * 0.75)))
  }

  size_bet <- function(frac) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    min(max_bet, max(min_bet, floor(max(pot, big_blind) * frac)))
  }

  if (monster && bot_has_action(bot_input, "raise")) {
    return(list(type = "raise", amount = bot_max_raise(bot_input)))
  }

  if (monster && bot_has_action(bot_input, "bet")) {
    return(list(type = "bet", amount = bot_max_bet(bot_input)))
  }

  if ((strong_hand || bluff) && pressure_spot && bot_has_action(bot_input, "raise")) {
    amount <- if (bluff && runif(1) < 0.35) bot_max_raise(bot_input) else size_raise(sample(3:6, size = 1))
    return(list(type = "raise", amount = amount))
  }

  if ((strong_hand || bluff) && bot_has_action(bot_input, "bet")) {
    amount <- if (bluff && runif(1) < 0.30) bot_max_bet(bot_input) else size_bet(if (strong_hand) 0.85 else 0.65)
    return(list(type = "bet", amount = amount))
  }

  if (playable && cheap_call) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (bot_has_action(bot_input, "all_in") && strong_hand && stack <= max(big_blind * 8, pot * 0.75)) {
    return(list(type = "all_in"))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}

# biermann_bot ----
biermann_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  prime_ranks <- length(ranks) == 2 && all(ranks %in% c(2, 3, 5, 7, 11, 13))
  symmetry <- paired || prime_ranks
  ace_high <- length(ranks) == 2 && ranks[1] == 14

  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  theoretical_green_light <- strong_made || symmetry || (suited && ace_high)
  practical_doubt <- runif(1) < 0.30
  cheap_call <- to_call <= max(big_blind * 2, stack * 0.10)
  derby_burst <- runif(1) < 0.12

  if ((theoretical_green_light || derby_burst) && !practical_doubt && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- min(max_raise, max(min_raise, big_blind * sample(2:5, size = 1)))
    return(list(type = "raise", amount = amount))
  }

  if ((theoretical_green_light || derby_burst) && !practical_doubt && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- min(max_bet, max(min_bet, big_blind * sample(2:4, size = 1)))
    return(list(type = "bet", amount = amount))
  }

  if ((medium_made || suited || symmetry || ace_high) && cheap_call) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (identical(street, "river") && strong_made && bot_has_action(bot_input, "all_in") && runif(1) < 0.20) {
    return(list(type = "all_in"))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}

guestBots <- list(
  king_bot,
  hatch_bot,
  gearan_bot,
  hu_bot,
  talmage_bot,
  spector_bot,
  kahn_bot,
  forde_bot,
  hawkins_bot,
  biermann_bot
)




# random_bot ----
#   Starter bots for testing and for student examples.
#   Each bot is called as bot_fn(bot_input), where bot_input is a list
#   created by safe_get_bot_action() inside game_engine.R.

# (moved to `bot_api.R` to avoid duplication; `bot_api.R` provides the
# canonical implementations `bot_has_action`, `bot_min_bet`, etc.)

hole_rank_values <- function(hole_cards) {
  if (is.null(hole_cards) || length(hole_cards) == 0) return(numeric(0))

  ranks <- substring(hole_cards, 1, nchar(hole_cards) - 1)

  vals <- c(
    "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,
    "7" = 7, "8" = 8, "9" = 9, "T" = 10, "J" = 11,
    "Q" = 12, "K" = 13, "A" = 14
  )

  unname(vals[ranks])
}


random_bot <- function(bot_input) {
  legal_types <- bot_input$legal_actions$legal_action_types
  choice <- sample(legal_types, size = 1)

  if (choice == "bet") {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)

    if (is.null(min_bet) || is.null(max_bet)) {
      return(list(type = "check"))
    }

    amount <- sample(seq.int(as.integer(min_bet), as.integer(max_bet)), size = 1)
    return(list(type = "bet", amount = amount))
  }

  if (choice == "raise") {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)

    if (is.null(min_raise) || is.null(max_raise)) {
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }

    amount <- sample(seq.int(as.integer(min_raise), as.integer(max_raise)), size = 1)
    return(list(type = "raise", amount = amount))
  }

  if (choice == "all_in") {
    return(list(type = "all_in"))
  }

  list(type = choice)
}



# always_call_bot ----

always_call_bot <- function(bot_input) {
  choose_preferred_action(bot_input, c("check", "call", "fold"))
}



# simple_preflop_strength_bot ----
simple_preflop_strength_bot <- function(bot_input) {
  hole_cards <- bot_input$hole_cards
  street <- bot_input$street

  if (identical(street, "preflop") && length(hole_cards) == 2) {
    vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

    paired <- length(unique(vals)) == 1
    premium_broadway <- min(vals) >= 12      # QQ+/AK/AQ/KQ by rank threshold heuristic
    strong_ace <- max(vals) == 14 && min(vals) >= 10

    premium <- paired || premium_broadway || strong_ace

    if (premium) {
      if (bot_has_action(bot_input, "raise")) {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
      if (bot_has_action(bot_input, "bet")) {
        return(list(type = "bet", amount = bot_min_bet(bot_input)))
      }
      if (bot_has_action(bot_input, "all_in")) {
        return(list(type = "all_in"))
      }
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    } else {
      return(choose_preferred_action(bot_input, c("check", "call", "fold")))
    }
  }

  choose_preferred_action(bot_input, c("check", "call", "fold"))
}



# aggressive_bot ----

aggressive_bot <- function(bot_input) {
  if (bot_has_action(bot_input, "raise")) {
    return(list(type = "raise", amount = bot_min_raise(bot_input)))
  }

  if (bot_has_action(bot_input, "bet")) {
    return(list(type = "bet", amount = bot_min_bet(bot_input)))
  }

  if (bot_has_action(bot_input, "all_in")) {
    return(list(type = "all_in"))
  }

  choose_preferred_action(bot_input, c("call", "check", "fold"))
}


# strength_by_street_bot ----

extract_rank_from_label <- function(card_label) {
  substring(card_label, 1, nchar(card_label) - 1)
}

extract_suit_from_label <- function(card_label) {
  substring(card_label, nchar(card_label), nchar(card_label))
}

made_hand_category <- function(hole_cards, board) {
  # Returns one of:
  # "high_card", "pair", "two_pair", "trips", "straight",
  # "flush", "full_house", "quads", "straight_flush"
  # This is intentionally simple and student-readable.
  # It does not try to compare kicker strength inside a category.

  cards <- c(hole_cards, board)

  if (length(cards) < 5) {
    return("high_card")
  }

  ranks <- extract_rank_from_label(cards)
  suits <- extract_suit_from_label(cards)

  rank_map <- c(
    "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6,
    "7" = 7, "8" = 8, "9" = 9, "T" = 10, "J" = 11,
    "Q" = 12, "K" = 13, "A" = 14
  )
  vals <- unname(rank_map[ranks])

  rank_counts <- sort(table(ranks), decreasing = TRUE)
  suit_counts <- sort(table(suits), decreasing = TRUE)

  has_flush <- length(suit_counts) > 0 && max(suit_counts) >= 5

  # Straight helper
  unique_vals <- sort(unique(vals))
  if (14 %in% unique_vals) {
    unique_vals <- sort(unique(c(1, unique_vals)))  # wheel support A-2-3-4-5
  }

  has_straight <- FALSE
  if (length(unique_vals) >= 5) {
    for (i in seq_len(length(unique_vals) - 4)) {
      window <- unique_vals[i:(i + 4)]
      if (all(diff(window) == 1)) {
        has_straight <- TRUE
        break
      }
    }
  }

  # Straight flush check
  has_straight_flush <- FALSE
  flush_suits <- names(suit_counts)[suit_counts >= 5]
  if (length(flush_suits) > 0) {
    for (s in flush_suits) {
      suited_cards <- cards[suits == s]
      suited_ranks <- extract_rank_from_label(suited_cards)
      suited_vals <- sort(unique(unname(rank_map[suited_ranks])))
      if (14 %in% suited_vals) {
        suited_vals <- sort(unique(c(1, suited_vals)))
      }
      if (length(suited_vals) >= 5) {
        for (i in seq_len(length(suited_vals) - 4)) {
          window <- suited_vals[i:(i + 4)]
          if (all(diff(window) == 1)) {
            has_straight_flush <- TRUE
            break
          }
        }
      }
      if (has_straight_flush) break
    }
  }

  if (has_straight_flush) return("straight_flush")
  if (max(rank_counts) == 4) return("quads")
  if (max(rank_counts) == 3 && length(rank_counts) >= 2 && rank_counts[2] >= 2) return("full_house")
  if (has_flush) return("flush")
  if (has_straight) return("straight")
  if (max(rank_counts) == 3) return("trips")
  if (sum(rank_counts >= 2) >= 2) return("two_pair")
  if (max(rank_counts) == 2) return("pair")
  "high_card"
}


strength_by_street_bot <- function(bot_input) {
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street

  if (identical(street, "preflop")) {
    if (length(hole_cards) == 2) {
      vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

      paired <- length(unique(vals)) == 1
      premium_broadway <- min(vals) >= 12
      strong_ace <- max(vals) == 14 && min(vals) >= 10

      premium <- paired || premium_broadway || strong_ace

      if (premium) {
        if (bot_has_action(bot_input, "raise")) {
          return(list(type = "raise", amount = bot_min_raise(bot_input)))
        }
        if (bot_has_action(bot_input, "bet")) {
          return(list(type = "bet", amount = bot_min_bet(bot_input)))
        }
        if (bot_has_action(bot_input, "all_in")) {
          return(list(type = "all_in"))
        }
      }
    }

    return(choose_preferred_action(bot_input, c("check", "call", "fold")))
  }

  category <- made_hand_category(hole_cards, board)

  strong_made_hands <- c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made_hands <- c("pair")

  if (category %in% strong_made_hands) {
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    if (bot_has_action(bot_input, "all_in")) {
      return(list(type = "all_in"))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (category %in% medium_made_hands) {
    return(choose_preferred_action(bot_input, c("check", "call", "fold")))
  }

  # Weak/no made hand
  return(choose_preferred_action(bot_input, c("check", "fold")))
}


# passive_bot ----

passive_bot <- function(bot_input) {
  choose_preferred_action(bot_input, c("check", "fold"))
}

# Mixed bot factory and generated mixed bots
#   passive      = 0.1
#   always_call  = 0.3
#   random       = 0.4
#   aggressive   = 0.2
# Usage:
#   mixed_bot <- make_mixed_bot()
# Or with custom probabilities:
#   mixed_bot <- make_mixed_bot(
#     passive_prob = 0.2,
#     always_call_prob = 0.2,
#     random_prob = 0.2,
#     aggressive_prob = 0.4
#   )
# The probabilities must be nonnegative and sum to 1
# (up to a small numerical tolerance).

make_mixed_bot <- function(
    passive_prob = 0.1,
    always_call_prob = 0.3,
    random_prob = 0.4,
    aggressive_prob = 0.2
) {
  probs <- c(
    passive = passive_prob,
    always_call = always_call_prob,
    random = random_prob,
    aggressive = aggressive_prob
  )

  if (any(!is.finite(probs))) {
    stop("All probabilities must be finite numbers.")
  }

  if (any(probs < 0)) {
    stop("All probabilities must be nonnegative.")
  }

  if (sum(probs) <= 0) {
    stop("At least one probability must be positive.")
  }

  if (abs(sum(probs) - 1) > 1e-8) {
    stop("Probabilities must sum to 1.")
  }

  bot_fn <- function(bot_input) {
    bot_type <- sample(names(probs), size = 1, prob = probs)

    if (bot_type == "passive") {
      return(passive_bot(bot_input))
    }

    if (bot_type == "always_call") {
      return(always_call_bot(bot_input))
    }

    if (bot_type == "random") {
      return(random_bot(bot_input))
    }

    if (bot_type == "aggressive") {
      return(aggressive_bot(bot_input))
    }

    # Defensive fallback
    choose_preferred_action(bot_input, c("check", "call", "fold"))
  }

  attr(bot_fn, "mixture_probs") <- probs
  bot_fn
}

# mixed_bot ----
mixed_bot<-make_mixed_bot()

# mixed_bot2 ----
mixed_bot2<-make_mixed_bot(.2,.3,.1,.4)

# student_bot_template ----

student_bot_template <- function(bot_input) {
  # Students should edit only the body of this function.
  # The engine passes in a single list called bot_input. Useful fields:
  #   bot_input$hole_cards
  #   bot_input$board
  #   bot_input$street
  #   bot_input$pot
  #   bot_input$stack
  #   bot_input$legal_actions$legal_action_types
  #   bot_input$legal_actions$actions
  # Valid return values include:
  #   list(type = "fold")
  #   list(type = "check")
  #   list(type = "call")
  #   list(type = "all_in")
  #   list(type = "bet", amount = x)
  #   list(type = "raise", amount = x)
  # Important:
  #   For "bet" and "raise", the amount must be legal.
  #   Use bot_min_bet(bot_input), bot_max_bet(bot_input),
  #   bot_min_raise(bot_input), and bot_max_raise(bot_input).

  choose_preferred_action(bot_input, c("check", "call", "fold"))
}


# lab_bot ----
lab_bot <- function(bot_input) {
  legal_types <- bot_input$legal_actions$legal_action_types
  hole_cards <- bot_input$hole_cards
  street <- bot_input$street
  big_blind <- bot_input$big_blind
  pot <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed <- bot_input$committed_this_round

  call_amount <- max(0, current_bet - committed)
  vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

  if (street == "preflop" && length(vals) == 2) {
    paired <- vals[1] == vals[2]
    ak <- identical(vals, c(14, 13))
    aq <- identical(vals, c(14, 12))

    if (paired || ak || aq) {
      if (bot_has_action(bot_input, "raise")) {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
      if (bot_has_action(bot_input, "bet")) {
        return(list(type = "bet", amount = bot_min_bet(bot_input)))
      }
    }

    if ("check" %in% legal_types) {
      return(list(type = "check"))
    }

    if ("call" %in% legal_types && call_amount <= big_blind) {
      return(list(type = "call"))
    }

    return(list(type = "fold"))
  }

  if ("check" %in% legal_types) {
    return(list(type = "check"))
  }

  if ("call" %in% legal_types) {
    threshold <- pot_odds(call_amount, pot)
    if (threshold <= 0.25) {
      return(list(type = "call"))
    }
  }

  list(type = "fold")
}


# lab_bot_v2 ----
lab_bot_v2 <- function(bot_input) {
  legal_types <- bot_input$legal_actions$legal_action_types
  board <- bot_input$board
  street <- bot_input$street
  pot <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed <- bot_input$committed_this_round
  call_amount <- max(0, current_bet - committed)

  if (street == "flop" && length(board) == 3) {
    board_df <- parse_cards(board)
    feats <- board_features(board_df)

    if ("bet" %in% legal_types && !isTRUE(feats$two_tone) && feats$connectivity <= 1) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
  }

  if ("check" %in% legal_types) {
    return(list(type = "check"))
  }

  if ("call" %in% legal_types) {
    threshold <- pot_odds(call_amount, pot)
    if (threshold <= 0.20) {
      return(list(type = "call"))
    }
  }

  list(type = "fold")
}



