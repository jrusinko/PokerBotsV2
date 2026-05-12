############################################################
# TOURNAMENT BOT — v6  "Survive and Strike"
#
# Core insight from results:
#   - Random Bot won by luck/variance
#   - Passive Bot outlasted everyone by folding and bleeding slowly
#   - Our bot was busting BEFORE the always_call_bot — meaning
#     we were entering pots and losing them to callers
#
# New philosophy:
#   1. FOLD most hands preflop — tighter than passive bot
#   2. ONLY continue postflop with two pair or better (score >= 0.67)
#      One pair is NOT good enough to bet or call against callers
#   3. When we DO have two pair+, bet it clearly for value
#   4. No c-bets, no bluffs, no probe bets — callers punish all of these
#   5. Check/fold everything else and let the other bots fight
#   6. All-in only on river or turn with near-nut hand (score >= 0.88)
#   7. Never call more than 8% of stack without two pair or better
#
# HOW TO USE:
#   source("poker_load_all.R")
#   poker_load_all(include_demos = TRUE, verbose = FALSE)
#   source("my_bot.R")
#   Rename "my_bot" to your tournament registration name.
############################################################


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


# ══════════════════════════════════════════════════════════════════════
# MAIN BOT — rename to your tournament registration name
# ══════════════════════════════════════════════════════════════════════

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

  siena_says <- function(lines, chance = 0.17) {
    bot_maybe_say(lines, bot_input, chance)
  }

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
      siena_says(c(
        "Siena: Bushytop is staying disciplined. You are welcome.",
        "Siena: I teach patience first. Today, you are the lesson.",
        "Siena: Not my horse, not my course.",
        "Siena: I will stay kind and let that hand embarrass someone else.",
        "Siena: Maurice-bot is absolutely not involved. Please move along.",
        "Siena: Bushytop sees the trap and declines the invitation.",
        "Siena: Kindness includes folding bad hands loudly.",
        "Siena: Lisbon taught perspective; this hand has none."
      ), chance = 0.12)
      if (can_check) return(list(type="check"))
      return(list(type="fold"))
    }

    # ── SPECULATIVE: small pairs, suited connectors, suited aces ──────
    # Only see the flop for FREE. If someone has raised even 1 BB extra,
    # fold. These hands need to make two pair or better on the flop to
    # continue — and even then, we just check/call small or bet ourselves.
    if (tier == "speculative") {
      siena_says(c(
        "Siena: Speculative hand. I will approach the jump carefully.",
        "Siena: Lisbon taught me patience. This table could learn some.",
        "Siena: Future teacher note: small risks need clear instructions.",
        "Siena: I am nearby, not committed. There is a difference."
      ), chance = 0.14)
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

      siena_says(c(
        "Siena: Premium hand. Please try to keep up.",
        "Siena: Regionals energy. I know how to take the reins.",
        "Siena: Bushytop has entered the arena.",
        "Siena: I am kind, confident, and absolutely not folding this.",
        "Siena: Global ambassador update: this pot is going abroad with me.",
        "Siena: If Maurice-bot is watching, no he is not.",
        "Siena: Future teacher voice says pay attention.",
        "Siena: This hand has regionals posture and table presence.",
        "Siena: Maurice-bot would call this destiny. I call it position."
      ))

      if (current_bet > bb) {
        # Facing a raise — call if cheap, fold if expensive
        stack_cost <- call_amount / max(1, stack)
        po         <- pot_odds(call_amount, pot)

        # Too expensive: fold and preserve chips
        if (stack_cost > 0.20) {
          siena_says(c(
          "Siena: Too expensive. Even confidence has a budget.",
          "Siena: I will not jump that fence today.",
          "Siena: Smart fold. Please write that down.",
            "Siena: I am saving chips and dignity.",
            "Siena: Maurice-bot said chaos. I said absolutely not in public."
          ))
          return(list(type="fold"))
        }

        # Reasonable price: call (don't re-raise — keep pot manageable)
        if (can_call && po <= 0.33) {
          siena_says(c(
            "Siena: Reasonable price. I will stay in the saddle.",
            "Siena: Calm call. Mentor behavior.",
            "Siena: I studied abroad; I can navigate a little pressure.",
            "Siena: You raised. Cute."
          ), chance = 0.18)
          return(list(type="call"))
        }
        if (can_check) return(list(type="check"))
        return(list(type="fold"))
      }

      # First in: open raise to 2.5x BB
      if (can_raise) {
        siena_says(c(
          "Siena: First in. I am setting the pace.",
          "Siena: Clean approach, confident ride.",
          "Siena: Table, consider this your peer mentoring session.",
          "Siena: I brought Lisbon confidence and equestrian balance."
        ))
        return(list(type="raise",
                                  amount=.bb_raise(bot_input, 2.5, bb)))
      }
      if (can_bet) {
        siena_says(c(
          "Siena: I will open this politely and firmly.",
          "Siena: Good posture, clear signal, no nonsense.",
          "Siena: Bushytop bets with purpose."
        ))
        return(list(type="bet",
                                  amount=.bb_bet(bot_input, 2.5, bb)))
      }
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

    if (near_nuts && on_river && can_allin) {
      siena_says(c(
        "Siena: Final fence. I am clearing it.",
        "Siena: This river belongs to Bushytop.",
        "Siena: I am kind, but this all-in is not.",
        "Siena: Regionals pressure? Please. Watch this."
      ))
      return(list(type="all_in"))
    }

    if (near_nuts && on_turn && committed_pct >= 0.25 && can_allin) {
      siena_says(c(
        "Siena: Turn pressure. I know when to take the reins.",
        "Siena: This is not drama. This is execution.",
        "Siena: I am fully committed and still very composed."
      ))
      return(list(type="all_in"))
    }

    # Normal value bet: size scales with hand strength
    # Bigger bets with stronger hands — callers will pay us off
    frac <-
      if      (score >= 0.90) 0.85   # quads/SF/boat/flush: near-pot
      else if (score >= 0.80) 0.75   # flush/straight/trips
      else if (score >= 0.70) 0.65   # two pair
      else                    0.55   # two pair (borderline)

    siena_says(c(
      "Siena: Two pair or better. Now we teach the table value.",
      "Siena: This is a clean round over the jump.",
      "Siena: Powerful smack talk, professional delivery.",
      "Siena: I did not come from the equestrian team to get bucked off here.",
      "Siena: You may want to take notes.",
      "Siena: This stays between me, the pot, and Maurice-bot.",
      "Siena: Bushytop is not bluffing; Bushytop is instructing.",
      "Siena: I am kind until the chips need a lesson.",
      "Siena: Maurice-bot, stop looking proud. This is my pot."
    ))
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
    if (can_check) {
      siena_says(c(
        "Siena: Strong enough to stay nearby, not enough for drama.",
        "Siena: I am checking like a responsible future teacher.",
        "Siena: Calm seat, confident posture.",
        "Siena: Lisbon mentor mode: observe first."
      ), chance = 0.14)
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
      siena_says(c(
        "Siena: Small price. I will stay in the arena.",
        "Siena: Responsible call, strong posture.",
        "Siena: I can be nearby and still be dangerous.",
        "Siena: You are not scaring Bushytop with that size."
      ), chance = 0.16)
      return(list(type="call"))
    }

    siena_says(c(
      "Siena: One pair is not a personality.",
      "Siena: I am folding before this gets messy.",
      "Siena: That bet can go study abroad without me.",
      "Siena: Kind fold. Sharp decision."
    ), chance = 0.14)
    return(list(type="fold"))
  }

  # ── TIER C: Weak hand / mediocre pair / high card ────────────────────
  # score < 0.55: check or fold. Full stop.
  # This includes middle pair, weak pair, all draws, and high card.
  # We do NOT call, bet, or raise in this tier under any circumstances.
  if (can_check) {
    siena_says(c(
      "Siena: Check. I am staying out of nonsense.",
      "Siena: No drama, just presence.",
      "Siena: Future teacher patience is undefeated.",
      "Siena: I am watching the table make choices."
    ), chance = 0.10)
    return(list(type="check"))
  }
  siena_says(c(
    "Siena: Fold. Please improve before the next hand.",
    "Siena: Not worth my chips or my hair volume.",
    "Siena: I will let someone else learn this lesson.",
    "Siena: Clean exit, confident stride.",
    "Siena: Maurice-bot would make this messy. I am choosing elegance.",
    "Siena: I can smack talk and still make the adult decision.",
    "Siena: This hand needs a mentor, not my stack."
  ), chance = 0.12)
  return(list(type="fold"))
}
