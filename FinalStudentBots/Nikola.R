############################################################
# Mathematics of Poker — Student Bot Template
#
# INSTRUCTIONS
# 1. Rename the function below to your bot name.
# 2. Write your strategy inside that function.
# 3. Use the testing section at the bottom to inspect bot_input.
#
# Your function name IS your bot name.
############################################################


############################################################
# OPTIONAL HELPERS
############################################################

## Bot helpers are provided by `bot_api.R` (canonical location).
## Students and example bots can use `bot_has_action()`, `bot_min_bet()`,
## `bot_min_raise()`, and `choose_preferred_action()` from there.


############################################################
# STUDENT BOT
#
# Rename this function to your bot name.
# Example:
#   joe_bot <- function(bot_input) { ... }
############################################################

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

  nikola_says <- function(lines, chance = 0.17) {
    bot_maybe_say(lines, bot_input, chance)
  }

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

      nikola_says(c(
        "Nikola: You misjudged me. That was your first bug.",
        "Nikola: Serbian confidence, compiled successfully.",
        "Nikola: This hand has bravado and clean logic.",
        "Nikola: I am quiet until the algorithm says attack.",
        "Nikola: Computer science major. Villain model says you are in trouble.",
        "Nikola: The villains underestimated my runtime.",
        "Nikola: This is not swagger. This is predictive modeling.",
        "Nikola: I have been alienated into excellent focus."
      ))

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
        nikola_says(c(
          "Nikola: Heads up and short stacked. Perfect time for drama.",
          "Nikola: You thought I was isolated. I call it focus.",
          "Nikola: Small stack, big Serbian energy.",
          "Nikola: Villain has misread the room."
        ))
        if ("raise" %in% legal_types) {
          return(list(type = "raise", amount = min_raise_safe()))
        }

       return(safe_action(c("call", "check", "fold")))
      }

      if (heads_up_playable) {

        nikola_says(c(
          "Nikola: Heads up, I can outthink one villain.",
          "Nikola: This is where the teasing begins.",
          "Nikola: I have a plan. You are mostly in it as a mistake.",
          "Nikola: One opponent is just a debugging exercise."
        ), chance = 0.14)

        if ("raise" %in% legal_types && amount_to_call <= 2 * big_blind) {

          return(list(type = "raise", amount = min_raise_safe()))

        }

        if (amount_to_call <= 4 * big_blind) {

          return(safe_action(c("call", "check", "fold")))

        }

      }

      if (amount_to_call == 0) {

        if (bluff_now && "bet" %in% legal_types) {

          nikola_says(c(
            "Nikola: Bluff deployed. Do not look so surprised.",
            "Nikola: This is not reckless. This is theatrical probability.",
            "Nikola: I am representing something. Maybe greatness.",
            "Nikola: You folded to the idea of me."
          ))

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

        nikola_says(c(
          "Nikola: Multiway pot. I will still find the weak process.",
          "Nikola: Too many villains, but I brought enough bravado.",
          "Nikola: Serbia versus the table. I like the fixture.",
          "Nikola: I feel alienated, but my range is social."
        ), chance = 0.14)

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

      nikola_says(c(
        "Nikola: Strong enough to pressure the villains.",
        "Nikola: I have been underestimated. Again. Convenient.",
        "Nikola: This line is elegant, unlike your call.",
        "Nikola: My code says raise. My ego says obviously."
      ), chance = 0.15)

      if ("raise" %in% legal_types && amount_to_call <= 2 * big_blind) {

        return(list(type = "raise", amount = min_raise_safe()))

      }

      return(safe_action(c("call", "check", "fold")))

    }

    if (playable && amount_to_call <= 1.5 * big_blind) {

      nikola_says(c(
        "Nikola: Playable. I will keep calculating.",
        "Nikola: Not my strongest hand, but I see the path.",
        "Nikola: You think this is small. I think it is information.",
        "Nikola: I can work from here."
      ), chance = 0.12)

      return(safe_action(c("call", "check", "fold")))

    }

    nikola_says(c(
      "Nikola: Fold. Even bravado has a base case.",
      "Nikola: This hand is not worth alienating my stack.",
      "Nikola: I refuse to donate to badly written villain code.",
      "Nikola: Not today. I will outsmart you later."
    ), chance = 0.12)
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
  nikola_says(c(
    "Nikola: Big call, insufficient equity. I am not your liquidity.",
    "Nikola: I bluff with purpose. I do not call nonsense.",
    "Nikola: Careful fold. The villain gets no extra chips from me.",
    "Nikola: This is not fear. This is code review."
  ), chance = 0.13)
  return(safe_action(c("fold", "check")))
}
  ##########################################################

  # 11. MONSTER HANDS

  #

  # Straight, flush, full house, quads, etc.

  # Goal: build the pot.

  ##########################################################

  if (monster || equity >= 0.75) {

    nikola_says(c(
      "Nikola: Monster hand. Now the bravado is just documentation.",
      "Nikola: You misjudged me and the board agreed.",
      "Nikola: Serbia scores on the counterattack.",
      "Nikola: This pot is about to learn computer science.",
      "Nikola: I have the hand you were pretending to have.",
      "Nikola: Villain, your narrative has failed unit testing.",
      "Nikola: This is the part where bravado becomes evidence.",
      "Nikola: I accept your chips as an apology."
    ))

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

    nikola_says(c(
      "Nikola: Strong hand. I will make the villains uncomfortable.",
      "Nikola: The model is confident and so am I.",
      "Nikola: You can call, but do not say I did not warn you.",
      "Nikola: I am outsmarting the room one branch at a time."
    ), chance = 0.16)

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

    nikola_says(c(
      "Nikola: Medium strength. Time for controlled arrogance.",
      "Nikola: This is not the loud hand. It is the clever hand.",
      "Nikola: I will not overplay it. Probably.",
      "Nikola: The villains expect chaos. I offer calculation."
    ), chance = 0.13)

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

    nikola_says(c(
      "Nikola: Bluff time. Try to keep up.",
      "Nikola: This is bravado with variable names.",
      "Nikola: I feel alienated, so I built my own pressure.",
      "Nikola: Villain thinks I missed. Villain is adorable.",
      "Nikola: I am not lying. I am simulating confidence.",
      "Nikola: This bluff has architecture.",
      "Nikola: You called me reckless. I call this calibrated theater.",
      "Nikola: The villain model says fold, but villains rarely listen."
    ))

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

    nikola_says(c(
      "Nikola: Check. Silence can be aggressive.",
      "Nikola: I will let the villains make mistakes first.",
      "Nikola: No bet. I am saving the speech for later.",
      "Nikola: Quiet mode enabled."
    ), chance = 0.10)

    return(safe_action(c("check", "fold")))

  }

  if (is_heads_up && equity >= 0.43 && pot_odds_value <= 0.32) {

    nikola_says(c(
      "Nikola: Heads up call. I have read your pattern.",
      "Nikola: One villain, one calculation.",
      "Nikola: I can continue. You are not that scary."
    ), chance = 0.12)

    return(safe_action(c("call", "fold")))

  }

  if (!is_heads_up && equity > pot_odds_value + 0.15) {

    nikola_says(c(
      "Nikola: Multiway call, but only because the math permits it.",
      "Nikola: I am still here. Annoying, yes.",
      "Nikola: Pot odds open the door. I walk in dramatically."
    ), chance = 0.12)

    return(safe_action(c("call", "fold")))

  }

  nikola_says(c(
    "Nikola: Fold. The ambush will wait.",
    "Nikola: You win this branch, not the program.",
    "Nikola: I was misjudged before the hand and I will be misjudged after.",
    "Nikola: Exit condition met.",
    "Nikola: I am not beaten. I am garbage collecting.",
    "Nikola: This fold is tactical invisibility."
  ), chance = 0.12)
  return(safe_action(c("fold", "check")))

}
############################################################

# TESTING / DEBUGGING SECTION

############################################################

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
