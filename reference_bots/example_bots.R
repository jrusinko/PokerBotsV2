############################################################
# Mathematics of Poker — Example Bots
# File: example_bots.R
#
# Purpose:
#   Starter bots for testing and for student examples.
#
# Current engine interface:
#   Each bot is called as bot_fn(bot_input), where bot_input is a list
#   created by safe_get_bot_action() inside game_engine.R.
############################################################

# ----------------------------------------------------------
# Helpers
# (moved to `bot_api.R` to avoid duplication; `bot_api.R` provides the
# canonical implementations `bot_has_action`, `bot_min_bet`, etc.)
# ----------------------------------------------------------

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

# ----------------------------------------------------------
# Random bot
# ----------------------------------------------------------

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

# ----------------------------------------------------------
# Talking bot
# ----------------------------------------------------------

talking_bot <- function(bot_input) {
  if (runif(1) < 0.20) {
    lines <- c(
      "TalkingBot: I have a feeling about this one.\n",
      "TalkingBot: The cards are telling a story.\n",
      "TalkingBot: Bold choice. Possibly mine.\n",
      "TalkingBot: Let me think... okay, done.\n",
      "TalkingBot: This table has excellent dramatic tension.\n"
    )
    cat(sample(lines, size = 1))
  }

  random_bot(bot_input)
}

# ----------------------------------------------------------
# Always-call / always-check bot
# ----------------------------------------------------------

always_call_bot <- function(bot_input) {
  choose_preferred_action(bot_input, c("check", "call", "fold"))
}

# ----------------------------------------------------------
# Simple preflop strength bot
# ----------------------------------------------------------

simple_preflop_strength_bot <- function(bot_input) {
  hole_cards <- bot_input$hole_cards
  street <- bot_input$street

  # Preflop only: use a very simple rule.
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

  # Postflop fallback:
  choose_preferred_action(bot_input, c("check", "call", "fold"))
}


# ----------------------------------------------------------
# Aggressive bot
# ----------------------------------------------------------

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

# ----------------------------------------------------------
# Postflop hand-strength helpers
# ----------------------------------------------------------

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
  #
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

# ----------------------------------------------------------
# Preflop + postflop strength bot
# ----------------------------------------------------------

strength_by_street_bot <- function(bot_input) {
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street

  # -------------------------
  # Preflop rule
  # -------------------------
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

  # -------------------------
  # Postflop rule
  # -------------------------
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

# ----------------------------------------------------------
# Passive bot
# ----------------------------------------------------------

passive_bot <- function(bot_input) {
  choose_preferred_action(bot_input, c("check", "fold"))
}
# ----------------------------------------------------------
# Mixed personality bot factory
# ----------------------------------------------------------
# Default mixture:
#   passive      = 0.1
#   always_call  = 0.3
#   random       = 0.4
#   aggressive   = 0.2
#
# Usage:
#   mixed_bot <- make_mixed_bot()
#
# Or with custom probabilities:
#   mixed_bot <- make_mixed_bot(
#     passive_prob = 0.2,
#     always_call_prob = 0.2,
#     random_prob = 0.2,
#     aggressive_prob = 0.4
#   )
#
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
mixed_bot<-make_mixed_bot()
mixed_bot2<-make_mixed_bot(.2,.3,.1,.4)
# ----------------------------------------------------------
# Student template
# ----------------------------------------------------------

student_bot_template <- function(bot_input) {
  # Students should edit only the body of this function.
  #
  # The engine passes in a single list called bot_input. Useful fields:
  #   bot_input$hole_cards
  #   bot_input$board
  #   bot_input$street
  #   bot_input$pot
  #   bot_input$stack
  #   bot_input$legal_actions$legal_action_types
  #   bot_input$legal_actions$actions
  #
  # Valid return values include:
  #   list(type = "fold")
  #   list(type = "check")
  #   list(type = "call")
  #   list(type = "all_in")
  #   list(type = "bet", amount = x)
  #   list(type = "raise", amount = x)
  #
  # Important:
  #   For "bet" and "raise", the amount must be legal.
  #   Use bot_min_bet(bot_input), bot_max_bet(bot_input),
  #   bot_min_raise(bot_input), and bot_max_raise(bot_input).

  choose_preferred_action(bot_input, c("check", "call", "fold"))
}

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
