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

  # ==========================================================================
  # Preflop Strategy:
  #
  # Before any board cards are shown, the bot only plays safer starting hands.
  # It plays pocket pairs or two high cards, and folds weaker hands.
  # ===========================================================================

  if (street == "preflop") {

    # A hand is treated as strong if it is a pair or both cards are 10 or higher
    is_strong_preflop <- (hole_vals[1] == hole_vals[2]) || (hole_vals[1] >= 10 && hole_vals[2] >= 10)


    if (is_strong_preflop) {
      # With a strong starting hand, the bot tries to raise; if that’s not legal, it bets;
      # if that’s not legal, it calls.
      if ("raise" %in% legal) return(list(type = "raise", amount = bot_min_raise(bot_input)))
      if ("bet" %in% legal) return(list(type = "bet", amount = bot_min_bet(bot_input)))
      if ("call" %in% legal) return(list(type = "call"))
    } else {
      # If the hand is not strong, check if it is legal; if it isn’t, fold.
      if ("check" %in% legal && !facing_bet) return(list(type = "check"))
      return(list(type = "fold"))
    }
  }

  # =====================================================================================================
  # Postflop Strategy
  #
  # If the bot has a made hand, it calls when facing a bet and bets aggressively if no one has bet yet.
  # If the bot has a weak hand, it folds when facing a bet and bluffs if no one has bet yet.
  # =====================================================================================================

  # Check if we have made a hand (pair or better).
  board_vals <- sort(hole_rank_values(bot_input$board), decreasing = TRUE)
  is_made_hand <- (hole_vals[1] == hole_vals[2]) ||
    (hole_vals[1] %in% board_vals) ||
    (hole_vals[2] %in% board_vals)

  # If we have a made hand, call if facing a bet;
  # if no one has bet yet, make a bet equal to the pot size.
  if (is_made_hand) {
    if (facing_bet) {
      if ("call" %in% legal) return(list(type = "call"))
    } else {
      if ("bet" %in% legal) return(list(type = "bet", amount = calc_pot_bet()))
    }
  }

  # If we have a weak hand: if facing a bet, don’t bluff, fold;
  # if no one has bet, bluff with a bet equal to the pot.
  else {
    if (facing_bet) {
      if ("check" %in% legal) return(list(type = "check"))
      return(list(type = "fold"))
    } else {
      if ("bet" %in% legal) return(list(type = "bet", amount = calc_pot_bet()))
    }
  }


  # ========================================================
  # Back-up Strategy
  #
  # If our code reaches here due to strange game states, default to safe moves.
  # ========================================================
  if ("check" %in% legal) return(list(type = "check"))
  if ("call" %in% legal) return(list(type = "call"))
  return(list(type = "fold"))
}
