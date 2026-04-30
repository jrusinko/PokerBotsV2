############################################################
# Bot Name: The Sniper
# Strategy: Zero-Bluff / Max-Value / Caller-Slayer
############################################################

budy_the_dinosaur <- function(bot_input) {
  street      <- bot_input$street
  pot         <- bot_input$pot
  hole_cards  <- bot_input$hole_cards
  board       <- bot_input$board

  category <- made_hand_category(hole_cards, board)

  # --- 1. THE NO-BLUFF RULE ---
  # If we don't have a Pair, Two-Pair, or better, we NEVER bet.
  has_value <- category %in% c("pair", "two_pair", "trips", "straight", "flush", "full_house", "quads")

  # --- 2. POSTFLOP EXECUTION ---
  if (has_value) {
    # If we have a hand, we bet HUGE.
    # Since the Caller won't fold, we want them to pay the "Maximum Tax."
    if (bot_has_action(bot_input, "bet")) {
      target_bet <- floor(pot * 1.5) # 150% Overbet (Only with real hands!)
      return(list(type = "bet", amount = clamp_bet(target_bet, bot_input)))
    }
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_max_raise(bot_input)))
    }
    return(choose_preferred_action(bot_input, c("call", "check")))
  }

  # --- 3. DEFENSE (The "AdvancedGTO" Shield) ---
  # If we have nothing, we just check and fold.
  # We don't try to "out-bully" the Advanced bot. We just wait for a hand.
  return(choose_preferred_action(bot_input, c("check", "fold")))
}

# (Keep the clamp_bet helper from before)