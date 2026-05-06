source("poker_math.R")
source("quant_tools.R")
source("equity_tools.R")
source("bot_api.R")

joel_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  pot <- bot_input$pot
  call_amount <- bot_call_amount(bot_input)


  # -------------------------
  # HAND STRENGTH
  # -------------------------
  vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

  paired <- length(unique(vals)) == 1
  high_card <- max(vals) >= 12   # A, K, Q
  ace_high <- max(vals) == 14
  king_high <- max(vals) == 13
  suited <- substr(hole_cards[1], 2, 2) == substr(hole_cards[2], 2, 2)
  connected <- abs(vals[1] - vals[2]) == 1

  # -------------------------
  # POT ODDS
  # -------------------------
  odds <- pot_odds(call_amount, pot)

  # -------------------------
  # DECISION LOGIC
  # -------------------------

  # STRONG → aggressive
  if (paired || max(vals) >= 13 ) {
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    return(choose_preferred_action(bot_input, c("call", "check")))
  }

  # MEDIUM → more pot odds and card dependent
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

  # WEAK → fold unless free
  if (call_amount == 0) {
    return(list(type = "check"))
  }

  return(list(type = "fold"))
}