source("poker_math.R")
source("quant_tools.R")
source("equity_tools.R")
source("bot_api.R")   # if this exists in your setup


student_bot_template <- function(bot_input) {
  
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