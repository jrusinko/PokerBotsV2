source("poker_math.R")
source("quant_tools.R")
source("equity_tools.R")
source("bot_api.R")

joel_bot <- function(bot_input) {

  hole_cards <- bot_input$hole_cards
  pot <- bot_input$pot
  call_amount <- bot_call_amount(bot_input)

  joel_says <- function(lines, chance = 0.18) {
    if (runif(1) < chance) {
      cat(sample(lines, size = 1), "\n")
    }
  }


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
    joel_says(c(
      "Joel: Analytics department likes this forecheck.",
      "Joel: Warm reminder: pressure is caring.",
      "Joel: This hand has first-line minutes.",
      "Joel: I have seen teams win three of four title games. Still processing.",
      "Joel: Coach says get pucks deep. Poker bot says raise small.",
      "Joel: If Lucy asks, this is statistically responsible hockey.",
      "Joel: I am not trying to impress Lucy. The model just looks impressive."
    ))
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
      joel_says(c(
        "Joel: Free look. Even I might attend this one.",
        "Joel: No cost, no complaint. Rare classroom energy.",
        "Joel: I will check and call it player development.",
        "Joel: I am checking calmly, which is definitely not because Lucy is nearby."
      ), chance = 0.14)
      return(list(type = "check"))
    }

    # Stronger medium hands sometimes raise
    if (ace_high && (suited || connected) && odds < 0.2) {

      if (bot_has_action(bot_input, "raise")) {
        joel_says(c(
          "Joel: The model says this is a controlled zone entry.",
          "Joel: Ace high with structure. Very coachable.",
          "Joel: Sports analytics says push. My dry sense of humor agrees.",
          "Joel: Lucy would probably explain this more kindly, but I am raising."
        ))
        return(list(type = "raise",
                    amount = bot_min_raise(bot_input)))
      }
    }

    # Ace-high hands call more often
    if (ace_high && odds < 0.4) {
      joel_says(c(
        "Joel: Ace high gets a shift.",
        "Joel: Reasonable odds. I am nurturing this possession.",
        "Joel: Calling here feels caring, statistically."
      ), chance = 0.16)
      return(list(type = "call"))
    }

    # King-high hands are tighter
    if (king_high && odds < 0.25) {
      joel_says(c(
        "Joel: King high, limited minutes.",
        "Joel: Tight call. Responsible coaching, allegedly.",
        "Joel: This is not a championship game four decision."
      ), chance = 0.16)
      return(list(type = "call"))
    }

    # Suited hands get extra value
    if (suited && odds < 0.3) {
      joel_says(c(
        "Joel: Suited cards have chemistry.",
        "Joel: The line pairings are interesting here.",
        "Joel: I like the underlying numbers. Quietly.",
        "Joel: Chemistry is important. In cards. Obviously just cards."
      ), chance = 0.16)
      return(list(type = "call"))
    }

    # Connected cards can make straights
    if (connected && odds < 0.25) {
      joel_says(c(
        "Joel: Connected cards. Good passing lane.",
        "Joel: This hand has puck movement.",
        "Joel: A little structure, a little hope, a very small call."
      ), chance = 0.16)
      return(list(type = "call"))
    }

    # Otherwise fold
    joel_says(c(
      "Joel: Folding is just load management.",
      "Joel: I will be absent from this pot. Consistent brand.",
      "Joel: Caring deeply means letting this hand go.",
      "Joel: Lucy, please do not grade this fold too harshly."
    ), chance = 0.16)
    return(list(type = "fold"))
}

  # WEAK → fold unless free
  if (call_amount == 0) {
    joel_says(c(
      "Joel: Free check. Attendance recorded.",
      "Joel: I am present for this hand, technically.",
      "Joel: No bet? Wonderful. Low-stakes development."
    ), chance = 0.14)
    return(list(type = "check"))
  }

  joel_says(c(
    "Joel: Not enough expected value, and frankly not enough coffee.",
    "Joel: Fold. We will review film later.",
    "Joel: This one can transfer to the discard pile."
  ), chance = 0.16)
  return(list(type = "fold"))
}
