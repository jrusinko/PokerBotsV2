############################################################
# Bot Name: Bill the Dinosaur (The Caller-Slayer Final)
# Strategy: Delayed Aggression / The Trap / Silent
############################################################

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
  clamp_bet <- function(amt) {
    return(max(min(amt, stack), bot_input$min_bet))
  }

  bill_says <- function(lines) {
    if (runif(1) < 0.35) {
      cat(sample(lines, size = 1), "\n")
    }
  }

  # 2. PREFLOP: THE TAX
  # We make it expensive for them to exist.
  if (identical(street, "preflop")) {
    if (max(vals) >= 13 || length(unique(vals)) == 1) {
      if (bot_has_action(bot_input, "raise")) {
        bill_says(c(
          "Bill: Found a monster. Starting the war.",
          "Bill: Mady, this one is for the data set.",
          "Bill: Mady, try not to be too impressed. Or do. That is fine."
        ))
        return(list(type = "raise", amount = clamp_bet(bot_input$min_bet * 5))) # 5x Raise!
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
        bill_says(c(
          "Bill: THE WAR IS ON. Overbetting for max value.",
          "Bill: Mady, this is what a not-so-secret crush looks like in chip form.",
          "Bill: Jake is old news. This river is current events."
        ))
        return(list(type = "raise", amount = stack)) # ALL IN
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
        bill_says(c(
          "Bill: THE WAR IS ON. Overbetting for max value.",
          "Bill: Mady, please update your model to include romantic pressure."
        ))
      } else {
        bill_says(c(
          "Bill: Controlling the pot with a solid hand.",
          "Bill: Mady, this is responsible value. Character growth."
        ))
      }
      return(list(type = "bet", amount = clamp_bet(floor(pot * 0.4))))
    }
    return(choose_preferred_action(bot_input, c("call", "check")))
  }

  # 4. THE NO-BLUFF POLICY
  bill_says(c(
    "Bill: No war today. Folding.",
    "Bill: Mady, I am folding responsibly. Please note the maturity.",
    "Bill: I fold, but my crush on Mady remains aggressively uncapped."
  ))
  return(choose_preferred_action(bot_input, c("check", "fold")))
}
