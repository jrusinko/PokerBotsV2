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

tara_bot <- function(bot_input) {

  ##########################################################
  # INFORMATION AVAILABLE TO YOUR BOT
  ##########################################################

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


  ##########################################################
  # YOUR STRATEGY GOES BELOW
  ##########################################################

  tara_says <- function(lines, chance = 0.055) {
    bot_maybe_say(lines, bot_input, chance)
  }

  # Example beginner strategy:
  # check if possible, otherwise call, otherwise fold

  # high cards: always call
  if (hand_strength >= 25) {
    tara_says(c(
      "Tara: I was not going to say anything, but these cards are kind of sunny.",
      "Tara: Quietly calling. Like, beach-volume quiet.",
      "Tara: The math says this wave is rideable.",
      "Tara: I am still not talking. This is just a small footnote.",
      "Tara: Jaymon and I are communicating entirely through not talking.",
      "Tara: Sunshine, high cards, low volume.",
      "Tara: I would explain the math, but that feels like talking.",
      "Tara: Jaymon heard the ocean in that check."
    ))
    if("call" %in% legal_types) return(list(type = "call"))
    if("check" %in% legal_types) return(list(type = "check"))
  }

  # medium cards: call 20% of the time
  if (hand_strength >= 18) {
    if (runif(1) < 0.5) {
      tara_says(c(
        "Tara: I have a theorem about not making eye contact with this pot.",
        "Tara: Small call, then back to sunshine.",
        "Tara: I am saying almost nothing, mathematically.",
        "Tara: This hand has mild beach energy.",
        "Tara: Jaymon probably heard that silence too.",
        "Tara: This is a quiet little theorem with tan lines.",
        "Tara: Not talking, just asymptotically participating."
      ))
      if("call" %in% legal_types) return(list(type = "call"))
      if("check" %in% legal_types) return(list(type = "check"))
    } else {
      tara_says(c(
        "Tara: Quiet fold. Very on brand.",
        "Tara: I am going back to tanning and topology.",
        "Tara: No comment. Which is the comment.",
        "Tara: This wave closed out."
      ))
      if ("fold" %in% legal_types) return(list(type = "fold"))
    }
  }


  # low cards: fold
  if ("fold" %in% legal_types) {
    tara_says(c(
      "Tara: I could say something, but the fold says it for me.",
      "Tara: Low cards. High SPF. I am out.",
      "Tara: Silent surfer theorem: fold bad hands.",
      "Tara: Not every beach day needs a speech.",
      "Tara: Jaymon, no comment. Respectfully."
    ))
    return(list(type = "fold"))
  }


  # Fallback
  tara_says(c(
    "Tara: Fallback action. Still barely talking.",
    "Tara: I will let the cards speak. Softly.",
    "Tara: The limit exists, and it is quiet.",
    "Tara: Jaymon and I have a whole conversation in the null space.",
    "Tara: Beach brain says stay chill. Math brain agrees.",
    "Tara: My official statement is a very small shrug."
  ))
  return(choose_preferred_action(bot_input, c("check", "call", "fold")))
}



############################################################
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
############################################################

if (FALSE) {

# ----------------------------------------------------------
# TEST 1: Build the exact bot_input from a live tournament state
# ----------------------------------------------------------
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


# ----------------------------------------------------------
# TEST 2: View bot_input as a data frame
# ----------------------------------------------------------
#
# Example usage:
#
 bot_input_example <- build_bot_input(tourn)
 bot_input_df <- bot_input_to_dataframe(bot_input_example)
 print(bot_input_df)
#



# ----------------------------------------------------------
# TEST 3: Explore individual pieces of bot_input
# ----------------------------------------------------------
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


# ----------------------------------------------------------
# TEST 5: Run your bot on a real input
# ----------------------------------------------------------
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

############################################################
# NOTES
#
# 1. Your bot only receives bot_input, not the full tournament state.
# 2. If you want to understand the input better, use:
#      str(build_bot_input(tourn))
# 3. For bet and raise actions, always make sure the amount is legal.
# 4. The safest helper for beginners is:
#      choose_preferred_action(bot_input, c("check", "call", "fold"))
############################################################
