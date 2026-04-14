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

my_bot_name <- function(bot_input) {

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

  ##########################################################
  # YOUR STRATEGY GOES BELOW
  ##########################################################

  # Example beginner strategy:
  # check if possible, otherwise call, otherwise fold

  if ("check" %in% legal_types) {
    return(list(type = "check"))
  }

  if ("call" %in% legal_types) {
    return(list(type = "call"))
  }

  if ("fold" %in% legal_types) {
    return(list(type = "fold"))
  }

  # Fallback
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

# ----------------------------------------------------------
# TEST 1: Build the exact bot_input from a live tournament state
# ----------------------------------------------------------
#
# Example usage:
#
 bot_input_example <- build_bot_input(tourn)
 str(bot_input_example)
 print(bot_input_example)
#
# Then try:#
 my_bot_name(bot_input_example)
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
 action <- my_bot_name(bot_input_example)
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