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

source("core_internal/bot_api.R")
source("core_internal/cards_and_hands.R")
source("core_internal/game_engine.R")
source("core_internal/tournament_runner.R")
source("reference_bots/example_bots.R")
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

lucy_bot <- function(bot_input) {

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

  lucy_says <- function(lines, chance = 0.16) {
    bot_maybe_say(lines, bot_input, chance)
  }

#Preflop

  if (street == "preflop" && length(hole_cards) == 2) {

    vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)

    paired <- length(unique(vals)) == 1
    strong_ace <- max(vals) == 14 && min(vals) >= 10
    high_cards <- min(vals) >= 11

    premium <- paired && vals[1] >= 10      # TT+
    strong <- strong_ace || high_cards      # AQ, AJ, KQ,...

    # Premium hands:play aggressively
    if (premium) {
      lucy_says(c(
        "Lucy: This hand has enough thrust for launch.",
        "Lucy: NASA Space Camp says we are go for pressure.",
        "Lucy: I am quietly very excited about these cards.",
        "Lucy: Physics note: strong initial conditions matter.",
        "Lucy: My boyfriend would say be careful. Joel would probably make a chart.",
        "Lucy: This launch trajectory is perfectly normal and not about Joel.",
        "Lucy: Teacher voice says everyone breathe; physics voice says raise.",
        "Lucy: Space Camp prepared me for pressure and awkward trajectories.",
        "Lucy: Joel's spreadsheet energy is not relevant, probably.",
        "Lucy: This hand has kind momentum and suspicious acceleration."
      ))
      if ("raise" %in% legal_types) {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
      if ("call" %in% legal_types) {
        return(list(type = "call"))
      }
    }

    # Strong Hands: call or raise occasionally
    if (strong) {
      if (runif(1) < 0.3 && "raise" %in% legal_types) {
        lucy_says(c(
          "Lucy: Small classroom demonstration: sometimes we raise.",
          "Lucy: The tutoring voice says show your work. The cards say raise.",
          "Lucy: Positive energy, careful orbit, tiny raise.",
          "Lucy: Joel, please do not make a hockey analogy about this orbit.",
          "Lucy: This is peer-reviewed encouragement with chips.",
          "Lucy: My boyfriend likes caution. The cards are requesting curiosity."
        ))
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
      if ("call" %in% legal_types) {
        lucy_says(c(
          "Lucy: I can learn one more street from this.",
          "Lucy: This is a nice little physics problem.",
          "Lucy: Quiet call. We are collecting data.",
          "Lucy: My boyfriend trusts my judgment. Joel probably trusts the spreadsheet."
        ), chance = 0.14)
        return(list(type = "call"))
      }
    }

    # Occasional Bluff
    if (runif(1) < 0.1 && "raise" %in% legal_types) {
      lucy_says(c(
        "Lucy: This is my tiny experimental bluff.",
        "Lucy: Space Camp taught me to trust the simulator occasionally.",
        "Lucy: I promise this is educational."
      ))
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }

    # Weak Hands
    lucy_says(c(
      "Lucy: This one can stay after class and think about choices.",
      "Lucy: No worries, we can fold kindly.",
      "Lucy: Not every orbit is stable."
    ), chance = 0.12)
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }


#Post-Flop

  category <- made_hand_category(hole_cards, board)

  strong_hands <- c("two_pair", "trips", "straight", "flush",
                    "full_house", "quads", "straight_flush")

  medium_hands <- c("pair")

#Strong Hands, Aggressive

  if (category %in% strong_hands) {
    lucy_says(c(
      "Lucy: Oh, this is a beautiful result.",
      "Lucy: The physics is working out nicely.",
      "Lucy: Teacher voice says this is a teachable moment.",
      "Lucy: Mission control, we have a hand.",
      "Lucy: Joel, no need to be impressed. But also, thank you.",
      "Lucy: This is the kind of solution I would happily tutor.",
      "Lucy: Positive attitude, strong hand, stable orbit."
    ))
    if ("raise" %in% legal_types) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if ("bet" %in% legal_types) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    if ("call" %in% legal_types) {
      return(list(type = "call"))
    }
  }

#Meduim Hands, Pot Odds

  if (category %in% medium_hands) {

    # Check if possible
    if ("check" %in% legal_types) {
      lucy_says(c(
        "Lucy: Gentle check. Everyone is doing great.",
        "Lucy: I will let this system evolve.",
        "Lucy: Quiet observation is still science."
      ), chance = 0.14)
      return(list(type = "check"))
    }

    # Only call small bets
    if ("call" %in% legal_types && current_bet < 0.25 * pot) {
      lucy_says(c(
        "Lucy: Small enough to tutor through.",
        "Lucy: I can call and still be emotionally supportive.",
        "Lucy: The numbers are not scary yet."
      ), chance = 0.14)
      return(list(type = "call"))
    }

    lucy_says(c(
      "Lucy: That bet has too much velocity.",
      "Lucy: I am going to step gently out of orbit.",
      "Lucy: Warm fold. No hard feelings."
    ), chance = 0.14)
    return(list(type = "fold"))
  }

#Weak Hands, Bluff Sometimes

  if (runif(1) < 0.15) {
    if ("bet" %in% legal_types) {
      lucy_says(c(
        "Lucy: Tiny hypothesis test.",
        "Lucy: This is not mischief, it is inquiry.",
        "Lucy: A small bet for science."
      ), chance = 0.18)
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
  }

#Default

  lucy_says(c(
      "Lucy: We can be patient. Space is big.",
      "Lucy: Quiet fold/check, positive attitude.",
      "Lucy: Sometimes the best lesson is restraint.",
      "Lucy: Restraint is important. That is a general statement, Joel.",
      "Lucy: I am being calm, supportive, and statistically evasive.",
      "Lucy: This system can cool down before re-entry."
  ), chance = 0.12)
  return(choose_preferred_action(bot_input, c("check", "fold")))
}



#Run tournament
if (FALSE) {
  results <- run_tournament(
    list(lucy_bot, random_bot, aggressive_bot, always_call_bot)
  )

  print(results)
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
#bot_input_example <- build_bot_input(tourn)
#str(bot_input_example)
#print(bot_input_example)
#
# Then try:#
#lucy_bot(bot_input_example)
#


# ----------------------------------------------------------
# TEST 2: View bot_input as a data frame
# ----------------------------------------------------------
#
# Example usage:
#
#bot_input_example <- build_bot_input(tourn)
#bot_input_df <- bot_input_to_dataframe(bot_input_example)
#print(bot_input_df)
#



# ----------------------------------------------------------
# TEST 3: Explore individual pieces of bot_input
# ----------------------------------------------------------
#
# Example usage:
#
#bot_input_example <- build_bot_input(tourn)
#
#bot_input_example$hole_cards
#bot_input_example$board
#bot_input_example$street
#bot_input_example$pot
#bot_input_example$stack
#bot_input_example$legal_actions$legal_action_types
#bot_input_example$legal_actions$actions
#bot_input_example$public_players
#bot_input_example$action_history
#


# ----------------------------------------------------------
# TEST 5: Run your bot on a real input
# ----------------------------------------------------------
#
# Example usage:
#
# bot_input_example <- build_bot_input(tourn)
# action <- lucy_bot(bot_input_example)
# print(action)
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
