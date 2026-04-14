############################################################
# Poker Bot App Practice Lab
# Mathematics of Poker
#
# Format: R script with commented instructions
# Goal: work through core poker math, ranges, bot inputs,
#       and simple bot design in the poker bot project.
############################################################

############################################################
# PURPOSE
#
# In this lab you will work through three connected ideas that
# sit at the heart of the poker bot app:
#
# 1. Mathematical decision tools such as pot odds,
#    break-even equity, expected value, and regret.
# 2. Ranges and uncertainty, including Monte Carlo equity
#    and weighted opponent ranges.
# 3. Bot design, where you inspect the information available
#    to a bot and then modify a starter bot.
#
# The goal is not to build a perfect poker agent. The goal is
# to understand how mathematical quantities become inputs to
# decisions, and how those decisions are encoded in an R function.
############################################################

############################################################
# LEARNING GOALS
#
# By the end of this lab, you should be able to:
# - compute and interpret core poker quantities,
# - estimate hand equity using simulation,
# - represent a simple weighted range in R,
# - inspect the bot_input object passed to a bot,
# - modify a simple bot so that its actions depend on
#   mathematical information.
############################################################

############################################################
# SUGGESTED TIMING FOR A 90-MINUTE LAB
#
# Part 0. Setup                      5 minutes
# Part I. Core calculations         20 minutes
# Part II. Equity and ranges        25 minutes
# Part III. Inspecting bot input    15 minutes
# Part IV. Modifying a bot          20 minutes
# Wrap-up                            5 minutes
############################################################


############################################################
# Part 0. Setup
############################################################

# This lab file now lives in assignments_demos/.
# Run it from the poker bot project root, then load the project files.

source("poker_load_all.R")
poker_load_all(include_demos = TRUE)

# If your project folder is set correctly, this should load the
# engine, math functions, example bots, and demos.

# Files you will use today:
# - shared_helpers/poker_math.R: core mathematical calculations
# - shared_helpers/equity_tools.R: Monte Carlo equity tools
# - shared_helpers/quant_tools.R: range utilities and additional quantitative tools
# - core_internal/game_engine.R: the tournament and hand state engine
# - reference_bots/example_bots.R: sample bots
# - student_work/BotTemplate.R: a student-facing template for writing a bot


############################################################
# Part I. Core Poker Math
############################################################

# A bot does not directly "know" whether a play is good.
# Instead, it uses quantities that help evaluate decisions
# under uncertainty.


# 1. Pot odds and break-even equity
#
# Suppose the pot is 120 chips and you must call 40 chips.
# Run the code below. Then interpret the result.
#
# Question:
# What minimum equity do you need for a call to break even?

pot_before_call <- 120
call_amount <- 40

pot_odds(call_amount, pot_before_call)


# Task 1
# Suppose the pot is 100 chips and you are considering a bluff
# of 75 chips. Run the code below.
#
# Then answer:
# What fold frequency does your opponent need to have for this
# bluff to break even?

pot_before_bet <- 100
bet_amount <- 75

break_even_fold_prob_bluff(pot_before_bet, bet_amount)

# Write your response here:
#
#


# 2. Expected value of a call
#
# Now suppose:
# - the pot is 150,
# - the amount to call is 50,
# - your equity when called is 0.35.
#
# Run the code below.

ev_call(equity = 0.35, call_amount = 50, pot_before_call = 150)

# Try a few values of equity and see how the output changes.

ev_call(equity = 0.20, call_amount = 50, pot_before_call = 150)
ev_call(equity = 0.30, call_amount = 50, pot_before_call = 150)
ev_call(equity = 0.40, call_amount = 50, pot_before_call = 150)

# Task 2
# At about what equity does the call become profitable?
# Compare your answer to the break-even equity from the pot odds
# calculation above.
#
# Write your response here:
#
#


# 3. Regret as a way to compare actions
#
# A bot may choose among several actions. One useful way to think
# is to compare the chosen action with the best available action.

ev_options <- c(fold = 0, call = 8, raise = 13)
ev_best_action(ev_options)
ev_regret(ev_options, chosen_action = "call")

# Task 3
# What is the regret of calling here?
# What does that regret mean in words?

# Write your response here:
#
#

#Challenge: Create a new function mixed_regret which calculutes the regret of a mixed strategy for a given chosen_action

############################################################
# Part II. Equity and Ranges
############################################################

# Poker decisions depend on hidden information. Since we do not
# know the opponent's exact hand, we work with ranges and with
# equity against possible holdings.


# 4. Monte Carlo hand-vs-hand equity
#
# Here is a classic matchup: pocket aces against pocket kings.

hole_1 <- data.frame(
  rank = c("A", "A"),
  suit = c("h", "s"),
  card = c("Ah", "As"),
  stringsAsFactors = FALSE
)

hole_2 <- data.frame(
  rank = c("K", "K"),
  suit = c("h", "s"),
  card = c("Kh", "Ks"),
  stringsAsFactors = FALSE
)

holdem_equity_mc_fast(
  hole_list = list(hole_1, hole_2),
  n_sims = 500
)

# Task 4
# Record the approximate equity of each hand.
# Then rerun with n_sims = 50, 1000, and 50000.
#
# Questions:
# - What changes as the number of simulations increases?
# - What seems to stabilize?

holdem_equity_mc_fast(
  hole_list = list(hole_1, hole_2),
  n_sims = 50
)

holdem_equity_mc_fast(
  hole_list = list(hole_1, hole_2),
  n_sims = 500
)

holdem_equity_mc_fast(
  hole_list = list(hole_1, hole_2),
  n_sims = 1000
)

# Write your response here:
#
#


# 5. Equity on a partial board
#
# Now suppose the flop is already known.

board_df <- data.frame(
  rank = c("A", "7", "2"),
  suit = c("d", "c", "h"),
  card = c("Ad", "7c", "2h"),
  stringsAsFactors = FALSE
)

holdem_equity_mc_fast(
  hole_list = list(hole_1, hole_2),
  board_df = board_df,
  n_sims = 500
)

# Task 5
# Why does Player 1's equity change so dramatically here?
# Use poker language if helpful, but be mathematically precise.
#
# Write your response here:
#
#


# 6. Weighted ranges
#
# A range is a collection of possible hands together with weights.
# In this project, a simple Hold'em range can be built with
# new_range_holdem().

example_range <- new_range_holdem(
  data.frame(
    c1 = c("Ah", "Ks", "Qh"),
    c2 = c("Kd", "Qc", "Qs"),
    w  = c(3, 2, 1)
  ),
  label = "Example weighted range"
)

example_range
example_range$combos
sum(example_range$weights)

# Notice that the weights are normalized automatically.

# Task 6
# Explain what it means that the weights are normalized.
# Why might weighted ranges be more realistic than treating every
# possible hand as equally likely?
#
# Write your response here:
#
#


# 7. Build your own simple range
#
# Create a range meant to represent a very strong preflop raising
# range. Use 6 specific two-card combinations and give larger
# weights to the strongest hands.

strong_range <- new_range_holdem(
  data.frame(
    c1 = c("Ah", "As", "Kh", "Ad"),
    c2 = c("Ac", "Kd", "Ks", "Kc"),
    w  = c(4, 4, 3, 2)
  ),
  label = "Strong opening range"
)

strong_range$combos
strong_range$weights

# Here is an example of using holdem_equity_mc_fast() with a range
# on one seat and a fixed hand on another seat.

opponent_range <- new_range_holdem(
  data.frame(
    c1 = c("Kh", "Qs", "Jd"),
    c2 = c("Kd", "Qc", "Jh"),
    w  = c(3, 2, 1)
  ),
  label = "Opponent range"
)

hero_hand <- data.frame(
  rank = c("A", "K"),
  suit = c("h", "d"),
  stringsAsFactors = FALSE
)

holdem_equity_mc_fast(list(hero_hand, opponent_range), n_sims = 500)

# Task 7
# Modify the example above to create:
# - a tight range
# - a loose range
#
# Then compare the size and weight distribution of the two ranges.
# After that, test range-versus-range equity on various boards.

tight_range <- strong_range
loose_range <- strong_range

range_size(tight_range)
range_size(loose_range)

tight_range$combos
loose_range$combos

# Optional place to test range-vs-range equity.
# Replace these with your own ranges once you build them.

holdem_equity_mc_fast(list(tight_range, loose_range), n_sims = 500)

# Write your response here:
#
#


# 8. Ranges from strings
#
# These functions let you build ranges from text strings similar to
# what you might copy from an online range tool.

expand_range_string_to_classes("77-JJ")
expand_range_string_to_classes("A5s-A2s")
expand_range_string_to_classes("KQo-KTo")

r1 <- new_range_holdem_from_string("77-JJ, A5s-A2s, KQo-KTo")
r2 <- new_range_holdem_from_string("QQ+, AKs, AKo")

holdem_equity_mc_fast(list(r1, r2), n_sims = 500)

# Task 8
# Create two additional range strings of your own and compare them. Hint you can use poker-tools
# to construct the range and then copy in the hands. You will likely need to add quotes
# Try at least one comparison on a specific flop.
#
# Example starting point for a fixed flop:

flop_board <- data.frame(
  rank = c("K", "T", "4"),
  suit = c("h", "h", "c"),
  card = c("Kh", "Th", "4c"),
  stringsAsFactors = FALSE
)

holdem_equity_mc_fast(list(r1, r2), board_df = flop_board, n_sims = 500)

# Write your response here:
#
#


# 9. Board texture features
#
# The project also includes simple board-texture tools.

flop_df <- data.frame(
  rank = c("K", "T", "4"),
  suit = c("h", "h", "c"),
  stringsAsFactors = FALSE
)

board_features(flop_df)

# Task 9
# Inspect the output.
# Which pieces of this output might be useful to a bot deciding
# whether to bet the flop?
#
# Write your response here:
#
#


############################################################
# Part III. What Information Does a Bot Actually Receive?
############################################################

# A poker bot does not see the whole tournament state directly.
# It is given a structured object called bot_input.


# 10. Create a live tournament state
#
# We will initialize a small tournament, start a hand, and inspect
# the current acting player's input.

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

# Now build the input that the acting bot will receive.

bot_input_example <- build_bot_input(tourn)
str(bot_input_example, max.level = 2)

# You can also view it as a data frame.

demo_show_bot_input(tourn)

# Task 10
# Find and record the following pieces of information inside
# bot_input_example:
# - your hole cards,
# - the current pot,
# - the current street,
# - the legal actions,
# - your current stack,
# - the public information about the other players.
#
# Write your response here:
#
#


# 11. Explore the legal action structure

bot_input_example$legal_actions
bot_input_example$legal_actions$legal_action_types
bot_input_example$legal_actions$actions

# Task 11
# Why is it important for a bot to check which actions are legal
# before returning an action?
#
# Write your response here:
#
#


############################################################
# Part IV. Reading and Modifying a Bot
############################################################

# 12. Read a starter bot
#
# Open reference_bots/example_bots.R and locate the function
# simple_preflop_strength_bot().
#
# This bot does something simple:
# - if it is preflop and it likes its hand, it plays aggressively,
# - otherwise it falls back to checking or calling when possible.

# Task 12
# Read the function and answer:
# 1. What counts as a "premium" hand in this bot?
# 2. What does the bot do with premium hands?
# 3. What does it do after the flop?
#
# Write your response here:
#
#


# 13. Try a starter template
#
# Open student_work/BotTemplate.R. You should see a function called my_bot_name()
# and helper functions such as:
# - bot_has_action()
# - bot_min_bet()
# - bot_min_raise()
# - choose_preferred_action()
#
# These help keep your bot legal and readable.


# 14. Build a simple math-based bot
#
# Create a new bot that follows this rule:
#
# Preflop:
# - raise minimum with pairs, ace-king, or ace-queen,
# - otherwise check if possible,
# - otherwise call if the call is at most one big blind,
# - otherwise fold.
#
# Postflop:
# - check when possible,
# - otherwise call only if the break-even equity threshold is at
#   most 0.25,
# - otherwise fold.
#### Challenge have your bot estimate the equity of your hand, versus some pre-set range for villain.
#### Have it call a bet only if the estimated equity exceeds the pot odds.
# The starter version is below.

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

# Task 13
# Explain the line below in words:
#
#   threshold <- pot_odds(call_amount, pot)
#
# What quantity is the bot calculating?
# What is it using that threshold for?
#
# Write your response here:
#
#


# 15. Test your bot on a live input
#
# Replace one of the bots in the tournament with your new bot.

bot_fns_test <- list(
  "Lab Bot" = lab_bot,
  "Random Bot" = random_bot,
  "Caller Bot" = always_call_bot
)

tourn2 <- initialize_tournament(
  bot_fns = bot_fns_test,
  player_names = names(bot_fns_test),
  starting_stack = 1000
)

tourn2 <- initialize_hand(tourn2)
tourn2 <- post_blinds_and_antes(tourn2)

bot_input_test <- build_bot_input(tourn2)
demo_show_bot_input(tourn2)
lab_bot(bot_input_test)

# Task 14
# Run this section several times by re-initializing the tournament.
# Does your bot always return a legal action?
# Describe one situation where your bot raises, one where it calls,
# and one where it folds.
#
# Write your response here:
#
#


# 16. challenge: make the bot more thoughtful
#
# Revise lab_bot() so that it also uses board texture on the flop.
# For example, you might decide that on the flop the bot should:
# - bet when checked to on dry boards,
# - check more often on coordinated two-tone boards,
# - call less often when the required equity threshold is large.
#
# A starter idea is below.

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

# Task 15
# Modify this bot and explain your design choices.
#
# Write your response here:
#
#


# Demo Tournament with lab_bot
#
# After revising lab_bot(), you can test it in a small tournament
# against a few of the sample bots.

source("poker_load_all.R")
poker_load_all(include_demos = TRUE, verbose = FALSE)

demo_result <- run_tournament(
  bot_fns = list(
    lab_bot,
    random_bot,
    always_call_bot,
    passive_bot,
    aggressive_bot
  ),
  player_names = c(
    "Lab Bot",
    "Random Bot",
    "Caller Bot",
    "Passive Bot",
    "Aggro Bot"
  ),
  starting_stack = 2500,
  tournament_id = "LAB_BOT_DEMO",
  rng_seed = 123,
  max_hands = 200,
  verbose = TRUE
)

data.frame(
  player = vapply(demo_result$players, function(p) p$name, character(1)),
  chips = vapply(demo_result$players, function(p) p$stack, numeric(1)),
  place = vapply(demo_result$players, function(p) p$finishing_place, integer(1))
)[order(
  vapply(demo_result$players, function(p) p$finishing_place, integer(1))
), ]

# If you want a fairer comparison, run the tournament several times
# with different rng_seed values and compare your bot's average
# finishing place.


############################################################
# Wrap-Up Questions
############################################################

# 17. Math to code
# Which part of today's lab felt most clearly mathematical,
# and which part felt most like programming?
# How did the two interact?
#
# Write your response here:
#
#


# 18. Limits of the current bots
# What is one important piece of information that your bot does not
# currently use, but probably should use in a more serious version?
#
# Write your response here:
#
#


# 19. Reflection
# A poker bot acts under uncertainty and with limited information.
# Give one example from today's lab where the bot had to rely on a
# model or approximation rather than exact knowledge.
#
# Write your response here:
#
#
