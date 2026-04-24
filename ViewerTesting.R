source("poker_load_all.R")
poker_load_all(include_demos = FALSE)

source("assignments_demos/poker_demos.R")

tourn <- demo_tournament_run(
  max_hands = 15,
  verbose = FALSE,
  show_hand_details = FALSE
)

run_viewer_app(tourn)





# hand quality check ------------------------------------------------------
source("poker_load_all.R")
poker_load_all(include_demos = FALSE)
source("MadeForTV.R")

bot_fns <- list(
  random_bot,
  aggressive_bot,
  simple_preflop_strength_bot,
  always_call_bot,
  strength_by_street_bot,
  passive_bot,
  mixed_bot,
  mixed_bot2
)

player_names <- c(
  "Rando",
  "Aggro",
  "PrePlanner",
  "GetAlong",
  "Da streets",
  "ScardyBot",
  "Confused",
  "MoreConfused"
)

scores <- simulate_interest_scores(
  n_tournaments = 10,
  tournament_args = list(
    bot_fns = bot_fns,
    player_names = player_names,
    starting_stack = 10000,
    tournament_id = "DEMO_TOURNAMENT",
    max_hands = 100,
    verbose = FALSE
  ),
  seed = 100
)

plot_interest_score_histogram(scores)

head(scores[, c("tournament_id", "hand_number", "interest_score", "interest_reasons")])

plot_interest_score_histogram(scores)
table(scores$interest_score)
