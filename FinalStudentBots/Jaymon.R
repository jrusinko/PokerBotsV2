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



jaymon_hand_strength <- function(hole_cards) {
  if (length(hole_cards) != 2) return(0)

  r1 <- substr(hole_cards[1], 1, nchar(hole_cards[1]) - 1)
  s1 <- substr(hole_cards[1], nchar(hole_cards[1]), nchar(hole_cards[1]))
  r2 <- substr(hole_cards[2], 1, nchar(hole_cards[2]) - 1)
  s2 <- substr(hole_cards[2], nchar(hole_cards[2]), nchar(hole_cards[2]))

  rank_val <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
                "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)

  v1 <- rank_val[r1]; v2 <- rank_val[r2]
  if (v1 < v2) { tmp <- v1; v1 <- v2; v2 <- tmp; tmp_s <- s1; s1 <- s2; s2 <- tmp_s }

  suited <- (s1 == s2)

  if      (v1 == 14) base <- 10
  else if (v1 == 13) base <- 8
  else if (v1 == 12) base <- 7
  else if (v1 == 11) base <- 6
  else               base <- v1 / 2

  if (v1 == v2) {
    score <- max(base * 2, 5)
  } else {
    score <- base
    if (suited) score <- score + 2
    gap <- v1 - v2 - 1
    if (gap == 0)      score <- score + 1
    else if (gap == 2) score <- score - 1
    else if (gap == 3) score <- score - 2
    else if (gap >= 4) score <- score - 4
    if (v1 <= 8 && gap <= 1) score <- score + 1
  }

  ceiling(score)
}

jaymon_make_villain_range <- function(hole_cards, board) {
  known <- c(hole_cards, board)
  all_ranks <- c("2","3","4","5","6","7","8","9","T","J","Q","K","A")
  all_suits <- c("h","s","c","d")
  remaining  <- setdiff(as.vector(outer(all_ranks, all_suits, paste0)), known)

  n <- length(remaining)
  if (n < 2) return(NULL)

  pairs <- combn(n, 2)
  combos_df <- data.frame(
    c1 = remaining[pairs[1, ]],
    c2 = remaining[pairs[2, ]],
    w  = 1.0,
    stringsAsFactors = FALSE
  )

  tryCatch(new_range_holdem(combos_df, label = "villain"), error = function(e) NULL)
}

jaymon_equity_estimate <- function(hole_cards, board, n_opponents = 1, n_sims = 700) {
  tryCatch({
    our_df       <- data.frame(rank = extract_rank_from_label(hole_cards), suit = extract_suit_from_label(hole_cards), stringsAsFactors = FALSE)
    board_df     <- data.frame(rank = extract_rank_from_label(board),      suit = extract_suit_from_label(board),      stringsAsFactors = FALSE)
    villain_rng  <- jaymon_make_villain_range(hole_cards, board)
    if (is.null(villain_rng)) return(0.5)

    n_opp     <- min(n_opponents, 4)
    hole_list <- c(list(our_df), replicate(n_opp, villain_rng, simplify = FALSE))

    result <- holdem_equity_mc_fast(hole_list, board_df, n_sims = n_sims)
    result$equity[1]
  }, error = function(e) jaymon_equity(hole_cards, board))
}

jaymon_equity <- function(hole_cards, board) {
  if (length(board) < 3) {
    rv <- c("2"=2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7,"8"=8,"9"=9,
            "T"=10,"J"=11,"Q"=12,"K"=13,"A"=14)
    r1 <- substr(hole_cards[1], 1, nchar(hole_cards[1])-1)
    r2 <- substr(hole_cards[2], 1, nchar(hole_cards[2])-1)
    return(clamp01((rv[r1] + rv[r2]) / 28))
  }
  switch(made_hand_category(hole_cards, board),
         "straight_flush" = 0.97,
         "quads"          = 0.95,
         "full_house"     = 0.88,
         "flush"          = 0.78,
         "straight"       = 0.72,
         "trips"          = 0.65,
         "two_pair"       = 0.58,
         "pair"           = 0.45,
         0.28
  )
}

jaymon_opp_tendencies <- function(action_history, player_id) {
  if (length(action_history) == 0) return(0.4)
  opp <- Filter(function(a) !is.null(a$player_id) && a$player_id != player_id, action_history)
  if (length(opp) == 0) return(0.4)
  agg_types <- c("raise", "bet", "all_in")
  sum(sapply(opp, function(a) !is.null(a$type) && a$type %in% agg_types)) / length(opp)
}

jaymon_bet <- function(bot_input, pot_fraction) {
  if (!bot_has_action(bot_input, "bet")) return(NULL)
  mn <- bot_min_bet(bot_input); mx <- bot_max_bet(bot_input)
  if (is.null(mn) || is.null(mx)) return(NULL)
  max(mn, min(mx, round(bot_input$pot * pot_fraction)))
}

jaymon_raise <- function(bot_input, pot_fraction) {
  if (!bot_has_action(bot_input, "raise")) return(NULL)
  mn <- bot_min_raise(bot_input); mx <- bot_max_raise(bot_input)
  if (is.null(mn) || is.null(mx)) return(NULL)
  max(mn, min(mx, round(bot_input$pot * pot_fraction)))
}


############################################################
# STUDENT BOT
#
# Rename this function to your bot name.
# Example:
#   joe_bot <- function(bot_input) { ... }
############################################################

jaymon_bot <- function(bot_input) {

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

  to_call <- max(0, current_bet - committed_this_round)

  jaymon_says <- function(lines, chance = 0.14) {
    bot_maybe_say(lines, bot_input, chance)
  }

  n_active <- 1L
  if (!is.null(public_players) && length(public_players) > 0) {
    n_active <- max(1L, sum(sapply(public_players, function(p) {
      !is.null(p$player_id) && p$player_id != player_id && !isTRUE(p$folded)
    })))
  }

  opp_agg <- jaymon_opp_tendencies(action_history, player_id)
  is_short_stack <- (stack <= 8 * big_blind)

  if (street == "preflop") {

    strength <- jaymon_hand_strength(hole_cards)

    t1 <- if (opp_agg > 0.65) 11 else 9
    t2 <- if (opp_agg > 0.65) 8  else 7
    t3 <- if (opp_agg > 0.65) 6  else 5

    if (is_short_stack && strength >= t2) {
      jaymon_says(c(
        "Jaymon: Shot clock is low. Captain has to create something.",
        "Jaymon: Five-six guard, full-court pressure, stack edition.",
        "Jaymon: Manchester to Geneva, I have seen tighter lanes than this.",
        "Jaymon: Tara and I are running the quiet offense.",
        "Jaymon: Senior guard read: attack the gap.",
        "Jaymon: Small guard, clean handle, decent cards.",
        "Jaymon: Tara heard that silence. That was the play call."
      ))
      if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      return(choose_preferred_action(bot_input, c("call", "raise", "check", "fold")))
    }

    if (strength >= t1) {
      jaymon_says(c(
        "Jaymon: This hand has captain energy.",
        "Jaymon: Red Jacket taught me to take the open shot.",
        "Jaymon: Point guard read says apply pressure."
      ))
      target <- round(3 * big_blind + to_call)
      size <- jaymon_raise(bot_input, NA)
      if (bot_has_action(bot_input, "raise")) {
        mn <- bot_min_raise(bot_input); mx <- bot_max_raise(bot_input)
        size <- max(mn, min(mx, target))
        return(list(type = "raise", amount = size))
      }
      if (bot_has_action(bot_input, "bet")) {
        mn <- bot_min_bet(bot_input); mx <- bot_max_bet(bot_input)
        size <- max(mn, min(mx, round(3 * big_blind)))
        return(list(type = "bet", amount = size))
      }
      if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }

    if (strength >= t2) {
      if (to_call <= big_blind) {
        jaymon_says(c(
          "Jaymon: Good spacing. I can work with this.",
          "Jaymon: Quiet possession, useful hand.",
          "Jaymon: Math says playable. Hoop brain agrees.",
          "Jaymon: No words. Tara understands the set."
        ))
        if (bot_has_action(bot_input, "raise")) {
          mn <- bot_min_raise(bot_input); mx <- bot_max_raise(bot_input)
          size <- max(mn, min(mx, round(2.5 * big_blind)))
          return(list(type = "raise", amount = size))
        }
        if (bot_has_action(bot_input, "bet")) {
          mn <- bot_min_bet(bot_input); mx <- bot_max_bet(bot_input)
          size <- max(mn, min(mx, round(2.5 * big_blind)))
          return(list(type = "bet", amount = size))
        }
      } else if (to_call <= 4 * big_blind) {
        return(choose_preferred_action(bot_input, c("call", "check", "fold")))
      }
      return(choose_preferred_action(bot_input, c("check", "fold")))
    }

    if (strength >= t3) {
      if (to_call <= 2 * big_blind) {
        jaymon_says(c(
          "Jaymon: I will bring this up the floor and see the set.",
          "Jaymon: Not loud, just organized.",
          "Jaymon: Software engineer mindset: small call, more data."
        ))
        return(choose_preferred_action(bot_input, c("check", "call", "fold")))
      }
      return(choose_preferred_action(bot_input, c("check", "fold")))
    }

    jaymon_says(c(
      "Jaymon: That is not my shot.",
      "Jaymon: Team captain can also call timeout.",
      "Jaymon: I am too short to chase bad angles.",
      "Jaymon: Tara and I both respect a quiet pass."
    ))
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  quick_eq <- jaymon_equity(hole_cards, board)
  if (quick_eq > 0.70 || quick_eq <= 0.28) {
    equity <- quick_eq
  } else {
    equity <- jaymon_equity_estimate(hole_cards, board, n_active, n_sims = 150)
  }

  po_needed <- if (to_call > 0) pot_odds(to_call, pot) else 0

  curr_spr <- if (pot > 0) stack / pot else Inf

  board_df <- data.frame(rank = extract_rank_from_label(board), suit = extract_suit_from_label(board), stringsAsFactors = FALSE)
  bf <- if (nrow(board_df) >= 3) board_features(board_df) else NULL

  risk <- 0
  if (!is.null(bf)) {
    if (isTRUE(bf$monotone))  risk <- risk + 0.1
    if (isTRUE(bf$paired))    risk <- risk + 0.06
    if (bf$connectivity >= 2) risk <- risk + 0.06
  }
  eff_eq <- equity - risk * (1 - equity)

  streets_left <- switch(street, "flop" = 2L, "turn" = 1L, "river" = 0L, 0L)

  implied_threshold <- if (streets_left > 0 && to_call > 0)
    pot_odds(to_call, pot + to_call * 2.5) else po_needed

  should_call <- eff_eq >= po_needed || (eff_eq >= implied_threshold && equity >= 0.28)

  if (is_short_stack && eff_eq >= 0.45) {
    jaymon_says(c(
      "Jaymon: Late-game possession. I know the assignment.",
      "Jaymon: This is where the senior guard settles the offense.",
      "Jaymon: Tiny guard, big decision."
    ))
    if (bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
  }

  if (eff_eq >= 0.62) {
    jaymon_says(c(
      "Jaymon: The model found a clean look.",
      "Jaymon: I like this line. Efficient offense.",
      "Jaymon: Math major says value. Guard says finish."
    ))
    if (to_call > 0) {
      if (bot_has_action(bot_input, "raise")) {
        frac <- if (curr_spr < 3) 1.0 else 0.8
        size <- jaymon_raise(bot_input, frac)
        if (!is.null(size)) return(list(type = "raise", amount = size))
      }
      if (should_call) return(choose_preferred_action(bot_input, c("call", "all_in")))
      return(choose_preferred_action(bot_input, c("call", "fold")))
    } else {
      frac <- if (!is.null(bf) && (isTRUE(bf$monotone) || isTRUE(bf$paired))) 0.75 else 0.65
      if (curr_spr < 2 && bot_has_action(bot_input, "all_in")) return(list(type = "all_in"))
      size <- jaymon_bet(bot_input, frac)
      if (!is.null(size)) return(list(type = "bet", amount = size))
      return(choose_preferred_action(bot_input, c("check", "call")))
    }
  }

  if (eff_eq >= 0.47) {
    jaymon_says(c(
      "Jaymon: This is a midrange jumper, not a dunk.",
      "Jaymon: Keep the dribble alive. Do not force it.",
      "Jaymon: Computer science says iterate, not panic."
    ))
    if (to_call > 0) {
      if (should_call) return(choose_preferred_action(bot_input, c("call", "check", "fold")))
      return(choose_preferred_action(bot_input, c("check", "fold")))
    } else {
      if (equity >= 0.65) {
        size <- jaymon_bet(bot_input, 0.50)
        if (!is.null(size)) return(list(type = "bet", amount = size))
      }
      return(choose_preferred_action(bot_input, c("check", "call", "fold")))
    }
  }

  if (eff_eq >= 0.30 && streets_left > 0) {
    jaymon_says(c(
      "Jaymon: Development branch. Still testing.",
      "Jaymon: I will keep this possession quiet.",
      "Jaymon: There might be an assist hiding in this hand."
    ), chance = 0.10)
    if (to_call > 0) {
      if (should_call) return(choose_preferred_action(bot_input, c("call", "check", "fold")))
      return(choose_preferred_action(bot_input, c("check", "fold")))
    } else {
      if (equity >= 0.36 && runif(1) < 0.35) {
        size <- jaymon_bet(bot_input, 0.40)
        if (!is.null(size)) return(list(type = "bet", amount = size))
      }
      return(choose_preferred_action(bot_input, c("check", "fold")))
    }
  }

  if (to_call > 0) {
    if (should_call && to_call <= big_blind) {
      jaymon_says(c(
        "Jaymon: Cheap enough to see one more.",
        "Jaymon: I can defend this possession.",
        "Jaymon: Small price, controlled tempo."
      ), chance = 0.10)
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }
    jaymon_says(c(
      "Jaymon: Bad shot selection. Pass.",
      "Jaymon: I have standards for contested looks.",
      "Jaymon: That lane closed fast."
    ), chance = 0.10)
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }

  jaymon_says(c(
      "Jaymon: Quiet check. Captain voice.",
      "Jaymon: No need to force the offense.",
      "Jaymon: I will let the possession breathe.",
      "Jaymon: Tara gets it. Sometimes silence is the whole play.",
      "Jaymon: Quiet set, good spacing, no panic.",
      "Jaymon: Software engineer brain says wait for better input.",
      "Jaymon: Tara and I just ran a whole conversation off-ball."
  ), chance = 0.10)
  return(choose_preferred_action(bot_input, c("check", "fold")))
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
bot_input_example <- build_bot_input(tourn)
str(bot_input_example)
print(bot_input_example)
#
# Then try:#
jaymon_bot(bot_input_example)
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
 action <- jaymon_bot(bot_input_example)
 print(action)
#
# Expected formats include:
   list(type = "fold")
   list(type = "check")
   list(type = "call")
   list(type = "all_in")
   list(type = "bet", amount = x)
   list(type = "raise", amount = x)
#

   source("poker_load_all.R")
   poker_load_all(include_demos = TRUE, verbose = FALSE)
   demo_result <- run_tournament(
     bot_fns = list(
       jaymon_bot,
       random_bot,
       always_call_bot,
       passive_bot,
       aggressive_bot
     ),
     player_names = c(
       "My Bot",
       "Random Bot",
       "Caller Bot",
       "Passive Bot",
       "Aggro Bot"
     ),
     starting_stack = 5000,
     tournament_id = "LAB_BOT_DEMO",
     rng_seed = 39,
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
