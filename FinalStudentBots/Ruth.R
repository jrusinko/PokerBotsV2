ruth_bot <- function(bot_input) {
  
  ##### --- 1. DATA EXTRACTION --- #####
  # Pulling current game state from the engine's input object
  seat <- bot_input$seat
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  pot <- bot_input$pot
  current_bet <- bot_input$current_bet
  committed_this_round <- bot_input$committed_this_round
  stack <- bot_input$stack
  legal_types <- bot_input$legal_actions$legal_action_types
  public_players <- bot_input$public_players
  
  ##### --- 2. DYNAMIC EQUITY CALCULATOR --- #####
  # Estimates our winning probability based on hand rank, board height, and draws.
  calculate_dynamic_equity <- function() {
    category <- made_hand_category(hole_cards, board)
    
    # Baseline win-rate estimates for various hand categories
    base_eq <- switch(category,
                      "straight_flush" = 0.99, "quads" = 0.95, "full_house" = 0.90,
                      "flush" = 0.85, "straight" = 0.80, "trips" = 0.75,
                      "two_pair" = 0.65, "pair" = 0.50, "high_card" = 0.15, 0.10
    )
    
    # REFINEMENT: Not all pairs are equal.
    # Checks if our pair is an Overpair (above the board), Top Pair, or Bottom Pair.
    if (category == "pair" && length(board) >= 1) {
      hole_vals <- hole_rank_values(hole_cards)
      board_vals <- rank_to_numeric(substring(board, 1, 1))
      max_board <- max(board_vals)
      my_pair_val <- ifelse(hole_vals[1] == hole_vals[2], hole_vals[1], 
                            hole_vals[hole_vals %in% board_vals][1])
      
      if (is_na(my_pair_val)) my_pair_val <- 0
      
      if (my_pair_val > max_board) base_eq <- 0.75      # Very strong: Pocket pair higher than board
      else if (my_pair_val == max_board) base_eq <- 0.60 # Strong: Pair matches highest board card
      else base_eq <- 0.40                               # Vulnerable: Mid/Bottom pair
    }
    
    # DRAW DETECTION: Implementation of the 'Rule of 2 and 4'
    # Estimates chances of hitting a flush on future streets.
    multiplier <- ifelse(street == "flop", 4, 2)
    if (length(board) >= 3) {
      suits <- c(substring(hole_cards, 2, 2), substring(board, 2, 2))
      # If 4 cards of the same suit are present (hole + board), we have a Flush Draw (9 outs)
      if (any(table(suits) == 4)) base_eq <- max(base_eq, (9 * multiplier) / 100)
    }
    
    return(base_eq)
  }
  
  ##### --- 3. HELPER FUNCTIONS --- #####
  
  # THE SAFETY GATE: Crucial for ensuring all bot actions are engine-legal.
  # Rounds values and clamps them between the current min and max legal limits.
  sanitize_action <- function(action_list) {
    type <- action_list$type
    if (type %in% c("bet", "raise")) {
      min_amt <- if(type == "bet") bot_min_bet(bot_input) else bot_min_raise(bot_input)
      max_amt <- if(type == "bet") bot_max_bet(bot_input) else bot_max_raise(bot_input)
      amount <- round(action_list$amount)
      amount <- max(min_amt, min(max_amt, amount))
      return(list(type = type, amount = amount))
    }
    # Fallback to a check/fold preference if an illegal action type is suggested
    if (!(type %in% legal_types)) return(choose_preferred_action(bot_input, c("check", "fold")))
    return(action_list)
  }
  
  # EXPLOITATIVE SIZING: Varies bet amounts to maximize value against different bot types.
  bet_mult <- function(strength = "medium") {
    if (strength == "strong") return(runif(1, 0.9, 1.3)) # Push max value with big hands
    return(runif(1, 0.4, 0.6)) # Standard sizing for mid-strength hands
  }
  
  ##### --- 4. CORE ENGINE & OPPONENT MODELING --- #####
  # Calculate Pot Odds: The cost to call relative to the total pot size.
  to_call <- current_bet - committed_this_round
  pot_odds <- ifelse(to_call > 0, to_call / (pot + to_call), 0)
  
  # Detect 'Maniac' behavior: High average commitment suggests an aggressive/bluffing table.
  avg_commit <- sum(sapply(public_players, function(p) p$committed_this_hand)) / length(public_players)
  is_maniac_table <- avg_commit > (pot * 0.7)
  
  equity <- calculate_dynamic_equity()
  
  # STRATEGY ADJUSTMENT: If the table is wild (maniacs), our 'relative' equity increases
  # because their betting range is much wider/weaker.
  if (is_maniac_table) equity <- equity + 0.12 
  
  ##### --- 5. STREET LOGIC --- #####
  
  # --- PREFLOP ---
  if (identical(street, "preflop")) {
    vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
    # Define 'Strong' as High Pairs, AK/AQ, or Big Broadway cards.
    strong_hand <- (length(unique(vals)) == 1) || (min(vals) >= 12) || (vals[1] == 14 && vals[2] >= 10)
    
    if (strong_hand && "raise" %in% legal_types) {
      return(sanitize_action(list(type = "raise", amount = pot * 0.8)))
    }
    # Defense: Call small raises if we have high-card strength (Jacks or better)
    if (to_call > 0 && pot_odds < 0.3 && vals[1] >= 11) return(list(type = "call"))
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }
  
  # --- POSTFLOP ---
  category <- made_hand_category(hole_cards, board)
  strong_made <- category %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  
  # STRATEGY A: High Value (The Nuts)
  # When holding a strong made hand, we play aggressively to extract value from callers.
  if (strong_made) {
    size <- pot * bet_mult("strong")
    if ("raise" %in% legal_types) return(sanitize_action(list(type = "raise", amount = size)))
    if ("bet" %in% legal_types) return(sanitize_action(list(type = "bet", amount = size)))
    return(list(type = "call"))
  }
  
  # STRATEGY B: Facing a Bet (Defensive)
  if (to_call > 0) {
    # RISK MANAGEMENT: Tighten up against near all-in bets to preserve tournament life.
    if ((to_call >= stack * 0.8) && equity < 0.7) return(list(type = "fold"))
    
    # POKER MATH: Call if our estimated win probability (equity) exceeds the pot odds.
    if (pot_odds < equity) return(list(type = "call"))
    return(list(type = "fold"))
  }
  
  # STRATEGY C: Leading Out (Uncontested Pots)
  if (to_call == 0 && "bet" %in% legal_types) {
    bluff_roll <- runif(1)
    # Value bet with pairs 50% of the time to build the pot.
    if (category == "pair" && bluff_roll < 0.5) {
      return(sanitize_action(list(type = "bet", amount = pot * bet_mult("medium"))))
    }
    # RESTRAINED BLUFFING: Only 5% frequency to maintain a tight table image.
    # Never bluff into maniacs who are likely to re-raise.
    if (category == "high_card" && bluff_roll < 0.05 && !is_maniac_table) {
      return(sanitize_action(list(type = "bet", amount = pot * 0.4)))
    }
  }
  
  ##### --- 6. DEFAULT ACTION --- #####
  # Default to check if possible, otherwise fold.
  return(choose_preferred_action(bot_input, c("check", "fold")))
}