ruth_bot <- function(bot_input) {
  
  ##### --- Basic info --- #####
  
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
  
  ##### --- Core calculations --- #####
  
  to_call <- current_bet - committed_this_round
  pot_odds <- ifelse(to_call > 0, to_call / (pot + to_call), 0)
  
  num_players <- length(public_players)
  is_late <- seat >= (num_players - 1)
  
  ##### --- Opponent modeling --- #####
  
  total_committed <- sum(sapply(public_players, function(p) p$committed_this_hand))
  avg_commit <- total_committed / max(1, num_players)
  
  passive_table <- avg_commit < pot * 0.3
  aggressive_table <- avg_commit > pot * 0.7
  
  ##### --- Stack awareness --- #####
  
  avg_stack <- mean(sapply(public_players, function(p) p$stack))
  short_stack <- stack < avg_stack * 0.5
  big_stack <- stack > avg_stack * 1.5
  
  ##### --- Helpers --- #####
  
  sanitize_amount <- function(amount, type) {
    amount <- round(amount)
    
    if (type == "bet") {
      min_amt <- bot_min_bet(bot_input)
      max_amt <- bot_max_bet(bot_input)
    } else if (type == "raise") {
      min_amt <- bot_min_raise(bot_input)
      max_amt <- bot_max_raise(bot_input)
    } else {
      return(NULL)
    }
    
    amount <- max(min_amt, amount)
    amount <- min(max_amt, amount)
    
    return(amount)
  }
  
  bet_mult <- function(strength = "medium") {
    if (strength == "strong") {
      return(runif(1, 0.6, 1.0))
    } else {
      return(runif(1, 0.4, 0.7))
    }
  }
  
  ##### --- Preflop --- #####
  
  if (identical(street, "preflop") && length(hole_cards) == 2) {
    
    vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
    
    paired <- length(unique(vals)) == 1
    strong_broadway <- min(vals) >= 12
    strong_ace <- max(vals) == 14 && min(vals) >= 10
    
    strong_hand <- paired || strong_broadway || strong_ace
    
    # SHORT STACK: shove wider
    if (short_stack && strong_hand && "all_in" %in% legal_types) {
      return(list(type = "all_in"))
    }
    
    # Raise strong hands
    if (strong_hand && "raise" %in% legal_types) {
      raw_amt <- pot * 0.6
      amt <- sanitize_amount(raw_amt, "raise")
      return(list(type = "raise", amount = amt))
    }
    
    # Late position steal (more aggressive if passive table)
    if (is_late && to_call == 0 && "raise" %in% legal_types) {
      steal_prob <- ifelse(passive_table, 0.7, 0.4)
      
      if (runif(1) < steal_prob) {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
    }
    
    # Call small bets
    if (to_call > 0 && to_call <= pot * 0.25) {
      return(list(type = "call"))
    }
    
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }
  
  ##### --- Postflop --- #####
  
  category <- made_hand_category(hole_cards, board)
  
  ##### --- Board texture --- #####
  
  flush_draw <- FALSE
  connected <- FALSE
  
  if (length(board) >= 3) {
    suits <- substring(board, 2, 2)
    flush_draw <- any(table(suits) >= 2)
    
    vals <- sort(rank_to_numeric(substring(board, 1, 1)))
    connected <- (max(vals) - min(vals) <= 4)
  }
  
  draw_board <- flush_draw || connected
  
  strong_hands <- c("two_pair", "trips", "straight", "flush",
                    "full_house", "quads", "straight_flush")
  
  ##### --- Strong hands --- #####
  
  if (category %in% strong_hands) {
    
    # Big stack = apply pressure
    if (big_stack && "raise" %in% legal_types && runif(1) < 0.5) {
      raw_amt <- pot * 1.0
      amt <- sanitize_amount(raw_amt, "raise")
      return(list(type = "raise", amount = amt))
    }
    
    if ("raise" %in% legal_types) {
      raw_amt <- pot * bet_mult("strong")
      amt <- sanitize_amount(raw_amt, "raise")
      return(list(type = "raise", amount = amt))
    }
    
    if ("bet" %in% legal_types) {
      raw_amt <- pot * bet_mult("strong")
      amt <- sanitize_amount(raw_amt, "bet")
      return(list(type = "bet", amount = amt))
    }
    
    if ("all_in" %in% legal_types) {
      return(list(type = "all_in"))
    }
  }
  
  ##### --- Pair --- #####
  
  if (category == "pair") {
    
    equity_estimate <- ifelse(street == "flop", 0.5,
                              ifelse(street == "turn", 0.55, 0.6))
    
    # Against aggressive players: tighten up
    if (aggressive_table) {
      equity_estimate <- equity_estimate + 0.1
    }
    
    if (to_call > 0 && pot_odds < equity_estimate) {
      return(list(type = "call"))
    }
    
    if (to_call == 0 && "bet" %in% legal_types && runif(1) < 0.6) {
      raw_amt <- pot * bet_mult("medium")
      amt <- sanitize_amount(raw_amt, "bet")
      return(list(type = "bet", amount = amt))
    }
    
    return(choose_preferred_action(bot_input, c("check", "fold")))
  }
  
  ##### --- Bluffing (adaptive) --- #####
  
  if (category == "high_card" && to_call == 0 && "bet" %in% legal_types) {
    
    bluff_prob <- 0.15
    
    if (passive_table) bluff_prob <- bluff_prob + 0.2
    if (draw_board) bluff_prob <- bluff_prob - 0.1
    
    if (runif(1) < bluff_prob) {
      raw_amt <- pot * 0.5
      amt <- sanitize_amount(raw_amt, "bet")
      return(list(type = "bet", amount = amt))
    }
  }
  
  ##### --- Pot-odds defense --- #####
  
  if (to_call > 0) {
    
    equity_estimate <- ifelse(category == "high_card", 0.2,
                              ifelse(category == "pair", 0.5, 0.75))
    
    if (aggressive_table) {
      equity_estimate <- equity_estimate + 0.05
    }
    
    if (pot_odds < equity_estimate) {
      return(list(type = "call"))
    } else {
      return(list(type = "fold"))
    }
  }
  
  ##### --- Default --- #####
  
  return(list(type = "check"))
}
