#omaha related code

print_holdem_hand <- function(showdown) {
  cat("\nPLAYERS:\n")
  for (i in seq_len(showdown$n_players)) {
    hc <- showdown$hole[[i]]
    cat(sprintf("Player %d hole: %s %s\n", i, hc$card[1], hc$card[2]))
  }

  cat("\nBOARD:\n")
  board_cards <- showdown$board$card
  board_labels <- c("Flop 1", "Flop 2", "Flop 3", "Turn", "River")[seq_along(board_cards)]
  board_out <- data.frame(
    street = board_labels,
    card = board_cards,
    stringsAsFactors = FALSE
  )
  rownames(board_out) <- NULL
  print(board_out, row.names = FALSE)

  cat("\nRESULTS:\n")
  for (i in seq_len(showdown$n_players)) {
    r <- showdown$results[[i]]
    cat(sprintf(
      "Player %d score: %s | best hand: %s\n",
      i, r$score, paste(r$best_hand$card, collapse = " ")
    ))
  }

  cat("\nWINNER(S): ", paste(showdown$winners, collapse = ", "), "\n", sep = "")
  invisible(showdown)
}

deal_omaha <- function(n_players) {
  if (!is.numeric(n_players) || length(n_players) != 1) stop("n_players must be a single number.")
  if (n_players < 2 || n_players > 10) stop("n_players must be between 2 and 10.")

  deck <- shuffle_deck(create_deck())
  dealt_hole <- deal_cards(deck, 4 * n_players)
  hole_cards_all <- dealt_hole$dealt
  deck <- dealt_hole$remaining

  hole_list <- split(hole_cards_all, rep(seq_len(n_players), each = 4))
  hole_list <- lapply(hole_list, function(df) {
    df <- as.data.frame(df)
    rownames(df) <- NULL
    df
  })

  dealt_flop <- deal_cards(deck, 3); flop <- dealt_flop$dealt; deck <- dealt_flop$remaining
  dealt_turn <- deal_cards(deck, 1); turn <- dealt_turn$dealt; deck <- dealt_turn$remaining
  dealt_river <- deal_cards(deck, 1); river <- dealt_river$dealt; deck <- dealt_river$remaining
  board <- rbind(flop, turn, river)
  rownames(board) <- NULL

  list(n_players = n_players, hole = hole_list, board = board, deck_remaining = deck)
}

evaluate_omaha_showdown <- function(deal) {
  stopifnot(is.list(deal), !is.null(deal$hole), !is.null(deal$board))
  n_players <- length(deal$hole)
  board <- deal$board
  results <- vector("list", n_players)

  for (i in seq_len(n_players)) {
    hv <- omaha_hand_value(deal$hole[[i]], board)
    results[[i]] <- list(
      player = i,
      score = hv$score,
      best_hand = hv$best_hand,
      best_hole = hv$best_hole,
      best_board = hv$best_board
    )
  }

  scores <- vapply(results, function(x) x$score, numeric(1))
  max_score <- max(scores)
  winners <- which(scores == max_score)

  list(
    n_players = n_players,
    board = board,
    results = results,
    winners = winners,
    winning_score = max_score,
    winning_hands = lapply(winners, function(i) results[[i]]$best_hand)
  )
}

play_omaha_hand <- function(n_players = 2) {
  deal <- deal_omaha(n_players)
  showdown <- evaluate_omaha_showdown(deal)
  showdown$hole <- deal$hole
  showdown
}

print_omaha_hand <- function(showdown) {
  cat("\nBOARD:\n")
  print(showdown$board)
  cat("\nPLAYERS:\n")
  for (i in seq_len(showdown$n_players)) {
    hc <- showdown$hole[[i]]
    cat(sprintf("Player %d hole: %s\n", i, paste(hc$card, collapse = " ")))
  }
  cat("\nRESULTS:\n")
  for (i in seq_len(showdown$n_players)) {
    r <- showdown$results[[i]]
    cat(sprintf("Player %d score: %s | best hand: %s\n", i, r$score, paste(r$best_hand$card, collapse = " ")))
    cat(sprintf("          uses hole: %s | board: %s\n",
                paste(r$best_hole$card, collapse = " "),
                paste(r$best_board$card, collapse = " ")))
  }
  cat("\nWINNER(S): ", paste(showdown$winners, collapse = ", "), "\n", sep = "")
  invisible(showdown)
}
