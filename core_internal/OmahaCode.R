# omaha related code
omaha_hand_value <- function(hole_df, board_df) {
  stopifnot(is.data.frame(hole_df), is.data.frame(board_df))
  if (nrow(hole_df) != 4) stop("hole_df must have exactly 4 cards for Omaha.")
  if (!(nrow(board_df) %in% c(3, 4, 5))) stop("board_df must have 3, 4, or 5 cards.")

  all_cards <- rbind(hole_df, board_df)
  ids <- paste0(all_cards$rank, all_cards$suit)
  if (anyDuplicated(ids)) stop("Duplicate card detected in inputs.")

  hole_pairs <- combn(nrow(hole_df), 2)
  board_trips <- combn(nrow(board_df), 3)

  best_score <- -Inf
  best_hand <- NULL
  best_hole <- NULL
  best_board <- NULL

  for (i in seq_len(ncol(hole_pairs))) {
    h2 <- hole_df[hole_pairs[, i], , drop = FALSE]
    for (j in seq_len(ncol(board_trips))) {
      b3 <- board_df[board_trips[, j], , drop = FALSE]
      h5 <- rbind(h2, b3)
      sc <- hand_value_5(h5)
      if (sc > best_score) {
        best_score <- sc
        best_hand <- h5
        best_hole <- h2
        best_board <- b3
      }
    }
  }

  list(score = best_score, best_hand = best_hand, best_hole = best_hole, best_board = best_board)
}

omaha_best_score_ids <- function(hole_ids, board_ids) {
  if (length(hole_ids) != 4) stop("Omaha hole_ids must have length 4.")
  if (!(length(board_ids) %in% c(3, 4, 5))) stop("board_ids must have length 3, 4, or 5.")
  ids <- c(hole_ids, board_ids)
  if (anyDuplicated(ids)) stop("Duplicate card detected in Omaha input.")

  h_idx <- combn(4, 2)
  b_idx <- combn(length(board_ids), 3)
  best_score <- -Inf
  best_ids <- NULL

  for (i in seq_len(ncol(h_idx))) {
    for (j in seq_len(ncol(b_idx))) {
      cand <- c(hole_ids[h_idx[, i]], board_ids[b_idx[, j]])
      sc <- hand_value_5_ids(cand)
      if (sc > best_score) {
        best_score <- sc
        best_ids <- cand
      }
    }
  }

  list(score = best_score, best_ids = best_ids)
}

omaha_equity_mc_fast <- function(
    hole_list,
    board_df = data.frame(rank = character(), suit = character()),
    n_sims = 10000
) {
  if (!is.list(hole_list) || length(hole_list) < 2) {
    stop("hole_list must be a list of at least two player hands.")
  }
  if (!is.numeric(n_sims) || length(n_sims) != 1 || is.na(n_sims) || n_sims <= 0) {
    stop("n_sims must be a positive integer.")
  }
  n_sims <- as.integer(n_sims)

  hole_ids_list <- lapply(hole_list, df_to_ids)

  for (i in seq_along(hole_ids_list)) {
    if (length(hole_ids_list[[i]]) != 4) {
      stop(sprintf("Player %d must have exactly four hole cards in Omaha.", i))
    }
    if (anyDuplicated(hole_ids_list[[i]])) {
      stop(sprintf("Player %d has duplicate hole cards.", i))
    }
  }

  board_ids_known <- if (nrow(board_df) == 0) integer(0) else df_to_ids(board_df)

  if (length(board_ids_known) > 5) {
    stop("board_df cannot contain more than 5 cards.")
  }
  if (anyDuplicated(board_ids_known)) {
    stop("Duplicate cards detected in board_df.")
  }

  used_ids <- c(unlist(hole_ids_list, use.names = FALSE), board_ids_known)
  if (anyDuplicated(used_ids)) {
    stop("Duplicate known cards detected in equity input.")
  }

  full_deck <- seq_len(52)
  unseen <- setdiff(full_deck, used_ids)
  n_board_needed <- 5 - length(board_ids_known)
  n_players <- length(hole_ids_list)
  wins <- numeric(n_players)

  for (sim in seq_len(n_sims)) {
    drawn <- if (n_board_needed > 0) {
      sample(unseen, n_board_needed, replace = FALSE)
    } else {
      integer(0)
    }

    board_ids <- c(board_ids_known, drawn)

    scores <- vapply(
      hole_ids_list,
      function(h) omaha_best_score_ids(h, board_ids)$score,
      numeric(1)
    )

    winners <- which(scores == max(scores))
    wins[winners] <- wins[winners] + 1 / length(winners)
  }

  data.frame(
    player = seq_len(n_players),
    equity = wins / n_sims
  )
}

new_range_omaha <- function(combos_df, label = NULL, normalize = TRUE) {
  needed <- c("c1", "c2", "c3", "c4", "w")
  if (!all(needed %in% names(combos_df))) stop("Omaha range needs columns c1, c2, c3, c4, w.")
  out <- list(game = "omaha", combos = combos_df, weights = combos_df$w, label = label)
  class(out) <- "poker_range"
  if (normalize) out <- range_normalize(out)
  out
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
