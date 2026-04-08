############################################################
# Mathematics of Poker — Cards and Hand Evaluation
# File: cards_and_hands.R
#
# Purpose:
#   Foundational card, deck, and hand-evaluation utilities.
#   This file consolidates the low-level functionality that was
#   previously split across poker_engine_base.R and poker_equity.R.
############################################################

############################################################
# 1. CARD DEFINITIONS
############################################################

ranks <- c("2", "3", "4", "5", "6", "7", "8", "9",
           "T", "J", "Q", "K", "A")

suits <- c("h", "s", "c", "d")

create_deck <- function() {
  deck <- expand.grid(rank = ranks, suit = suits, stringsAsFactors = FALSE)
  deck$card <- paste0(deck$rank, deck$suit)
  deck
}

shuffle_deck <- function(deck = create_deck()) {
  deck[sample(nrow(deck)), , drop = FALSE]
}

deal_cards <- function(deck, n) {
  if (!is.data.frame(deck)) stop("deck must be a data.frame.")
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0) {
    stop("n must be a single nonnegative number.")
  }
  n <- as.integer(n)
  if (n > nrow(deck)) stop("Cannot deal more cards than remain in the deck.")

  list(
    dealt = deck[seq_len(n), , drop = FALSE],
    remaining = if (n == nrow(deck)) deck[0, , drop = FALSE] else deck[(n + 1):nrow(deck), , drop = FALSE]
  )
}

############################################################
# 2. 5-CARD HAND UTILITIES (data.frame version)
############################################################

rank_to_numeric <- function(rank) {
  match(rank, ranks)
}

rank_value <- function(rank) {
  match(rank, ranks) + 1L
}

rank_counts <- function(hand_df) {
  sort(table(hand_df$rank), decreasing = TRUE)
}

is_pair <- function(hand_df) {
  counts <- rank_counts(hand_df)
  any(counts == 2)
}

is_two_pair <- function(hand_df) {
  counts <- rank_counts(hand_df)
  sum(counts == 2) == 2
}

is_three_of_a_kind <- function(hand_df) {
  counts <- rank_counts(hand_df)
  any(counts == 3) && !is_full_house(hand_df)
}

is_full_house <- function(hand_df) {
  counts <- rank_counts(hand_df)
  any(counts == 3) && any(counts == 2)
}

is_flush <- function(hand_df) {
  length(unique(hand_df$suit)) == 1
}

is_straight <- function(hand_df) {
  vals <- sort(rank_to_numeric(hand_df$rank))
  if (all(diff(vals) == 1)) return(TRUE)
  identical(sort(hand_df$rank), c("2", "3", "4", "5", "A"))
}

is_straight_flush <- function(hand_df) {
  is_straight(hand_df) && is_flush(hand_df)
}

is_four_of_a_kind <- function(hand_df) {
  counts <- rank_counts(hand_df)
  any(counts == 4)
}

encode_tiebreak <- function(v) {
  stopifnot(length(v) == 5)
  base <- 15
  sum(v * base^(4:0))
}

straight_high_card <- function(hand_df) {
  vals <- sort(rank_value(hand_df$rank))
  if (identical(vals, c(2, 3, 4, 5, 14))) return(5)
  max(vals)
}

hand_category_code_5 <- function(hand_df) {
  if (is_straight_flush(hand_df)) return(8L)
  if (is_four_of_a_kind(hand_df)) return(7L)
  if (is_full_house(hand_df)) return(6L)
  if (is_flush(hand_df)) return(5L)
  if (is_straight(hand_df)) return(4L)
  if (is_three_of_a_kind(hand_df)) return(3L)
  if (is_two_pair(hand_df)) return(2L)
  if (is_pair(hand_df)) return(1L)
  0L
}

tiebreak_vector_5 <- function(hand_df) {
  code <- hand_category_code_5(hand_df)
  vals <- rank_value(hand_df$rank)
  counts <- table(vals)

  dec <- function(x) sort(x, decreasing = TRUE)

  if (code %in% c(8L, 4L)) {
    h <- straight_high_card(hand_df)
    return(c(h, 0, 0, 0, 0))
  }

  if (code %in% c(5L, 0L)) {
    return(c(dec(vals), rep(0, 5 - length(vals)))[1:5])
  }

  if (code == 7L) {
    quad_rank <- as.integer(names(counts)[counts == 4])
    kicker <- as.integer(names(counts)[counts == 1])
    return(c(quad_rank, kicker, 0, 0, 0))
  }

  if (code == 6L) {
    trips_rank <- as.integer(names(counts)[counts == 3])
    pair_rank <- as.integer(names(counts)[counts == 2])
    return(c(trips_rank, pair_rank, 0, 0, 0))
  }

  if (code == 3L) {
    trips_rank <- as.integer(names(counts)[counts == 3])
    kickers <- dec(as.integer(names(counts)[counts == 1]))
    return(c(trips_rank, kickers, 0, 0)[1:5])
  }

  if (code == 2L) {
    pair_ranks <- dec(as.integer(names(counts)[counts == 2]))
    kicker <- as.integer(names(counts)[counts == 1])
    return(c(pair_ranks, kicker, 0, 0)[1:5])
  }

  if (code == 1L) {
    pair_rank <- as.integer(names(counts)[counts == 2])
    kickers <- dec(as.integer(names(counts)[counts == 1]))
    return(c(pair_rank, kickers, 0)[1:5])
  }

  stop("Unexpected hand category.")
}

hand_value_5 <- function(hand_df) {
  stopifnot(is.data.frame(hand_df))
  stopifnot(all(c("rank", "suit") %in% names(hand_df)))
  stopifnot(nrow(hand_df) == 5)

  code <- hand_category_code_5(hand_df)
  tie <- tiebreak_vector_5(hand_df)
  sep <- 15^5
  code * sep + encode_tiebreak(tie)
}

compare_hands_5 <- function(hand1_df, hand2_df) {
  v1 <- hand_value_5(hand1_df)
  v2 <- hand_value_5(hand2_df)
  if (v1 > v2) return(1L)
  if (v1 < v2) return(-1L)
  0L
}

best_hand_from_cards <- function(cards_df) {
  stopifnot(is.data.frame(cards_df))
  stopifnot(all(c("rank", "suit") %in% names(cards_df)))
  n <- nrow(cards_df)
  if (n < 5) stop("Need at least 5 available cards to score a 5-card hand.")

  if (n == 5) {
    sc <- hand_value_5(cards_df)
    return(list(score = sc, best_hand = cards_df, n_available = n))
  }

  idx_mat <- combn(n, 5)
  best_score <- -Inf
  best_idx <- NULL

  for (j in seq_len(ncol(idx_mat))) {
    idx <- idx_mat[, j]
    h5 <- cards_df[idx, , drop = FALSE]
    sc <- hand_value_5(h5)
    if (sc > best_score) {
      best_score <- sc
      best_idx <- idx
    }
  }

  list(score = best_score, best_hand = cards_df[best_idx, , drop = FALSE], n_available = n)
}

holdem_hand_value <- function(hole_df, board_df) {
  stopifnot(is.data.frame(hole_df), is.data.frame(board_df))
  if (nrow(hole_df) != 2) stop("hole_df must have exactly 2 cards.")
  if (!(nrow(board_df) %in% c(3, 4, 5))) stop("board_df must have 3, 4, or 5 cards.")

  cards <- rbind(hole_df, board_df)
  card_ids <- paste0(cards$rank, cards$suit)
  if (anyDuplicated(card_ids)) stop("Duplicate card detected in inputs.")

  best_hand_from_cards(cards)
}

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

############################################################
# 3. FAST INTEGER-BASED HAND UTILITIES
############################################################

.card_ranks <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
.card_suits <- c("h", "s", "c", "d")

card_to_id <- function(rank, suit) {
  r <- match(rank, .card_ranks)
  s <- match(suit, .card_suits)
  if (any(is.na(r)) || any(is.na(s))) stop("Invalid rank or suit in card_to_id().")
  (r - 1L) * 4L + s
}

df_to_ids <- function(df) {
  stopifnot(is.data.frame(df), all(c("rank", "suit") %in% names(df)))
  card_to_id(df$rank, df$suit)
}

.id_rank <- rep(seq_len(13), each = 4)
.id_suit <- rep(seq_len(4), times = 13)
.id_rank_val <- .id_rank + 1L

.encode_tiebreak_fast <- function(v5) {
  15^4 * v5[1] + 15^3 * v5[2] + 15^2 * v5[3] + 15 * v5[4] + v5[5]
}

.straight_high_fast <- function(vals_sorted_unique) {
  if (identical(vals_sorted_unique, c(2L, 3L, 4L, 5L, 14L))) return(5L)
  max(vals_sorted_unique)
}

hand_value_5_ids <- function(ids5) {
  stopifnot(length(ids5) == 5)
  if (anyDuplicated(ids5)) stop("Duplicate card in 5-card hand.")

  rv <- .id_rank_val[ids5]
  sv <- .id_suit[ids5]
  cnt <- tabulate(rv, nbins = 14)
  nz <- which(cnt > 0)
  mult <- cnt[nz]
  ranks_nz <- nz

  is_flush_fast <- length(unique(sv)) == 1
  is_straight_fast <- FALSE
  straight_high <- 0L
  if (length(ranks_nz) == 5) {
    r_sorted <- sort(ranks_nz)
    if (all(diff(r_sorted) == 1L) || identical(r_sorted, c(2L, 3L, 4L, 5L, 14L))) {
      is_straight_fast <- TRUE
      straight_high <- .straight_high_fast(r_sorted)
    }
  }

  code <- 0L
  tie <- c(0L, 0L, 0L, 0L, 0L)

  if (is_straight_fast && is_flush_fast) {
    code <- 8L
    tie <- c(straight_high, 0L, 0L, 0L, 0L)
  } else if (any(mult == 4L)) {
    code <- 7L
    quad_rank <- ranks_nz[mult == 4L][1]
    kicker <- ranks_nz[mult == 1L][1]
    tie <- c(quad_rank, kicker, 0L, 0L, 0L)
  } else if (any(mult == 3L) && any(mult == 2L)) {
    code <- 6L
    trips_rank <- ranks_nz[mult == 3L][1]
    pair_rank <- ranks_nz[mult == 2L][1]
    tie <- c(trips_rank, pair_rank, 0L, 0L, 0L)
  } else if (is_flush_fast) {
    code <- 5L
    tie <- sort(rv, decreasing = TRUE)
  } else if (is_straight_fast) {
    code <- 4L
    tie <- c(straight_high, 0L, 0L, 0L, 0L)
  } else if (any(mult == 3L)) {
    code <- 3L
    trips_rank <- ranks_nz[mult == 3L][1]
    kickers <- sort(ranks_nz[mult == 1L], decreasing = TRUE)
    tie <- c(trips_rank, kickers, 0L, 0L)[1:5]
  } else if (sum(mult == 2L) == 2L) {
    code <- 2L
    pair_ranks <- sort(ranks_nz[mult == 2L], decreasing = TRUE)
    kicker <- ranks_nz[mult == 1L][1]
    tie <- c(pair_ranks, kicker, 0L, 0L)[1:5]
  } else if (any(mult == 2L)) {
    code <- 1L
    pair_rank <- ranks_nz[mult == 2L][1]
    kickers <- sort(ranks_nz[mult == 1L], decreasing = TRUE)
    tie <- c(pair_rank, kickers, 0L)[1:5]
  } else {
    code <- 0L
    tie <- sort(rv, decreasing = TRUE)
  }

  code * 15^5 + .encode_tiebreak_fast(tie)
}

.best5_from_n_ids <- function(ids) {
  stopifnot(length(ids) >= 5)
  idx <- combn(length(ids), 5)
  best_score <- -Inf
  best_ids <- NULL
  for (j in seq_len(ncol(idx))) {
    cand <- ids[idx[, j]]
    sc <- hand_value_5_ids(cand)
    if (sc > best_score) {
      best_score <- sc
      best_ids <- cand
    }
  }
  list(score = best_score, best_ids = best_ids)
}

holdem_best_score_ids <- function(hole_ids, board_ids) {
  if (length(hole_ids) != 2) stop("Hold'em hole_ids must have length 2.")
  if (!(length(board_ids) %in% c(3, 4, 5))) stop("board_ids must have length 3, 4, or 5.")
  ids <- c(hole_ids, board_ids)
  if (anyDuplicated(ids)) stop("Duplicate card detected in Hold'em input.")
  .best5_from_n_ids(ids)
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

############################################################
# 4. DEAL + SHOWDOWN HELPERS (NO BETTING ENGINE YET)
############################################################

deal_holdem <- function(n_players) {
  if (!is.numeric(n_players) || length(n_players) != 1) stop("n_players must be a single number.")
  if (n_players < 2 || n_players > 10) stop("n_players must be between 2 and 10.")

  deck <- shuffle_deck(create_deck())
  dealt_hole <- deal_cards(deck, 2 * n_players)
  hole_cards_all <- dealt_hole$dealt
  deck <- dealt_hole$remaining

  hole_list <- split(hole_cards_all, rep(seq_len(n_players), each = 2))
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

evaluate_holdem_showdown <- function(deal) {
  stopifnot(is.list(deal), !is.null(deal$hole), !is.null(deal$board))
  n_players <- length(deal$hole)
  board <- deal$board
  results <- vector("list", n_players)

  for (i in seq_len(n_players)) {
    hv <- holdem_hand_value(deal$hole[[i]], board)
    results[[i]] <- list(player = i, score = hv$score, best_hand = hv$best_hand)
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

play_holdem_hand <- function(n_players = 2) {
  deal <- deal_holdem(n_players)
  showdown <- evaluate_holdem_showdown(deal)
  showdown$hole <- deal$hole
  showdown
}
print_holdem_hand <- function(showdown) {
  cat("\nPLAYERS:\n")
  for (i in seq_len(showdown$n_players)) {
    hc <- showdown$hole[[i]]
    cat(sprintf("Player %d hole: %s %s\n", i, hc$card[1], hc$card[2]))
  }

  cat("\nBOARD:\n")
  board_cards <- showdown$board$card
  board_labels <- c("Flop 1", "Flop 2", "Flop 3", "Turn", "River")[seq_along(board_cards)]
  for (i in seq_along(board_cards)) {
    cat(sprintf("%-6s: %s\n", board_labels[i], board_cards[i]))
  }

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
