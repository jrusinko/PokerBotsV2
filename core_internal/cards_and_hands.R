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
#
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
############################################################
# Range string parser for Hold'em
#
# Supported examples:
#   "44+, A2s+, K5s+, Q8s+, J9s+, T9s, A7o+, K9o+, QTo+, JTo+"
#   "77-JJ, A5s-A2s, KQo-KTo"
#
# Output:
#   a poker_range object created with new_range_holdem(...)
############################################################

.holdem_ranks <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
.holdem_rank_to_value <- setNames(seq_along(.holdem_ranks), .holdem_ranks)
.holdem_suits <- c("h", "d", "c", "s")

normalize_range_string <- function(x) {
  x <- gsub("\\s+", "", x)
  x <- gsub("::\\{\\}$", "", x)
  x
}

holdem_rank_index <- function(r) {
  out <- unname(.holdem_rank_to_value[as.character(r)])
  if (any(is.na(out))) {
    stop("Invalid rank detected.")
  }
  out
}
is_pair_token <- function(token) {
  grepl("^[2-9TJQKA]{2}$", token)
}

is_nonpair_token <- function(token) {
  grepl("^[2-9TJQKA][2-9TJQKA][so]$", token)
}

expand_pair_token <- function(token) {
  plus <- grepl("\\+$", token)
  base <- sub("\\+$", "", token)

  if (!is_pair_token(base)) {
    stop(sprintf("Invalid pair token: %s", token))
  }

  r1 <- substr(base, 1, 1)
  r2 <- substr(base, 2, 2)

  if (r1 != r2) {
    stop(sprintf("Token %s is not a pair token.", token))
  }

  idx <- holdem_rank_index(r1)

  if (!plus) {
    return(base)
  }

  ranks_to_use <- .holdem_ranks[1:idx]
  paste0(ranks_to_use, ranks_to_use)
}

expand_nonpair_token <- function(token) {
  plus <- grepl("\\+$", token)
  base <- sub("\\+$", "", token)

  if (!is_nonpair_token(base)) {
    stop(sprintf("Invalid non-pair token: %s", token))
  }

  r1 <- substr(base, 1, 1)
  r2 <- substr(base, 2, 2)
  suited_flag <- substr(base, 3, 3)

  if (r1 == r2) {
    stop(sprintf("Token %s should not use suited/offsuited notation for a pair.", token))
  }

  idx1 <- holdem_rank_index(r1)
  idx2 <- holdem_rank_index(r2)

  if (idx2 <= idx1) {
    stop(sprintf("Token %s is not in canonical high-card form.", token))
  }

  if (!plus) {
    return(base)
  }

  second_ranks <- .holdem_ranks[idx2:(idx1 + 1)]
  paste0(r1, second_ranks, suited_flag)
}

expand_pair_dash_token <- function(left, right) {
  if (!is_pair_token(left) || !is_pair_token(right)) {
    stop(sprintf("Invalid dashed pair range: %s-%s", left, right))
  }

  r_left <- substr(left, 1, 1)
  r_right <- substr(right, 1, 1)

  idx_left <- holdem_rank_index(r_left)
  idx_right <- holdem_rank_index(r_right)

  lo <- min(idx_left, idx_right)
  hi <- max(idx_left, idx_right)

  ranks_to_use <- .holdem_ranks[lo:hi]
  paste0(ranks_to_use, ranks_to_use)
}

expand_nonpair_dash_token <- function(left, right) {
  if (!is_nonpair_token(left) || !is_nonpair_token(right)) {
    stop(sprintf("Invalid dashed non-pair range: %s-%s", left, right))
  }

  r1_left <- substr(left, 1, 1)
  r2_left <- substr(left, 2, 2)
  t_left  <- substr(left, 3, 3)

  r1_right <- substr(right, 1, 1)
  r2_right <- substr(right, 2, 2)
  t_right  <- substr(right, 3, 3)

  if (r1_left != r1_right) {
    stop(sprintf(
      "Dashed non-pair ranges must keep the first rank fixed: %s-%s",
      left, right
    ))
  }

  if (t_left != t_right) {
    stop(sprintf(
      "Dashed non-pair ranges must keep suitedness fixed: %s-%s",
      left, right
    ))
  }

  idx1 <- holdem_rank_index(r1_left)
  idx2_left <- holdem_rank_index(r2_left)
  idx2_right <- holdem_rank_index(r2_right)

  if (idx2_left <= idx1 || idx2_right <= idx1) {
    stop(sprintf("Invalid dashed non-pair range: %s-%s", left, right))
  }

  lo <- min(idx2_left, idx2_right)
  hi <- max(idx2_left, idx2_right)

  second_ranks <- .holdem_ranks[lo:hi]
  paste0(r1_left, second_ranks, t_left)
}

expand_dash_token <- function(token) {
  parts <- strsplit(token, "-", fixed = TRUE)[[1]]

  if (length(parts) != 2) {
    stop(sprintf("Invalid dashed token: %s", token))
  }

  left <- parts[1]
  right <- parts[2]

  if (is_pair_token(left) && is_pair_token(right)) {
    return(expand_pair_dash_token(left, right))
  }

  if (is_nonpair_token(left) && is_nonpair_token(right)) {
    return(expand_nonpair_dash_token(left, right))
  }

  stop(sprintf("Unsupported dashed range format: %s", token))
}

expand_range_token <- function(token) {
  if (token == "") return(character(0))

  token <- toupper(token)
  token <- gsub("S", "s", token)
  token <- gsub("O", "o", token)

  if (grepl("-", token, fixed = TRUE)) {
    return(expand_dash_token(token))
  }

  if (grepl("^[2-9TJQKA]{2}\\+?$", token)) {
    return(expand_pair_token(token))
  }

  if (grepl("^[2-9TJQKA][2-9TJQKA][so]\\+?$", token)) {
    return(expand_nonpair_token(token))
  }

  stop(sprintf("Unsupported token format: %s", token))
}

expand_range_string_to_classes <- function(range_string) {
  x <- normalize_range_string(range_string)
  tokens <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  tokens <- tokens[tokens != ""]

  hand_classes <- unlist(lapply(tokens, expand_range_token), use.names = FALSE)
  unique(hand_classes)
}

hand_class_to_combos <- function(hand_class) {
  hand_class <- toupper(hand_class)
  hand_class <- gsub("S", "s", hand_class)
  hand_class <- gsub("O", "o", hand_class)

  if (grepl("^[2-9TJQKA]{2}$", hand_class)) {
    r <- substr(hand_class, 1, 1)

    combos <- list()
    k <- 1
    for (i in 1:3) {
      for (j in (i + 1):4) {
        combos[[k]] <- c(
          paste0(r, .holdem_suits[i]),
          paste0(r, .holdem_suits[j])
        )
        k <- k + 1
      }
    }
    return(do.call(rbind, combos))
  }

  if (grepl("^[2-9TJQKA][2-9TJQKA][so]$", hand_class)) {
    r1 <- substr(hand_class, 1, 1)
    r2 <- substr(hand_class, 2, 2)
    typ <- substr(hand_class, 3, 3)

    combos <- list()
    k <- 1

    if (typ == "s") {
      for (s in .holdem_suits) {
        combos[[k]] <- c(paste0(r1, s), paste0(r2, s))
        k <- k + 1
      }
    } else if (typ == "o") {
      for (s1 in .holdem_suits) {
        for (s2 in .holdem_suits) {
          if (s1 != s2) {
            combos[[k]] <- c(paste0(r1, s1), paste0(r2, s2))
            k <- k + 1
          }
        }
      }
    } else {
      stop(sprintf("Unexpected type in hand class: %s", hand_class))
    }

    return(do.call(rbind, combos))
  }

  stop(sprintf("Unsupported hand class: %s", hand_class))
}

range_string_to_combos_df <- function(range_string, weight = 1) {
  classes <- expand_range_string_to_classes(range_string)

  combo_rows <- lapply(classes, hand_class_to_combos)
  combo_mat <- do.call(rbind, combo_rows)

  out <- data.frame(
    c1 = combo_mat[, 1],
    c2 = combo_mat[, 2],
    w = weight,
    stringsAsFactors = FALSE
  )

  out <- unique(out)
  rownames(out) <- NULL
  out
}

new_range_holdem_from_string <- function(range_string, weight = 1) {
  combos <- range_string_to_combos_df(range_string, weight = weight)
  new_range_holdem(combos = combos)
}
