############################################################
# Mathematics of Poker — Equity Tools
# File: equity_tools.R
#
# Purpose:
#   Monte Carlo equity for Hold'em and Omaha, built on the
#   integer-based evaluator from cards_and_hands.R.
#
# Dependencies:
#   source("cards_and_hands.R")
############################################################

parse_holdem_range_cards <- function(cards) {
  card_strings <- as.character(cards)
  if (!is.character(card_strings)) {
    stop("Hold'em range cards must be character strings.")
  }

  ranks <- substr(card_strings, 1, nchar(card_strings) - 1)
  suits <- substr(card_strings, nchar(card_strings), nchar(card_strings))

  df_to_ids(
    data.frame(
      rank = ranks,
      suit = suits,
      stringsAsFactors = FALSE
    )
  )
}

validate_holdem_range <- function(range_obj) {
  if (!inherits(range_obj, "poker_range")) {
    stop("Range inputs must be poker_range objects.")
  }
  if (!identical(range_obj$game, "holdem")) {
    stop("Only holdem poker_range objects are supported in holdem_equity_mc_fast().")
  }

  combos <- range_obj$combos
  if (!is.data.frame(combos)) {
    stop("Hold'em range combos must be a data.frame.")
  }

  needed <- c("c1", "c2", "w")
  if (!all(needed %in% names(combos))) {
    stop("Hold'em range needs columns c1, c2, w.")
  }

  ids1 <- parse_holdem_range_cards(combos$c1)
  ids2 <- parse_holdem_range_cards(combos$c2)

  if (any(ids1 == ids2)) {
    stop("Hold'em range combos must contain two different hole cards.")
  }

  # Use the weights stored in the combos data frame.
  weights <- as.numeric(as.character(combos$w))

  if (any(is.na(weights))) {
    stop("Hold'em range weights must be numeric and not missing.")
  }
  if (length(weights) != nrow(combos)) {
    stop("Hold'em range weights length must match number of combos.")
  }
  if (any(weights < 0)) {
    stop("Hold'em range weights must be non-negative.")
  }
  if (all(weights == 0)) {
    stop("Hold'em range weights cannot all be zero.")
  }

  list(
    ids = cbind(ids1, ids2),
    weights = weights
  )
}

holdem_equity_mc_fast <- function(
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

  is_range <- vapply(hole_list, inherits, logical(1), what = "poker_range")
  range_info <- vector("list", length(hole_list))
  hole_ids_list <- vector("list", length(hole_list))

  for (i in seq_along(hole_list)) {
    if (is_range[i]) {
      range_info[[i]] <- validate_holdem_range(hole_list[[i]])
    } else {
      hole_ids <- df_to_ids(hole_list[[i]])

      if (length(hole_ids) != 2) {
        stop(sprintf("Player %d must have exactly two hole cards in Hold'em.", i))
      }
      if (hole_ids[1] == hole_ids[2]) {
        stop(sprintf("Player %d has duplicate hole cards.", i))
      }

      hole_ids_list[[i]] <- hole_ids
    }
  }

  board_ids_known <- if (nrow(board_df) == 0) integer(0) else df_to_ids(board_df)

  if (length(board_ids_known) > 5) {
    stop("board_df cannot contain more than 5 cards.")
  }
  if (anyDuplicated(board_ids_known)) {
    stop("Duplicate cards detected in board_df.")
  }

  fixed_ids <- unlist(hole_ids_list[!is_range], use.names = FALSE)
  known_ids <- c(fixed_ids, board_ids_known)

  if (anyDuplicated(known_ids)) {
    stop("Duplicate known cards detected in equity input.")
  }

  full_deck <- seq_len(52)
  n_board_needed <- 5 - length(board_ids_known)
  n_players <- length(hole_list)
  wins <- numeric(n_players)

  for (sim in seq_len(n_sims)) {
    sim_hole_ids <- hole_ids_list
    cur_used <- known_ids

    # First sample hole cards for range players, sequentially,
    # removing card conflicts as we go.
    if (any(is_range)) {
      for (i in which(is_range)) {
        range_ids <- range_info[[i]]$ids
        valid_idx <- which(
          !(range_ids[, 1] %in% cur_used) &
            !(range_ids[, 2] %in% cur_used)
        )

        if (length(valid_idx) == 0L) {
          stop(sprintf(
            "No valid combos remain for range player %d given known cards and previously sampled hands.",
            i
          ))
        }

        weights <- range_info[[i]]$weights[valid_idx]

        if (length(weights) != length(valid_idx)) {
          stop("Internal error: range weights and valid indices length mismatch.")
        }
        if (all(weights == 0)) {
          stop(sprintf(
            "Range player %d has no positive-weight combos remaining after conflicts are removed.",
            i
          ))
        }

        chosen <- sample(valid_idx, size = 1, prob = weights)
        sim_hole_ids[[i]] <- range_ids[chosen, ]
        cur_used <- c(cur_used, sim_hole_ids[[i]])
      }
    }

    # Then deal the rest of the board from the remaining deck.
    unseen <- setdiff(full_deck, cur_used)
    drawn_board <- if (n_board_needed > 0) {
      sample(unseen, n_board_needed, replace = FALSE)
    } else {
      integer(0)
    }

    board_ids <- c(board_ids_known, drawn_board)

    scores <- vapply(
      sim_hole_ids,
      function(h) holdem_best_score_ids(h, board_ids)$score,
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

