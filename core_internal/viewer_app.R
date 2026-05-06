############################################################
# Mathematics of Poker — Viewer App
# File: viewer_app.R
#
# Purpose:
#   Replay hands, inspect action histories, and view tournament results.
#   First draft: hand selector + snapshot stepper + summary panels.
############################################################

run_viewer_app <- function(log_data = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for run_viewer_app().")
  }

  # -----------------------------
  # Internal helper functions
  # -----------------------------
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  compact_chr <- function(x, collapse = ", ") {
    if (is.null(x) || length(x) == 0) return("")
    paste(as.character(x), collapse = collapse)
  }

  format_cards <- function(x) {
    if (is.null(x) || length(x) == 0) return("(none)")
    paste(as.character(x), collapse = " ")
  }

  card_suit_symbol <- function(card) {
    if (is.null(card) || length(card) == 0) return("")
    suit <- substr(as.character(card), nchar(as.character(card)), nchar(as.character(card)))
    switch(
      suit,
      h = "\u2665",
      d = "\u2666",
      s = "\u2660",
      c = "\u2663",
      suit
    )
  }

  card_rank_label <- function(card) {
    if (is.null(card) || length(card) == 0) return("")
    card <- as.character(card)
    substr(card, 1, nchar(card) - 1)
  }

  card_color_class <- function(card) {
    suit <- substr(as.character(card), nchar(as.character(card)), nchar(as.character(card)))
    if (suit %in% c("h", "d")) "viewer-card-red" else "viewer-card-black"
  }

  as_scalar_chr <- function(x, default = "") {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
    as.character(x[1])
  }

  as_scalar_num <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
    as.numeric(x[1])
  }

  normalize_replay_data <- function(x) {
    if (is.null(x)) {
      stop(
        "run_viewer_app() requires `log_data`.\n",
        "Pass a tournament_state, a hand_log list, or a replay-style list."
      )
    }

    # Case 1: full tournament_state
    if (inherits(x, "tournament_state")) {
      return(list(
        tournament_id = x$tournament_id %||% "Unknown Tournament",
        status = x$status %||% "unknown",
        players = x$players %||% list(),
        hand_log = x$hand_log %||% list(),
        elimination_order = x$elimination_order %||% character(0)
      ))
    }

    # Case 2: raw hand_log list
    if (is.list(x) &&
        length(x) > 0 &&
        is.null(x$hand_log) &&
        all(vapply(x, is.list, logical(1)))) {
      maybe_hand <- x[[1]]
      if (!is.null(maybe_hand$hand_id) || !is.null(maybe_hand$hand_number)) {
        return(list(
          tournament_id = "Replay",
          status = "unknown",
          players = list(),
          hand_log = x,
          elimination_order = character(0)
        ))
      }
    }

    # Case 3: replay-style list
    if (is.list(x) && !is.null(x$hand_log)) {
      return(list(
        tournament_id = x$tournament_id %||% "Replay",
        status = x$status %||% "unknown",
        players = x$players %||% list(),
        hand_log = x$hand_log %||% list(),
        elimination_order = x$elimination_order %||% character(0)
      ))
    }

    stop(
      "`log_data` was not recognized.\n",
      "Expected a tournament_state, a hand_log list, or a replay-style list containing $hand_log."
    )
  }

  hand_label <- function(hand) {
    hn <- hand$hand_number %||% NA_integer_
    hid <- hand$hand_id %||% ""
    paste0("Hand ", hn, if (!identical(hid, "")) paste0(" (", hid, ")") else "")
  }

  get_hand_snapshots <- function(hand) {
    # Allow a few reasonable field names while the logging format settles.
    snaps <- hand$state_snapshots %||% hand$snapshots %||% hand$hand_snapshots %||% NULL
    if (is.null(snaps)) return(list())
    if (!is.list(snaps)) return(list())
    snaps
  }

  player_name_lookup <- function(hand) {
    lookup <- list()
    sources <- c(
      hand$starting_stack_summary %||% list(),
      hand$ending_stack_summary %||% list(),
      hand$stack_summary %||% list()
    )

    for (p in sources) {
      pid <- as.character(p$player_id %||% "")
      seat <- as.character(p$seat %||% "")
      name <- as.character(p$player_name %||% p$name %||% pid)
      if (!identical(pid, "")) lookup[[paste0("pid:", pid)]] <- name
      if (!identical(seat, "")) lookup[[paste0("seat:", seat)]] <- name
    }

    showdown_hands <- hand$showdown_summary$hands %||% list()
    for (p in showdown_hands) {
      pid <- as.character(p$player_id %||% "")
      seat <- as.character(p$seat %||% "")
      name <- as.character(p$player_name %||% pid)
      if (!identical(pid, "")) lookup[[paste0("pid:", pid)]] <- name
      if (!identical(seat, "")) lookup[[paste0("seat:", seat)]] <- name
    }

    lookup
  }

  build_replay_snapshots <- function(hand) {
    snaps <- get_hand_snapshots(hand)
    if (length(snaps) == 0) {
      return(list())
    }

    elim_ids <- hand$eliminations_this_hand %||% character(0)
    if (length(elim_ids) == 0) {
      return(snaps)
    }

    last_snap <- snaps[[length(snaps)]]
    if (identical(as_scalar_chr(last_snap$snapshot_type, ""), "elimination")) {
      return(snaps)
    }

    lookup <- player_name_lookup(hand)
    elim_names <- vapply(elim_ids, function(pid) {
      lookup[[paste0("pid:", pid)]] %||% pid
    }, character(1))

    elim_snap <- last_snap
    elim_snap$snapshot_type <- "elimination"
    elim_snap$message <- paste0(
      if (length(elim_names) > 1) "Eliminated: " else "Eliminated: ",
      paste(elim_names, collapse = ", ")
    )
    elim_snap$step <- as.numeric(last_snap$step %||% length(snaps)) + 1

    c(snaps, list(elim_snap))
  }

  get_snapshot <- function(hand, idx) {
    snaps <- build_replay_snapshots(hand)
    n <- length(snaps)
    if (n == 0) return(NULL)
    idx <- max(1L, min(as.integer(idx), n))
    snaps[[idx]]
  }

  snapshot_to_player_df <- function(snapshot) {
    players <- snapshot$players %||% snapshot$player_states %||% list()
    if (!is.list(players) || length(players) == 0) {
      return(data.frame(
        Seat = integer(0),
        Name = character(0),
        Stack = numeric(0),
        Folded = logical(0),
        AllIn = logical(0),
        Status = character(0),
        CommittedStreet = numeric(0),
        CommittedHand = numeric(0),
        HoleCards = character(0),
        stringsAsFactors = FALSE
      ))
    }

    out <- lapply(players, function(p) {
      data.frame(
        Seat = as_scalar_num(p$seat),
        Name = as_scalar_chr(p$name, p$player_id %||% ""),
        Stack = as_scalar_num(p$stack),
        Folded = isTRUE(p$folded),
        AllIn = isTRUE(p$all_in),
        Status = as_scalar_chr(p$status),
        CommittedStreet = as_scalar_num(p$committed_this_round, 0),
        CommittedHand = as_scalar_num(p$committed_this_hand, 0),
        HoleCards = format_cards(p$hole_cards %||% character(0)),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, out)
    out <- out[order(out$Seat), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  snapshot_players <- function(snapshot, hand = NULL, include_eliminated = TRUE) {
    players <- snapshot$players %||% snapshot$player_states %||% list()

    if ((!is.list(players) || length(players) == 0) && !is.null(hand)) {
      fallback <- hand$ending_stack_summary %||% hand$stack_summary %||% list()
      players <- lapply(fallback, function(p) {
        list(
          player_id = p$player_id %||% NA_character_,
          name = p$player_name %||% p$player_id %||% NA_character_,
          seat = p$seat %||% NA_integer_,
          stack = p$stack %||% NA_real_,
          folded = FALSE,
          all_in = FALSE,
          status = p$status %||% NA_character_,
          committed_this_round = 0,
          committed_this_hand = 0,
          hole_cards = character(0)
        )
      })
    }

    if (!is.list(players) || length(players) == 0) {
      return(list())
    }

    players <- players[order(vapply(players, function(p) as.numeric(p$seat %||% Inf), numeric(1)))]

    if (!is.null(hand) &&
        !identical(as_scalar_chr(snapshot$snapshot_type, ""), "elimination")) {
      elim_ids <- hand$eliminations_this_hand %||% character(0)
      if (length(elim_ids) > 0) {
        players <- lapply(players, function(p) {
          if ((p$player_id %||% "") %in% elim_ids) {
            p$status <- "active"
          }
          p
        })
      }
    }

    if (!isTRUE(include_eliminated)) {
      keep_idx <- vapply(
        players,
        function(p) !identical(as.character(p$status %||% ""), "eliminated"),
        logical(1)
      )
      players <- players[keep_idx]
    }

    players
  }

  hand_player_ids <- function(hand) {
    starters <- hand$starting_stack_summary %||% list()
    if (length(starters) > 0) {
      keep <- vapply(
        starters,
        function(p) !identical(as.character(p$status %||% ""), "eliminated"),
        logical(1)
      )
      starters <- starters[keep]
      return(vapply(starters, function(p) as.character(p$player_id %||% ""), character(1)))
    }

    snaps <- get_hand_snapshots(hand)
    if (length(snaps) > 0) {
      snap_players <- snaps[[1]]$players %||% list()
      if (length(snap_players) > 0) {
        return(vapply(snap_players, function(p) as.character(p$player_id %||% ""), character(1)))
      }
    }

    character(0)
  }

  hand_player_seats <- function(hand) {
    starters <- hand$starting_stack_summary %||% list()
    if (length(starters) > 0) {
      keep <- vapply(
        starters,
        function(p) !identical(as.character(p$status %||% ""), "eliminated"),
        logical(1)
      )
      starters <- starters[keep]
      return(vapply(starters, function(p) as.numeric(p$seat %||% NA_real_), numeric(1)))
    }

    snaps <- get_hand_snapshots(hand)
    if (length(snaps) > 0) {
      snap_players <- snaps[[1]]$players %||% list()
      if (length(snap_players) > 0) {
        return(vapply(snap_players, function(p) as.numeric(p$seat %||% NA_real_), numeric(1)))
      }
    }

    numeric(0)
  }

  hand_player_names <- function(hand) {
    starters <- hand$starting_stack_summary %||% list()
    if (length(starters) > 0) {
      keep <- vapply(
        starters,
        function(p) !identical(as.character(p$status %||% ""), "eliminated"),
        logical(1)
      )
      starters <- starters[keep]
      ids <- vapply(starters, function(p) as.character(p$player_id %||% ""), character(1))
      labels <- vapply(starters, function(p) as.character(p$player_name %||% p$player_id %||% ""), character(1))
      keep_named <- !is.na(ids) & nzchar(ids) & !is.na(labels) & nzchar(labels)
      ids <- ids[keep_named]
      labels <- labels[keep_named]
      if (length(ids) == 0) {
        return(setNames(character(0), character(0)))
      }
      return(stats::setNames(ids, labels))
    }

    setNames(character(0), character(0))
  }

  showdown_player_ids <- function(hand) {
    hands <- hand$showdown_summary$hands %||% list()
    if (!is.list(hands) || length(hands) == 0) {
      return(character(0))
    }

    vapply(hands, function(p) as.character(p$player_id %||% ""), character(1))
  }

  table_seat_positions <- function(n) {
    if (n <= 0) return(list())

    angles <- seq(from = -pi / 2, length.out = n, by = (2 * pi) / n)
    lapply(angles, function(a) {
      list(
        left = 50 + 38 * cos(a),
        top = 50 + 30 * sin(a)
      )
    })
  }

  display_seat_position <- function(pos) {
    if (is.null(pos)) {
      return(NULL)
    }

    left <- as.numeric(pos$left %||% NA_real_)
    top <- as.numeric(pos$top %||% NA_real_)
    if (!is.finite(left) || !is.finite(top)) {
      return(NULL)
    }

    vertical_shift <- if (top < 42) {
      -7
    } else if (top > 58) {
      7
    } else {
      0
    }

    list(
      left = left,
      top = top + vertical_shift
    )
  }

  table_chip_position <- function(pos) {
    if (is.null(pos)) {
      return(NULL)
    }
    left <- as.numeric(pos$left %||% NA_real_)
    top <- as.numeric(pos$top %||% NA_real_)
    if (!is.finite(left) || !is.finite(top)) {
      return(NULL)
    }
    if (top < 42 && left > 55) {
      return(list(
        left = 50 + (left - 50) * 0.68,
        top = 50 + (top - 50) * 0.62 + 1
      ))
    }

    radial_scale <- if (top < 42 && left < 45) 0.58 else 0.48
    list(
      left = 50 + (left - 50) * radial_scale,
      top = 50 + (top - 50) * radial_scale
    )
  }

  center_pot_chip_position <- function() {
    list(left = 74, top = 5)
  }

  build_card_tags <- function(cards, hidden = FALSE, size = c("normal", "small")) {
    size <- match.arg(size)
    if (length(cards) == 0) {
      return(list(shiny::tags$div(class = paste("viewer-card viewer-card-", size, sep = ""), "(none)")))
    }

    lapply(cards, function(card) {
      card <- as.character(card)
      classes <- c("viewer-card", paste0("viewer-card-", size))

      if (isTRUE(hidden)) {
        classes <- c(classes, "viewer-card-hidden")
        return(shiny::tags$div(class = paste(classes, collapse = " "), "?"))
      }

      classes <- c(classes, card_color_class(card))
      shiny::tags$div(
        class = paste(classes, collapse = " "),
        shiny::tags$div(class = "viewer-card-rank", card_rank_label(card)),
        shiny::tags$div(class = "viewer-card-suit", card_suit_symbol(card))
      )
    })
  }

  player_status_label <- function(player) {
    if (identical(player$status %||% "", "eliminated")) return("Eliminated")
    if (isTRUE(player$folded)) return("Folded")
    if (isTRUE(player$all_in)) return("All-in")
    "Active"
  }

  player_status_class <- function(player) {
    if (identical(player$status %||% "", "eliminated")) return("viewer-status-eliminated")
    if (isTRUE(player$folded)) return("viewer-status-folded")
    if (isTRUE(player$all_in)) return("viewer-status-allin")
    "viewer-status-active"
  }

  player_role_badges <- function(player, snapshot) {
    badges <- list()
    seat <- as_scalar_num(player$seat)

    if (identical(seat, as_scalar_num(snapshot$button_seat))) {
      badges[[length(badges) + 1L]] <- shiny::tags$span(class = "viewer-role viewer-role-button", "D")
    }
    if (identical(seat, as_scalar_num(snapshot$small_blind_seat))) {
      badges[[length(badges) + 1L]] <- shiny::tags$span(class = "viewer-role viewer-role-sb", "SB")
    }
    if (identical(seat, as_scalar_num(snapshot$big_blind_seat))) {
      badges[[length(badges) + 1L]] <- shiny::tags$span(class = "viewer-role viewer-role-bb", "BB")
    }

    badges
  }

  chip_action_label <- function(hand, snapshot, player) {
    if (is.null(hand)) {
      return(NULL)
    }

    action_history <- hand$action_history %||% list()
    if (!is.list(action_history) || length(action_history) == 0) {
      return(NULL)
    }

    action_count <- as.integer(snapshot$action_count %||% length(action_history))
    action_count <- max(0L, min(action_count, length(action_history)))
    if (action_count == 0) {
      return(NULL)
    }

    seat <- as_scalar_num(player$seat)
    street <- as_scalar_chr(snapshot$street, "")
    past_actions <- action_history[seq_len(action_count)]

    matching <- Filter(function(a) {
      identical(as_scalar_num(a$seat, NA_real_), seat) &&
        identical(as_scalar_chr(a$street, ""), street)
    }, past_actions)

    if (length(matching) == 0) {
      return(NULL)
    }

    last_type <- as_scalar_chr(matching[[length(matching)]]$type, "bet")

    if (last_type == "check") {
      return("Check")
    }

    if (last_type %in% c("call", "all_in_call", "all_in_short")) {
      return("Call")
    }
    if (last_type %in% c("raise", "all_in_raise")) {
      return("Raise")
    }
    if (last_type %in% c("bet", "all_in_bet")) {
      return("Bet")
    }
    if (last_type == "post_sb") {
      return("SB")
    }
    if (last_type == "post_bb") {
      return("BB")
    }
    if (last_type == "post_ante") {
      return("Ante")
    }

    tools::toTitleCase(gsub("_", " ", last_type))
  }

  hand_chatter_entries <- function(hand, snapshot, max_entries = 4L) {
    action_history <- hand$action_history %||% list()
    if (!is.list(action_history) || length(action_history) == 0 || is.null(snapshot)) {
      return(list())
    }

    action_count <- as.integer(snapshot$action_count %||% length(action_history))
    action_count <- max(0L, min(action_count, length(action_history)))
    if (action_count == 0) {
      return(list())
    }

    past_actions <- action_history[seq_len(action_count)]
    entries <- Filter(function(a) {
      chatter <- a$chatter %||% a$table_talk %||% ""
      nzchar(trimws(paste(as.character(chatter), collapse = " ")))
    }, past_actions)

    if (length(entries) == 0) {
      return(list())
    }

    tail(entries, max_entries)
  }

  build_chatter_ui <- function(hand, snapshot, broadcast_mode = FALSE) {
    if (!isTRUE(broadcast_mode)) {
      return(NULL)
    }

    entries <- hand_chatter_entries(hand, snapshot)
    if (length(entries) == 0) {
      return(NULL)
    }

    rows <- lapply(entries, function(a) {
      chatter <- paste(trimws(as.character(a$chatter %||% a$table_talk %||% "")), collapse = "\n")
      chatter <- strip_chatter_speaker_prefix(chatter)
      shiny::tags$div(
        class = "viewer-chatter-row",
        shiny::tags$div(class = "viewer-chatter-speaker", as_scalar_chr(a$player_name, a$player_id %||% "Player")),
        shiny::tags$div(class = "viewer-chatter-text", chatter)
      )
    })

    shiny::tags$div(
      class = "viewer-chatter-panel",
      shiny::tags$div(class = "viewer-chatter-title", "Table Chatter"),
      rows
    )
  }

  strip_chatter_speaker_prefix <- function(chatter) {
    chatter <- trimws(as_scalar_chr(chatter, ""))
    sub("^[[:alnum:] ._'?-]{1,45}:\\s*", "", chatter)
  }

  chip_stack_breakdown <- function(amount) {
    amount <- as.numeric(amount)
    if (is.na(amount) || amount <= 0) {
      return(list())
    }

    denoms <- c(1000, 500, 100, 25, 5, 1)
    colors <- c("#2a2f36", "#2e67a8", "#2f8f57", "#c63f3f", "#f0efe9", "#d8a62d")
    labels <- c("1k", "500", "100", "25", "5", "1")

    out <- list()
    remaining <- round(amount)

    for (i in seq_along(denoms)) {
      n <- remaining %/% denoms[i]
      if (n > 0) {
        out[[length(out) + 1L]] <- list(
          denom = denoms[i],
          count = as.integer(n),
          display_count = min(as.integer(n), 4L),
          color = colors[i],
          label = labels[i]
        )
        remaining <- remaining %% denoms[i]
      }
    }

    out
  }

  build_chip_stack_ui <- function(amount, label, pos = NULL) {
    if (is.null(pos)) {
      return(NULL)
    }
    left <- as.numeric(pos$left %||% NA_real_)
    top <- as.numeric(pos$top %||% NA_real_)
    if (!is.finite(left) || !is.finite(top)) {
      return(NULL)
    }

    stacks <- chip_stack_breakdown(amount)
    if (length(stacks) == 0) {
      return(NULL)
    }

    stack_nodes <- lapply(stacks, function(s) {
      chip_nodes <- lapply(seq_len(s$display_count), function(i) {
        shiny::tags$div(
          class = "viewer-chip-disc",
          style = paste0("background:", s$color, "; bottom:", (i - 1L) * 5L, "px;")
        )
      })

      shiny::tags$div(
        class = "viewer-chip-stack",
        chip_nodes,
        if (s$count > s$display_count) {
          shiny::tags$div(class = "viewer-chip-count", paste0("x", s$count))
        },
        shiny::tags$div(class = "viewer-chip-denom", s$label)
      )
    })

    shiny::tags$div(
      class = "viewer-chip-bet",
      style = paste0("left:", round(left, 1), "%; top:", round(top, 1), "%;"),
      shiny::tags$div(class = "viewer-chip-stack-row", stack_nodes)
    )
  }

  hand_total_pot <- function(hand) {
    winners <- hand$winners %||% list()
    if (is.list(winners) && length(winners) > 0) {
      total <- 0
      for (w in winners) {
        if (identical(w$win_type %||% "", "win_by_folds")) {
          total <- total + as.numeric(w$amount %||% 0)
        }
        if (identical(w$win_type %||% "", "showdown_pot_award")) {
          total <- total + as.numeric(w$pot_amount %||% 0)
        }
      }
      if (is.finite(total) && total > 0) {
        return(total)
      }
    }

    deltas <- hand$stack_deltas %||% list()
    if (is.list(deltas) && length(deltas) > 0) {
      gains <- sum(vapply(deltas, function(x) max(0, as.numeric(x$delta %||% 0)), numeric(1)))
      if (is.finite(gains) && gains > 0) {
        return(gains)
      }
    }

    as.numeric(hand$pot %||% 0)
  }

  display_pot_value <- function(snapshot, hand) {
    pot <- as.numeric(snapshot$pot %||% NA_real_)
    if (!is.na(pot) && pot > 0) {
      return(pot)
    }

    if (identical(as_scalar_chr(snapshot$snapshot_type, ""), "showdown") ||
        identical(as_scalar_chr(snapshot$snapshot_type, ""), "hand_end")) {
      return(hand_total_pot(hand))
    }

    pot %||% 0
  }

  current_round_total <- function(players) {
    if (!is.list(players) || length(players) == 0) {
      return(0)
    }

    sum(vapply(
      players,
      function(p) as.numeric(p$committed_this_round %||% 0),
      numeric(1)
    ), na.rm = TRUE)
  }

  displayed_pot_chip_value <- function(snapshot, hand) {
    pot_value <- as.numeric(display_pot_value(snapshot, hand) %||% 0)
    max(0, pot_value)
  }

  build_board_center_ui <- function(board) {
    if (length(board) == 0) {
      return(
        shiny::tags$div(
          class = "viewer-center-brand",
          shiny::tags$div(class = "viewer-center-brand-top", "HWS"),
          shiny::tags$div(class = "viewer-center-brand-main", "Poker Capstone")
        )
      )
    }

    shiny::tags$div(class = "viewer-board-cards", build_card_tags(board, hidden = FALSE, size = "normal"))
  }

  build_amount_chip_ui <- function(amount, label = NULL, pos = NULL, extra_class = NULL) {
    amount <- as.numeric(amount)
    if (is.na(amount) || amount <= 0) {
      return(NULL)
    }

    classes <- c("viewer-chip-token")
    if (!is.null(extra_class)) {
      classes <- c(classes, extra_class)
    }

    shiny::tags$div(
      class = paste(classes, collapse = " "),
      style = if (!is.null(pos)) paste0("left:", round(pos$left, 1), "%; top:", round(pos$top, 1), "%;") else NULL,
      shiny::tags$div(class = "viewer-chip-token-top", label %||% "Chips"),
      shiny::tags$div(class = "viewer-chip-token-value", as_scalar_chr(amount, "0"))
    )
  }

  winner_display_data <- function(hand) {
    winners <- hand$winners %||% list()
    if (!is.list(winners) || length(winners) == 0) {
      return(NULL)
    }

    player_lookup <- player_name_lookup(hand)
    seat_lookup <- list()
    player_sources <- c(
      hand$ending_stack_summary %||% list(),
      hand$starting_stack_summary %||% list(),
      hand$stack_summary %||% list()
    )

    for (p in player_sources) {
      pid <- as.character(p$player_id %||% "")
      seat <- as.character(p$seat %||% "")
      name <- as.character(p$player_name %||% p$name %||% pid)
      if (!identical(seat, "")) {
        seat_lookup[[name]] <- as.numeric(p$seat %||% NA_real_)
      }
    }

    winner_names <- character(0)
    winner_amounts <- numeric(0)

    for (w in winners) {
      if (identical(w$win_type %||% "", "win_by_folds")) {
        pid_key <- paste0("pid:", as.character(w$player_id %||% ""))
        seat_key <- paste0("seat:", as.character(w$seat %||% ""))
        name <- player_lookup[[pid_key]] %||% player_lookup[[seat_key]] %||% paste0("Seat ", as.character(w$seat %||% "?"))
        winner_names <- c(winner_names, name)
        winner_amounts <- c(winner_amounts, as.numeric(w$amount %||% NA_real_))
      }

      if (identical(w$win_type %||% "", "showdown_pot_award")) {
        seats <- as.character(w$winner_seats %||% character(0))
        payouts <- w$payouts %||% numeric(0)
        for (seat in seats) {
          name <- player_lookup[[paste0("seat:", seat)]] %||% paste0("Seat ", seat)
          winner_names <- c(winner_names, name)
          amt <- suppressWarnings(as.numeric(payouts[[seat]] %||% NA_real_))
          winner_amounts <- c(winner_amounts, amt)
        }
      }
    }

    if (length(winner_names) == 0) {
      return(NULL)
    }

    amount_by_name <- tapply(
      ifelse(is.na(winner_amounts), 0, winner_amounts),
      winner_names,
      sum
    )

    list(
      names = names(amount_by_name),
      amounts = as.numeric(amount_by_name),
      seats = unname(vapply(names(amount_by_name), function(name) {
        seat_lookup[[name]] %||% NA_real_
      }, numeric(1)))
    )
  }

  build_elimination_banner_ui <- function(snapshot, hand) {
    if (is.null(snapshot) || is.null(hand)) {
      return(NULL)
    }

    if (!identical(as_scalar_chr(snapshot$snapshot_type, ""), "elimination")) {
      return(NULL)
    }

    elim_ids <- hand$eliminations_this_hand %||% character(0)
    if (length(elim_ids) == 0) {
      return(NULL)
    }

    lookup <- player_name_lookup(hand)
    elim_names <- vapply(elim_ids, function(pid) {
      lookup[[paste0("pid:", pid)]] %||% pid
    }, character(1))

    shiny::tags$div(
      class = "viewer-elimination-banner",
      shiny::tags$div(class = "viewer-elimination-title", if (length(elim_names) > 1) "Players Eliminated" else "Player Eliminated"),
      shiny::tags$div(class = "viewer-elimination-names", paste(elim_names, collapse = ", ")),
      shiny::tags$div(class = "viewer-elimination-sub", "Tournament life ends here.")
    )
  }

  build_winner_banner_ui <- function(snapshot, hand, players, positions) {
    if (is.null(snapshot) || is.null(hand)) {
      return(NULL)
    }

    if (!identical(as_scalar_chr(snapshot$snapshot_type, ""), "hand_end")) {
      return(NULL)
    }

    winner_info <- winner_display_data(hand)
    if (is.null(winner_info) || length(winner_info$names) == 0) {
      return(NULL)
    }

    payout_text <- paste(
      vapply(seq_along(winner_info$names), function(i) {
        amt <- winner_info$amounts[i]
        if (!is.na(amt) && amt > 0) {
          paste0(winner_info$names[i], " +", amt)
        } else {
          winner_info$names[i]
        }
      }, character(1)),
      collapse = "  |  "
    )

    banner_style <- NULL
    if (length(winner_info$names) == 1L && length(players) == length(positions)) {
      winner_seat <- winner_info$seats[1]
      winner_idx <- which(vapply(players, function(p) identical(as_scalar_num(p$seat), winner_seat), logical(1)))
      if (length(winner_idx) == 1L) {
        pos <- positions[[winner_idx]]
        banner_style <- paste0(
          "left:", round(pos$left, 1), "%; top:", round(pos$top - 10, 1), "%; transform: translate(-50%, -100%);"
        )
      }
    }

    shiny::tags$div(
      class = "viewer-winner-banner",
      style = banner_style %||% NULL,
      shiny::tags$div(class = "viewer-winner-title", "Hand Winner"),
      shiny::tags$div(class = "viewer-winner-names", paste(winner_info$names, collapse = ", ")),
      shiny::tags$div(class = "viewer-winner-payout", payout_text)
    )
  }

  winner_announcement_text <- function(hand) {
    winner_info <- winner_display_data(hand)
    if (is.null(winner_info) || length(winner_info$names) == 0) {
      return("")
    }

    payout_text <- paste(
      vapply(seq_along(winner_info$names), function(i) {
        amt <- winner_info$amounts[i]
        if (!is.na(amt) && amt > 0) {
          paste0(winner_info$names[i], " won ", amt, " chips")
        } else {
          winner_info$names[i]
        }
      }, character(1)),
      collapse = ". "
    )

    paste("Hand Winner.", payout_text)
  }

  elimination_announcement_text <- function(snapshot, hand) {
    if (is.null(snapshot) || is.null(hand) ||
        !identical(as_scalar_chr(snapshot$snapshot_type, ""), "elimination")) {
      return("")
    }

    elim_ids <- hand$eliminations_this_hand %||% character(0)
    if (length(elim_ids) == 0) {
      return("")
    }

    lookup <- player_name_lookup(hand)
    elim_names <- vapply(elim_ids, function(pid) {
      lookup[[paste0("pid:", pid)]] %||% pid
    }, character(1))

    paste(
      if (length(elim_names) > 1) "Players Eliminated." else "Player Eliminated.",
      paste(elim_names, collapse = ", "),
      "Tournament life ends here."
    )
  }

  viewer_word_count <- function(text) {
    text <- trimws(gsub("\\s+", " ", as.character(text %||% "")))
    if (!nzchar(text)) return(0L)
    length(strsplit(text, "\\s+")[[1]])
  }

  estimated_speech_ms <- function(text, seconds_per_word = 0.36, extra_ms = 500L) {
    words <- viewer_word_count(text)
    if (words <= 0L) return(0L)
    as.integer(ceiling(words * seconds_per_word * 1000 + extra_ms))
  }

  snapshot_speech_hold_ms <- function(hand, snapshot) {
    if (is.null(hand) || is.null(snapshot)) {
      return(0L)
    }

    texts <- character(0)
    action_history <- hand$action_history %||% list()
    action_count <- as.integer(snapshot$action_count %||% 0L)
    action_count <- max(0L, min(action_count, length(action_history)))
    if (action_count > 0L) {
      action <- action_history[[action_count]]
      chatter <- paste(trimws(as.character(action$chatter %||% action$table_talk %||% "")), collapse = " ")
      chatter <- strip_chatter_speaker_prefix(chatter)
      if (nzchar(chatter)) {
        speaker <- as_scalar_chr(action$player_name, action$player_id %||% "")
        texts <- c(texts, if (nzchar(speaker)) paste(speaker, "says:", chatter) else chatter)
      }
    }

    snap_type <- as_scalar_chr(snapshot$snapshot_type, "")
    if (identical(snap_type, "hand_end")) {
      texts <- c(texts, winner_announcement_text(hand))
    }
    if (identical(snap_type, "elimination")) {
      texts <- c(texts, elimination_announcement_text(snapshot, hand))
    }

    max(vapply(texts, estimated_speech_ms, integer(1)), 0L)
  }

  intro_players_from_replay <- function(replay) {
    first_hand <- replay$hand_log[[1]] %||% list()
    starters <- first_hand$starting_stack_summary %||% list()
    if (!is.list(starters) || length(starters) == 0) {
      starters <- replay$players %||% list()
    }

    if (!is.list(starters) || length(starters) == 0) {
      return(data.frame(
        player_id = character(0),
        player_name = character(0),
        seat = numeric(0),
        stack = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    rows <- lapply(starters, function(p) {
      data.frame(
        player_id = as_scalar_chr(p$player_id, p$id %||% ""),
        player_name = as_scalar_chr(p$player_name, p$name %||% p$player_id %||% "Player"),
        seat = as_scalar_num(p$seat),
        stack = as_scalar_num(p$stack),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    out <- out[order(out$seat, out$player_name), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  intro_chatter_lookup <- function(replay) {
    lookup <- list()
    for (hand in replay$hand_log %||% list()) {
      actions <- hand$action_history %||% list()
      if (!is.list(actions) || length(actions) == 0) next
      for (a in actions) {
        chatter <- paste(trimws(as.character(a$chatter %||% a$table_talk %||% "")), collapse = " ")
        chatter <- strip_chatter_speaker_prefix(chatter)
        if (!nzchar(chatter)) next

        keys <- unique(c(
          paste0("pid:", as_scalar_chr(a$player_id, "")),
          paste0("name:", as_scalar_chr(a$player_name, ""))
        ))
        keys <- keys[nzchar(sub("^[^:]+:", "", keys))]
        for (key in keys) {
          lookup[[key]] <- unique(c(lookup[[key]] %||% character(0), chatter))
        }
      }
    }
    lookup
  }

  intro_comment_for_player <- function(player, lookup) {
    pid_key <- paste0("pid:", as_scalar_chr(player$player_id, ""))
    name_key <- paste0("name:", as_scalar_chr(player$player_name, ""))
    lines <- unique(c(lookup[[pid_key]] %||% character(0), lookup[[name_key]] %||% character(0)))
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) return("")
    sample(lines, size = 1)
  }

  intro_announcement_text <- function(player) {
    paste0(
      "In seat ", as_scalar_chr(player$seat, "?"),
      ", we have ", as_scalar_chr(player$player_name, "Player"),
      ", who enters the day with ", as_scalar_chr(player$stack, "unknown"),
      " chips."
    )
  }

  intro_player_hold_ms <- function(player, comment = "") {
    text <- intro_announcement_text(player)
    if (nzchar(comment)) {
      text <- paste(text, as_scalar_chr(player$player_name, "Player"), "says:", comment)
    } else {
      text <- paste(text, "Bot signal.")
    }
    max(2600L, estimated_speech_ms(text, extra_ms = 1100L))
  }

  build_intro_table_ui <- function(players, active_index = NA_integer_) {
    if (!is.data.frame(players) || nrow(players) == 0) {
      return(shiny::tags$div(class = "viewer-empty-state", "No players were found for the tournament intro."))
    }

    positions <- table_seat_positions(nrow(players))
    seat_nodes <- Map(function(i, pos) {
      seat_pos <- display_seat_position(pos)
      if (is.null(seat_pos)) return(NULL)

      p <- players[i, , drop = FALSE]
      classes <- c("viewer-seat", "viewer-intro-seat")
      if (identical(as.integer(i), as.integer(active_index))) {
        classes <- c(classes, "viewer-intro-seat-active")
      }

      shiny::tags$div(
        class = paste(classes, collapse = " "),
        style = paste0("left:", round(seat_pos$left, 1), "%; top:", round(seat_pos$top, 1), "%;"),
        shiny::tags$div(
          class = "viewer-seat-head",
          shiny::tags$div(class = "viewer-seat-name", as_scalar_chr(p$player_name, "Player")),
          shiny::tags$div(class = "viewer-seat-seatno", paste0("Seat ", as_scalar_chr(p$seat, "?")))
        ),
        shiny::tags$div(class = "viewer-seat-stack", paste0("Stack ", as_scalar_chr(p$stack, "?"))),
        shiny::tags$div(class = "viewer-status-pill viewer-status-active", "Ready")
      )
    }, seq_len(nrow(players)), positions)

    active_player <- if (!is.na(active_index) && active_index >= 1L && active_index <= nrow(players)) {
      players[active_index, , drop = FALSE]
    } else {
      NULL
    }

    shiny::tags$div(
      class = "viewer-replay-layout",
      shiny::tags$div(
        class = "viewer-table-wrap",
        shiny::tags$div(class = "viewer-table-felt"),
        shiny::tags$div(
          class = "viewer-table-center",
          shiny::tags$div(class = "viewer-table-street", "TABLE INTRO"),
          shiny::tags$div(
            class = "viewer-center-brand",
            shiny::tags$div(class = "viewer-center-brand-top", "HWS"),
            shiny::tags$div(class = "viewer-center-brand-main", "Poker Capstone")
          ),
          shiny::tags$div(
            class = "viewer-snapshot-message",
            if (is.null(active_player)) {
              "Press Play Intro to introduce the table."
            } else {
              intro_announcement_text(active_player)
            }
          )
        ),
        seat_nodes
      )
    )
  }

  build_player_seat_ui <- function(player, snapshot, hand, pos, reveal_player_ids = NULL) {
    seat_pos <- display_seat_position(pos)
    if (is.null(seat_pos)) {
      return(NULL)
    }

    is_acting <- identical(as_scalar_num(player$seat), as_scalar_num(snapshot$acting_seat))
    hole_cards <- player$hole_cards %||% character(0)
    badges <- player_role_badges(player, snapshot)
    classes <- c("viewer-seat")
    chip_label <- chip_action_label(hand, snapshot, player)
    committed_round <- as.numeric(player$committed_this_round %||% 0)
    is_folded <- isTRUE(player$folded)
    player_id <- as.character(player$player_id %||% "")
    reveal_all <- is.null(reveal_player_ids) || identical(reveal_player_ids, "__ALL__")
    showdown_frame <- as_scalar_chr(snapshot$snapshot_type, "") %in% c("showdown", "hand_end", "elimination")
    forced_showdown_reveal <- showdown_frame && player_id %in% showdown_player_ids(hand)
    show_hole_cards <- forced_showdown_reveal || isTRUE(reveal_all) || player_id %in% (reveal_player_ids %||% character(0))

    if (is_acting) {
      classes <- c(classes, "viewer-seat-acting")
    }
    if (is_folded) {
      classes <- c(classes, "viewer-seat-folded", "viewer-seat-folded-compact")
    }
    if (identical(player$status %||% "", "eliminated")) {
      classes <- c(classes, "viewer-seat-eliminated")
    }

    shiny::tags$div(
      class = paste(classes, collapse = " "),
      style = paste0("left:", round(seat_pos$left, 1), "%; top:", round(seat_pos$top, 1), "%;"),
      if (!is.null(chip_label) && (!is.na(committed_round) && committed_round > 0 || identical(chip_label, "Check"))) {
        shiny::tags$div(
          class = "viewer-seat-bettext",
          if (identical(chip_label, "Check")) chip_label else paste0(chip_label, " ", as_scalar_chr(player$committed_this_round, 0))
        )
      },
      shiny::tags$div(
        class = "viewer-seat-head",
        shiny::tags$div(class = "viewer-seat-name", as_scalar_chr(player$name, as_scalar_chr(player$player_id, "Player"))),
        shiny::tags$div(class = "viewer-seat-seatno", paste0("Seat ", as_scalar_chr(player$seat, "?")))
      ),
      shiny::tags$div(class = "viewer-seat-stack", paste0("Stack ", as_scalar_chr(player$stack, "?"))),
      if (!is_folded) shiny::tags$div(class = "viewer-seat-roles", badges),
      if (!is_folded) shiny::tags$div(class = paste("viewer-status-pill", player_status_class(player)), player_status_label(player)),
      if (!is_folded) shiny::tags$div(
        class = "viewer-hole-cards",
        build_card_tags(hole_cards, hidden = !isTRUE(show_hole_cards), size = "small")
      )
    )
  }

  build_table_ui <- function(snapshot, hand, reveal_player_ids = NULL, broadcast_mode = FALSE) {
    if (is.null(snapshot)) {
      return(shiny::tags$div(class = "viewer-empty-state", "No snapshots were found for this hand."))
    }

    players <- snapshot_players(snapshot, hand, include_eliminated = TRUE)
    active_hand_ids <- hand_player_ids(hand)
    if (length(active_hand_ids) > 0) {
      keep_idx <- vapply(
        players,
        function(p) as.character(p$player_id %||% "") %in% active_hand_ids,
        logical(1)
      )
      players <- players[keep_idx]
    }
    positions <- table_seat_positions(length(players))
    board <- snapshot$board %||% hand$board %||% character(0)
    message <- snapshot$message %||% ""
    pot_value <- display_pot_value(snapshot, hand)

    seat_nodes <- Map(
      f = function(player, pos) build_player_seat_ui(player, snapshot, hand, pos, reveal_player_ids = reveal_player_ids),
      player = players,
      pos = positions
    )

    chip_nodes <- Map(
      f = function(player, pos) {
        chip_label <- chip_action_label(hand, snapshot, player)
        committed_round <- as.numeric(player$committed_this_round %||% 0)
        if (is.null(chip_label) || is.na(committed_round) || committed_round <= 0) {
          return(NULL)
        }

        build_chip_stack_ui(
          amount = committed_round,
          label = chip_label,
          pos = table_chip_position(pos)
        )
      },
      player = players,
      pos = positions
    )

    pot_chip_node <- build_amount_chip_ui(
      amount = displayed_pot_chip_value(snapshot, hand),
      label = "Pot",
      pos = center_pot_chip_position(),
      extra_class = "viewer-pot-chip"
    )

    shiny::tags$div(
      class = "viewer-replay-layout",
      shiny::tags$div(
        class = "viewer-table-wrap",
        shiny::tags$div(class = "viewer-table-felt"),
        build_winner_banner_ui(snapshot, hand, players, positions),
        build_elimination_banner_ui(snapshot, hand),
        shiny::tags$div(
          class = "viewer-table-center",
          shiny::tags$div(class = "viewer-table-street", toupper(as_scalar_chr(snapshot$street, "UNKNOWN"))),
          build_board_center_ui(board),
          pot_chip_node,
          shiny::tags$div(class = "viewer-snapshot-message", if (identical(message, "")) " " else message)
        ),
        chip_nodes,
        seat_nodes
      ),
      build_chatter_ui(hand, snapshot, broadcast_mode = broadcast_mode)
    )
  }

  action_history_df <- function(hand) {
    ah <- hand$action_history %||% list()
    if (!is.list(ah) || length(ah) == 0) {
      return(data.frame(
        Index = integer(0),
        Street = character(0),
        Seat = integer(0),
        Player = character(0),
        Type = character(0),
        Amount = numeric(0),
        Chatter = character(0),
        stringsAsFactors = FALSE
      ))
    }

    out <- lapply(seq_along(ah), function(i) {
      a <- ah[[i]]
      data.frame(
        Index = i,
        Street = as_scalar_chr(a$street),
        Seat = as_scalar_num(a$seat),
        Player = as_scalar_chr(a$player_name, a$player_id %||% ""),
        Type = as_scalar_chr(a$type),
        Amount = as_scalar_num(a$amount, 0),
        Chatter = strip_chatter_speaker_prefix(a$chatter %||% a$table_talk),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, out)
    rownames(out) <- NULL
    out
  }

  hand_summary_text <- function(hand) {
    paste0(
      "Hand ID: ", as_scalar_chr(hand$hand_id, "(unknown)"), "\n",
      "Hand number: ", as_scalar_chr(hand$hand_number, "(unknown)"), "\n",
      "Button seat: ", as_scalar_chr(hand$button_seat, "(unknown)"), "\n",
      "Small blind seat: ", as_scalar_chr(hand$small_blind_seat, "(unknown)"), "\n",
      "Big blind seat: ", as_scalar_chr(hand$big_blind_seat, "(unknown)"), "\n",
      "Final street: ", as_scalar_chr(hand$final_street, hand$street %||% "(unknown)"), "\n",
      "Board: ", format_cards(hand$board %||% character(0)), "\n",
      "Action count: ", as_scalar_chr(hand$action_count, length(hand$action_history %||% list())), "\n",
      "Snapshots stored: ", length(get_hand_snapshots(hand))
    )
  }

  snapshot_summary_text <- function(snapshot, step_index = NA_integer_) {
    if (is.null(snapshot)) {
      return("No snapshots were found for this hand.")
    }

    acting_seat <- snapshot$acting_seat %||% snapshot$hand_state$acting_seat %||% NA
    street <- snapshot$street %||% snapshot$hand_state$street %||% ""
    pot <- snapshot$pot %||% snapshot$hand_state$pot %||% NA
    current_bet <- snapshot$current_bet %||% snapshot$hand_state$current_bet %||% NA
    board <- snapshot$board %||% snapshot$hand_state$board %||% character(0)
    msg <- snapshot$message %||% snapshot$label %||% ""

    paste0(
      "Step: ", step_index, "\n",
      "Street: ", as_scalar_chr(street, "(unknown)"), "\n",
      "Board: ", format_cards(board), "\n",
      "Pot: ", as_scalar_chr(pot, "(unknown)"), "\n",
      "Current bet: ", as_scalar_chr(current_bet, "(unknown)"), "\n",
      "Acting seat: ", as_scalar_chr(acting_seat, "(none)"),
      if (!identical(as_scalar_chr(msg, ""), "")) paste0("\nMessage: ", msg) else ""
    )
  }

  overview_players_df <- function(replay) {
    players <- replay$players %||% list()

    # If tournament-level players are missing, fall back to last hand summary.
    if (length(players) == 0 && length(replay$hand_log) > 0) {
      last_hand <- replay$hand_log[[length(replay$hand_log)]]
      ss <- last_hand$stack_summary %||% list()
      if (length(ss) > 0) {
        out <- lapply(ss, function(p) {
          data.frame(
            Seat = as_scalar_num(p$seat),
            Player = as_scalar_chr(p$player_name, p$player_id %||% ""),
            Stack = as_scalar_num(p$stack),
            Status = as_scalar_chr(p$status),
            FinishingPlace = as_scalar_num(p$finishing_place),
            stringsAsFactors = FALSE
          )
        })
        out <- do.call(rbind, out)
        out <- out[order(out$Seat), , drop = FALSE]
        rownames(out) <- NULL
        return(out)
      }
    }

    if (length(players) == 0) {
      return(data.frame(
        Seat = integer(0),
        Player = character(0),
        Stack = numeric(0),
        Status = character(0),
        FinishingPlace = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    out <- lapply(players, function(p) {
      data.frame(
        Seat = as_scalar_num(p$seat),
        Player = as_scalar_chr(p$name, p$player_id %||% ""),
        Stack = as_scalar_num(p$stack),
        Status = as_scalar_chr(p$status),
        FinishingPlace = as_scalar_num(p$finishing_place),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, out)
    out <- out[order(out$Seat), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  replay_player_roster <- function(replay) {
    roster <- list()

    add_players <- function(players) {
      if (!is.list(players) || length(players) == 0) {
        return(invisible(NULL))
      }
      for (p in players) {
        pid <- as.character(p$player_id %||% "")
        if (identical(pid, "")) next
        if (is.null(roster[[pid]])) {
          roster[[pid]] <<- list(
            player_id = pid,
            player_name = as.character(p$player_name %||% p$name %||% pid),
            seat = as.numeric(p$seat %||% NA_real_)
          )
        }
      }
    }

    add_players(replay$players %||% list())
    for (hand in replay$hand_log %||% list()) {
      add_players(hand$starting_stack_summary %||% list())
      add_players(hand$ending_stack_summary %||% list())
      add_players(hand$stack_summary %||% list())
    }

    roster
  }

  current_hand_stack_df <- function(replay, hand) {
    roster <- replay_player_roster(replay)
    starters <- hand$starting_stack_summary %||% list()
    starter_map <- setNames(
      starters,
      vapply(starters, function(p) as.character(p$player_id %||% ""), character(1))
    )
    elim_order <- as.character(hand$elimination_order_before_hand %||% character(0))

    rows <- lapply(names(roster), function(pid) {
      meta <- roster[[pid]]
      p <- starter_map[[pid]]
      if (is.null(p)) {
        list(
          Player = meta$player_name,
          Seat = meta$seat,
          Chips = 0,
          Status = "eliminated",
          EliminationOrder = match(pid, elim_order)
        )
      } else {
        list(
          Player = as.character(p$player_name %||% p$name %||% meta$player_name),
          Seat = as.numeric(p$seat %||% meta$seat),
          Chips = as.numeric(p$stack %||% 0),
          Status = as.character(p$status %||% "active"),
          EliminationOrder = match(pid, elim_order)
        )
      }
    })

    df <- do.call(rbind, lapply(rows, function(x) {
      data.frame(
        Player = x$Player,
        Seat = x$Seat,
        Chips = x$Chips,
        Status = x$Status,
        EliminationOrder = x$EliminationOrder,
        stringsAsFactors = FALSE
      )
    }))

    df$EliminationOrder[is.na(df$EliminationOrder)] <- Inf
    is_active <- df$Status != "eliminated"
    df <- df[order(!is_active, ifelse(is_active, -df$Chips, -df$EliminationOrder), df$Seat), , drop = FALSE]
    rownames(df) <- NULL
    df$EliminationOrder[df$EliminationOrder == Inf] <- NA
    df
  }

  chip_history_df <- function(replay, max_hand_index) {
    max_hand_index <- max(1L, min(as.integer(max_hand_index), length(replay$hand_log)))
    hands <- replay$hand_log[seq_len(max_hand_index)]

    all_players <- list()
    for (hand in hands) {
      starters <- hand$starting_stack_summary %||% list()
      for (p in starters) {
        pid <- as.character(p$player_id %||% "")
        if (!identical(pid, "") && is.null(all_players[[pid]])) {
          all_players[[pid]] <- list(
            player_id = pid,
            player_name = as.character(p$player_name %||% p$player_id %||% ""),
            seat = as.numeric(p$seat %||% NA_real_)
          )
        }
      }

      enders <- hand$ending_stack_summary %||% hand$stack_summary %||% list()
      for (p in enders) {
        pid <- as.character(p$player_id %||% "")
        if (!identical(pid, "") && is.null(all_players[[pid]])) {
          all_players[[pid]] <- list(
            player_id = pid,
            player_name = as.character(p$player_name %||% p$player_id %||% ""),
            seat = as.numeric(p$seat %||% NA_real_)
          )
        }
      }
    }

    if (length(all_players) == 0) {
      return(data.frame(
        Hand = integer(0),
        PlayerId = character(0),
        Player = character(0),
        Seat = numeric(0),
        Chips = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    rows <- list()
    for (i in seq_along(hands)) {
      hand <- hands[[i]]
      starters <- hand$starting_stack_summary %||% list()
      starter_map <- setNames(starters, vapply(starters, function(p) as.character(p$player_id %||% ""), character(1)))

      for (pid in names(all_players)) {
        player_meta <- all_players[[pid]]
        p <- starter_map[[pid]]
        chips <- if (!is.null(p)) as.numeric(p$stack %||% NA_real_) else 0
        rows[[length(rows) + 1L]] <- data.frame(
          Hand = as.integer(hand$hand_number %||% i),
          PlayerId = player_meta$player_id,
          Player = player_meta$player_name,
          Seat = player_meta$seat,
          Chips = chips,
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(rows) == 0) {
      return(data.frame(
        Hand = integer(0),
        PlayerId = character(0),
        Player = character(0),
        Seat = numeric(0),
        Chips = numeric(0),
        stringsAsFactors = FALSE
      ))
    }

    out <- do.call(rbind, rows)
    out <- out[order(out$Hand, out$Seat, out$Player), , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  # -----------------------------
  # Prepare replay data
  # -----------------------------
  replay <- normalize_replay_data(log_data)

  if (length(replay$hand_log) == 0) {
    stop("The supplied log_data contains no hands in $hand_log.")
  }

  has_tv_tags <- all(vapply(replay$hand_log, function(hand) !is.null(hand$for_tv), logical(1)))
  if (!has_tv_tags && exists("annotate_replay_for_tv", mode = "function")) {
    replay <- tryCatch(
      annotate_replay_for_tv(replay, include_equity = FALSE),
      error = function(e) replay
    )
  }

  intro_players <- intro_players_from_replay(replay)
  intro_chatter <- intro_chatter_lookup(replay)

  hand_choices <- stats::setNames(
    as.character(seq_along(replay$hand_log)),
    vapply(replay$hand_log, hand_label, character(1))
  )
  first_hand_choice <- unname(hand_choices[[1]])

  # -----------------------------
  # UI
  # -----------------------------
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .viewer-replay-layout { margin-top: 12px; }
        .viewer-broadcast-banner {
          margin: 8px 0 14px;
          padding: 12px 16px;
          border-radius: 18px;
          background: linear-gradient(90deg, #173a60 0%, #22588f 55%, #b1862e 100%);
          color: #f7f3ea;
          box-shadow: 0 10px 24px rgba(0,0,0,0.16);
        }
        .viewer-broadcast-title {
          font-size: 12px;
          letter-spacing: 1.3px;
          text-transform: uppercase;
          font-weight: 800;
          opacity: 0.85;
        }
        .viewer-broadcast-text {
          margin-top: 4px;
          font-size: 18px;
          font-weight: 800;
        }
        .viewer-broadcast-sub {
          margin-top: 4px;
          font-size: 12px;
          font-weight: 600;
          opacity: 0.9;
        }
        .viewer-table-wrap {
          position: relative;
          min-height: 700px;
          border-radius: 28px;
          background:
            radial-gradient(circle at center, rgba(255,255,255,0.08), rgba(255,255,255,0) 42%),
            linear-gradient(180deg, #153d30 0%, #0c261e 100%);
          box-shadow: inset 0 0 0 2px rgba(255,255,255,0.08), 0 18px 40px rgba(0,0,0,0.18);
          overflow: hidden;
        }
        .viewer-table-felt {
          position: absolute;
          left: 50%;
          top: 50%;
          width: 58%;
          height: 43%;
          transform: translate(-50%, -50%);
          border-radius: 999px;
          background: radial-gradient(circle at center, #1f7a5c 0%, #155741 58%, #0e3a2d 100%);
          border: 14px solid #7a5732;
          box-shadow: inset 0 0 0 2px rgba(255,255,255,0.12), 0 10px 24px rgba(0,0,0,0.28);
        }
        .viewer-table-center {
          position: absolute;
          left: 50%;
          top: 50%;
          width: 44%;
          transform: translate(-50%, -50%);
          text-align: center;
          color: #f4f1e8;
          z-index: 2;
        }
        .viewer-winner-banner {
          position: absolute;
          left: 50%;
          top: 17%;
          transform: translateX(-50%);
          min-width: 260px;
          max-width: 540px;
          padding: 12px 18px;
          border-radius: 18px;
          background: linear-gradient(180deg, rgba(251,245,221,0.98) 0%, rgba(241,216,150,0.98) 100%);
          color: #3d2d0f;
          text-align: center;
          box-shadow: 0 12px 28px rgba(0,0,0,0.22);
          z-index: 4;
        }
        .viewer-winner-title {
          font-size: 11px;
          letter-spacing: 1.3px;
          text-transform: uppercase;
          font-weight: 800;
          opacity: 0.8;
        }
        .viewer-winner-names {
          margin-top: 4px;
          font-size: 22px;
          font-weight: 800;
          line-height: 1.1;
        }
        .viewer-winner-payout {
          margin-top: 5px;
          font-size: 12px;
          font-weight: 700;
          opacity: 0.9;
        }
        .viewer-elimination-banner {
          position: absolute;
          left: 50%;
          top: 18%;
          transform: translateX(-50%);
          min-width: 280px;
          max-width: 580px;
          padding: 14px 20px;
          border-radius: 18px;
          background: linear-gradient(180deg, rgba(66,18,18,0.96) 0%, rgba(27,8,8,0.96) 100%);
          color: #f7ebe0;
          text-align: center;
          box-shadow: 0 14px 30px rgba(0,0,0,0.30);
          z-index: 4;
        }
        .viewer-elimination-title {
          font-size: 11px;
          letter-spacing: 1.4px;
          text-transform: uppercase;
          font-weight: 800;
          opacity: 0.82;
        }
        .viewer-elimination-names {
          margin-top: 5px;
          font-size: 24px;
          font-weight: 800;
          line-height: 1.08;
        }
        .viewer-elimination-sub {
          margin-top: 6px;
          font-size: 12px;
          font-weight: 700;
          opacity: 0.88;
        }
        .viewer-table-street {
          font-size: 12px;
          letter-spacing: 2px;
          font-weight: 700;
          opacity: 0.85;
          margin-bottom: 10px;
        }
        .viewer-board-cards, .viewer-hole-cards {
          display: flex;
          gap: 8px;
          justify-content: center;
          flex-wrap: wrap;
        }
        .viewer-center-brand {
          display: inline-flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          min-width: 180px;
          min-height: 84px;
          border-radius: 18px;
          background: rgba(8, 18, 16, 0.30);
          box-shadow: inset 0 0 0 1px rgba(255,255,255,0.08);
          padding: 10px 16px;
        }
        .viewer-center-brand-top {
          font-size: 14px;
          letter-spacing: 4px;
          font-weight: 800;
          opacity: 0.85;
        }
        .viewer-center-brand-main {
          margin-top: 4px;
          font-size: 22px;
          font-weight: 800;
          line-height: 1.05;
        }
        .viewer-card {
          background: #fbfaf7;
          border: 1px solid rgba(25,25,25,0.15);
          border-radius: 10px;
          box-shadow: 0 3px 10px rgba(0,0,0,0.12);
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          align-items: center;
          font-weight: 700;
        }
        .viewer-card-normal { width: 52px; height: 72px; padding: 6px 0; font-size: 20px; }
        .viewer-card-small { width: 38px; height: 52px; padding: 4px 0; font-size: 15px; }
        .viewer-card-hidden {
          background: linear-gradient(135deg, #1c3557 0%, #355f90 100%);
          color: #f7fbff;
          justify-content: center;
          font-size: 18px;
        }
        .viewer-card-red { color: #ba2d2d; }
        .viewer-card-black { color: #1d2430; }
        .viewer-card-rank { line-height: 1; }
        .viewer-card-suit { line-height: 1; }
        .viewer-pot-row {
          display: flex;
          justify-content: center;
          gap: 10px;
          margin-top: 58px;
          flex-wrap: wrap;
        }
        .viewer-metric-box {
          min-width: 94px;
          padding: 8px 12px;
          border-radius: 12px;
          background: rgba(8, 18, 16, 0.38);
          box-shadow: inset 0 0 0 1px rgba(255,255,255,0.08);
        }
        .viewer-metric-label {
          font-size: 10px;
          text-transform: uppercase;
          letter-spacing: 1px;
          opacity: 0.72;
        }
        .viewer-metric-value {
          font-size: 18px;
          font-weight: 700;
        }
        .viewer-snapshot-message {
          margin-top: 14px;
          min-height: 22px;
          font-size: 14px;
          font-weight: 600;
        }
        .viewer-seat {
          position: absolute;
          transform: translate(-50%, -50%);
          width: 180px;
          padding: 12px;
          border-radius: 18px;
          background: rgba(247, 243, 233, 0.96);
          color: #1c2228;
          box-shadow: 0 10px 28px rgba(0,0,0,0.20);
          z-index: 3;
        }
        .viewer-seat-bettext {
          position: absolute;
          left: 50%;
          top: -18px;
          transform: translateX(-50%);
          padding: 4px 9px;
          border-radius: 999px;
          background: rgba(246, 241, 228, 0.98);
          color: #382a16;
          font-size: 11px;
          font-weight: 800;
          box-shadow: 0 6px 14px rgba(0,0,0,0.16);
          white-space: nowrap;
        }
        .viewer-chip-bet {
          position: absolute;
          transform: translate(-50%, -50%);
          padding: 8px 10px 9px;
          border-radius: 18px;
          background: linear-gradient(180deg, rgba(14,53,41,0.96) 0%, rgba(10,34,27,0.98) 100%);
          color: #f4ead6;
          font-size: 11px;
          font-weight: 800;
          border: 1px solid rgba(234, 212, 166, 0.25);
          box-shadow: 0 10px 20px rgba(0,0,0,0.22);
          white-space: nowrap;
          min-width: 84px;
          text-align: center;
          z-index: 2;
          animation: viewer-chip-float 2.8s ease-in-out infinite;
        }
        .viewer-chip-caption {
          margin-bottom: 6px;
          font-size: 10px;
          letter-spacing: 0.4px;
          text-transform: uppercase;
          color: #f7e6bf;
        }
        .viewer-chip-stack-row {
          display: flex;
          align-items: flex-end;
          justify-content: center;
          gap: 4px;
          min-height: 20px;
        }
        .viewer-chip-stack {
          position: relative;
          width: 14px;
          height: 16px;
        }
        .viewer-chip-disc {
          position: absolute;
          left: 0;
          width: 14px;
          height: 6px;
          border-radius: 999px;
          border: 1px solid rgba(20,20,20,0.28);
          box-shadow: inset 0 1px 0 rgba(255,255,255,0.3);
        }
        .viewer-chip-count {
          position: absolute;
          right: -10px;
          top: -3px;
          font-size: 8px;
          font-weight: 700;
          color: #f0e2bf;
        }
        .viewer-chip-denom {
          position: absolute;
          left: 50%;
          bottom: -11px;
          transform: translateX(-50%);
          font-size: 7px;
          font-weight: 700;
          color: #c6b692;
        }
        .viewer-chip-token {
          position: absolute;
          transform: translate(-50%, -50%);
          min-width: 72px;
          padding: 8px 10px;
          border-radius: 999px;
          background: radial-gradient(circle at 35% 30%, #f9e8b0 0%, #ddb74d 45%, #a97618 100%);
          color: #3f2c0b;
          text-align: center;
          box-shadow: 0 8px 16px rgba(0,0,0,0.18), inset 0 1px 0 rgba(255,255,255,0.35);
          border: 2px solid rgba(92, 61, 8, 0.35);
          z-index: 2;
          animation: viewer-chip-float 3.4s ease-in-out infinite;
        }
        .viewer-chip-token-top {
          font-size: 9px;
          letter-spacing: 0.8px;
          text-transform: uppercase;
          font-weight: 800;
          opacity: 0.82;
        }
        .viewer-chip-token-value {
          margin-top: 2px;
          font-size: 15px;
          font-weight: 900;
          line-height: 1;
        }
        .viewer-pot-chip {
          background: radial-gradient(circle at 35% 30%, #fff1c5 0%, #e4c96d 45%, #b78b2b 100%);
          z-index: 3;
        }
        @keyframes viewer-chip-float {
          0% { transform: translate(-50%, -50%); }
          50% { transform: translate(-50%, calc(-50% - 3px)); }
          100% { transform: translate(-50%, -50%); }
        }
        .viewer-seat-acting {
          box-shadow: 0 0 0 3px #ffd166, 0 12px 30px rgba(0,0,0,0.24);
        }
        .viewer-seat-folded {
          opacity: 0.76;
        }
        .viewer-seat-folded-compact {
          width: 128px;
          padding: 8px 10px;
        }
        .viewer-seat-folded-compact .viewer-seat-head {
          display: block;
        }
        .viewer-seat-folded-compact .viewer-seat-name {
          font-size: 13px;
        }
        .viewer-seat-folded-compact .viewer-seat-seatno {
          margin-top: 2px;
        }
        .viewer-seat-folded-compact .viewer-seat-stack {
          margin-top: 4px;
          font-size: 11px;
        }
        .viewer-seat-eliminated {
          opacity: 0.58;
        }
        .viewer-intro-seat {
          transition: transform 220ms ease, box-shadow 220ms ease, border-color 220ms ease;
        }
        .viewer-intro-seat-active {
          transform: translate(-50%, -50%) scale(1.1);
          border-color: #f1c94c;
          box-shadow: 0 0 0 4px rgba(241, 201, 76, 0.34), 0 16px 34px rgba(20, 27, 38, 0.24);
          z-index: 5;
        }
        .viewer-seat-head {
          display: flex;
          justify-content: space-between;
          gap: 8px;
          align-items: baseline;
        }
        .viewer-seat-name {
          font-size: 15px;
          font-weight: 700;
        }
        .viewer-seat-seatno {
          font-size: 11px;
          color: #54606e;
        }
        .viewer-seat-roles {
          display: flex;
          gap: 6px;
          margin-top: 8px;
          min-height: 22px;
          flex-wrap: wrap;
        }
        .viewer-role {
          font-size: 10px;
          font-weight: 700;
          padding: 3px 7px;
          border-radius: 999px;
        }
        .viewer-role-button { background: #1f3558; color: #fff; }
        .viewer-role-sb { background: #d7e7f8; color: #17385d; }
        .viewer-role-bb { background: #f6ddb3; color: #5a3d10; }
        .viewer-status-pill {
          display: inline-block;
          margin-top: 8px;
          padding: 4px 8px;
          border-radius: 999px;
          font-size: 11px;
          font-weight: 700;
        }
        .viewer-status-active { background: #def4e8; color: #1d6b45; }
        .viewer-status-folded { background: #ece7e2; color: #6a5f55; }
        .viewer-status-allin { background: #f8dfdf; color: #9a2d2d; }
        .viewer-status-eliminated { background: #ddd5d5; color: #6b4f4f; }
        .viewer-seat-stack {
          margin-top: 6px;
          font-size: 12px;
        }
        .viewer-hole-cards {
          margin-top: 10px;
          justify-content: flex-start;
        }
        .viewer-empty-state {
          padding: 24px;
          border-radius: 16px;
          background: #f3efe7;
          color: #4e5a65;
        }
        .viewer-chatter-panel {
          margin-top: 12px;
          padding: 12px 14px;
          border-radius: 16px;
          background: #171f29;
          color: #f4efe3;
          box-shadow: 0 10px 24px rgba(0,0,0,0.16);
        }
        .viewer-chatter-title {
          font-size: 11px;
          letter-spacing: 1.2px;
          text-transform: uppercase;
          font-weight: 800;
          color: #e7c36a;
          margin-bottom: 8px;
        }
        .viewer-chatter-row {
          display: flex;
          gap: 10px;
          align-items: baseline;
          padding: 7px 0;
          border-top: 1px solid rgba(255,255,255,0.10);
        }
        .viewer-chatter-row:first-of-type {
          border-top: 0;
          padding-top: 0;
        }
        .viewer-chatter-speaker {
          min-width: 140px;
          max-width: 190px;
          font-size: 12px;
          font-weight: 800;
          color: #f5d889;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .viewer-chatter-text {
          flex: 1;
          font-size: 14px;
          font-weight: 650;
          line-height: 1.35;
        }
      ")),
      shiny::tags$script(shiny::HTML("
        (function() {
          function playToneSequence(kind) {
            var AudioContext = window.AudioContext || window.webkitAudioContext;
            if (!AudioContext) return;

            var ctx = window.viewerAudioContext || new AudioContext();
            window.viewerAudioContext = ctx;
            if (ctx.state === 'suspended') {
              ctx.resume();
            }

            var now = ctx.currentTime;
            var notes = kind === 'elimination'
              ? [
                  { f: 392, endF: 330, t: 0.00, d: 0.34 },
                  { f: 330, endF: 277, t: 0.38, d: 0.34 },
                  { f: 277, endF: 220, t: 0.76, d: 0.46 }
                ]
              : kind === 'bot'
                ? [
                    { f: 880, t: 0.00, d: 0.08 },
                    { f: 1175, t: 0.14, d: 0.08 },
                    { f: 988, t: 0.28, d: 0.08 },
                    { f: 1319, t: 0.42, d: 0.10 }
                  ]
              : kind === 'chatter'
                ? [
                    { f: 1568, t: 0.00, d: 0.055 },
                    { f: 2093, t: 0.07, d: 0.075 }
                  ]
              : kind === 'level'
                ? [
                  { f: 740, t: 0.00, d: 0.11 },
                  { f: 740, t: 0.24, d: 0.11 },
                  { f: 740, t: 0.48, d: 0.11 },
                  { f: 980, t: 0.76, d: 0.18 }
                ]
                : [
                  { f: 392, t: 0.00, d: 0.08 },
                  { f: 587, t: 0.09, d: 0.08 },
                  { f: 784, t: 0.24, d: 0.08 },
                  { f: 587, t: 0.33, d: 0.08 },
                  { f: 880, t: 0.48, d: 0.10 },
                  { f: 1175, t: 0.62, d: 0.18 }
                ];

            notes.forEach(function(note) {
              var osc = ctx.createOscillator();
              var gain = ctx.createGain();
              osc.type = kind === 'elimination' ? 'sawtooth' : 'sine';
              osc.frequency.setValueAtTime(note.f, now + note.t);
              if (kind === 'elimination' && note.endF) {
                osc.frequency.exponentialRampToValueAtTime(note.endF, now + note.t + note.d);
              }
              gain.gain.setValueAtTime(0.0001, now + note.t);
              gain.gain.exponentialRampToValueAtTime(kind === 'level' ? 0.07 : kind === 'chatter' ? 0.045 : 0.10, now + note.t + 0.015);
              gain.gain.exponentialRampToValueAtTime(0.0001, now + note.t + note.d);
              osc.connect(gain);
              gain.connect(ctx.destination);
              osc.start(now + note.t);
              osc.stop(now + note.t + note.d + 0.04);
            });
          }

          function speakChatter(message) {
            if (!('speechSynthesis' in window) || !window.SpeechSynthesisUtterance) return;

            function availableVoices() {
              var voices = window.speechSynthesis.getVoices();
              window.viewerSpeechVoices = voices;
              return voices;
            }

            function shuffledIndices(n) {
              var out = Array.from({ length: n }, function(_, i) { return i; });
              for (var i = out.length - 1; i > 0; i--) {
                var j = Math.floor(Math.random() * (i + 1));
                var tmp = out[i];
                out[i] = out[j];
                out[j] = tmp;
              }
              return out;
            }

            function getBotVoiceProfiles(voices) {
              var botNames = [
                'Rando', 'Aggro', 'PrePlanner', 'GetAlong', 'Da streets',
                'ScardyBot', 'Confused', 'MoreConfused', 'LabBot', 'LabBot2',
                'Jaymon', 'Joel', 'Nikola', 'Mehdi', 'Nate',
                'Mady', 'Tara', 'Lucy', 'Siena', 'Ruth',
                'King Rikki', 'Hatch Bot', 'Gearan up to beat you', 'Sir Hu McBluff', 'Talmage Bot',
                'Bot inSpector', 'Khan you fold?', 'Fordeing Ahead', 'Maurice Hawkins', 'Biermann'
              ];
              var aliasMap = { 'Biermann Bot': 'Biermann' };
              var pitchCycle = [0.86, 0.94, 1.02, 1.10, 1.18];
              var rateCycle = [0.94, 0.98, 1.02, 1.06];

              if (!voices || !voices.length) return {};
              if (window.viewerBotVoiceProfiles && window.viewerBotVoiceProfileCount === voices.length) {
                return window.viewerBotVoiceProfiles;
              }

              var botVoiceIndices = voices.map(function(_, i) { return i; }).filter(function(i) {
                return i >= 1 && i <= 21;
              });
              if (!botVoiceIndices.length) {
                botVoiceIndices = voices.map(function(_, i) { return i; });
              }

              var voiceOrder = [];
              while (voiceOrder.length < botNames.length) {
                voiceOrder = voiceOrder.concat(shuffledIndices(botVoiceIndices.length).map(function(i) {
                  return botVoiceIndices[i];
                }));
              }

              var profiles = {};
              botNames.forEach(function(name, i) {
                profiles[name] = {
                  voiceIndex: voiceOrder[i],
                  pitch: pitchCycle[i % pitchCycle.length],
                  rate: rateCycle[i % rateCycle.length]
                };
              });

              Object.keys(aliasMap).forEach(function(alias) {
                profiles[alias] = profiles[aliasMap[alias]];
              });

              window.viewerBotVoiceProfiles = profiles;
              window.viewerBotVoiceProfileCount = voices.length;
              return profiles;
            }

            var text = message && message.text ? String(message.text) : '';
            var speaker = message && message.speaker ? String(message.speaker) : '';
            text = text.replace(/\\s+/g, ' ').trim();
            speaker = speaker.replace(/\\s+/g, ' ').trim();
            if (!text) return;

            if (text.length > 260) {
              text = text.slice(0, 257) + '...';
            }

            var utterance = new SpeechSynthesisUtterance(speaker ? speaker + ' says: ' + text : text);
            var voices = availableVoices();
            if (!voices.length && !message.__voiceRetry) {
              window.setTimeout(function() {
                message.__voiceRetry = true;
                speakChatter(message);
              }, 250);
              return;
            }

            var speakerAlias = speaker.replace(/\\s+Bot$/, '');
            var voiceProfiles = getBotVoiceProfiles(voices);
            var profile = Object.prototype.hasOwnProperty.call(voiceProfiles, speaker)
              ? voiceProfiles[speaker]
              : Object.prototype.hasOwnProperty.call(voiceProfiles, speakerAlias)
                ? voiceProfiles[speakerAlias]
                : null;
            if (profile && voices[profile.voiceIndex]) {
              utterance.voice = voices[profile.voiceIndex];
            }
            utterance.rate = profile ? profile.rate : 1.02;
            utterance.pitch = profile ? profile.pitch : 1.05;
            utterance.volume = 0.95;

            if (!message || !message.queue) {
              window.speechSynthesis.cancel();
            }
            window.setTimeout(function() {
              window.speechSynthesis.speak(utterance);
            }, 180);
          }

          function speakAnnouncement(message) {
            if (!('speechSynthesis' in window) || !window.SpeechSynthesisUtterance) return;

            var text = message && message.text ? String(message.text) : '';
            text = text.replace(/\\s+/g, ' ').trim();
            if (!text) return;

            var voices = window.speechSynthesis.getVoices();
            window.viewerSpeechVoices = voices;
            if (!voices.length && !message.__voiceRetry) {
              window.setTimeout(function() {
                message.__voiceRetry = true;
                speakAnnouncement(message);
              }, 250);
              return;
            }

            var utterance = new SpeechSynthesisUtterance(text);
            if (voices[0]) {
              utterance.voice = voices[0];
            }
            utterance.rate = 0.98;
            utterance.pitch = 1.00;
            utterance.volume = 0.96;

            if (!message || !message.queue) {
              window.speechSynthesis.cancel();
            }
            window.setTimeout(function() {
              window.speechSynthesis.speak(utterance);
            }, 180);
          }

          if (window.Shiny) {
            if ('speechSynthesis' in window) {
              window.viewerSpeechVoices = window.speechSynthesis.getVoices();
              window.speechSynthesis.onvoiceschanged = function() {
                window.viewerSpeechVoices = window.speechSynthesis.getVoices();
              };
            }
            Shiny.addCustomMessageHandler('viewer-play-sound', function(message) {
              var kind = message && message.kind;
              playToneSequence(kind === 'level' || kind === 'elimination' || kind === 'chatter' || kind === 'bot' ? kind : 'feature');
            });
            Shiny.addCustomMessageHandler('viewer-speak-chatter', function(message) {
              speakChatter(message || {});
            });
            Shiny.addCustomMessageHandler('viewer-speak-announcement', function(message) {
              speakAnnouncement(message || {});
            });
          }
        })();
      "))
    ),
    shiny::titlePanel("Poker Bot Replay Viewer"),
    shiny::uiOutput("broadcast_banner_ui"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tags$strong("Tournament"),
        shiny::verbatimTextOutput("tournament_info"),
        shiny::hr(),
        shiny::uiOutput("sidebar_controls"),
        width = 3
      ),

      shiny::mainPanel(
        shiny::tabsetPanel(
          id = "main_tab",
          selected = "Overview",
          shiny::tabPanel(
            "Table Intro",
            shiny::uiOutput("table_intro_ui")
          ),
          shiny::tabPanel(
            "Overview",
            shiny::h4("Tournament summary"),
            shiny::h4("Chip stacks at selected hand"),
            shiny::plotOutput("current_stack_plot", height = "320px"),
            shiny::hr(),
            shiny::h4("Standings at selected hand"),
            shiny::tableOutput("overview_players"),
            shiny::hr(),
            shiny::h4("Chip counts through selected hand"),
            shiny::plotOutput("chip_history_plot", height = "320px")
          ),
          shiny::tabPanel(
            "Hand Replay",
            shiny::uiOutput("table_replay_ui")
          ),
          shiny::tabPanel(
            "Action History",
            shiny::h4("Actions"),
            shiny::tableOutput("action_history")
          )
        )
      )
    )
  )

  # -----------------------------
  # Server
  # -----------------------------
  server <- function(input, output, session) {
    is_playing <- shiny::reactiveVal(FALSE)
    is_overview_playing <- shiny::reactiveVal(FALSE)
    broadcast_feature_hand <- shiny::reactiveVal(NA_integer_)
    broadcast_banner <- shiny::reactiveVal(NULL)
    resume_overview_after_feature <- shiny::reactiveVal(FALSE)
    resume_overview_after_intro <- shiny::reactiveVal(FALSE)
    pending_feature_hand_start <- shiny::reactiveVal(NA_integer_)
    last_seen_broadcast_level <- shiny::reactiveVal(as.integer(replay$hand_log[[as.integer(first_hand_choice)]]$level %||% NA_integer_))
    last_elimination_sound_key <- shiny::reactiveVal("")
    last_chatter_sound_key <- shiny::reactiveVal("")
    last_winner_announcement_key <- shiny::reactiveVal("")
    last_replay_advance_key <- shiny::reactiveVal("")
    intro_playing <- shiny::reactiveVal(FALSE)
    intro_active_index <- shiny::reactiveVal(NA_integer_)
    last_intro_advance_key <- shiny::reactiveVal("")
    current_intro_comment <- shiny::reactiveVal("")

    current_hand_choice <- function() {
      selected <- as.character(input$hand_index %||% first_hand_choice)
      valid_choices <- unname(hand_choices)
      if (!selected %in% valid_choices) {
        return(first_hand_choice)
      }
      selected
    }

    hand_is_featured <- function(hand_idx) {
      if (is.na(hand_idx) || hand_idx < 1L || hand_idx > length(replay$hand_log)) {
        return(FALSE)
      }
      isTRUE(replay$hand_log[[hand_idx]]$for_tv)
    }

    hand_feature_reason <- function(hand_idx) {
      if (is.na(hand_idx) || hand_idx < 1L || hand_idx > length(replay$hand_log)) {
        return("")
      }
      as_scalar_chr(replay$hand_log[[hand_idx]]$interest_reasons, "")
    }

    show_broadcast_banner <- function(title, text, subtitle = NULL, duration_ms = 2200L) {
      broadcast_banner(list(
        title = title,
        text = text,
        subtitle = subtitle %||% "",
        expires_at = Sys.time() + (duration_ms / 1000)
      ))
    }

    play_broadcast_sound <- function(kind = c("feature", "level", "elimination", "chatter", "bot")) {
      kind <- match.arg(kind)
      if (!isTRUE(input$broadcast_mode %||% FALSE)) {
        return(invisible(NULL))
      }

      session$sendCustomMessage("viewer-play-sound", list(kind = kind))
      invisible(NULL)
    }

    speak_broadcast_chatter <- function(text, speaker = "", queue = FALSE) {
      if (!isTRUE(input$broadcast_mode %||% FALSE) ||
          !isTRUE(input$broadcast_speak_chatter %||% TRUE)) {
        return(invisible(NULL))
      }

      session$sendCustomMessage(
        "viewer-speak-chatter",
        list(
          text = as_scalar_chr(text, ""),
          speaker = as_scalar_chr(speaker, ""),
          queue = isTRUE(queue)
        )
      )
      invisible(NULL)
    }

    speak_broadcast_announcement <- function(text, queue = TRUE) {
      if (!isTRUE(input$broadcast_mode %||% FALSE) ||
          !isTRUE(input$broadcast_speak_chatter %||% TRUE)) {
        return(invisible(NULL))
      }

      session$sendCustomMessage(
        "viewer-speak-announcement",
        list(
          text = as_scalar_chr(text, ""),
          queue = isTRUE(queue)
        )
      )
      invisible(NULL)
    }

    start_featured_hand <- function(hand_idx) {
      if (is.na(hand_idx) || hand_idx < 1L || hand_idx > length(replay$hand_log)) {
        return(invisible(NULL))
      }

      is_overview_playing(FALSE)
      broadcast_feature_hand(as.integer(hand_idx))
      pending_feature_hand_start(as.integer(hand_idx))
      shiny::updateSelectInput(session, "hand_index", selected = as.character(hand_idx))
      play_broadcast_sound("feature")

      reason_text <- hand_feature_reason(hand_idx)
      show_broadcast_banner(
        title = "TV Spotlight",
        text = hand_label(replay$hand_log[[hand_idx]]),
        subtitle = if (nzchar(reason_text)) reason_text else "Featured hand replay"
      )

      invisible(NULL)
    }

    finish_featured_hand <- function(skipped = FALSE) {
      hand_idx <- suppressWarnings(as.integer(broadcast_feature_hand()))
      if (is.na(hand_idx)) {
        hand_idx <- suppressWarnings(as.integer(input$hand_index %||% NA_integer_))
      }

      next_idx <- if (is.na(hand_idx)) NA_integer_ else hand_idx + 1L

      broadcast_feature_hand(NA_integer_)
      pending_feature_hand_start(NA_integer_)
      is_playing(FALSE)
      shiny::updateTabsetPanel(session, "main_tab", selected = "Overview")

      if (!is.na(next_idx) && next_idx <= length(replay$hand_log)) {
        shiny::updateSelectInput(session, "hand_index", selected = as.character(next_idx))
        show_broadcast_banner(
          title = if (isTRUE(skipped)) "Feature Skipped" else "Back To Tournament",
          text = hand_label(replay$hand_log[[next_idx]]),
          subtitle = "Overview replay resumed"
        )
        if (isTRUE(input$broadcast_mode %||% FALSE)) {
          resume_overview_after_feature(TRUE)
        }
      } else {
        shiny::updateSelectInput(session, "hand_index", selected = as.character(length(replay$hand_log)))
        show_broadcast_banner(
          title = if (isTRUE(skipped)) "Feature Skipped" else "Tournament Replay Complete",
          text = "No more hands to replay",
          subtitle = NULL
        )
        is_overview_playing(FALSE)
      }

      invisible(NULL)
    }

    selected_hand_index <- shiny::reactive({
      idx <- suppressWarnings(as.integer(input$hand_index))
      if (length(idx) != 1 || is.na(idx) || idx < 1L || idx > length(replay$hand_log)) {
        return(1L)
      }
      idx
    })

    current_hand <- shiny::reactive({
      idx <- selected_hand_index()
      replay$hand_log[[idx]]
    })

    current_snapshots <- shiny::reactive({
      build_replay_snapshots(current_hand())
    })

    output$step_control_ui <- shiny::renderUI({
      snaps <- current_snapshots()
      n <- length(snaps)

      if (n == 0) {
        shiny::helpText("This hand has no stored snapshots yet.")
      } else {
        shiny::sliderInput(
          inputId = "step_index",
          label = "Replay step",
          min = 1,
          max = n,
          value = 1,
          step = 1,
          animate = FALSE
        )
      }
    })

    current_snapshot <- shiny::reactive({
      snaps <- current_snapshots()
      if (length(snaps) == 0) return(NULL)

      idx <- input$step_index %||% 1L
      idx <- max(1L, min(as.integer(idx), length(snaps)))
      snaps[[idx]]
    })

    shiny::observeEvent(input$play_replay, {
      if (length(current_snapshots()) > 0) {
        is_playing(TRUE)
      }
    })

    shiny::observeEvent(input$overview_play, {
      if (length(replay$hand_log) > 0) {
        is_overview_playing(TRUE)
      }
    })

    output$sidebar_controls <- shiny::renderUI({
      current_tab <- input$main_tab %||% "Overview"
      current_overview_speed <- isolate(input$overview_replay_speed %||% "250")
      current_replay_speed <- isolate(input$replay_speed %||% "1500")
      current_hand_end_behavior <- isolate(input$hand_end_behavior %||% "pause")

      if (identical(current_tab, "Table Intro")) {
        return(
          shiny::tagList(
            shiny::checkboxInput(
              inputId = "broadcast_mode",
              label = "Broadcast mode",
              value = isTRUE(input$broadcast_mode %||% FALSE)
            ),
            if (isTRUE(input$broadcast_mode %||% FALSE)) {
              shiny::checkboxInput(
                inputId = "broadcast_speak_chatter",
                label = "Speak intro and comments",
                value = isTRUE(input$broadcast_speak_chatter %||% TRUE)
              )
            },
            shiny::fluidRow(
              shiny::column(width = 4, shiny::actionButton("intro_play", "Play Intro")),
              shiny::column(width = 4, shiny::actionButton("intro_pause", "Pause")),
              shiny::column(width = 4, shiny::actionButton("intro_reset", "Reset"))
            ),
            shiny::hr(),
            shiny::actionButton("intro_start_tournament", "Start Tournament Broadcast")
          )
        )
      }

      if (identical(current_tab, "Overview")) {
        return(
          shiny::tagList(
            shiny::checkboxInput(
              inputId = "broadcast_mode",
              label = "Broadcast mode",
              value = isTRUE(input$broadcast_mode %||% FALSE)
            ),
            if (isTRUE(input$broadcast_mode %||% FALSE)) {
              shiny::checkboxInput(
                inputId = "broadcast_speak_chatter",
                label = "Speak player comments",
                value = isTRUE(input$broadcast_speak_chatter %||% TRUE)
              )
            },
            shiny::selectInput(
              inputId = "hand_index",
              label = "Show tournament state through hand",
              choices = hand_choices,
              selected = current_hand_choice()
            ),
            shiny::selectInput(
              inputId = "overview_replay_speed",
              label = "Overview replay speed",
              choices = c(
                "Slow" = "2000",
                "Standard" = "1000",
                "Fast" = "250"
              ),
              selected = current_overview_speed
            ),
            shiny::fluidRow(
              shiny::column(width = 4, shiny::actionButton("overview_play", "Play")),
              shiny::column(width = 4, shiny::actionButton("overview_pause", "Pause")),
              shiny::column(width = 4, shiny::actionButton("overview_reset", "Reset"))
            )
          )
        )
      }

      if (identical(current_tab, "Hand Replay")) {
        return(
          shiny::tagList(
            shiny::checkboxInput(
              inputId = "broadcast_mode",
              label = "Broadcast mode",
              value = isTRUE(input$broadcast_mode %||% FALSE)
            ),
            if (isTRUE(input$broadcast_mode %||% FALSE)) {
              shiny::checkboxInput(
                inputId = "broadcast_speak_chatter",
                label = "Speak player comments",
                value = isTRUE(input$broadcast_speak_chatter %||% TRUE)
              )
            },
            shiny::selectInput(
              inputId = "hand_index",
              label = "Select hand",
              choices = hand_choices,
              selected = current_hand_choice()
            ),
            shiny::uiOutput("step_control_ui"),
            shiny::hr(),
            shiny::uiOutput("hole_card_selector_ui"),
            shiny::selectInput(
              inputId = "replay_speed",
              label = "Replay speed",
              choices = c(
                "Slow" = "3000",
                "Standard" = "1500",
                "Fast" = "100"
              ),
              selected = current_replay_speed
            ),
            shiny::selectInput(
              inputId = "hand_end_behavior",
              label = "At hand end",
              choices = c(
                "Pause" = "pause",
                "Continue to next hand" = "continue"
              ),
              selected = current_hand_end_behavior
            ),
            shiny::fluidRow(
              shiny::column(width = 4, shiny::actionButton("play_replay", "Play")),
              shiny::column(width = 4, shiny::actionButton("pause_replay", "Pause")),
              shiny::column(width = 4, shiny::actionButton("reset_replay", "Reset"))
            ),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::actionButton("next_step", "Next step")),
              shiny::column(width = 6, shiny::actionButton("next_hand", "Next hand"))
            ),
            if (isTRUE(input$broadcast_mode %||% FALSE) && !is.na(broadcast_feature_hand())) {
              shiny::actionButton("skip_featured_hand", "Skip featured hand")
            }
          )
        )
      }

      shiny::tagList(
        shiny::selectInput(
          inputId = "hand_index",
          label = "Select hand",
          choices = hand_choices,
          selected = current_hand_choice()
        )
      )
    })

    output$hole_card_selector_ui <- shiny::renderUI({
      player_choices <- hand_player_names(current_hand())
      if (length(player_choices) > 0) {
        valid_choice_idx <- which(!is.na(names(player_choices)) &
                                    nzchar(names(player_choices)) &
                                    !is.na(player_choices) &
                                    nzchar(player_choices))
        player_choices <- player_choices[valid_choice_idx]
      }
      choices <- c("All players" = "__ALL__", player_choices)
      selected <- if (isTRUE(input$broadcast_mode %||% FALSE)) "__ALL__" else character(0)
      shiny::checkboxGroupInput(
        inputId = "visible_hole_cards",
        label = "Show hole cards for",
        choices = choices,
        selected = selected
      )
    })

    output$table_intro_ui <- shiny::renderUI({
      build_intro_table_ui(intro_players, intro_active_index())
    })

    shiny::observeEvent(input$intro_play, {
      if (nrow(intro_players) == 0) {
        return()
      }
      if (is.na(intro_active_index())) {
        intro_active_index(1L)
        current_intro_comment("")
      }
      last_intro_advance_key("")
      intro_playing(TRUE)
    })

    shiny::observeEvent(input$intro_pause, {
      intro_playing(FALSE)
      last_intro_advance_key("")
    })

    shiny::observeEvent(input$intro_reset, {
      intro_playing(FALSE)
      intro_active_index(NA_integer_)
      current_intro_comment("")
      last_intro_advance_key("")
    })

    shiny::observeEvent(input$intro_start_tournament, {
      intro_playing(FALSE)
      intro_active_index(NA_integer_)
      current_intro_comment("")
      last_intro_advance_key("")
      shiny::updateTabsetPanel(session, "main_tab", selected = "Overview")
      shiny::updateSelectInput(session, "hand_index", selected = first_hand_choice)
      if (isTRUE(input$broadcast_mode %||% FALSE)) {
        resume_overview_after_intro(TRUE)
      }
    })

    shiny::observe({
      if (!isTRUE(intro_playing())) {
        return()
      }
      if (!identical(input$main_tab %||% "Table Intro", "Table Intro")) {
        intro_playing(FALSE)
        return()
      }
      if (nrow(intro_players) == 0) {
        intro_playing(FALSE)
        return()
      }

      idx <- isolate(intro_active_index())
      if (is.na(idx)) {
        idx <- 1L
        intro_active_index(idx)
      }
      idx <- max(1L, min(as.integer(idx), nrow(intro_players)))
      player <- intro_players[idx, , drop = FALSE]

      advance_key <- paste(idx, as_scalar_chr(player$player_id, ""), sep = ":")
      if (!identical(advance_key, last_intro_advance_key())) {
        comment <- intro_comment_for_player(player, intro_chatter)
        current_intro_comment(comment)
        last_intro_advance_key(advance_key)

        speak_broadcast_announcement(intro_announcement_text(player), queue = TRUE)
        if (nzchar(comment)) {
          play_broadcast_sound("chatter")
          speak_broadcast_chatter(
            text = comment,
            speaker = as_scalar_chr(player$player_name, player$player_id %||% ""),
            queue = TRUE
          )
        } else {
          play_broadcast_sound("bot")
        }

        shiny::invalidateLater(intro_player_hold_ms(player, comment), session)
        return()
      }

      last_intro_advance_key("")
      if (idx >= nrow(intro_players)) {
        intro_playing(FALSE)
        show_broadcast_banner(
          title = "Table Intro Complete",
          text = "Players are seated",
          subtitle = "Ready for tournament broadcast",
          duration_ms = 1800L
        )
        return()
      }

      intro_active_index(idx + 1L)
    })

    shiny::observeEvent(input$pause_replay, {
      is_playing(FALSE)
      last_replay_advance_key("")
    })

    shiny::observeEvent(input$overview_pause, {
      is_overview_playing(FALSE)
    })

    shiny::observeEvent(input$skip_featured_hand, {
      finish_featured_hand(skipped = TRUE)
    })

    shiny::observeEvent(input$reset_replay, {
      is_playing(FALSE)
      last_replay_advance_key("")
      shiny::updateSelectInput(session, "hand_index", selected = first_hand_choice)
      if (length(current_snapshots()) > 0) {
        shiny::updateSliderInput(session, "step_index", value = 1)
      }
    })

    shiny::observeEvent(input$overview_reset, {
      is_overview_playing(FALSE)
      shiny::updateSelectInput(session, "hand_index", selected = first_hand_choice)
      if (length(current_snapshots()) > 0) {
        shiny::updateSliderInput(session, "step_index", value = 1)
      }
    })

    shiny::observeEvent(input$next_step, {
      is_playing(FALSE)
      last_replay_advance_key("")
      snaps <- current_snapshots()
      if (length(snaps) == 0) {
        return()
      }
      current_idx <- input$step_index %||% 1L
      next_idx <- min(as.integer(current_idx) + 1L, length(snaps))
      shiny::updateSliderInput(session, "step_index", value = next_idx)
    })

    shiny::observeEvent(input$next_hand, {
      is_playing(FALSE)
      last_replay_advance_key("")
      hand_idx <- selected_hand_index()
      if (!is.na(hand_idx) && hand_idx < length(replay$hand_log)) {
        shiny::updateSelectInput(session, "hand_index", selected = as.character(hand_idx + 1L))
      }
    })

    shiny::observe({
      if (!isTRUE(is_playing())) {
        return()
      }

      snaps <- current_snapshots()
      if (length(snaps) == 0) {
        is_playing(FALSE)
        return()
      }

      delay_ms <- suppressWarnings(as.integer(isolate(input$replay_speed %||% 5000L)))
      if (is.na(delay_ms) || delay_ms < 100L) {
        delay_ms <- 5000L
      }

      current_idx <- isolate(input$step_index %||% 1L)
      current_idx <- max(1L, min(as.integer(current_idx), length(snaps)))
      snap <- snaps[[current_idx]]
      if (isTRUE(isolate(input$broadcast_mode %||% FALSE)) &&
          isTRUE(isolate(input$broadcast_speak_chatter %||% TRUE))) {
        delay_ms <- max(delay_ms, snapshot_speech_hold_ms(current_hand(), snap))
      }

      advance_key <- paste(selected_hand_index(), current_idx, delay_ms, sep = ":")
      if (!identical(advance_key, last_replay_advance_key())) {
        last_replay_advance_key(advance_key)
        shiny::invalidateLater(delay_ms, session)
        return()
      }

      last_replay_advance_key("")
      if (current_idx >= length(snaps)) {
        if (isTRUE(input$broadcast_mode %||% FALSE) &&
            !is.na(isolate(broadcast_feature_hand())) &&
            identical(isolate(broadcast_feature_hand()), isolate(selected_hand_index()))) {
          finish_featured_hand(skipped = FALSE)
          return()
        }

        if (identical(isolate(input$hand_end_behavior %||% "pause"), "continue")) {
          hand_idx <- isolate(selected_hand_index())
          if (!is.na(hand_idx) && hand_idx < length(replay$hand_log)) {
            shiny::updateSelectInput(session, "hand_index", selected = as.character(hand_idx + 1L))
          } else {
            is_playing(FALSE)
          }
        } else {
          is_playing(FALSE)
        }
        return()
      }

      shiny::updateSliderInput(session, "step_index", value = current_idx + 1L)
    })

    shiny::observeEvent(input$step_index, {
      if (!isTRUE(input$broadcast_mode %||% FALSE)) {
        return()
      }

      snap <- current_snapshot()
      if (is.null(snap)) {
        return()
      }

      action_history <- current_hand()$action_history %||% list()
      action_count <- as.integer(snap$action_count %||% 0L)
      action_count <- max(0L, min(action_count, length(action_history)))
      if (action_count > 0L) {
        action <- action_history[[action_count]]
        chatter <- paste(trimws(as.character(action$chatter %||% action$table_talk %||% "")), collapse = " ")
        chatter_key <- paste(selected_hand_index(), action_count, sep = ":")
        if (nzchar(chatter) && !identical(chatter_key, last_chatter_sound_key())) {
          last_chatter_sound_key(chatter_key)
          play_broadcast_sound("chatter")
          speak_broadcast_chatter(
            text = strip_chatter_speaker_prefix(chatter),
            speaker = as_scalar_chr(action$player_name, action$player_id %||% "")
          )
        }
      }

      if (identical(as_scalar_chr(snap$snapshot_type, ""), "hand_end")) {
        key <- paste(selected_hand_index(), as.integer(input$step_index %||% NA_integer_), "winner", sep = ":")
        if (!identical(key, last_winner_announcement_key())) {
          announcement <- winner_announcement_text(current_hand())
          if (nzchar(announcement)) {
            last_winner_announcement_key(key)
            speak_broadcast_announcement(announcement)
          }
        }
      }

      if (!identical(as_scalar_chr(snap$snapshot_type, ""), "elimination")) {
        return()
      }

      key <- paste(selected_hand_index(), as.integer(input$step_index %||% NA_integer_), sep = ":")
      if (identical(key, last_elimination_sound_key())) {
        return()
      }

      last_elimination_sound_key(key)
      play_broadcast_sound("elimination")
      speak_broadcast_announcement(elimination_announcement_text(snap, current_hand()))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$main_tab, {
      current_tab <- input$main_tab %||% "Overview"
      if (!identical(current_tab, "Overview")) {
        is_overview_playing(FALSE)
      }
      if (!identical(current_tab, "Hand Replay")) {
        is_playing(FALSE)
      }
      if (identical(current_tab, "Overview") && isTRUE(resume_overview_after_feature())) {
        resume_overview_after_feature(FALSE)
        is_overview_playing(TRUE)
      }
      if (identical(current_tab, "Overview") && isTRUE(resume_overview_after_intro())) {
        resume_overview_after_intro(FALSE)
        is_overview_playing(TRUE)
      }
    }, ignoreInit = TRUE)

    shiny::observe({
      if (!isTRUE(is_overview_playing())) {
        return()
      }

      if (!identical(input$main_tab %||% "Overview", "Overview")) {
        is_overview_playing(FALSE)
        return()
      }

      hand_idx <- isolate(selected_hand_index())
      if (isTRUE(input$broadcast_mode %||% FALSE) &&
          !is.na(hand_idx) &&
          hand_is_featured(hand_idx) &&
          !identical(isolate(input$main_tab %||% "Overview"), "Hand Replay")) {
        start_featured_hand(hand_idx)
        return()
      }

      delay_ms <- suppressWarnings(as.integer(isolate(input$overview_replay_speed %||% 1000L)))
      if (is.na(delay_ms) || delay_ms < 100L) {
        delay_ms <- 1000L
      }

      shiny::invalidateLater(delay_ms, session)

      if (is.na(hand_idx) || hand_idx >= length(replay$hand_log)) {
        is_overview_playing(FALSE)
        return()
      }

      shiny::updateSelectInput(session, "hand_index", selected = as.character(hand_idx + 1L))
    })

    shiny::observe({
      banner <- broadcast_banner()
      if (is.null(banner)) {
        return()
      }
      ms_left <- as.integer(max(0, as.numeric(difftime(banner$expires_at, Sys.time(), units = "secs")) * 1000))
      if (ms_left <= 0L) {
        broadcast_banner(NULL)
        return()
      }
      shiny::invalidateLater(ms_left, session)
      if (Sys.time() >= banner$expires_at) {
        broadcast_banner(NULL)
      }
    })

    output$broadcast_banner_ui <- shiny::renderUI({
      banner <- broadcast_banner()
      if (is.null(banner)) {
        return(NULL)
      }
      shiny::tags$div(
        class = "viewer-broadcast-banner",
        shiny::tags$div(class = "viewer-broadcast-title", as_scalar_chr(banner$title, "Broadcast")),
        shiny::tags$div(class = "viewer-broadcast-text", as_scalar_chr(banner$text, "")),
        if (nzchar(as_scalar_chr(banner$subtitle, ""))) {
          shiny::tags$div(class = "viewer-broadcast-sub", as_scalar_chr(banner$subtitle, ""))
        }
      )
    })

    output$tournament_info <- shiny::renderText({
      hand <- current_hand()
      players_remaining <- length(hand_player_ids(hand))
      blind_level <- hand$level %||% NA_integer_
      sb <- hand$small_blind %||% NA_real_
      bb <- hand$big_blind %||% NA_real_

      paste0(
        "Tournament ID: ", as_scalar_chr(replay$tournament_id, "(unknown)"), "\n",
        "Hand Number: ", as_scalar_chr(hand$hand_number, "(unknown)"), "\n",
        "Blind Level: ", as_scalar_chr(blind_level, "(unknown)"),
        " (SB ", as_scalar_chr(sb, "?"), " / BB ", as_scalar_chr(bb, "?"), ")\n",
        "Players Remaining: ", players_remaining
      )
    })

    output$overview_players <- shiny::renderTable({
      df <- current_hand_stack_df(replay, current_hand())
      names(df)[names(df) == "EliminationOrder"] <- "EliminationOrder"
      df
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$chip_history_plot <- shiny::renderPlot({
      hand_idx <- selected_hand_index()
      df <- chip_history_df(replay, hand_idx)

      if (nrow(df) == 0) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No hand-start chip history available.")
        return(invisible(NULL))
      }

      players <- unique(df$Player)
      colors <- grDevices::hcl.colors(length(players), palette = "Dark 3")
      names(colors) <- players

      x_range <- range(df$Hand, na.rm = TRUE)
      y_range <- range(df$Chips, na.rm = TRUE)
      if (!is.finite(y_range[1]) || !is.finite(y_range[2])) {
        y_range <- c(0, 1)
      }
      if (diff(y_range) == 0) {
        y_range <- c(max(0, y_range[1] - 1), y_range[2] + 1)
      }

      graphics::plot(
        x_range,
        y_range,
        type = "n",
        xlab = "Hand Number (start of hand)",
        ylab = "Chips",
        main = "Chip Counts Through Start of Current Hand",
        xaxt = "n"
      )

      hand_ticks <- sort(unique(df$Hand))
      graphics::axis(1, at = hand_ticks)
      graphics::grid(col = "gray85", lty = "dotted")

      for (player in players) {
        pdat <- df[df$Player == player, , drop = FALSE]
        pdat <- pdat[order(pdat$Hand), , drop = FALSE]
        graphics::lines(pdat$Hand, pdat$Chips, col = colors[[player]], lwd = 2)
        graphics::points(pdat$Hand, pdat$Chips, col = colors[[player]], pch = 19, cex = 0.8)
      }

      graphics::legend(
        "topright",
        legend = players,
        col = colors[players],
        lwd = 2,
        pch = 19,
        cex = 0.8,
        bg = "white"
      )
    })

    output$current_stack_plot <- shiny::renderPlot({
      df <- current_hand_stack_df(replay, current_hand())

      if (nrow(df) == 0) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No chip stack data available.")
        return(invisible(NULL))
      }

      bar_colors <- ifelse(df$Status == "eliminated", "#8c5a5a", "#2f8f57")
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mar = c(8, 4, 4, 1) + 0.1)

      mids <- graphics::barplot(
        height = df$Chips,
        names.arg = df$Player,
        las = 2,
        col = bar_colors,
        border = NA,
        main = "Chip Stacks at Selected Hand",
        ylab = "Chips",
        cex.names = 0.8
      )

      if (nrow(df) > 0) {
        graphics::text(
          x = mids,
          y = df$Chips,
          labels = as.character(df$Chips),
          pos = 3,
          cex = 0.8
        )
      }

      graphics::legend(
        "topright",
        legend = c("Active", "Eliminated"),
        fill = c("#2f8f57", "#8c5a5a"),
        bg = "white",
        cex = 0.85
      )
    })

    output$hand_info <- shiny::renderText({
      hand_summary_text(current_hand())
    })

    output$snapshot_info <- shiny::renderText({
      snapshot_summary_text(
        snapshot = current_snapshot(),
        step_index = input$step_index %||% NA_integer_
      )
    })

    output$board_info <- shiny::renderText({
      snap <- current_snapshot()
      if (is.null(snap)) {
        return(format_cards(current_hand()$board %||% character(0)))
      }

      board <- snap$board %||% snap$hand_state$board %||% current_hand()$board %||% character(0)
      format_cards(board)
    })

    output$table_replay_ui <- shiny::renderUI({
      reveal_ids <- input$visible_hole_cards
      if (is.null(reveal_ids)) {
        reveal_ids <- character(0)
      }
      if ("__ALL__" %in% reveal_ids) {
        reveal_ids <- "__ALL__"
      }
      if (isTRUE(input$broadcast_mode %||% FALSE)) {
        reveal_ids <- "__ALL__"
      }
      build_table_ui(
        snapshot = current_snapshot(),
        hand = current_hand(),
        reveal_player_ids = reveal_ids,
        broadcast_mode = isTRUE(input$broadcast_mode %||% FALSE)
      )
    })

    output$snapshot_players <- shiny::renderTable({
      df <- snapshot_to_player_df(current_snapshot())

      if (nrow(df) > 0) {
        active_hand_seats <- hand_player_seats(current_hand())
        if (length(active_hand_seats) > 0) {
          df <- df[df$Seat %in% active_hand_seats, , drop = FALSE]
        }
      }

      reveal_ids <- input$visible_hole_cards
      if (is.null(reveal_ids)) {
        reveal_ids <- character(0)
      }
      if (!("__ALL__" %in% reveal_ids) && "HoleCards" %in% names(df)) {
        player_map <- hand_player_names(current_hand())
        visible_names <- names(player_map)[player_map %in% reveal_ids]
        hide_idx <- which(is.na(df$Name) | !(df$Name %in% visible_names))
        if (length(hide_idx) > 0) {
          df$HoleCards[hide_idx] <- "(hidden)"
        }
      }

      df
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$action_history <- shiny::renderTable({
      action_history_df(current_hand())
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    # Reset step slider to 1 whenever the selected hand changes.
    shiny::observeEvent(input$hand_index, {
      hand_idx <- selected_hand_index()
      current_level <- as.integer(replay$hand_log[[hand_idx]]$level %||% NA_integer_)
      previous_level <- last_seen_broadcast_level()

      if (isTRUE(input$broadcast_mode %||% FALSE) &&
          !is.na(current_level) &&
          !is.na(previous_level) &&
          current_level > previous_level) {
        play_broadcast_sound("level")
        show_broadcast_banner(
          title = "Blind Level Up",
          text = paste0("Level ", current_level),
          subtitle = paste0(
            "Blinds ",
            as_scalar_chr(replay$hand_log[[hand_idx]]$small_blind, "?"),
            " / ",
            as_scalar_chr(replay$hand_log[[hand_idx]]$big_blind, "?")
          ),
          duration_ms = 1800L
        )
      }
      last_seen_broadcast_level(current_level)
      last_elimination_sound_key("")
      last_chatter_sound_key("")
      last_winner_announcement_key("")
      last_replay_advance_key("")

      snaps <- current_snapshots()
      if (length(snaps) > 0) {
        shiny::updateSliderInput(session, "step_index", value = 1, min = 1, max = length(snaps))
      }
      pending_feature <- pending_feature_hand_start()
      if (!is.na(pending_feature) && identical(as.integer(pending_feature), as.integer(hand_idx))) {
        pending_feature_hand_start(NA_integer_)
        shiny::updateTabsetPanel(session, "main_tab", selected = "Hand Replay")
        if (length(snaps) > 0) {
          is_playing(TRUE)
        }
      }
      if (!isTRUE(is_playing())) {
        is_playing(FALSE)
      }
    }, ignoreInit = TRUE)
  }

  shiny::shinyApp(ui = ui, server = server)
}

estimate_broadcast_runtime <- function(log_data,
                                       overview_replay_speed = c("fast", "standard", "slow"),
                                       replay_speed = c("standard", "slow", "fast"),
                                       speak_chatter = TRUE,
                                       include_speech_padding = TRUE,
                                       seconds_per_word = 0.36,
                                       speech_extra_seconds = 0.5,
                                       feature_transition_seconds = 2.2,
                                       return_per_hand = FALSE) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }

  as_scalar_chr <- function(x, default = "") {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
    as.character(x[1])
  }

  normalize_replay_data <- function(x) {
    if (inherits(x, "tournament_state")) {
      return(list(hand_log = x$hand_log %||% list()))
    }
    if (is.list(x) && !is.null(x$hand_log)) {
      return(list(hand_log = x$hand_log %||% list()))
    }
    if (is.list(x) &&
        length(x) > 0 &&
        is.null(x$hand_log) &&
        all(vapply(x, is.list, logical(1)))) {
      return(list(hand_log = x))
    }
    stop("Expected a tournament_state, a replay list with $hand_log, or a raw hand_log list.")
  }

  speed_ms <- function(value, choices) {
    if (is.numeric(value)) {
      return(as.numeric(value[1]))
    }
    key <- tolower(as.character(value[1]))
    choices[[key]] %||% choices[["standard"]]
  }

  get_hand_snapshots <- function(hand) {
    snaps <- hand$state_snapshots %||% hand$snapshots %||% hand$hand_snapshots %||% NULL
    if (is.null(snaps) || !is.list(snaps)) list() else snaps
  }

  player_name_lookup <- function(hand) {
    lookup <- list()
    sources <- c(
      hand$starting_stack_summary %||% list(),
      hand$ending_stack_summary %||% list(),
      hand$stack_summary %||% list()
    )
    for (p in sources) {
      pid <- as.character(p$player_id %||% "")
      seat <- as.character(p$seat %||% "")
      name <- as.character(p$player_name %||% p$name %||% pid)
      if (!identical(pid, "")) lookup[[paste0("pid:", pid)]] <- name
      if (!identical(seat, "")) lookup[[paste0("seat:", seat)]] <- name
    }
    lookup
  }

  build_replay_snapshots <- function(hand) {
    snaps <- get_hand_snapshots(hand)
    if (length(snaps) == 0) {
      return(list())
    }

    elim_ids <- hand$eliminations_this_hand %||% character(0)
    if (length(elim_ids) == 0) {
      return(snaps)
    }

    last_snap <- snaps[[length(snaps)]]
    if (identical(as_scalar_chr(last_snap$snapshot_type, ""), "elimination")) {
      return(snaps)
    }

    elim_snap <- last_snap
    elim_snap$snapshot_type <- "elimination"
    elim_snap$step <- as.numeric(last_snap$step %||% length(snaps)) + 1
    c(snaps, list(elim_snap))
  }

  winner_display_data <- function(hand) {
    winners <- hand$winners %||% list()
    if (!is.list(winners) || length(winners) == 0) {
      return(NULL)
    }

    player_lookup <- player_name_lookup(hand)
    winner_names <- character(0)
    winner_amounts <- numeric(0)

    for (w in winners) {
      if (identical(w$win_type %||% "", "win_by_folds")) {
        pid_key <- paste0("pid:", as.character(w$player_id %||% ""))
        seat_key <- paste0("seat:", as.character(w$seat %||% ""))
        name <- player_lookup[[pid_key]] %||% player_lookup[[seat_key]] %||% paste0("Seat ", as.character(w$seat %||% "?"))
        winner_names <- c(winner_names, name)
        winner_amounts <- c(winner_amounts, as.numeric(w$amount %||% NA_real_))
      }
      if (identical(w$win_type %||% "", "showdown_pot_award")) {
        seats <- as.character(w$winner_seats %||% character(0))
        payouts <- w$payouts %||% numeric(0)
        for (seat in seats) {
          name <- player_lookup[[paste0("seat:", seat)]] %||% paste0("Seat ", seat)
          winner_names <- c(winner_names, name)
          winner_amounts <- c(winner_amounts, as.numeric(payouts[[seat]] %||% NA_real_))
        }
      }
    }

    if (length(winner_names) == 0) {
      return(NULL)
    }

    amount_by_name <- tapply(
      ifelse(is.na(winner_amounts), 0, winner_amounts),
      winner_names,
      sum
    )
    list(names = names(amount_by_name), amounts = as.numeric(amount_by_name))
  }

  word_count <- function(text) {
    text <- trimws(gsub("\\s+", " ", as.character(text %||% "")))
    if (!nzchar(text)) return(0L)
    length(strsplit(text, "\\s+")[[1]])
  }

  announcement_words <- function(hand, has_elimination) {
    total <- 0L
    winner_info <- winner_display_data(hand)
    if (!is.null(winner_info)) {
      payout_text <- paste(
        vapply(seq_along(winner_info$names), function(i) {
          amt <- winner_info$amounts[i]
          if (!is.na(amt) && amt > 0) paste0(winner_info$names[i], " won ", amt, " chips") else winner_info$names[i]
        }, character(1)),
        collapse = ". "
      )
      total <- total + word_count(paste("Hand Winner.", payout_text))
    }
    if (isTRUE(has_elimination)) {
      elim_ids <- hand$eliminations_this_hand %||% character(0)
      lookup <- player_name_lookup(hand)
      elim_names <- vapply(elim_ids, function(pid) lookup[[paste0("pid:", pid)]] %||% pid, character(1))
      total <- total + word_count(paste(
        if (length(elim_names) > 1) "Players Eliminated." else "Player Eliminated.",
        paste(elim_names, collapse = ", "),
        "Tournament life ends here."
      ))
    }
    total
  }

  chatter_words <- function(hand) {
    actions <- hand$action_history %||% list()
    if (!is.list(actions) || length(actions) == 0) {
      return(0L)
    }
    sum(vapply(actions, function(a) {
      chatter <- paste(trimws(as.character(a$chatter %||% a$table_talk %||% "")), collapse = " ")
      chatter <- sub("^[[:alnum:] ._'?-]{1,45}:\\s*", "", chatter)
      word_count(chatter)
    }, integer(1)))
  }

  snapshot_speech_words <- function(hand, snapshot) {
    words <- 0L
    action_history <- hand$action_history %||% list()
    action_count <- as.integer(snapshot$action_count %||% 0L)
    action_count <- max(0L, min(action_count, length(action_history)))
    if (action_count > 0L) {
      action <- action_history[[action_count]]
      chatter <- paste(trimws(as.character(action$chatter %||% action$table_talk %||% "")), collapse = " ")
      chatter <- sub("^[[:alnum:] ._'?-]{1,45}:\\s*", "", chatter)
      if (nzchar(trimws(chatter))) {
        speaker <- as_scalar_chr(action$player_name, action$player_id %||% "")
        words <- words + word_count(if (nzchar(speaker)) paste(speaker, "says:", chatter) else chatter)
      }
    }

    snap_type <- as_scalar_chr(snapshot$snapshot_type, "")
    if (identical(snap_type, "hand_end")) {
      words <- words + announcement_words(hand, FALSE)
    }
    if (identical(snap_type, "elimination")) {
      elim_ids <- hand$eliminations_this_hand %||% character(0)
      lookup <- player_name_lookup(hand)
      elim_names <- vapply(elim_ids, function(pid) lookup[[paste0("pid:", pid)]] %||% pid, character(1))
      words <- words + word_count(paste(
        if (length(elim_names) > 1) "Players Eliminated." else "Player Eliminated.",
        paste(elim_names, collapse = ", "),
        "Tournament life ends here."
      ))
    }

    words
  }

  speech_seconds_for_words <- function(words) {
    if (!isTRUE(include_speech_padding) || !isTRUE(speak_chatter) || words <= 0L) {
      return(0)
    }
    words * seconds_per_word + speech_extra_seconds
  }

  replay <- normalize_replay_data(log_data)
  hand_log <- replay$hand_log %||% list()
  n_hands <- length(hand_log)
  if (n_hands == 0) {
    out <- list(
      total_seconds = 0,
      total_minutes = 0,
      total_hms = "00:00:00",
      hands = 0,
      featured_hands = 0,
      replay_steps = 0,
      elimination_hands = 0,
      assumptions = list()
    )
    class(out) <- "broadcast_runtime_estimate"
    return(out)
  }

  overview_ms <- speed_ms(overview_replay_speed, c(slow = 2000, standard = 1000, fast = 250))
  replay_ms <- speed_ms(replay_speed, c(slow = 3000, standard = 1500, fast = 100))

  per_hand <- do.call(rbind, lapply(seq_along(hand_log), function(i) {
    hand <- hand_log[[i]]
    snaps <- build_replay_snapshots(hand)
    n_steps <- length(snaps)
    featured <- isTRUE(hand$for_tv)
    eliminated <- length(hand$eliminations_this_hand %||% character(0)) > 0
    words <- if (featured && isTRUE(speak_chatter)) chatter_words(hand) else 0L
    announcer_words <- if (featured && isTRUE(speak_chatter)) announcement_words(hand, eliminated) else 0L

    overview_seconds <- overview_ms / 1000
    feature_seconds <- if (featured) {
      step_seconds <- if (n_steps > 0) {
        vapply(snaps, function(snapshot) {
          max(replay_ms / 1000, speech_seconds_for_words(snapshot_speech_words(hand, snapshot)))
        }, numeric(1))
      } else {
        numeric(0)
      }
      feature_transition_seconds + sum(step_seconds)
    } else {
      0
    }

    data.frame(
      hand_index = i,
      hand_number = as.integer(hand$hand_number %||% i),
      for_tv = featured,
      steps = n_steps,
      eliminated = eliminated,
      chatter_words = words,
      announcer_words = announcer_words,
      estimated_seconds = overview_seconds + feature_seconds,
      stringsAsFactors = FALSE
    )
  }))

  total_seconds <- sum(per_hand$estimated_seconds, na.rm = TRUE)
  h <- floor(total_seconds / 3600)
  m <- floor((total_seconds %% 3600) / 60)
  s <- round(total_seconds %% 60)

  out <- list(
    total_seconds = total_seconds,
    total_minutes = total_seconds / 60,
    total_hms = sprintf("%02d:%02d:%02d", h, m, s),
    hands = n_hands,
    featured_hands = sum(per_hand$for_tv, na.rm = TRUE),
    replay_steps = sum(per_hand$steps[per_hand$for_tv], na.rm = TRUE),
    elimination_hands = sum(per_hand$eliminated, na.rm = TRUE),
    assumptions = list(
      overview_replay_speed_ms = overview_ms,
      featured_hand_replay_speed_ms = replay_ms,
      feature_transition_seconds = feature_transition_seconds,
      speak_chatter = isTRUE(speak_chatter),
      seconds_per_word = seconds_per_word,
      speech_extra_seconds = speech_extra_seconds,
      speech_padding_included = isTRUE(include_speech_padding)
    )
  )
  if (isTRUE(return_per_hand)) {
    out$per_hand <- per_hand
  }
  class(out) <- "broadcast_runtime_estimate"
  out
}

print.broadcast_runtime_estimate <- function(x, ...) {
  cat("Estimated broadcast runtime:", x$total_hms, "\n")
  cat("  Hands:", x$hands, "\n")
  cat("  Featured hands:", x$featured_hands, "\n")
  cat("  Featured replay steps:", x$replay_steps, "\n")
  cat("  Elimination hands:", x$elimination_hands, "\n")
  cat("  Assumptions: overview", x$assumptions$overview_replay_speed_ms, "ms;",
      "feature replay", x$assumptions$featured_hand_replay_speed_ms, "ms;",
      "speech", if (isTRUE(x$assumptions$speech_padding_included)) "included" else "not included", "\n")
  invisible(x)
}


append_hand_snapshot <- function(tournament_state,
                                 message = NULL,
                                 snapshot_type = "state",
                                 include_hole_cards = TRUE) {
  if (!inherits(tournament_state, "tournament_state")) {
    stop("`tournament_state` must inherit from 'tournament_state'.")
  }

  hand_state <- tournament_state$current_hand
  if (is.null(hand_state) || !inherits(hand_state, "hand_state")) {
    stop("No valid current hand found in `tournament_state$current_hand`.")
  }

  if (is.null(hand_state$state_snapshots)) {
    hand_state$state_snapshots <- list()
  }

  snap <- capture_hand_snapshot(
    tournament_state = tournament_state,
    message = message,
    snapshot_type = snapshot_type,
    include_hole_cards = include_hole_cards
  )

  hand_state$state_snapshots[[length(hand_state$state_snapshots) + 1]] <- snap
  tournament_state$current_hand <- hand_state
  tournament_state
}
