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

  get_snapshot <- function(hand, idx) {
    snaps <- get_hand_snapshots(hand)
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

  # -----------------------------
  # Prepare replay data
  # -----------------------------
  replay <- normalize_replay_data(log_data)

  if (length(replay$hand_log) == 0) {
    stop("The supplied log_data contains no hands in $hand_log.")
  }

  hand_choices <- stats::setNames(
    as.character(seq_along(replay$hand_log)),
    vapply(replay$hand_log, hand_label, character(1))
  )

  # -----------------------------
  # UI
  # -----------------------------
  ui <- shiny::fluidPage(
    shiny::titlePanel("Poker Bot Replay Viewer"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::tags$strong("Tournament"),
        shiny::verbatimTextOutput("tournament_info"),
        shiny::hr(),

        shiny::selectInput(
          inputId = "hand_index",
          label = "Select hand",
          choices = hand_choices,
          selected = names(hand_choices)[1]
        ),

        shiny::uiOutput("step_control_ui"),
        shiny::hr(),

        shiny::checkboxInput(
          inputId = "show_hole_cards",
          label = "Show hole cards in player table",
          value = TRUE
        ),
        width = 3
      ),

      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Overview",
            shiny::h4("Tournament summary"),
            shiny::tableOutput("overview_players")
          ),
          shiny::tabPanel(
            "Hand Replay",
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::h4("Hand metadata"),
                shiny::verbatimTextOutput("hand_info")
              ),
              shiny::column(
                width = 4,
                shiny::h4("Current snapshot"),
                shiny::verbatimTextOutput("snapshot_info")
              ),
              shiny::column(
                width = 4,
                shiny::h4("Board"),
                shiny::verbatimTextOutput("board_info")
              )
            ),
            shiny::hr(),
            shiny::h4("Players"),
            shiny::tableOutput("snapshot_players")
          ),
          shiny::tabPanel(
            "Action History",
            shiny::h4("Actions"),
            shiny::tableOutput("action_history")
          ),
          shiny::tabPanel(
            "Raw Snapshot",
            shiny::h4("Snapshot structure"),
            shiny::verbatimTextOutput("raw_snapshot")
          )
        )
      )
    )
  )

  # -----------------------------
  # Server
  # -----------------------------
  server <- function(input, output, session) {

    current_hand <- shiny::reactive({
      idx <- as.integer(input$hand_index)
      replay$hand_log[[idx]]
    })

    current_snapshots <- shiny::reactive({
      get_hand_snapshots(current_hand())
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

    output$tournament_info <- shiny::renderText({
      paste0(
        "Tournament ID: ", as_scalar_chr(replay$tournament_id, "(unknown)"), "\n",
        "Status: ", as_scalar_chr(replay$status, "(unknown)"), "\n",
        "Hands logged: ", length(replay$hand_log), "\n",
        "Eliminations recorded: ", length(replay$elimination_order %||% character(0))
      )
    })

    output$overview_players <- shiny::renderTable({
      overview_players_df(replay)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

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

    output$snapshot_players <- shiny::renderTable({
      df <- snapshot_to_player_df(current_snapshot())

      if (!isTRUE(input$show_hole_cards) && "HoleCards" %in% names(df)) {
        df$HoleCards <- NULL
      }

      df
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$action_history <- shiny::renderTable({
      action_history_df(current_hand())
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$raw_snapshot <- shiny::renderPrint({
      snap <- current_snapshot()
      if (is.null(snap)) {
        cat("No snapshot available for this hand.\n")
      } else {
        str(snap, max.level = 2)
      }
    })

    # Reset step slider to 1 whenever the selected hand changes.
    shiny::observeEvent(input$hand_index, {
      snaps <- current_snapshots()
      if (length(snaps) > 0) {
        shiny::updateSliderInput(session, "step_index", value = 1, min = 1, max = length(snaps))
      }
    }, ignoreInit = TRUE)
  }

  shiny::shinyApp(ui = ui, server = server)
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