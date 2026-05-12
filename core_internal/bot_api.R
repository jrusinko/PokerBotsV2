############################################################
# Mathematics of Poker — Bot API
# File: bot_api.R
#
# Purpose:
#   Minimal student-facing bot interface.
#   This file defines:
#     - a standard action constructor,
#     - basic validation for bot outputs,
#     - a simple bot registration helper.
#
# Notes:
#   Bot execution is handled by game_engine.R, not here.
############################################################

# Bot input contract (engine -> bot)
# ----------------------------------
# The engine calls each bot as `bot_fn(bot_input)` where `bot_input` is a
# list constructed by `build_bot_input()` in `game_engine.R`. Useful fields
# (not exhaustive):
#   - player_id, player_name, seat
#   - hole_cards (character vector), board (character vector), street
#   - pot, current_bet, committed_this_round, committed_this_hand, stack
#   - small_blind, big_blind, ante
#   - legal_actions: a list with `legal_action_types` (character vector)
#       and `actions` (possibly containing `bet` and `raise` with
#       `min_amount`/`max_amount`).
#   - public_players (list) and action_history (list)
#
# Bots must return a list describing the action, e.g.:
#   list(type = "fold"), list(type = "check"), list(type = "call"),
#   list(type = "all_in"), list(type = "bet", amount = x),
#   list(type = "raise", amount = x)
# For `bet`/`raise` the `amount` should be a single numeric value and legal
# according to `bot_input$legal_actions`. The engine will defensively
# normalize missing/malformed amounts for `bet`/`raise` to the minimum legal
# amount.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}

new_bot_action <- function(type, amount = NULL) {
  if (!is.character(type) || length(type) != 1 || is.na(type) || !nzchar(type)) {
    stop("`type` must be a nonempty character string.")
  }

  out <- list(type = type)

  if (!is.null(amount)) {
    if (!is.numeric(amount) || length(amount) != 1 || is.na(amount)) {
      stop("`amount` must be a single numeric value when provided.")
    }
    out$amount <- amount
  }

  out
}

bot_chatter_probability <- function() {
  getOption("pokerbots.chatter_probability", 0.10)
}

bot_social_chatter_probability <- function() {
  getOption("pokerbots.social_chatter_probability", 0.15)
}

bot_chatter_usage_env <- new.env(parent = emptyenv())

reset_bot_chatter_usage <- function() {
  rm(list = ls(bot_chatter_usage_env, all.names = TRUE), envir = bot_chatter_usage_env)
  invisible(NULL)
}

bot_chatter_quote_allowed <- function(line, max_uses = getOption("pokerbots.max_chatter_quote_uses", 3L)) {
  line <- trimws(paste(as.character(line %||% ""), collapse = " "))
  if (!nzchar(line)) {
    return(FALSE)
  }

  max_uses <- as.integer(max_uses)
  if (length(max_uses) != 1L || is.na(max_uses) || max_uses < 1L) {
    max_uses <- 3L
  }

  key <- line
  current_count <- bot_chatter_usage_env[[key]] %||% 0L
  current_count <- as.integer(current_count)

  if (current_count >= max_uses) {
    return(FALSE)
  }

  bot_chatter_usage_env[[key]] <- current_count + 1L
  TRUE
}

bot_choose_chatter_line <- function(lines, bot_input = NULL) {
  lines <- as.character(lines)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) {
    return("")
  }

  public_players <- if (is.list(bot_input)) bot_input$public_players else list()
  if (!is.list(public_players) || length(public_players) == 0) {
    return(sample(lines, size = 1))
  }

  table_names <- tolower(vapply(public_players, function(p) {
    paste(
      as.character(p$player_name %||% ""),
      as.character(p$name %||% ""),
      as.character(p$bot_name %||% "")
    )
  }, character(1)))

  social_names <- c("mady", "nate", "lucy", "joel", "tara", "jaymon", "siena", "maurice", "hawkins")
  present <- social_names[vapply(social_names, function(nm) any(grepl(nm, table_names, fixed = TRUE)), logical(1))]
  if (length(present) == 0) {
    return(sample(lines, size = 1))
  }

  social_idx <- vapply(
    lines,
    function(line) any(grepl(paste(present, collapse = "|"), tolower(line))),
    logical(1)
  )
  if (!any(social_idx)) {
    return(sample(lines, size = 1))
  }

  sample(c(lines, rep(lines[social_idx], 4)), size = 1)
}

bot_maybe_say <- function(lines, bot_input = NULL, chance = NULL) {
  if (is.null(chance)) {
    chance <- bot_chatter_probability()
  } else {
    chance <- bot_chatter_probability()
  }

  public_players <- if (is.list(bot_input)) bot_input$public_players else list()
  if (is.list(public_players) && length(public_players) > 0) {
    table_names <- tolower(vapply(public_players, function(p) {
      paste(
        as.character(p$player_name %||% ""),
        as.character(p$name %||% ""),
        as.character(p$bot_name %||% "")
      )
    }, character(1)))
    social_names <- c("mady", "nate", "lucy", "joel", "tara", "jaymon", "siena", "maurice", "hawkins")
    present <- social_names[vapply(social_names, function(nm) any(grepl(nm, table_names, fixed = TRUE)), logical(1))]
    if (length(present) > 0 && any(grepl(paste(present, collapse = "|"), tolower(paste(lines, collapse = " "))))) {
      chance <- max(chance, bot_social_chatter_probability())
    }
  }

  if (runif(1) < chance) {
    line <- bot_choose_chatter_line(lines, bot_input)
    if (bot_chatter_quote_allowed(line)) {
      cat(line, "\n")
    }
  }
  invisible(NULL)
}

# Bot helper functions for student/example bots (canonical location)
# These helpers inspect the `bot_input` list passed by the engine and
# provide safe access to legal action amounts and checks.
bot_has_action <- function(bot_input, action_type) {
  if (is.null(bot_input$legal_actions$legal_action_types)) return(FALSE)
  action_type %in% bot_input$legal_actions$legal_action_types
}

bot_min_bet <- function(bot_input) {
  if (!bot_has_action(bot_input, "bet")) return(NULL)
  bot_input$legal_actions$actions$bet$min_amount
}

bot_max_bet <- function(bot_input) {
  if (!bot_has_action(bot_input, "bet")) return(NULL)
  bot_input$legal_actions$actions$bet$max_amount
}

bot_min_raise <- function(bot_input) {
  if (!bot_has_action(bot_input, "raise")) return(NULL)
  bot_input$legal_actions$actions$raise$min_amount
}

bot_max_raise <- function(bot_input) {
  if (!bot_has_action(bot_input, "raise")) return(NULL)
  bot_input$legal_actions$actions$raise$max_amount
}

bot_call_amount <- function(bot_input) {
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  max(0, current_bet - committed)
}

pot_odds <- function(call_amount, pot_before_call) {
  call_amount <- as.numeric(call_amount %||% 0)
  pot_before_call <- as.numeric(pot_before_call %||% 0)
  if (!is.finite(call_amount) || !is.finite(pot_before_call) || call_amount <= 0) {
    return(0)
  }
  call_amount / (pot_before_call + call_amount)
}

choose_preferred_action <- function(bot_input, preferences = c("check", "call", "fold")) {
  legal_types <- bot_input$legal_actions$legal_action_types

  for (a in preferences) {
    if (a %in% legal_types) {
      if (a == "bet") {
        return(list(type = "bet", amount = bot_min_bet(bot_input)))
      }
      if (a == "raise") {
        return(list(type = "raise", amount = bot_min_raise(bot_input)))
      }
      if (a == "all_in") {
        return(list(type = "all_in"))
      }
      return(list(type = a))
    }
  }

  stop("No preferred legal action found.")
}


validate_bot_action <- function(
    action,
    legal_actions = c("fold", "check", "call", "bet", "raise", "all_in")
) {
  if (!is.list(action) || is.null(action$type)) {
    return(list(
      valid = FALSE,
      reason = "Action must be a list with a `type` field."
    ))
  }

  type <- as.character(action$type)[1]

  if (!(type %in% legal_actions)) {
    return(list(
      valid = FALSE,
      reason = sprintf("Illegal action type: %s", type)
    ))
  }

  if (type %in% c("bet", "raise")) {
    if (is.null(action$amount) ||
        !is.numeric(action$amount) ||
        length(action$amount) != 1 ||
        is.na(action$amount)) {
      return(list(
        valid = FALSE,
        reason = "Bet/raise actions require a single numeric `amount`."
      ))
    }

    if (action$amount < 0) {
      return(list(
        valid = FALSE,
        reason = "Action amount must be nonnegative."
      ))
    }
  }

  if (type %in% c("fold", "check", "call", "all_in")) {
    if (!is.null(action$amount)) {
      return(list(
        valid = FALSE,
        reason = sprintf("Action type `%s` should not include an `amount`.", type)
      ))
    }
  }

  list(valid = TRUE, reason = NULL)
}


register_bot <- function(name, bot_fn, metadata = list()) {
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)) {
    stop("Bot name must be a nonempty string.")
  }

  if (!is.function(bot_fn)) {
    stop("`bot_fn` must be a function.")
  }

  if (!is.list(metadata)) {
    stop("`metadata` must be a list.")
  }

# For compatibility with the engine, return the function itself so the
# value can be passed directly into `initialize_tournament(bot_fns=...)`.
# Attach registration info as attributes to the function for inspection.
  attr(bot_fn, "bot_name") <- name
  attr(bot_fn, "bot_metadata") <- metadata
  bot_fn
}

# Helper: extract registration info from a bot function returned by `register_bot()`
# Returns a list with `name` and `metadata` (or NULLs if not present).
get_registered_bot_info <- function(bot_fn) {
  if (!is.function(bot_fn)) {
    stop("`bot_fn` must be a function.")
  }

  list(
    name = attr(bot_fn, "bot_name", exact = TRUE),
    metadata = attr(bot_fn, "bot_metadata", exact = TRUE)
  )
}
