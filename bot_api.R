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

new_action <- function(type, amount = NULL) {
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

  list(
    name = name,
    bot = bot_fn,
    metadata = metadata
  )
}