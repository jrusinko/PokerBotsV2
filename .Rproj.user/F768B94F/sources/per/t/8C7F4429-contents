############################################################
# Mathematics of Poker — Master Loader
# File: poker_load_all.R
#
# Purpose:
#   Loads all core modules for the refactored platform.
############################################################
poker_load_all <- function(include_demos = FALSE, verbose = TRUE) {

  required_files <- c(
    "cards_and_hands.R",
    "poker_math.R",
    "equity_tools.R",
    "quant_tools.R",
    "bot_api.R",
    "game_engine.R",
    "example_bots.R",
    "tournament_runner.R",
    "viewer_app.R"
  )

  # Optional demo file (NOT loaded by default)
  demo_file <- "poker_demos.R"

  # --- Check all required files exist BEFORE sourcing ---
  missing <- required_files[!file.exists(required_files)]
  if (length(missing) > 0) {
    stop(paste("Missing required files:", paste(missing, collapse = ", ")))
  }

  # --- Source core files ---
  for (f in required_files) {
    if (verbose) cat("Loading:", f, "\n")
    source(f, local = FALSE)
  }

  # --- Optionally load demos ---
  if (include_demos) {
    if (file.exists(demo_file)) {
      if (verbose) cat("Loading demo file:", demo_file, "\n")
      source(demo_file, local = FALSE)
    } else {
      warning("Demo file not found:", demo_file)
    }
  }

  if (verbose) {
    cat("\nAll poker modules loaded successfully.\n")
    if (!include_demos) {
      cat("Demos NOT loaded (set include_demos = TRUE to include them).\n")
    }
  }
}