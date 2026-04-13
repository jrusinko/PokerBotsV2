############################################################
# Mathematics of Poker — Master Loader
# File: poker_load_all.R
#
# Purpose:
#   Loads the project modules from the reorganized folder structure.
############################################################
poker_load_all <- function(include_demos = FALSE, verbose = TRUE) {

  required_files <- c(
    "core_internal/cards_and_hands.R",
    "core_internal/OmahaCode.R",
    "shared_helpers/poker_math.R",
    "shared_helpers/equity_tools.R",
    "shared_helpers/quant_tools.R",
    "core_internal/bot_api.R",
    "core_internal/game_engine.R",
    "reference_bots/example_bots.R",
    "core_internal/tournament_runner.R",
    "core_internal/viewer_app.R"
  )

  # Optional demo file (NOT loaded by default)
  demo_file <- "assignments_demos/poker_demos.R"

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
