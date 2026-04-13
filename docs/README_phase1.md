# Poker Bot Platform - Phase 1 Refactor

This directory reorganizes the code base into clearer folders for teaching and student work.

## Folder layout

- `core_internal/` - engine and game-state files students usually should not edit
- `shared_helpers/` - reusable math, range, and equity helper code
- `assignments_demos/` - labs, demos, and instructor-facing walkthroughs
- `reference_bots/` - example bots students can read for ideas
- `student_work/` - student-owned bot files and helper files
- `tests/` - local test scripts
- `docs/` - project notes

## Key files

- `core_internal/cards_and_hands.R` - deck logic, 5-card scoring, Hold'em/Omaha evaluation, simple deal/showdown helpers
- `core_internal/OmahaCode.R` - Omaha-specific evaluation and demo helpers
- `shared_helpers/poker_math.R` - pot odds, EV, MDF, bluff:value, geometric sizing
- `shared_helpers/equity_tools.R` - Monte Carlo equity for Hold'em and Omaha
- `shared_helpers/quant_tools.R` - Monte Carlo error bars, outs, ranges, board texture
- `core_internal/bot_api.R` - student-facing bot contract and action validation
- `core_internal/game_engine.R` - tournament and hand-state engine
- `reference_bots/example_bots.R` - sample bots
- `core_internal/tournament_runner.R` - tournament orchestration helpers
- `core_internal/viewer_app.R` - viewer app scaffold
- `assignments_demos/Bot_App_Student_Practice_Lab.R` - main student lab
- `assignments_demos/poker_demos.R` - demo functions
- `student_work/BotTemplate.R` - student bot template
- `student_work/student_helpers.R` - student-owned helper file
- `poker_load_all.R` - root loader entrypoint

## Working conventions

- Students should primarily work in `student_work/`.
- Students can read `reference_bots/` and `shared_helpers/`.
- Most students should not need to edit files in `core_internal/`.
- Use `source("poker_load_all.R")` from the project root to load the codebase.
