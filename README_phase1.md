# Poker Bot Platform — Phase 1 Refactor

This directory reorganizes the current code base into a cleaner platform skeleton.

## Files

- `cards_and_hands.R` — deck logic, 5-card scoring, Hold'em/Omaha hand evaluation, simple deal/showdown helpers
- `poker_math.R` — pot odds, EV, MDF, bluff:value, geometric sizing
- `equity_tools.R` — fast Monte Carlo equity for Hold'em and Omaha
- `quant_tools.R` — Monte Carlo error bars, outs, ranges, board texture
- `bot_api.R` — student-facing bot contract, state sanitization, action validation
- `game_engine.R` — central game-state layer; currently scaffold plus minimal demos
- `example_bots.R` — random and starter bots
- `tournament_runner.R` — placeholder for match/tournament orchestration
- `viewer_app.R` — placeholder for a Shiny replay viewer
- `poker_load_all.R` — master loader

## What is already working

- Card and deck creation
- Hand scoring for 5-card poker hands
- Hold'em and Omaha best-hand evaluation
- Hold'em and Omaha Monte Carlo equity
- Basic poker math utilities
- Range/board-feature helper tools
- Minimal bot validation scaffold
- Minimal demo hand playback without betting logic

## What is intentionally left blank

- Full betting-round engine
- Blinds, antes, seat rotation, and button movement
- Pot accounting beyond trivial demos
- Side pots and all-in handling
- Match runner and tournament runner
- Log export pipeline
- Shiny replay viewer

## Suggested next step

Implement `initialize_hand()`, `run_betting_round()`, and `play_hand()` in `game_engine.R` for a small initial target such as heads-up Hold'em.
