# Corrected Poker Bot Feature Matrix Documentation

This document describes the variables in the corrected bot feature matrix.


## Interpretation

- Each row corresponds to a poker bot.
- Each column corresponds to a strategic or structural feature.
- Binary coding:
  - 1 = feature appears present
  - 0 = feature does not appear present

## Feature Definitions

- **can_fold**: Bot contains fold logic.
- **can_check**: Bot contains check logic.
- **can_call**: Bot contains call logic.
- **can_bet**: Bot contains betting logic.
- **can_raise**: Bot contains raising logic.
- **can_all_in**: Bot contains explicit shove/all-in behavior.
- **uses_hole_cards**: Bot evaluates its private cards.
- **uses_board**: Bot references board/community cards.
- **uses_street**: Bot changes behavior by street.
- **uses_pot**: Bot references pot size.
- **uses_current_bet**: Bot references current bet or amount to call.
- **uses_call_amount**: Bot computes explicit call amounts.
- **uses_stack**: Bot references stack sizes.
- **uses_big_blind**: Bot references blind levels.
- **uses_public_players**: Bot references opponent/public player information.
- **uses_action_history**: Bot references previous actions.
- **uses_preflop_thresholds**: Bot uses preflop hand tiers or thresholds.
- **uses_made_hand_category**: Bot categorizes poker hands.
- **uses_board_texture**: Bot evaluates board texture.
- **uses_draw_detection**: Bot detects draws or outs.
- **uses_equity_estimate**: Bot estimates equity.
- **uses_monte_carlo_equity**: Bot uses simulation/Monte Carlo equity.
- **computes_pot_odds**: Bot computes pot odds or compares pot to call amount.
- **uses_ev_threshold**: Bot uses EV-style threshold logic.
- **uses_call_amount_threshold**: Bot compares action to call-size thresholds.
- **uses_pot_relative_sizing**: Bot sizes bets relative to pot.
- **uses_stack_relative_sizing**: Bot sizes bets relative to stack.
- **uses_min_bet_only**: Bot frequently uses minimum sizing.
- **uses_max_bet_or_shove**: Bot uses shove/jam/max pressure betting.
- **uses_randomness**: Bot randomizes decisions.
- **uses_opponent_modeling**: Bot models opponents.
- **uses_persistent_memory**: Bot stores persistent state across hands.
- **changes_by_street**: Bot strategy changes across streets.
- **changes_by_stack_depth**: Bot strategy changes by stack depth.
- **changes_by_tournament_context**: Bot changes behavior based on tournament context.