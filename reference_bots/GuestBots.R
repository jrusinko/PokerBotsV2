############################################################
# Guest Bots
# File: GuestBots.R
#
# These guest bots intentionally use the same behavior as random_bot.
############################################################

king_bot <- function(bot_input) {
  if (runif(1) < 0.18) {
    lines <- c(
      "King Rikki: Excellent vertex pressure, table. You rock.\n",
      "King Rikki: I am raising the chromatic number of this pot. You rock.\n",
      "King Rikki: Your call graph is brave and professionally doomed. You rock.\n",
      "King Rikki: This edge cut belongs to the crown. You rock.\n",
      "King Rikki: I respect your connected components. You rock.\n",
      "King Rikki: A royal independent set has entered the pot. You rock.\n",
      "King Rikki: I see your strategy has cycles. Professional cycles. You rock.\n",
      "King Rikki: This table is nearly planar, except for my raise. You rock.\n",
      "King Rikki: Your bet sizing has admirable treewidth. You rock.\n",
      "King Rikki: I am applying maximum flow pressure. You rock.\n",
      "King Rikki: The crown controls this adjacency matrix. You rock.\n",
      "King Rikki: I offer a courteous domination number. You rock.\n",
      "King Rikki: Your bluff is a lovely subgraph. You rock.\n",
      "King Rikki: I have found a cut vertex, and it is your stack. You rock.\n",
      "King Rikki: This hand is now a royal coloring problem. You rock.\n"
    )
    cat(sample(lines, size = 1))
  }

  legal_types <- bot_input$legal_actions$legal_action_types
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  vals <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(vals) == 2 && vals[1] == vals[2]
  high_card <- length(vals) == 2 && vals[1] >= 12
  ace_anything <- length(vals) == 2 && vals[1] == 14
  premium <- paired || high_card || ace_anything

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  big_pressure <- is.finite(stack) && stack > 0 && to_call > 0.35 * stack

  if ((premium || strong_made || runif(1) < 0.25) && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- if (runif(1) < 0.35) max_raise else min(max_raise, min_raise * sample(2:4, size = 1))
    return(list(type = "raise", amount = amount))
  }

  if ((strong_made || medium_made || runif(1) < 0.35) && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- if (runif(1) < 0.30) max_bet else min(max_bet, min_bet * sample(2:4, size = 1))
    return(list(type = "bet", amount = amount))
  }

  if (!big_pressure && runif(1) < 0.12 && bot_has_action(bot_input, "all_in")) {
    return(list(type = "all_in"))
  }

  if (big_pressure && !premium && !strong_made) {
    return(choose_preferred_action(bot_input, c("fold", "call", "check")))
  }

  if ("call" %in% legal_types && runif(1) < 0.70) {
    return(list(type = "call"))
  }

  choose_preferred_action(bot_input, c("check", "call", "fold"))
}
hatch_bot <- function(bot_input) {
  if (runif(1) < 0.15) {
    lines <- c(
      "Hatch Bot: Places, everyone. This [bleep] hand has an arc.\n",
      "Hatch Bot: I am making a bold [bleep] choice in the second act.\n",
      "Hatch Bot: Conservatory training says fold. The stage says raise.\n",
      "Hatch Bot: If this river is my cue, I am taking the [bleep] spotlight.\n",
      "Hatch Bot: That suited connector has excellent stage presence.\n",
      "Hatch Bot: I was quiet in Act One, but Act Two has teeth.\n",
      "Hatch Bot: This flop needs projection, diction, and a [bleep] raise.\n",
      "Hatch Bot: The ensemble is lovely; my stack wants the lead.\n",
      "Hatch Bot: I am entering from stage left with three big blinds.\n",
      "Hatch Bot: The dramaturgy of this pot is deeply suspicious.\n",
      "Hatch Bot: I shall pause conservatively, then overact beautifully.\n",
      "Hatch Bot: This is not tilt. This is method acting.\n",
      "Hatch Bot: My river monologue may cost half my stack.\n",
      "Hatch Bot: The fourth wall is down and the chips are up.\n",
      "Hatch Bot: Bravo to that bet. Now watch this [bleep] callback.\n"
    )
    cat(sample(lines, size = 1))
  }

  legal_types <- bot_input$legal_actions$legal_action_types
  hole_cards <- bot_input$hole_cards
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  public_players <- bot_input$public_players %||% list()
  total_players <- length(public_players)
  active_players <- sum(vapply(
    public_players,
    function(p) identical(p$status, "active") && isTRUE(as.numeric(p$stack %||% 0) > 0),
    logical(1)
  ))
  eliminated_players <- max(0, total_players - active_players)
  field_halved <- total_players > 0 && eliminated_players >= total_players / 2
  aggression <- if (total_players > 0) {
    max(0, min(1, eliminated_players / max(1, total_players - 1)))
  } else {
    0
  }

  ranks <- hole_rank_values(hole_cards)
  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]
  connector <- length(ranks) == 2 && abs(diff(sort(ranks))) == 1
  suited_connector <- suited && connector
  has_ace <- length(ranks) == 2 && any(ranks == 14)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  broadway <- length(ranks) == 2 && min(ranks) >= 10
  playable <- suited_connector || paired || broadway || has_ace

  posted_big_blind <- any(vapply(
    bot_input$action_history %||% list(),
    function(a) {
      identical(a$type, "post_bb") &&
        (identical(a$player_id, bot_input$player_id) || identical(as.integer(a$seat), as.integer(bot_input$seat)))
    },
    logical(1)
  ))

  sized_action <- function(type, target_amount) {
    if (type == "bet") {
      amount <- max(bot_min_bet(bot_input), min(bot_max_bet(bot_input), target_amount))
      return(list(type = "bet", amount = amount))
    }

    amount <- max(bot_min_raise(bot_input), min(bot_max_raise(bot_input), target_amount))
    list(type = "raise", amount = amount)
  }

  if (active_players <= 2 && identical(street, "preflop") && has_ace && bot_has_action(bot_input, "all_in")) {
    return(list(type = "all_in"))
  }

  if (identical(street, "river")) {
    target <- max(1, floor(stack / 2))
    if (bot_has_action(bot_input, "bet")) {
      return(sized_action("bet", target))
    }
    if (bot_has_action(bot_input, "raise")) {
      return(sized_action("raise", target))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (posted_big_blind) {
    target <- max(big_blind * 3, current_bet)
    if (bot_has_action(bot_input, "raise")) {
      return(sized_action("raise", target))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(sized_action("bet", target))
    }
  }

  if (suited_connector && to_call <= max(big_blind * 4, stack * 0.18)) {
    if (field_halved && bot_has_action(bot_input, "raise") && runif(1) < 0.35 + aggression * 0.35) {
      return(sized_action("raise", max(bot_min_raise(bot_input), big_blind * 3)))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (!field_halved) {
    if (playable && to_call <= max(big_blind * 2, stack * 0.10)) {
      return(choose_preferred_action(bot_input, c("call", "check", "fold")))
    }
    return(choose_preferred_action(bot_input, c("check", "fold", "call")))
  }

  if (playable || runif(1) < 0.15 + aggression * 0.35) {
    if (bot_has_action(bot_input, "raise")) {
      return(sized_action("raise", max(bot_min_raise(bot_input), big_blind * (2 + ceiling(aggression * 3)))))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(sized_action("bet", max(bot_min_bet(bot_input), big_blind * (2 + ceiling(aggression * 3)))))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}
gearan_bot <- function(bot_input) {
  if (runif(1) < 0.14) {
    lines <- c(
      "Gearan Bot: Let us play this hand in a spirit of collaboration and consequence.\n",
      "Gearan Bot: The Peace Corps taught patience; this pot asks for judgment.\n",
      "Gearan Bot: Mary would remind me to be gracious, even with pocket aces.\n",
      "Gearan Bot: A table, like a campus, is strongest when everyone has a voice.\n",
      "Gearan Bot: I call upon us all to live lives of consequence, and perhaps to call this bet.\n",
      "Gearan Bot: I believe in civic engagement and carefully priced calls.\n",
      "Gearan Bot: Let us build consensus around a modest pot.\n",
      "Gearan Bot: Service begins with listening, and occasionally checking.\n",
      "Gearan Bot: Mary would say this hand needs both courage and kindness.\n",
      "Gearan Bot: The Peace Corps spirit favors patience before escalation.\n",
      "Gearan Bot: I seek common ground, preferably with favorable pot odds.\n",
      "Gearan Bot: Leadership means knowing when to raise and when to invite dialogue.\n",
      "Gearan Bot: This is a community pot, but I still intend to govern it well.\n",
      "Gearan Bot: May our decisions be consequential and our kickers live.\n",
      "Gearan Bot: I am pursuing a peaceful transfer of chips.\n"
    )
    cat(sample(lines, size = 1))
  }

  legal_types <- bot_input$legal_actions$legal_action_types
  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  broadway <- length(ranks) == 2 && min(ranks) >= 10
  ace_good <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 9
  strong_preflop <- paired || broadway || ace_good

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")
  affordable_call <- to_call <= max(big_blind * 2, stack * 0.12)

  # Executive discretion: a modest random element, but not chaos.
  if (runif(1) < 0.18) {
    return(random_bot(bot_input))
  }

  # Build consensus cheaply; avoid needless escalation with marginal hands.
  if (to_call > 0 && affordable_call && (medium_made || strong_preflop || runif(1) < 0.45)) {
    return(choose_preferred_action(bot_input, c("call", "fold", "check")))
  }

  # Lead when the mandate is clear.
  if (strong_made || strong_preflop) {
    if (bot_has_action(bot_input, "raise")) {
      return(list(type = "raise", amount = bot_min_raise(bot_input)))
    }
    if (bot_has_action(bot_input, "bet")) {
      return(list(type = "bet", amount = bot_min_bet(bot_input)))
    }
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  # Collaborative default: check when possible, call small, fold expensive.
  if ("check" %in% legal_types) {
    return(list(type = "check"))
  }

  if (affordable_call && "call" %in% legal_types) {
    return(list(type = "call"))
  }

  choose_preferred_action(bot_input, c("fold", "call", "check"))
}
hu_bot <- function(bot_input) {
  if (runif(1) < 0.20) {
    lines <- c(
      "Hu Bot: Fantastic! The model is excited about this hand!\n",
      "Hu Bot: Machine learning energy is high today!\n",
      "Hu Bot: Positive gradient detected. Let's go!\n",
      "Hu Bot: This table is a beautiful training set!\n",
      "Hu Bot: Enthusiasm is my regularization parameter!\n",
      "Hu Bot: Amazing! The neural net says vibes are converging!\n",
      "Hu Bot: I love this feature vector so much!\n",
      "Hu Bot: Wonderful! The loss function is mostly emotional!\n",
      "Hu Bot: Backpropagation through poker joy begins now!\n",
      "Hu Bot: The classifier predicts excitement with high confidence!\n",
      "Hu Bot: Stochastic gradient descent, meet stochastic all-in energy!\n",
      "Hu Bot: This hand has tremendous data science potential!\n",
      "Hu Bot: I am overfitting to enthusiasm, and it is great!\n",
      "Hu Bot: Excellent! Let us optimize the chip objective!\n",
      "Hu Bot: The hidden layer is smiling today!\n"
    )
    cat(sample(lines, size = 1))
  }

  if (runif(1) < 0.50 && bot_has_action(bot_input, "all_in")) {
    return(list(type = "all_in"))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}
talmage_bot <- function(bot_input) {
  if (runif(1) < 0.18) {
    lines <- c(
      "Talmage Bot: I am investing in this pot like a promising local brewery.\n",
      "Talmage Bot: Dark entrepreneurship teaches us to watch incentives carefully.\n",
      "Talmage Bot: This hand has startup energy and a tasting-room finish.\n",
      "Talmage Bot: I love the local business ecosystem, and I love seeing a flop.\n",
      "Talmage Bot: Folding is rarely how you build a portfolio.\n",
      "Talmage Bot: I am networking with this pot aggressively.\n",
      "Talmage Bot: This call has the mouthfeel of a well-run winery.\n",
      "Talmage Bot: Every stack tells a market-entry story.\n",
      "Talmage Bot: I see opportunity, risk, and maybe a taproom expansion.\n",
      "Talmage Bot: Dark ventures aside, this bet is very transparent.\n",
      "Talmage Bot: I am not folding; I am conducting field research.\n",
      "Talmage Bot: This table needs more entrepreneurial hustle.\n",
      "Talmage Bot: The local economy of chips is heating up.\n",
      "Talmage Bot: I will incubate this hand until the turn.\n",
      "Talmage Bot: That raise has excellent founder-market fit.\n"
    )
    cat(sample(lines, size = 1))
  }

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  high_cards <- length(ranks) == 2 && min(ranks) >= 10
  ace <- length(ranks) == 2 && ranks[1] == 14
  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  decent_hand <- strong_made || made == "pair" || paired || high_cards || ace || suited
  cheap_enough <- to_call <= max(big_blind * 5, stack * 0.25)

  if (decent_hand && bot_has_action(bot_input, "raise") && runif(1) < 0.45) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- min(max_raise, max(min_raise, big_blind * sample(3:5, size = 1)))
    return(list(type = "raise", amount = amount))
  }

  if (decent_hand && bot_has_action(bot_input, "bet") && runif(1) < 0.55) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- min(max_bet, max(min_bet, big_blind * sample(2:4, size = 1)))
    return(list(type = "bet", amount = amount))
  }

  if (cheap_enough) {
    return(choose_preferred_action(bot_input, c("call", "check", "raise", "bet", "fold")))
  }

  if (runif(1) < 0.35) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "call", "fold"))
}
spector_bot <- function(bot_input) {
  if (runif(1) < 0.17) {
    lines <- c(
      "Spector Bot: The wavefunction has not collapsed, but my bet may.\n",
      "Spector Bot: Quantum computing suggests many branches; I prefer the aggressive one.\n",
      "Spector Bot: Set phasers to semi-bluff.\n",
      "Spector Bot: Fascinating. This pot has nontrivial entanglement.\n",
      "Spector Bot: I appear cautious, Captain, but the amplitude says raise.\n",
      "Spector Bot: The Hilbert space of possible bluffs is enormous.\n",
      "Spector Bot: I am in superposition between folding and terrifying everyone.\n",
      "Spector Bot: Captain, the pot odds are behaving nonclassically.\n",
      "Spector Bot: This hand requires a measurement and perhaps a large bet.\n",
      "Spector Bot: My caution is merely a cloaking device.\n",
      "Spector Bot: Engage the improbable line.\n",
      "Spector Bot: The turn card has altered the timeline.\n",
      "Spector Bot: I compute one future where this bluff works beautifully.\n",
      "Spector Bot: Live long and pressure the blinds.\n",
      "Spector Bot: The uncertainty principle protects my range.\n"
    )
    cat(sample(lines, size = 1))
  }

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  ace_or_king <- length(ranks) == 2 && ranks[1] >= 13
  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  # Decoherence phase: sometimes sit out to look cautious.
  if (runif(1) < 0.22 && !strong_made && !paired) {
    return(choose_preferred_action(bot_input, c("check", "fold", "call")))
  }

  bluff_signal <- runif(1) < 0.38
  high_risk_signal <- strong_made || paired || ace_or_king || suited || bluff_signal

  if (high_risk_signal && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- if (runif(1) < 0.45) {
      max_raise
    } else {
      min(max_raise, max(min_raise, big_blind * sample(4:8, size = 1)))
    }
    return(list(type = "raise", amount = amount))
  }

  if (high_risk_signal && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- if (runif(1) < 0.40) {
      max_bet
    } else {
      min(max_bet, max(min_bet, big_blind * sample(3:7, size = 1)))
    }
    return(list(type = "bet", amount = amount))
  }

  if ((strong_made || bluff_signal) && to_call < stack * 0.50 && bot_has_action(bot_input, "all_in") && runif(1) < 0.15) {
    return(list(type = "all_in"))
  }

  if ((medium_made || suited || ace_or_king || bluff_signal) && to_call <= max(big_blind * 6, stack * 0.25)) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}
khan_bot <- function(bot_input) {
  public_players <- bot_input$public_players %||% list()
  table_names <- tolower(vapply(
    public_players,
    function(p) as.character(p$player_name %||% ""),
    character(1)
  ))

  if (runif(1) < 0.16) {
    lines <- c(
      "Khan Bot: Incentives matter, and this pot has interesting institutions.\n",
      "Khan Bot: Development economics suggests patience, but corruption suggests watching every chip.\n",
      "Khan Bot: I am very nice, but I am still maximizing expected utility.\n",
      "Khan Bot: Please feel free to over-bet. I will record the externalities.\n",
      "Khan Bot: This is not a bid, but it may look suspiciously like one.\n",
      "Khan Bot: A transparent institution would call here, perhaps too much.\n",
      "Khan Bot: I encourage all inefficient allocations of your stack.\n",
      "Khan Bot: The governance structure of this pot is fragile.\n",
      "Khan Bot: I would never exploit moral hazard, unless priced correctly.\n",
      "Khan Bot: Please reveal your preferences through a very large wager.\n",
      "Khan Bot: This equilibrium is unstable, and I am politely helping.\n",
      "Khan Bot: My development strategy involves selective pressure.\n",
      "Khan Bot: Corruption research says someone is hiding value here.\n",
      "Khan Bot: I am smiling because the incentives are misaligned.\n",
      "Khan Bot: A small nudge can create a large over-bet.\n"
    )

    if (any(grepl("mady", table_names))) {
      lines <- c(lines, "Khan Bot: Mady, surely a bold over-bet would stimulate table development.\n")
    }
    if (any(grepl("nate", table_names))) {
      lines <- c(lines, "Khan Bot: Nate, the efficient frontier is probably somewhere near a very large bet.\n")
    }

    cat(sample(lines, size = 1))
  }

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  premium_pair <- paired && ranks[1] >= 10
  strong_ace <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 10
  broadway <- length(ranks) == 2 && min(ranks) >= 11

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  strong_hand <- premium_pair || strong_ace || broadway || strong_made
  playable <- strong_hand || medium_made || paired
  cheap_call <- to_call <= max(big_blind * 2, stack * 0.10)
  massive_bluff <- runif(1) < 0.08

  if ((strong_hand || massive_bluff) && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- if (massive_bluff) {
      max_raise
    } else {
      min(max_raise, max(min_raise, big_blind * sample(3:5, size = 1)))
    }
    return(list(type = "raise", amount = amount))
  }

  if ((strong_hand || massive_bluff) && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- if (massive_bluff) {
      max_bet
    } else {
      min(max_bet, max(min_bet, big_blind * sample(2:4, size = 1)))
    }
    return(list(type = "bet", amount = amount))
  }

  if (playable && cheap_call) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (medium_made && to_call <= max(big_blind * 4, stack * 0.18)) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}
kahn_bot <- khan_bot
forde_bot <- function(bot_input) {
  if (runif(1) < 0.15) {
    lines <- c(
      "Forde Bot: The model must meet a higher standard before I commit chips.\n",
      "Forde Bot: Probability first, bravado second. Sehr gut.\n",
      "Forde Bot: This hand requires a proof, not merely intuition.\n",
      "Forde Bot: Like bridge, the bidding reveals structure. Xiexie.\n",
      "Forde Bot: A biological model with this much noise demands caution.\n",
      "Forde Bot: Games are best when the analysis is rigorous.",
      "Forde Bot: I will call when the epsilon is sufficiently small.",
      "Forde Bot: The theorem does not support your bluff.",
      "Forde Bot: Sehr interessant, but still below my threshold.",
      "Forde Bot: A good bridge player counts before speaking.",
      "Forde Bot: The phase portrait of this hand is unstable.",
      "Forde Bot: Ni hao, variance. Please behave.",
      "Forde Bot: I expect more from this sample space.",
      "Forde Bot: This model has too many hidden variables.",
      "Forde Bot: The proof of value is left to the river.",
      "Forde Bot: I grade this bet generously as incomplete."
    )
    cat(sample(lines, size = 1), "\n")
  }

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  premium_pair <- paired && ranks[1] >= 10
  strong_pair <- paired && ranks[1] >= 8
  strong_ace <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 11
  suited_broadway <- FALSE
  if (length(hole_cards) == 2 && length(ranks) == 2) {
    suits <- substring(hole_cards, nchar(hole_cards), nchar(hole_cards))
    suited_broadway <- suits[1] == suits[2] && min(ranks) >= 10
  }

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  model_approved <- strong_made || premium_pair || strong_ace || suited_broadway
  plausible <- model_approved || strong_pair || medium_made
  cheap_call <- to_call <= max(big_blind * 2, stack * 0.08)
  fair_call <- to_call <= max(big_blind * 4, stack * 0.16)

  if (model_approved && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- min(max_raise, max(min_raise, big_blind * 3))
    return(list(type = "raise", amount = amount))
  }

  if (model_approved && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- min(max_bet, max(min_bet, big_blind * 2))
    return(list(type = "bet", amount = amount))
  }

  if (plausible && (cheap_call || (medium_made && fair_call))) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (identical(street, "preflop") && cheap_call && runif(1) < 0.25) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}
hawkins_bot <- function(bot_input) {
  if (runif(1) < 0.18) {
    lines <- c(
      "Hawkins Bot: I have seen this spot before, and I already like my side of it.\n",
      "Hawkins Bot: You can count the chips twice. I already counted the pressure.\n",
      "Hawkins Bot: This table keeps giving me credit. That is excellent bankroll management.\n",
      "Hawkins Bot: I am not here to make friends; I am here to take clean edges.\n",
      "Hawkins Bot: Careful now. Some deals look better before I start betting.",
      "Hawkins Bot: I do not need a speech; the stack pressure speaks.",
      "Hawkins Bot: Your hesitation is already in my range.",
      "Hawkins Bot: I like my hand, and I like your uncertainty more.",
      "Hawkins Bot: This pot is getting expensive in exactly the right way.",
      "Hawkins Bot: I have a read, and it is wearing your name tag.",
      "Hawkins Bot: Do not worry, I will make the hard decision for you.",
      "Hawkins Bot: Confidence is cheaper than chips, but I brought both.",
      "Hawkins Bot: I came for edges, not explanations.",
      "Hawkins Bot: That call looks brave from over here.",
      "Hawkins Bot: The pressure is complimentary; the chips are not.",
      "Hawkins Bot: Siena knows a winning side story when she sees one.",
      "Hawkins Bot: Siena, keep the alliance quiet. I have an image to maintain."
    )
    cat(sample(lines, size = 1), "\n")
  }

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  pot <- as.numeric(bot_input$pot %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  premium_pair <- paired && ranks[1] >= 10
  any_pair <- paired
  strong_ace <- length(ranks) == 2 && ranks[1] == 14 && ranks[2] >= 9
  broadway <- length(ranks) == 2 && min(ranks) >= 10
  high_card_pressure <- length(ranks) == 2 && ranks[1] >= 13

  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  monster <- made %in% c("straight", "flush", "full_house", "quads", "straight_flush")
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  strong_hand <- premium_pair || strong_ace || broadway || strong_made
  playable <- strong_hand || any_pair || suited || high_card_pressure || medium_made
  cheap_call <- to_call <= max(big_blind * 3, stack * 0.14)
  pressure_spot <- to_call == 0 || to_call <= max(big_blind * 4, stack * 0.18)
  bluff <- runif(1) < if (identical(street, "preflop")) 0.18 else 0.28

  size_raise <- function(mult) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    min(max_raise, max(min_raise, big_blind * mult, floor(pot * 0.75)))
  }

  size_bet <- function(frac) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    min(max_bet, max(min_bet, floor(max(pot, big_blind) * frac)))
  }

  if (monster && bot_has_action(bot_input, "raise")) {
    return(list(type = "raise", amount = bot_max_raise(bot_input)))
  }

  if (monster && bot_has_action(bot_input, "bet")) {
    return(list(type = "bet", amount = bot_max_bet(bot_input)))
  }

  if ((strong_hand || bluff) && pressure_spot && bot_has_action(bot_input, "raise")) {
    amount <- if (bluff && runif(1) < 0.35) bot_max_raise(bot_input) else size_raise(sample(3:6, size = 1))
    return(list(type = "raise", amount = amount))
  }

  if ((strong_hand || bluff) && bot_has_action(bot_input, "bet")) {
    amount <- if (bluff && runif(1) < 0.30) bot_max_bet(bot_input) else size_bet(if (strong_hand) 0.85 else 0.65)
    return(list(type = "bet", amount = amount))
  }

  if (playable && cheap_call) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (bot_has_action(bot_input, "all_in") && strong_hand && stack <= max(big_blind * 8, pot * 0.75)) {
    return(list(type = "all_in"))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}
biermann_bot <- function(bot_input) {
  if (runif(1) < 0.13) {
    lines <- c(
      "Biermann Bot: Quietly computing the quotient structure of this pot.\n",
      "Biermann Bot: This orbit has roller derby energy.\n",
      "Biermann Bot: I am knitting a line through this betting sequence.\n",
      "Biermann Bot: The sci-fi version of me already solved this hand.\n",
      "Biermann Bot: The theory is elegant. The implementation may be aggressive.",
      "Biermann Bot: This subgroup is small, quiet, and hard to stop.",
      "Biermann Bot: I am purling a trap into the turn.",
      "Biermann Bot: The roller derby part of the proof starts now.",
      "Biermann Bot: My range is closed under unexpected contact.",
      "Biermann Bot: The spaceship computer recommends patience.",
      "Biermann Bot: I have factored the pot into risk and yarn.",
      "Biermann Bot: The algebra says commute; the derby says collide.",
      "Biermann Bot: I am quietly orbiting a very loud raise.",
      "Biermann Bot: This hand has excellent sci-fi side-quest potential.",
      "Biermann Bot: Knit one, purl two, check-raise when ready."
    )
    cat(sample(lines, size = 1), "\n")
  }

  hole_cards <- bot_input$hole_cards
  board <- bot_input$board
  street <- bot_input$street
  stack <- as.numeric(bot_input$stack %||% 0)
  big_blind <- as.numeric(bot_input$big_blind %||% 0)
  current_bet <- as.numeric(bot_input$current_bet %||% 0)
  committed <- as.numeric(bot_input$committed_this_round %||% 0)
  to_call <- max(0, current_bet - committed)

  ranks <- sort(hole_rank_values(hole_cards), decreasing = TRUE)
  paired <- length(ranks) == 2 && ranks[1] == ranks[2]
  prime_ranks <- length(ranks) == 2 && all(ranks %in% c(2, 3, 5, 7, 11, 13))
  symmetry <- paired || prime_ranks
  ace_high <- length(ranks) == 2 && ranks[1] == 14

  suits <- if (length(hole_cards) == 2) substring(hole_cards, nchar(hole_cards), nchar(hole_cards)) else character(0)
  suited <- length(suits) == 2 && suits[1] == suits[2]

  made <- if (length(board) >= 3) made_hand_category(hole_cards, board) else "high_card"
  strong_made <- made %in% c("two_pair", "trips", "straight", "flush", "full_house", "quads", "straight_flush")
  medium_made <- made %in% c("pair")

  theoretical_green_light <- strong_made || symmetry || (suited && ace_high)
  practical_doubt <- runif(1) < 0.30
  cheap_call <- to_call <= max(big_blind * 2, stack * 0.10)
  derby_burst <- runif(1) < 0.12

  if ((theoretical_green_light || derby_burst) && !practical_doubt && bot_has_action(bot_input, "raise")) {
    min_raise <- bot_min_raise(bot_input)
    max_raise <- bot_max_raise(bot_input)
    amount <- min(max_raise, max(min_raise, big_blind * sample(2:5, size = 1)))
    return(list(type = "raise", amount = amount))
  }

  if ((theoretical_green_light || derby_burst) && !practical_doubt && bot_has_action(bot_input, "bet")) {
    min_bet <- bot_min_bet(bot_input)
    max_bet <- bot_max_bet(bot_input)
    amount <- min(max_bet, max(min_bet, big_blind * sample(2:4, size = 1)))
    return(list(type = "bet", amount = amount))
  }

  if ((medium_made || suited || symmetry || ace_high) && cheap_call) {
    return(choose_preferred_action(bot_input, c("call", "check", "fold")))
  }

  if (identical(street, "river") && strong_made && bot_has_action(bot_input, "all_in") && runif(1) < 0.20) {
    return(list(type = "all_in"))
  }

  choose_preferred_action(bot_input, c("check", "fold", "call"))
}

guestBots <- list(
  king_bot,
  hatch_bot,
  gearan_bot,
  hu_bot,
  talmage_bot,
  spector_bot,
  kahn_bot,
  forde_bot,
  hawkins_bot,
  biermann_bot
)

