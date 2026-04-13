source("poker_load_all.R")

# Example hold'em ranges built with new_range_holdem().
range_a <- new_range_holdem(
  data.frame(
    c1 = c("Ah", "Ks", "Qh"),
    c2 = c("Kd", "Qc", "Qs"),
    w  = c(3, 2, 1)
  ),
  label = "Range A"
)

range_b <- new_range_holdem(
  data.frame(
    c1 = c("2h", "3s", "4c"),
    c2 = c("5d", "6h", "7s"),
    w  = c(1, 1, 1)
  ),
  label = "Range B"
)

fixed_hand <- data.frame(
  rank = c("A", "K"),
  suit = c("s", "h"),
  stringsAsFactors = FALSE
)

cat("Range vs range equity:\n")
print(holdem_equity_mc_fast(list(range_a, range_b), n_sims = 1000))

cat("Range vs fixed hand equity:\n")
print(holdem_equity_mc_fast(list(range_a, fixed_hand), n_sims = 1000))

cat("Invalid range elimination test (expected error):\n")
invalid_range <- new_range_holdem(
  data.frame(
    c1 = c("Ah", "As"),
    c2 = c("Kd", "Kh"),
    w  = c(1, 1)
  ),
  label = "Invalid range"
)

board_known <- data.frame(
  rank = c("A", "K"),
  suit = c("h", "d"),
  stringsAsFactors = FALSE
)

tryCatch(
  {
    holdem_equity_mc_fast(list(invalid_range, fixed_hand), board_df = board_known, n_sims = 10)
    cat("ERROR: invalid range should not have produced equity.\n")
  },
  error = function(e) {
    cat("Caught expected error:\n")
    cat(conditionMessage(e), '\n')
  }
)
