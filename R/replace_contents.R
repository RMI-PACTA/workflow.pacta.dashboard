replace_contents <- function(
  data,
  display_currency
) {
  mutate(data, across(.cols = everything(), .fns = ~ gsub("_CUR_", display_currency, .x)))
}
