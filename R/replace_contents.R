replace_contents <- function(
  data,
  display_currency
) {
  dplyr::mutate(
    data,
    across(
      .cols = everything(),
      .fns = ~ gsub(
        pattern = "_CUR_",
        replacement = display_currency,
        x = .x,
        fixed = TRUE
      )
    )
  )
}
