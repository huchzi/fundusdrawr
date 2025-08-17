sort_items <- function(item_list) {
  ord <- sapply(item_list, function(x) x$type) |>
    factor(levels = c("detachment", "equatorial", "tear"), ordered = TRUE) |>
    order()
  item_list[ord]
}
