detachment <- rep(180, 12)
detachment[4:8] <- 0
stringr::str_c(ora_clip,
               fundus_template,
               render_detachment(180, detachment),
               render_tear(list(lokalisation=6, groesse = "gross"))) |>
  finalize_svg() |> writeLines(con = "inst/test.svg")
