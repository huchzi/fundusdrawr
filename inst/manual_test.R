detachment_list <- rep(180, 12)
detachment_list[1:3] <- c(90, 70, 90)
# detachment[4:8] <- 0

object_list <- list(detachment_list, tear_list)

stringr::str_c(ora_clip,
               fundus_template,
               detachment(detachment_list),
               tear(list(lokalisation=2, groesse = "gross", eccentricity = 95))) |>
  fundus_image() |> writeLines(con = "inst/test.svg")
