detachment_obj <- list(type = "detachment", inner_radii = rep(180, 12))
detachment_obj$inner_radii[1:3] <-  c(90, 70, 90)

tear_obj <- list(type = "tear",
              lokalisation = "2",
              eccentricity = 95,
              groesse = "gross")

object_list <- list(detachment_obj, tear_obj)

stringr::str_c(ora_clip,
               fundus_template,
               render_objects(object_list)) |>
  fundus_image() |> writeLines(con = "inst/test.svg")
