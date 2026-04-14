lattice <- function(obj, parent) {

if (is.null(obj$from) || is.null(obj$to)) {
  stop("Lattice requires 'from' and 'to'.")
}
from <- as.numeric(obj$from)
to <- as.numeric(obj$to)

if (is.null(obj$eccentricity)) {
  warning("Lattice: eccentricity set to default of 95°.")
  eccentricity <- 95
} else {
  eccentricity <- obj$eccentricity
}


  coords <- clock_to_xy(c(from, to), rep(eccentricity, 2))

  coords <- clock_to_xy(
    c(
      from, to,
      from, to
    ),
    c(
      eccentricity - 3, eccentricity - 3,
      eccentricity + 17, eccentricity + 17
    )
  )

  new_group <- xml2::xml_add_child(parent, "g")

  xml2::xml_add_child(new_group, "path",
    d = glue::glue("M {coords$x[1]},{coords$y[1]}
    A 85,85 0 0,1 {coords$x[2]},{coords$y[2]}
    A 5,5 0 0,0 {coords$x[4]},{coords$y[4]}
    A 105,105 0 0,0 {coords$x[3]},{coords$y[3]}
    A 5,5 0 0,0 {coords$x[1]},{coords$y[1]}"),
    fill = "none",
    stroke = "black",
    `stroke-width` = "2"
  )

  defs <- xml2::xml_add_child(new_group, "defs")
  
  path_id <- glue::glue("lattice-{from}-{to}-{eccentricity}")
  arc_d <- glue::glue('M {coords$x[1]},{coords$y[1]} A {eccentricity},{eccentricity} 0 0,1 {coords$x[2]},{coords$y[2]}')
  arc <- xml2::xml_add_child(defs, "path", id = path_id, d = arc_d)
  text <- xml2::xml_add_child(new_group, "text", `font-size` = "20", fill = "black")
  xml2::xml_add_child(text, "textPath", href = glue::glue("#{path_id}"), "XXXXXXXXXXXXXXXXX")

}
