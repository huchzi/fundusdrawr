lattice <- function(obj, parent) {
  type <- obj$type
  from <- obj$from
  to <- obj$to

  if (exists("obj$eccentricity")) {
    eccentricity <- obj$eccentricity
  } else {
    warning("Lattice: eccentricity set to default of 95°.")
    eccentricity <- 95
  }

  coords <- clock_to_xy(c(from, to), rep(eccentricity, 2))

  lattice <- glue::glue('<defs> <path id="bogen" d="M {coords$x[1]},{coords$y[1]}
                        A {eccentricity},{eccentricity} 0 0,1 {coords$x[2]},{coords$y[2]}" />
                        </defs>
                        <text font-size="20" fill="black">
                        <textPath href="#bogen">XXXXXXXXXXXXXXXXX</textPath>
                        </text>
                        <path href="#bogen" stroke="black"/>')

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

  #   lattice_border <- glue::glue('<path d="
  # M {coords$x[1]},{coords$y[1]}
  # A 85,85 0 0,1 {coords$x[2]},{coords$y[2]}
  # A 5,5 0 0,0 {coords$x[4]},{coords$y[4]}
  # A 105,105 0 0,0 {coords$x[3]},{coords$y[3]}
  # A 5,5 0 0,0 {coords$x[1]},{coords$y[1]}
  # "
  # fill="none"
  # stroke="black"
  # stroke-width="2"/>')

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

  xml2::xml_add_child(new_group, lattice)
}
