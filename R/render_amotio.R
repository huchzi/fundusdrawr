# Verwende generate_svg_ring zur Darstellung der Amotio
render_detachment_list <- function(amotio_list, outer_radius = 120, default_inner_radius = 60) {
  if (is.null(amotio_list)) return("")

  # Standard: alle Positionen auf äußeren Radius
  inner_radii <- rep(outer_radius, 12)

  for (amotio in amotio_list) {
    if (!is.list(amotio)) next

    # Prüfen, ob "uhrzeit" vorhanden ist
    if (!is.null(amotio$uhrzeit)) {
      stunde <- suppressWarnings(as.numeric(amotio$uhrzeit))

      if (!is.na(stunde) && stunde >= 1 && stunde <= 12) {
        index <- ((stunde - 1) %% 12) + 1

        # Radius sicher extrahieren
        if (!is.null(amotio$radius)) {
          radius_value <- suppressWarnings(as.numeric(amotio$radius))
          if (!is.na(radius_value)) {
            inner_radii[index] <- radius_value
          } else {
            inner_radii[index] <- default_inner_radius
          }
        } else {
          inner_radii[index] <- default_inner_radius
        }
      }
    }
  }

  generate_svg_ring(outer_radius = outer_radius, inner_radii = inner_radii)
}
