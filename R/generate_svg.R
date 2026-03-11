# Generate the SVG file from a content string
#' @export
fundus_image <- function(eye_side, objects, scale_image = 1) {
  h <- 400 * scale_image
  w <- 400 * scale_image
  svg_header <- glue::glue('<svg width="{w}" height="{h}" xmlns="http://www.w3.org/2000/svg">\n')
  svg_footer <- "</svg>\n"

  fundus_template_xml <- read_xml("data/fundus_template.txt")

  if (eye_side == "OS") xml_attr(fundus_template_xml, "transform") <- "translate(400 0) scale(-1 1)"

  for (elem in render_objects(objects)) {
    xml_add_child(fundus_template_xml, elem)
  }
  svg_content <- as.character(fundus_template_xml)

  stringr::str_c(
    svg_header,
    '<rect width="100%" height="100%" fill="white"/>',
    glue::glue('<g transform="scale({scale_image})">\n{svg_content}\n</g>'),
    svg_footer
  )
}

# Mirror a group in left by adding a transform
mirror_group <- function(content) {
  glue::glue('<g transform="translate(400,0) scale(-1,1)">\n{content}</g>\n')
}

mirror_left_eyes <- function(content, list_data) {
  is_left <- tolower(list_data$seite %||% "rechts") == "links"
  ifelse(is_left, mirror_group(content), content)
}
