# Generate the SVG file from a content string
#' @export
fundus_image <- function(eye_side, objects, clip = TRUE, scale_image = 1) {
  h <- 400 * scale_image
  w <- 400 * scale_image

  fundus_template_xml <- create_template(eye_side, clip = clip, scale_image)

  lesions <- xml_find_first(fundus_template_xml, ".//g[@id='lesions']")

  for (elem in render_objects(objects)) {
    xml_add_child(lesions, elem)
  }
  xml_code <- as.character(fundus_template_xml)

  return(xml_code)
}

# Mirror a group in left by adding a transform
mirror_group <- function(content) {
  glue::glue('<g transform="translate(400,0) scale(-1,1)">\n{content}</g>\n')
}

mirror_left_eyes <- function(content, list_data) {
  is_left <- tolower(list_data$seite %||% "rechts") == "links"
  ifelse(is_left, mirror_group(content), content)
}
