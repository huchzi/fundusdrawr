# Generate the SVG file from a content string
#' @export
fundus_image <- function(svg_content, scale_image = 1) {
  h <- 400 * scale_image
  w <- 400 * scale_image
  svg_header <- glue::glue('<svg width="{w}" height="{h}" xmlns="http://www.w3.org/2000/svg">\n')
  svg_footer <- '</svg>\n'

  stringr::str_c(svg_header, glue::glue('<g transform="scale({scale_image})">\n{svg_content}\n</g>'), svg_footer)
}

# Mirror a group in left by adding a transform
mirror_group <- function(content) {
  glue::glue('<g transform="translate(400,0) scale(-1,1)">\n{content}</g>\n')
}

mirror_left_eyes <- function(content, list_data) {
  is_left <- tolower(list_data$seite %||% "rechts") == "links"
  ifelse(is_left, mirror_group(content), content)
}

# SVG-Generator
generate_svg_content <- function(list_data, template_content) {

  stringr::str_c(
    mirror_left_eyes(template_content, list_data),
    ora_clip,
    render_amotio(list_data$amotio),
    render_tear_list(list_data$risse),
    sep = "\n"
  )

}
