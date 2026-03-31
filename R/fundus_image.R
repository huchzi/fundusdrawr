# Generate the SVG file from a content string
#' @export
fundus_image <- function(eye_side, objects, clip = TRUE, scale_image = 1) {
  h <- 400 * scale_image
  w <- 400 * scale_image

  fundus_template_xml <- create_template(eye_side, clip = clip, scale_image)

  lesions <- xml_find_first(fundus_template_xml, ".//g[@id='lesions']")

  for (elem in objects) {
    print(elem$type)
    render_fun <- get(elem$type, mode = "function")
    print(render_fun)
    arguments <- names(formals(render_fun))
    stopifnot("obj" %in% arguments & "parent" %in% arguments)
    render_fun(obj = elem, parent = lesions)
  }
  xml_code <- as.character(fundus_template_xml)

  return(xml_code)
}
