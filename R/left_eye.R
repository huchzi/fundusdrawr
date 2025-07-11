left_eye <- function(content) {
  glue::glue('<g transform="translate(400,0) scale(-1,1)">\n{content}</g>\n')
}
