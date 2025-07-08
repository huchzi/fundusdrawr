# SVG-Inhalt als Text laden
load_template_svg <- function(path) {
  svg_lines <- readLines(path, warn = FALSE)
  svg_content <- paste(svg_lines, collapse = "\n")
  # Entferne <svg ...> und </svg>
  gsub("^.*<svg[^>]*>|</svg>.*$", "", svg_content)
}
