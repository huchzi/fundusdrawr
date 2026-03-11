fundus_template_xml <- read_xml("data/fundus_template.txt")
new_group <- xml_add_child(fundus_template_xml, "g")
circles <- xml_find_all(
  fundus_template_xml,
  "./d1:circle",
  xml_ns(fundus_template_xml)
)

for (c in circles) {
  xml_add_child(new_group, c)
}

for (c in circles) {
  xml_remove(c)
}

#### Old code

stringr::str_c(
  ifelse(input$eye == "OS", ora_clip_OS, ora_clip),
  ifelse(input$eye == "OS",
    left_eye(fundusdrawr::fundus_template),
    fundusdrawr::fundus_template
  ),
  render_objects(fundus_items())
) |>
  fundus_image(scale_image = 1.3) |>
  HTML()

fundus_image("OS", tear_list, scale_image = 1) |> writeLines(con = "test.svg")
closed_form("a")
