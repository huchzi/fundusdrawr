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
