library(jsonlite)

json_schema <- list()

json_schema$`$schema` <- "https://json-schema.org/draft/2020-12/schema"
json_schema$type <- "object"
json_schema$properties <- list(area = list(type = "object", items = list(
  type = "object",
  properties = list()
)))

toJSON(json_schema, pretty = TRUE, auto_unbox = TRUE)
