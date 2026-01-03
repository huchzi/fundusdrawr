prompt <- 'Erstelle ausschließlich valides JSON (ohne Begleittext) nach folgendem Schema
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/ophthalmology-schema.json",
  "title": "Retina Lesion Data",
  "description": "Schema für verschiedene ophthalmologische Strukturen (detachment, equatorial, tear).",
  "type": "array",
  "items": {
    "type": "object",
    "required": ["type"],
    "properties": {
      "type": {
        "type": "string",
        "enum": ["detachment", "equatorial", "tear"]
      }
    },
    "allOf": [
      {
        "if": { "properties": { "type": { "const": "detachment" } } },
        "then": {
          "required": ["inner_radii", "path"],
          "properties": {
            "inner_radii": {
              "type": "array",
              "description": "Innenradien der detachment-Grenzen.",
              "items": { "type": "number" },
              "minItems": 1
            },
            "path": {
              "type": "array",
              "description": "Liste der Punkte entlang des Ablösungsrandes.",
              "items": {
                "type": "object",
                "required": ["clock", "ecc", "cx", "cy", "N", "selected"],
                "properties": {
                  "clock": { "type": "number", "description": "Position in Stunden auf dem Uhrzifferblatt (0–12)." },
                  "ecc": { "type": "number", "description": "Exzentrizität (Abstand vom Zentrum)." },
                  "cx": { "type": "number", "description": "X-Koordinate." },
                  "cy": { "type": "number", "description": "Y-Koordinate." },
                  "N": { "type": "integer", "description": "Laufende Punktnummer oder ID." },
                  "selected": { "type": "boolean", "description": "Markierungsstatus." }
                },
                "additionalProperties": false
              }
            }
          },
          "additionalProperties": false
        }
      },
      {
        "if": { "properties": { "type": { "const": "equatorial" } } },
        "then": {
          "required": ["from", "to"],
          "properties": {
            "from": { "type": "number", "description": "Startposition (z. B. Uhrzeit 0–12)." },
            "to": { "type": "number", "description": "Endposition (z. B. Uhrzeit 0–12)." }
          },
          "additionalProperties": false
        }
      },
      {
        "if": { "properties": { "type": { "const": "tear" } } },
        "then": {
          "required": ["clock", "eccentricity", "size"],
          "properties": {
            "clock": { "type": "number", "description": "Position der Ruptur in Stunden (0–12)." },
            "eccentricity": { "type": "number", "description": "Exzentrizität vom Zentrum." },
            "size": {
              "type": "string",
              "enum": ["small", "medium", "large"],
              "description": "Größenkategorie der Ruptur."
            }
          },
          "additionalProperties": false
        }
      }
    ]
  }
} für'
