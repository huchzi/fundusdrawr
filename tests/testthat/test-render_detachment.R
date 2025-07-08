test_that("complete ring", {
  detachment <- rep(180, 12)
  detachment[4:8] <- 0 # detachment of inferior hemisphere

  detachment_svg <- '<path d="M 300.00 26.79 L 373.21 100.00 L 400.00 200.00 L 373.21 300.00 L 300.00 373.21 L 200.00 400.00 L 100.00 373.21 L 26.79 300.00 L 0.00 200.00 L 26.79 100.00 L 100.00 26.79 L 200.00 0.00 Z M 200.00 20.00 L 110.00 44.12 L 44.12 110.00 L 20.00 200.00 L 200.00 200.00 L 200.00 200.00 L 200.00 200.00 L 200.00 200.00 L 200.00 200.00 L 380.00 200.00 L 355.88 110.00 L 290.00 44.12 Z" fill="blue" fill-opacity="0.5" clip-path="url(#oraClip)"/>'

  expect_equal(render_detachment(detachment),
               detachment_svg)
})
