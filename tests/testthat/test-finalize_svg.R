test_that("finalize_svg", {
  expect_equal(fundus_image(""),
               '<svg width="400" height="400" xmlns="http://www.w3.org/2000/svg">\n</svg>\n')
})
