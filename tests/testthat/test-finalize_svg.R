test_that("finalize_svg", {
  expect_equal(finalize_svg(""),
               '<svg width="400" height="400" xmlns="http://www.w3.org/2000/svg">\n</svg>\n')
})
