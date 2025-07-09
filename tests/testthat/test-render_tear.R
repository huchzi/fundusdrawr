test_that("tear at 1h", {
  tear_obj <- list(lokalisation = "3",
               groesse = "gross")

  expect_equal(tear(tear_obj),
               '<path d="M280,200 A20,20 0 0,0 310,200 A15,15 0 0,1 280,200 Z" fill="red" stroke="blue" stroke-width="2" transform="rotate(90, 295, 200)" />')
})
