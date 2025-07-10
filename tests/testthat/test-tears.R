test_that("two tears at 3 and 6 o'clock", {
  tear1 <- list(type = "tear",
                clock = "3",
               size = "large")
  tear2 <- list(type = "tear",
                clock = "6",
                size = "small")
  tear_list <- list(tear1, tear2)
  expect_equal(render_objects(tear_list),
               "<path d=\"M280,200 A20,20 0 0,0 310,200 A15,15 0 0,1 280,200 Z\" fill=\"red\" stroke=\"blue\" stroke-width=\"2\" transform=\"rotate(90, 295, 200)\" />\n<path d=\"M192,295 A12,12 0 0,0 208,295 A8,8 0 0,1 192,295 Z\" fill=\"red\" stroke=\"blue\" stroke-width=\"2\" transform=\"rotate(180, 200, 295)\" />")
})
