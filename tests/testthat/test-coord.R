test_that("coord_convert", {
  expect_equal(coord_convert(c('120,30', '121,31'), 'wgs', 'wgs'), c('120,30', '121,31'))
  expect_equal(coord_convert(c('120,30', '121,31', NA), 'wgs', 'wgs'), c('120,30', '121,31', NA))
  expect_equal(is.na(coord_convert(c('120,30', '121,31', NA), from='GCJ-02', to='BD-09')),
              c(FALSE, FALSE, TRUE))
})

test_that('conversion', {
  expect_equal(as.vector(gcj2bd(120, 30)), c(120.00640999946, 30.006359999864998))
  expect_equal(as.vector(gcj2wgs(120, 30)), c(119.99533955440265,30.002465668303905))
  expect_equal(as.vector(bd2gcj(120, 30)), c(119.993590816153,29.9936621660547))
  expect_equal(as.vector(wgs2gcj(120, 30)), c(120.00466044559735,29.997534331696095))
  expect_equal(as.vector(wgs2bd(120, 30)), c(120.011070620552,30.0038830555128))
  expect_equal(as.vector(bd2wgs(120, 30)), c(119.988947478458,29.9961473316092))
})
