
test_that('reverse_location', {
  expect_true(is.na(reverse_location(NA)))
  expect_equal(reverse_location('120,30'), '30,120')
  expect_equal(reverse_location(c(NA, '120,30')), c(NA, '30,120'))
})

test_that('split_location', {
  expect_equal(split_location('120,30', simplify=FALSE), list(c(120, 30)))
  expect_equal(split_location(c(NA, '120,30'), simplify=FALSE), list(c(NA, NA), c(120, 30)))
  expect_equal(split_location(c('120,30','30,120'), simplify=FALSE), list(c(120, 30), c(30, 120)))
  expect_equal(split_location(c('120,30','30,120'), reverse=TRUE, simplify=FALSE), list(c(30, 120), c(120, 30)))
})

test_that('get_location', {
  expect_true(is.na(get_location(lnglat=NA)))
  expect_true(is.na(get_location(lnglat='')))
  expect_true(is.na(get_location(latlng=NA)))
  expect_true(is.na(get_location(latlng='')))
  expect_error(get_location(longitude=NA))
  expect_true(is.na(get_location(longitude=NA, latitude='30')))
  expect_true(is.na(get_location(longitude='', latitude='30')))
  expect_true(is.na(get_location(longitude='120', latitude=NA)))
  expect_true(is.na(get_location(longitude='120', latitude='')))
  expect_equal(get_location(lnglat='120,30'), '120,30')
  expect_equal(get_location(latlng='30,120'), '120,30')
  expect_equal(get_location(longitude='120', latitude='30'), '120,30')
})

