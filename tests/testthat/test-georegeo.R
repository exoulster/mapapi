test_that("georegeo", {
  expect_true(is.successful.amap(geo.amap('上海五角场和颐酒店', sign=TRUE)))
  expect_true(is.successful.amap(regeo.amap('120,30', sign=TRUE)))
  expect_true(is.successful.baidu(geo.baidu('上海五角场和颐酒店', city='上海', sign=TRUE)))
  expect_true(is.successful.baidu(regeo.baidu('120,30', sign=TRUE)))
})
