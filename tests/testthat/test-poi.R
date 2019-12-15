test_that("poi.amap", {
  expect_true({is.successful.amap(poi.amap('上海五角场和颐酒店', sign=TRUE))})
  expect_true({is.successful.baidu(poi.baidu('上海五角场和颐酒店', region='上海', sign=TRUE))})
})
