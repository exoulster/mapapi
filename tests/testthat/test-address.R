test_that("address", {
  expect_equal(get_city_list('上海市'), '上海市')
  expect_equal(get_city_list('北京市'), '北京市')
  expect_equal(get_city_list('浙江省'), c('杭州市', '宁波市', '温州市', '嘉兴市',
    '湖州市', '绍兴市', '金华市', '衢州市', '舟山市', '台州市', '丽水市'))
})
