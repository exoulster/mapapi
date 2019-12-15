test_that("sign.baidu", {
  path = '/geocoder/v2/'
  query = list(
    address='百度大厦',
    output='json',
    ak='yourak'
  )
  expect_equal(as.character(sign.baidu(path, query, 'yoursk')), '7de5a22212ffaa9e326444c75a58f9a0')
})
