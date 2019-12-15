test_that("leaflet", {
  address = '五角场和颐'
  city = '上海'
  res.amap = poi.amap(keywords=address, city=city, types='酒店')
  res.baidu = poi.baidu(query=address, region=city, tag='酒店',
                        ret_coordtype = 'gcj02ll')
  data = bind_rows(c(label='amap_res', parse_poi(res.amap)),
                   c(label='baidu_res', parse_poi(res.baidu))
  )
})
