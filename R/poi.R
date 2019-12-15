
#' POI Search using amap
#' @details
#' Reference: https://lbs.amap.com/api/webservice/guide/api/search
#' @param keywords address and place keywords
#' @param city city
#' @export
poi.amap = function(keywords, city=NULL, types=NULL, citylimit=FALSE,
                    children=0, offset=20, page=1, extensions=c('base', 'all'),
                    sign=FALSE, output=c('JSON', 'XML'),
                    opts=list(), headers=list()) {
  path = 'v3/place/text'
  params = list(
    keywords=keywords,
    types=types,
    city=city,
    citylimit=citylimit,
    children=children,
    offset=offset,
    page=page,
    extensions=match.arg(extensions),
    output=match.arg(output)
  )

  res = request(vendor='amap', path=path, params=params, sign=sign, opts=opts, headers=headers)
  class(res) = c('amap', class(res))
  res
}


#' Baidu Map POI Search
#' @param query address and place keywords
#' @param region city or province name. Refer to http://wiki.lbsyun.baidu.com/cms/citycode/BaiduMap_cityCode_1102.zip
#' @param tag category e.g. 酒店、美食. Refer to http://lbsyun.baidu.com/index.php?title=lbscloud/poitags for more detail
#' @param city_limit TRUE/FALSE, whether to restrict result to the city
#' @param scope basic (1) or detailed (2) POI information
#' @param filter filter result
#' @param coord_type request coord_type, possible values include
#' \{1: 'wgs84ll', 2: 'gcj02ll', 3: 'bd09ll' (default), 4: 'bd09ll'\}
#' @param ret_coordtype response coord_type, possible value 'gcj02ll'. Refer to http://lbsyun.baidu.com/index.php?title=coordinate
#' @param output output type: json or xml
#' @details
#' Ref: http://lbsyun.baidu.com/index.php?title=webapi/guide/webservice-placeapi
#' @export
poi.baidu = function(query, region, tag=NULL, city_limit=NULL,
                     output=c('json', 'xml'), scope=c('1', '2'), filter=NULL,
                     coord_type=3, ret_coordtype=c('bd09ll', 'gcj02ll', 'bd09mc'),
                     page_size=10, page_num=0, sign=TRUE,
                     opts=list(), headers=list()) {
  path = 'place/v2/search'
  params = list(
    query=query,
    region=region,
    tag=tag,
    city_limit=city_limit,
    output=match.arg(output),
    scope=match.arg(scope),
    filter=filter,
    coord_type=coord_type,
    ret_coordtype=match.arg(ret_coordtype),
    page_size=page_size,
    page_num=page_num
  )

  res = request(vendor='baidu', path=path, params=params, sign=sign, opts=opts, headers=headers)
  class(res) = c('baidu', class(res))
  res
}


# id,id,uid
# name,name,name
# address,address,address
# province,pname,province
# city,cityname,city
# area,adname,area
# tel,tel,telephone
# lnglat,location,c(location.lng, location.lat)

#' @export
parse_poi = function(x) {
  UseMethod('parse_poi')
}

#' @export
parse_poi.amap = function(res) {
  if (res$status_code != 200) {
    stop('Request not successful')
  }

  if (!is.successful(res)) {
    stop('Result is not valid')
  }

  pois = res$parse(encoding='utf-8') %>%
    rjson::fromJSON() %>%
    .[['pois']]
  if(length(pois) > 0) {
    lst = pois[[1]]
    new_lst = list(
      id = lst[['id']],
      name = lst[['name']],
      address = lst[['address']],
      province = lst[['pname']],
      city = lst[['cityname']],
      area = lst[['adname']],
      tel = lst[['tel']],
      lnglat = lst[['location']]
    )
    new_lst[sapply(new_lst, is.null)] = NA  # replace NULL with NA
    new_lst
  } else {
    list()
  }
}

#' @export
parse_poi.baidu = function(res) {
  if (res$status_code != 200) {
    stop('Request not successful')
  }

  if (!is.successful(res)) {
    stop('Result is not valid')
  }

  pois = res$parse(encoding='utf-8') %>%
    rjson::fromJSON() %>%
    .[['results']]
  if(length(pois) > 0) {
    lst = pois[[1]]
    new_lst = list(
      id = lst[['uid']],
      name = lst[['name']],
      address = lst[['address']],
      province = lst[['province']],
      city = lst[['city']],
      area = lst[['area']],
      tel = lst[['telephone']],
      lnglat = paste(lst[['location']][['lng']], lst[['location']][['lat']], sep=',')
    )
    new_lst[sapply(new_lst, is.null)] = NA  # replace NULL with NA
    new_lst
  } else {
    list()
  }
}
