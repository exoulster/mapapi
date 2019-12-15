
#' @export
geo.amap = function(address, city=NULL,
                    batch=FALSE, sign=FALSE, output=c('JSON', 'XML'),
                    opts=list(), headers=list()) {
  path = 'v3/geocode/geo'
  params = list(
    address=address,
    city=city,
    batch=batch,
    match.arg(output)
  )

  request(vendor='amap', path=path, params=params, sign=sign, opts=opts, headers=headers)
}


#' @export
regeo.amap = function(lnglat, poitype=NULL, radius=1000,
                      extensions=c('base', 'all'), roadlevel=1,
                      batch=FALSE, sign=FALSE, output=c('JSON', 'XML'),
                    opts=list(), headers=list()) {
  path = 'v3/geocode/regeo'
  params = list(
    location=lnglat,
    poitype=poitype,
    radius=radius,
    extensions=match.arg(extensions),
    roadlevel=roadlevel,
    batch=batch,
    output=match.arg(output)
  )

  request(vendor='amap', path=path, params=params, sign=sign, opts=opts, headers=headers)
}


#' @export
geo.baidu = function(address, city,
                     ret_coordtype=c('bd09ll', 'gcj02ll', 'bd09mc'),
                     output=c('json', 'xml'),
                     sign=TRUE,
                     opts=list(), headers=list()) {
  path = 'geocoding/v3'
  params = list(
    address=address,
    city=city,
    ret_coordtype=match.arg(ret_coordtype),
    output=match.arg(output)
  )

  request(vendor='baidu', path=path, params=params, sign=sign, opts=opts, headers=headers)
}


#' @export
regeo.baidu = function(lnglat, coordtype=c('bd09ll', 'gcj02ll'),
                       ret_coordtype=c('bd09ll', 'gcj02ll', 'bd09mc'),
                       radius=1000, extensions_poi=0,
                       extensions_road=c('false', 'true'),
                       extensions_town=c('true', 'false'),
                      sign=TRUE, output=c('json', 'xml'),
                      opts=list(), headers=list()) {
  path = 'reverse_geocoding/v3'
  params = list(
    location=reverse_location(lnglat),
    coordtype=match.arg(coordtype),
    ret_coordtype=match.arg(ret_coordtype),
    radius=radius,
    extensions_poi=extensions_poi,
    extensions_road=match.arg(extensions_road),
    extensions_town=match.arg(extensions_town),
    output=match.arg(output)
  )

  request(vendor='baidu', path=path, params=params, sign=sign, opts=opts, headers=headers)
}



