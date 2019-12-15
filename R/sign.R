sign.amap = function(params) {
  secret = Sys.getenv('AMAP_SECRET')
  if (secret == '') stop('Please set environment variable AMAP_SECRET')

  sorted_params = params[sort(names(params))]
  query = paste0(crul:::make_query(sorted_params), secret)
  openssl::md5(query)
}


sign.baidu = function(path, query, secret=NULL) {
  if (is.null(secret)) {
    secret = Sys.getenv('BAIDU_MAP_SECRET')
  }
  if (secret=='') {
    stop('Please set environment variable BAIDU_MAP_SECRET')
  }
  raw = paste0(crul::url_build('', path, query), secret)
  openssl::md5(URLencode(raw, reserved=TRUE, repeated=TRUE))
}
