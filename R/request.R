#' @export
is.successful = function(x, ...) {
  UseMethod('is.successful', x)
}

is.successful.default = function(res, status) {
  if (!res$success()) {
    return(FALSE)
  }
  content = rjson::fromJSON(res$parse(encoding='utf-8'))
  if (content$status == status) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is.successful.amap = function(res) {
  is.successful.default(res, status='1')
}

is.successful.baidu = function(res) {
  is.successful.default(res, status='0')
}


set_config = function(vendor) {
  if (vendor=='amap') {
    key = Sys.getenv('AMAP_KEY')
    if (key=='') {
      stop('Please set environment variable AMAP_KEY')
    }
    base_url = 'https://restapi.amap.com'
    default_params = list(
      key=key
    )
    verb = 'get'
  } else if (vendor=='baidu') {
    key = Sys.getenv('BAIDU_MAP_KEY')
    if (key=='') {
      stop('Please set environment variable BAIDU_MAP_KEY')
    }
    base_url = 'http://api.map.baidu.com'
    default_params = list(
      ak=key,
      output='json'
    )
    verb = 'get'
  }
  list(
    base_url=base_url,
    default_params=default_params,
    verb=verb
  )
}


request = function(vendor, path, ..., params=NULL, sign=TRUE, opts=list(), headers=list()) {
  # prepare for config
  config = set_config(vendor)

  # prepare for request
  url = crul::url_build(config$base_url, path)
  if (!is.null(params))
    params = modifyList(config$default_params, params)
  else {
    params = modifyList(config$default_params, list(...))
  }

  # sign
  if (sign) {
    if (vendor=='amap') {
      sig = sign.amap(params)
      params = modifyList(params, list(sig=sig))
    } else if (vendor=='baidu') {
      params = modifyList(params, list(timestamp=as.numeric(round(Sys.time()))))
      sn = sign.baidu(path, params)
      params = modifyList(params, list(sn=sn))
    }
  }

  # send request
  opts = modifyList(list('timeout'=3), opts)
  headers = modifyList(list(
    'Content-Type'='application/json'
  ), headers)
  client = crul::HttpClient$new(url=url, opts=opts, headers=headers)
  try(client$verb(verb=config$verb, query=params))
}


async_request = function(vendor, path, params_list, sign=TRUE, opts=list(), headers=list()) {
  # prepare for config
  config = set_config(vendor)

  url = crul::url_build(config$base_url, path)

  req_list = lapply(params_list, function(params) {
    params = modifyList(config$default_params, params)

    # sign
    if (sign) {
      if (vendor=='amap') {
        sig = sign.amap(params)
        params = modifyList(params, list(sig=sig))
      } else if (vendor=='baidu') {
        params = modifyList(params, list(timestamp=as.numeric(round(Sys.time()))))
        sn = sign.baidu(path, params)
        params = modifyList(params, list(sn=sn))
      }
    }

    # build request
    opts = modifyList(list('timeout'=3), opts)
    headers = modifyList(list(
      'Content-Type'='application/json'
    ), headers)
    client = crul::HttpRequest$new(url=url, opts=opts, headers=headers)
    client$verb(verb=config$verb, query=params)
  })

  client.async = crul::AsyncVaried$new(.list=req_list)
  client.async$request()
  client.async
}
