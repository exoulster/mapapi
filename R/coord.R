
#' Coordinate System Conversion
#' @param lnglat location in the form of longitude + latitude, seperated by ','
#' @param from,to one of "WGS-84", "GCJ-02" or "BD-09". Can be abbrivated.
#' @export
coord_convert = function(lnglat, from, to) {
  arg.from = match.arg(toupper(from), choices=c("WGS-84", "GCJ-02", "BD-09"))
  arg.to = match.arg(toupper(to), choices=c("WGS-84", "GCJ-02", "BD-09"))

  idx.na = is.na(lnglat)
  lnglat[idx.na] = '0,0'  # fill with 0,0
  loc_m = split_location(lnglat)

  if (arg.from == arg.to) {
    lnglat[idx.na] = NA
    lnglat
  } else {
    if (arg.from == "WGS-84" & arg.to == "GCJ-02")
      loc_new = wgs2gcj(loc_m[,1], lat=loc_m[,2])
    if (arg.from == "WGS-84" & arg.to == "BD-09")
      loc_new = wgs2bd(loc_m[,1], lat=loc_m[,2])
    if (arg.from == "GCJ-02" & arg.to == "WGS-84")
      loc_new = gcj2wgs(loc_m[,1], lat=loc_m[,2])
    if (arg.from == "GCJ-02" & arg.to == "BD-09")
      loc_new = gcj2bd(loc_m[,1], lat=loc_m[,2])
    if (arg.from == "BD-09" & arg.to == "WGS-84")
      loc_new = bd2wgs(loc_m[,1], lat=loc_m[,2])
    if (arg.from == "BD-09" & arg.to == "GCJ-02")
      loc_new = bd2gcj(loc_m[,1], lat=loc_m[,2])

    lnglat_new = apply(loc_new, 1, paste, collapse=',')  # concatenate
    lnglat_new[idx.na] = NA  # replace 0 with NA
    lnglat_new
  }
}

x_pi = 3.14159265358979324 * 3000.0 / 180.0
pi = 3.1415926535897932384626
a = 6378245.0
ee = 0.00669342162296594323


out_of_china = function(lng, lat) {
  if (lng < 72.004 | lng > 137.8347) {
    TRUE
  } else if (lat < 0.8293 | lat > 55.8271) {
    TRUE
  }
  FALSE
}

transformlat = function(lng, lat) {
  ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  ret = ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret = ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret = ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
  ret
}

transformlng = function(lng, lat) {
  ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
  ret = ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret = ret + (20.0 * sin(lng * pi) + 40.0 * sin(lng / 3.0 * pi)) * 2.0 / 3.0
  ret = ret + (150.0 * sin(lng / 12.0 * pi) + 300.0 * sin(lng / 30.0 * pi)) * 2.0 / 3.0
  ret
}

#'
gcj2bd = function(lng, lat) {
  z = sqrt(lng^2 + lat^2) + 0.00002 * sin(lat * x_pi)
  theta = atan2(lat, lng) + 0.000003 * cos(lng * x_pi)
  bd_lng = z * cos(theta) + 0.0065
  bd_lat = z * sin(theta) + 0.006
  matrix(c(bd_lng, bd_lat), ncol=2, dimnames=list(NULL, c('bd_lng', 'bd_lat')))
}

bd2gcj = function(lng, lat) {
  x = lng - 0.0065
  y = lat - 0.006
  z = sqrt(x^2 + y^2) - 0.00002 * sin(y * x_pi)
  theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
  gcj_lng = z * cos(theta)
  gcj_lat = z * sin(theta)
  matrix(c(gcj_lng, gcj_lat), ncol=2, dimnames=list(NULL, c('gcj_lng', 'gcj_lat')))
}

gcj2wgs = function(lng, lat) {
  coord = wgs2gcj(lng, lat)
  mglat = coord[,2]
  mglng = coord[,1]
  wgs_lat = lat * 2 - mglat
  wgs_lng = lng * 2 - mglng
  matrix(c(wgs_lng, wgs_lat), ncol=2, dimnames=list(NULL, c('wgs_lng', 'wgs_lat')))
}

wgs2gcj = function(lng, lat) {
  if (out_of_china(lng, lat)) {
    matrix(c(lng, lat), ncol=2, dimnames=list(NULL, c('lng', 'lat')))
  }
  dlat = transformlat(lng - 105, lat - 35);
  dlng = transformlng(lng - 105, lat - 35);
  radlat = lat / 180 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic^2
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180) / (a / sqrtmagic * cos(radlat) * pi)
  gcj_lat = lat + dlat
  gcj_lng = lng + dlng
  matrix(c(gcj_lng, gcj_lat), ncol=2, dimnames=list(NULL, c('gcj_lng', 'gcj_lat')))
}

wgs2bd = function(lng, lat) {
  gcj = wgs2gcj(lng, lat)
  gcj2bd(gcj[,1], gcj[,2])
}

bd2wgs = function(lng, lat) {
  gcj = bd2gcj(lng, lat)
  gcj2wgs(gcj[,1], gcj[,2])
}

