
#' Reverse Location
#' @param location character string of longitude latitude in the form of "lng,lat"
#' @return reversed location in the form of "lat,lng"
#' @export
reverse_location = function(location) {
  idx.na = sapply(location, is.na)
  location[idx.na] = ''  # convert NA to ''
  raw = strsplit(location, ',')
  res = lapply(raw, function(r) {
    if (any(is.na(r))) return(NA)
    else paste(trimws(rev(r)), collapse=',')
  })
  res = unlist(res)
  res[idx.na] = NA
  res
}


#' Split Location
#' @param location character string of longitude latitude in the form of "lng,lat"
#' @return list of longitude, latitude pairs in the form of c(lng, lat)
#' @export
split_location = function(location, reverse=FALSE, simplify=TRUE) {
  if (reverse) location = unlist(reverse_location(location))

  idx.na = sapply(location, is.na)
  location[idx.na] = ''  # convert NA to ''
  raw = strsplit(location, ',')

  res = lapply(raw, function(r) {
    if (length(r) < 2) c(NA, NA)
    else as.numeric(trimws(r))
  })
  if (simplify)
    t(simplify2array(res))
  else
    res
}


#' Get Location from input arguments
#' @export
get_location = function(lnglat=NULL, latlng=NULL, longitude=NULL, latitude=NULL) {
  if (length(lnglat)>0) { # length == 0 ==> NULL or character(0)
    if (is.na(lnglat)) {
      location = NA
    } else if (lnglat=='') {
      location = NA
    } else {
      location = lnglat
    }
  } else if (length(latlng)>0) {
    if (is.na(latlng)) {
      location = NA
    } else if (latlng == '') {
      location = NA
    } else {
      location = reverse_location(latlng)
    }
  } else if (length(longitude)>0 & length(latitude)>0) {
    if (is.na(longitude)) location = NA
    else if (longitude == '') location = NA
    else if (is.na(latitude)) location = NA
    else if (latitude == '') location = NA
    else location = paste(longitude, latitude, sep=',')
  } else {
    stop('Please input both longitude and latitude')
  }
  return(location)
}


