#' Distance between two locations
#' @export
distance = function(lnglat1, lnglat2, coordsys) {
  if (coordsys=='GCJ-02') {
    lnglat1 = gcj2wgs(lnglat1)
    lnglat2 = gcj2wgs(lnglat2)
  } else if (coordsys=='BD-09') {
    lnglat1 = bd2wgs(lnglat1)
    lnglat2 = bd2wgs(lnglat2)
  }
  l1 = split_location(lnglat1)
  l2 = split_location(lnglat2)
  mapply(geosphere::distHaversine, l1, l2, SIMPLIFY = TRUE)
}
