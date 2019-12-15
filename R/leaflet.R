#' @export
draw_amap = function(data) {
  mdata = data %>%
    separate(lnglat, c('lng', 'lat'), sep=',', convert=TRUE)
  m = leaflet::leaflet(mdata) %>%
    leafletCN::amap() %>%
    leaflet::addMarkers(label = ~ label,
                        labelOptions=leaflet::labelOptions(
                          permanent=FALSE
                        ))
  m
}

#' @export
launch_poi_search = function(port=getOption("shiny.port")) {
  library(shiny)
  library(dplyr)
  library(tidyr)

  shiny::runApp(appDir = system.file("poisearch", package = 'mapapi'), port=port)
}
