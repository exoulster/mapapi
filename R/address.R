
data('areacode', package = "mapapi")

#' @export
areacode


#' @import dplyr
full_plist = areacode %>%
  filter(level==1) %>%
  pull(name)

full_plist_short = full_plist %>%
  stringr::str_replace_all(c('市'='', '省'='', '维吾尔自治区'='', '回族自治区'='',
                         '壮族自治区'='', '自治区'=''))

full_clist = areacode %>%
  filter(level==2, !name %in% c('市辖区', '自治区直辖县级行政区划',
                                '省直辖县级行政区划', '县')) %>%
  pull(name)

full_clist_short = full_clist %>%
  stringr::str_replace_all(c('市'=''))


#' @export
get_city_list = function(pname) {
  if (stringr::str_detect(pname, '市')) {
    return(pname)
  }

  code = areacode %>%
    filter(name==pname, level==1) %>%
    pull(code)

  if (length(code)==0) {
    return(NA)
  }

  areacode[areacode$pcode==code,][['name']]
}
