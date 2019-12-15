#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {

    updateSelectizeInput(session, inputId='province', selected='上海市',
                         choices = mapapi:::full_plist, server = TRUE)
    observeEvent(input$province, {
        city_list = get_city_list(pname=input$province)
        if (length(city_list) > 1) {
            city_list = c(NA, city_list)
        }
        updateSelectizeInput(session, inputId='city', selected='上海市',
                             choices = city_list, server = TRUE)
    })


    # submit query
    observeEvent(input$submit, {
        res.amap = poi.amap(keywords=input$address, city=input$city, types='酒店')
        res.baidu = poi.baidu(query=input$address, region=input$city, tag='酒店',
                              ret_coordtype = 'gcj02ll')
        data = bind_rows(c(label='amap_res', parse_poi(res.amap)),
                         c(label='baidu_res', parse_poi(res.baidu))
        )
        if (input$lnglat != '') {
            req.amap = list(
                label = 'amap_req',
                lnglat = input$lnglat
            )
            req.baidu = list(
                label = 'baidu_req',
                lnglat = coord_convert(input$lnglat, from='bd', to='gcj')
            )
            data = bind_rows(data, req.amap, req.baidu)
        }

        output$data = renderDataTable(data)
        output$map = leaflet::renderLeaflet({
            draw_amap(data)
        })
    })
})
