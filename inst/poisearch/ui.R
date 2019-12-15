#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
    # Application title
    titlePanel("POI Search"),

    sidebarLayout(
        sidebarPanel(
            width=2,
            textInput(inputId='address', label='Address'),
            selectizeInput(inputId='province', label='Province', choices=NULL),
            selectizeInput(inputId='city', label='City', choices=NULL),
            textInput(inputId='lnglat', label='Location', value=NA,
                      placeholder='Longitude,Latitude'),
            actionButton(inputId='submit', label='Submit')
        ),
        mainPanel(
            dataTableOutput(outputId='data'),
            leaflet::leafletOutput(outputId='map')
        )
    )
))
