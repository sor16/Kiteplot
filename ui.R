library(shiny)
library(xlsx)

shinyUI(fluidPage(
        titlePanel("Kiteplot Generator"),
        sidebarLayout(
                sidebarPanel(
                        fileInput('file1', 'Choose .xlsx File'),
                        textInput("title","Choose Title for Plot"),
                        textInput("interval", "Height Interval of Measurements"),
                        radioButtons("method","Method of Research",c("Proportions"="prop","Individuals"="individ", "Biomass" = "biomass")),
                        conditionalPanel(
                              condition = "input.method == 'biomass'",
                              textInput("unit", "Size of Surface in m^2")
                        ),
                        downloadButton('downloadPlot', label = "Download Plot as PDF")
                ),
                
                mainPanel(
                        plotOutput('algaeplot')
                )
        )
))