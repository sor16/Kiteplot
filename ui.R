library(shiny)
library(xlsx)

shinyUI(fluidPage(
        titlePanel("Kiteplot generator"),
        sidebarLayout(
                sidebarPanel(
                        fileInput('file1', 'Choose xlsx file'),
                        textInput("text_field","Choose title for plot"),
                        radioButtons("method","Method of research",c("Proportion"="prop","Individuals"="individ")),
                        downloadButton('downloadPlot', label = "Download plot as PDF")
                ),
                
                mainPanel(
                        plotOutput('algaeplot')
                )
        )
))