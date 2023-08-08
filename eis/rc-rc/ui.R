library(shiny)

shinyUI(fluidPage(
  titlePanel("Simulating an RC-RC circuit"),
  
  sidebarLayout(
    sidebarPanel(
      
      tags$head(
        tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
        tags$style(type="text/css", "select { max-width: 200px; }"),
        tags$style(type="text/css", "textarea { max-width: 185px; }"),
        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
        tags$style(type='text/css', ".well { max-width: 310px; }"),
        tags$style(type='text/css', ".span4 { max-width: 310px; }")
      ),
      
      p("Try the suggested initial values to begin with, then experiment!"),
      sliderInput("f.range", "log frequency range:",
                  min = -3, max = 7, value = c(-2,7), step = 0.2),
      fluidRow(
        column(width = 6,
      numericInput('set.R1', label = "R1 / Ohm", value = 1000),
      numericInput('set.R2', label = "R2 / Ohm", value = 1500)
      ),
        column(width = 6,
      numericInput('set.C1', label = "C1 / uF ", value = 1),
      numericInput('set.C2', label = "C2 / uF", value = 50)
        )
      ),
      actionButton('simulate', "Simulate!")
  ),
  mainPanel(
  plotOutput('plot'),
  textOutput('text')
  )
  )
  )  
)
  