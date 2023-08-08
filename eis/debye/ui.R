library(shiny)

shinyUI(fluidPage(
  titlePanel("Simulating the Debye circuit impedance"),
  
  sidebarLayout(
    sidebarPanel(
      
      p("Try the suggested initial values to begin with, then experiment!"),
      sliderInput("f.range", "log frequency range:",
                  min = -1, max = 6, value = c(0,5), step = 0.2),
      fluidRow(
        column(width = 6,
      numericInput('thickness', label = "layer thickness / um", value = 100),
      numericInput('sigma', label = "conductivity / uS/cm ", value = 10),
      numericInput('epsilon', label = "dielectric constant, epsilon", value = 5)
        ),
        column(width = 6,
               numericInput('set.C', label = "Cdl / uF   (or Qdl / uS s^n)", value = 20),
               numericInput('nvalue', label = "n exponent (0 < n < 1)", value = 0.92, max = 1, min = 0)
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
  