library(shiny)

shinyUI(fluidPage(
  titlePanel("Simulating the Randles circuit impedance"),
  
  sidebarLayout(
    sidebarPanel(
  p("Try the suggested initial values to begin with, then experiment!"),
  sliderInput("f.range", "log (frequency / Hz) range:",
              min = -3, max = 6, value = c(-1,5), step = 0.2),
  numericInput('set.R1', label = "Rs / Ohm", value = 5),
  numericInput('set.C', label = "Cdl / uF   (or Qdl / uS s^n, if n < 1)", value = 20),
  numericInput('nvalue', label = "n exponent (0 < n <= 1)", value = 1, max = 1, min = 0),
  numericInput('set.R2', label = "Rct / Ohm ", value = 75),
  numericInput('set.W', label = "W / Ohm s^-1/2", value = 100),
  actionButton('simulate', "Simulate!")
    ),
  mainPanel(
  plotOutput('plot')
  )
  )
  )  
)
  
