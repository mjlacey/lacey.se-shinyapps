library(shiny)
library(ggplot2)
library(scales)
library(grid)

theme_Lacey <- function(base_size=18) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
  + theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour=NA),
          plot.background = element_rect(fill = "transparent", colour=NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(size = rel(1)),
          axis.title.y = element_text(angle=90),
          axis.text = element_text(size = rel(0.75)),
          axis.line.x = element_line(size=0.5, colour="#000000"),
          axis.line.y = element_line(size=0.5, colour="#000000"),
          axis.ticks.length = unit(0.15, "cm"),
          axis.text.x = element_text(margin = margin(0.3, 0, 0.2, 0, "cm")),
          axis.text.y = element_text(margin = margin(0, 0.3, 0, 0.2, "cm")),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.key.size = unit(0.6, "cm"),
          legend.margin = unit(0, "cm"),
          legend.title = element_text(face = "bold"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face = "bold", size = rel(0.75))
  ))

}

shinyServer(function(input, output) {

#  mytheme <- theme_bw() +
#    theme(text = element_text(size=20)) +
#    theme(axis.title.x = element_text(vjust=-0.3)) +
#    theme(axis.title.y = element_text(vjust=0.3)) +
#    theme(panel.border = element_rect(size=1,color = "black")) +
#    theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm")) +
#    theme(panel.grid.major = element_line(size=0.5))

df <- eventReactive(input$simulate, {

  low.logf <- input$f.range[1]
  high.logf <- input$f.range[2]
  p.per.dec <- 10

  logf <- seq(low.logf, high.logf, length.out = (p.per.dec * (high.logf - low.logf)) + 1)
  omega <- 2 * pi * (10^logf)

  i = complex(real = 0, imaginary = 1)

  R <- 1/((input$sigma * 1E-6) / (input$thickness * 1E-4))
  cap <- (input$epsilon * 8.854E-12) / (input$thickness * 1E-4)
  C <- complex(real = 0, imaginary = (1/(omega * cap)))
  Cdl <- (1/((input$set.C/1E6) * omega^input$nvalue)) * exp((0.5 * pi * input$nvalue * i))

  df <- data.frame(logf, omega, R, C, Cdl)

  return(df)

})

output$plot <- renderPlot({

  df <- df()

  df$CRC <- 1/((1/df$C) + (1/(df$R + df$Cdl)))

  ggplot(df) +
    geom_point(aes(x=Re(CRC), y=Im(CRC)), shape = 21, size = 3) +
    geom_path(aes(x=Re(CRC), y=Im(CRC)), size = 1) +
    coord_fixed() +
    xlab("Z' / "~Omega~"cm"^2~"") +
    ylab("-Z'' / "~Omega~"cm"^2~"") +
    scale_y_continuous(limits=c(0,(1.4 * max(df$R)))) +
    scale_x_continuous(limits=c(0,(1.4 * max(df$R)))) +
    theme_Lacey()

})

outtext <- eventReactive(input$simulate, {

  R <- 1/((input$sigma * 1E-6) / (input$thickness * 1E-4))
  cap <- (input$epsilon * 8.854E-12) / (input$thickness * 1E-4)
  fstar <- 1/(2 * pi * R * cap)

  paste("Based on these values, ionic resistance is ", signif(R, digits = 3), "Ohm cm^2 (3 s.f.), and the relaxation frequency of
    the dielectric semicircle is ", signif(fstar, digits = 3), "Hz (3 s.f.)")

})

output$text <- renderText({

  outtext()

})

})
