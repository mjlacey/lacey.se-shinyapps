#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(scales)

# Define functions up here.

theme_Lacey2 <- function(base_size=16, legend.position = "top",
                         panel.background.fill = "#fafafa") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(size = rel(1.3), hjust = 0, face = "bold"),
            plot.subtitle = element_text(face = "italic"),
            text = element_text(),
            panel.background = element_rect(fill = panel.background.fill),
            plot.background = element_rect(fill = "transparent", colour=NA),
            panel.border = element_rect(colour = "#333333", size = 0.3),
            axis.title = element_text(size = rel(1), colour="#333333"),
            axis.title.y = element_text(angle=90, colour="#333333"),
            axis.text = element_text(size = rel(0.8)),
            axis.ticks.length=unit(0.15, "cm"),
            axis.text.x = element_text(margin = margin(0.2, 0, 0.2, 0, "cm"), colour="#333333"),
            axis.text.y = element_text(margin = margin(0, 0.2, 0, 0.2, "cm"), colour="#333333"),
            panel.grid.major = element_line(colour="#eaeaea", size = 0.5),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.key.size = unit(0.6, "cm"),
            legend.background = element_blank(),
            strip.background=element_rect(colour="#eaeaea",fill="#eaeaea"),
            strip.text = element_text(colour = "#333333", lineheight=0.7),
            legend.title = element_text(size = rel(0.8)),
            legend.position = legend.position
    ))
}

lossfrac <- function(Q.p, Q.n, l.p, l.n) {

  C.cell <- min(Q.p, Q.n)
  Q.max <- min(Q.p * (1 + l.p), Q.n * (1 + l.n))

  Q.n.100 <- Q.max / (Q.n * (1 + l.n)) * Q.n
  Q.p.100 <- Q.max / (Q.p * (1 + l.p)) * Q.p

  Q.n.new <- min(Q.n, Q.n.100)
  Q.p.new <- min(Q.p, Q.p.100)
  C.cell.new <- min(Q.n.new, Q.p.new)

  lossfrac <- 1 - (C.cell.new / C.cell)

  return(lossfrac)
}

cyl_calculate <- function(p.specific.cap = 200, p.areal.cap = 4, p.massfrac = 0.92, p.density = 3.4, p.nominalV = 3.8, p.firstloss = 10, p.arealR = 10,
                          s.thickness = 25, s.porosity = 0.44, s.bulkdensity = 0.855,
                          e.ratio = 2, e.density = 1.22,
                          n.specific.cap = 350, n.areal.cap = 4.4, n.massfrac = 0.96, n.density = 1.7, n.nominalV = 0.1, n.firstloss = 10, n.arealR = 10,
                          p.cc.thickness = 14, n.cc.thickness = 14, p.cc.density = 2.7, n.cc.density = 8.95,
                          cell.diameter = 1.8, cell.height = 6.5, cell.can.thickness = 0.02,
                          cell.can.density = 7.9, cell.void.diameter = 0.01, cell.headspace = 0.5,
                          cell.extra.mass = 3) {

    ## DEFINITION OF QUANTITIES INTO LISTS
    p <- list(
        specific.cap = p.specific.cap, # specific capacity in mAh/g
        areal.cap = p.areal.cap, # areal capacity in mAh/cm2
        massfrac = p.massfrac, # active material fraction (between 0 and 1)
        density = p.density, # density of the *electrode* (i.e., composite)
        nominalV = p.nominalV,
        firstloss = p.firstloss,
        arealR = p.arealR # areal resistance in Ohm cm^2
    )

    n <- list(
        specific.cap = n.specific.cap,
        areal.cap = n.areal.cap,
        massfrac = n.massfrac,
        density = n.density,
        nominalV = n.nominalV,
        firstloss = n.firstloss,
        arealR = n.arealR
    )

    s <- list(
        thickness = s.thickness, # in microns
        porosity = s.porosity,
        bulkdensity = s.bulkdensity # polypropylene is 0.855
    )

    e <- list(
        ratio = e.ratio, # mL/Ah
        density = e.density # 1.22 for BASF LP40
    )

    p$cc.thickness <- p.cc.thickness # in microns
    p$cc.density <- p.cc.density # for Al
    n$cc.thickness <- n.cc.thickness # in microns
    n$cc.density <- n.cc.density # in microns

    cell <- list(
        diameter = cell.diameter, #in cm
        height = cell.height, # in cm
        can.thickness = cell.can.thickness, # in cm
        can.density = cell.can.density, # 7.9 for typical steel
        void.diameter = cell.void.diameter, # empty gap in the centre of the can, in cm
        headspace = cell.headspace, # headspace between top of jelly roll and top of can
        extra.mass = cell.extra.mass
    )

    # BEGIN CALCULATIONS

    ## Composite electrode masses
    p$comp.mass <- (p$areal.cap / p$specific.cap) / p$massfrac
    n$comp.mass <- (n$areal.cap / n$specific.cap) / n$massfrac

    p$comp.thickness <- p$comp.mass / p$density
    n$comp.thickness <- n$comp.mass / n$density

    ## Separator mass
    s$mass <- s$thickness * 1E-4 * (1 - s$porosity) * s$bulkdensity

    ## Current collector mass
    p$cc.mass <- p$cc.thickness * 1E-4 * p$cc.density
    n$cc.mass <- n$cc.thickness * 1E-4 * n$cc.density

    ## Electrolyte mass
    e$mass <- min(p$areal.cap, n$areal.cap) * (e$ratio / 1000) * e$density # old

    ## Stack areal mass and volume
    stack.mass.areal <- (2 * p$comp.mass) + (2 * n$comp.mass) + (2 * s$mass) + p$cc.mass + n$cc.mass + (2 * e$mass)
    stack.thickness <- (2 * p$comp.thickness) + (2 * n$comp.thickness) + (2 * s$thickness * 1E-4) + (p$cc.thickness * 1E-4) + (n$cc.thickness * 1E-4)

    ## Jellyroll
    jellyroll <- cyl_dim(diameter = cell$diameter, height = cell$height, void.diameter = cell$void.diameter, can.thickness = cell$can.thickness, stack.thickness = stack.thickness, headspace = cell$headspace)
    cell$can.mass <- cyl_can_weight(diameter = cell$diameter, height = cell$height, can.thickness = cell$can.thickness, density = cell$can.density, extra.mass = cell$extra.mass)

    ## first cycle loss
    cell$firstcycleloss <- lossfrac(Q.p = p$areal.cap, Q.n = n$areal.cap, l.p = p$firstloss / 100, l.n = n$firstloss / 100)

    ## Cell properties
    cell$capacity <- 0.001 * 2 * min(p$areal.cap, n$areal.cap) * jellyroll$area * (1 - cell$firstcycleloss) # in Ah
    cell$energy <- cell$capacity * (p$nominalV - n$nominalV)

    cell$mass <- (stack.mass.areal * jellyroll$area) + cell$can.mass
    cell$energy.grav <- 1000 * cell$energy / cell$mass # in Wh/kg

    cell$energy.vol <- 1000 * cell$energy / (pi * (cell$diameter / 2) * cell$height)

    ## Mass summary
    mass.summary <- data.frame(
        component = c("positive electrode", "negative electrode", "separator", "electrolyte", "positive c.c.", "negative c.c.", "can"),
        value = c(
            2 * p$comp.mass * jellyroll$area, 2 * n$comp.mass * jellyroll$area, 2 * s$mass * jellyroll$area, 2 * e$mass * jellyroll$area, p$cc.mass * jellyroll$area, n$cc.mass * jellyroll$area, cell$can.mass
        )
    )

    out <- list(p = p,
                n = n,
                s = s,
                e = e,
                cell = cell,
                jellyroll = jellyroll,
                mass.summary = mass.summary
    )

    return(out)

}

cyl_can_weight <- function(diameter, height, can.thickness, density, extra.mass) {

    # volume of the can itself is:
    vol <- (((pi * (diameter/2)^2) - (pi * (diameter/2 - can.thickness)^2)) * height) +
        (2 * pi * (diameter/2)^2 * can.thickness)

    # weight is given by:
    wt <- (vol * density) + extra.mass

    return(wt)
}

cyl_dim <- function(diameter, height, void.diameter, can.thickness, stack.thickness, headspace) {

    # This calculates electrode area using the Archimedes spiral formula

    # calculate the number of turns in the jelly roll
    turns <- ((diameter - (2 * can.thickness) - stack.thickness - void.diameter) / 2) / stack.thickness

    # create a data frame of theta steps and the difference between them, dtheta
    df <- data.frame(
        theta = seq(0, turns * 2 * pi, length.out = 2000)
    )

    # numerical integration using Archimedes spiral
    df2 <- mutate(df,
                  dtheta = c(0, diff(theta)),
                  r = (void.diameter / 2) + (stack.thickness * theta / (2 * pi)),
                  increment = sqrt(
                      ((void.diameter / 2) + (stack.thickness * theta / (2 * pi)))^2 + (stack.thickness / (2 * pi))^2
                  ),
                  length = cumsum(increment * dtheta)
    )

    # return number of turns, jelly roll length, and area
    return(list(
        df = df2,
        turns = turns,
        length = last(df2$length),
        area = last(df2$length) * (height - headspace)
    ))

}

# Define UI
ui <- fluidPage(

    h1("Estimating energy density of batteries"),
    h2("About"),
    p("This calculator estimates cell-level energy density for alkali-ion or alkali-metal batteries
    (e.g. Li-ion, Li metal, Na-ion, Mg-ion) based on specifications of the constituent parts.
      This calculator uses either the 18650 or 21700 form factors as the basis for the calculations.
      The default values for the positive and negative electrodes are based on the suggested baseline
      data for NMC532 vs graphite presented by Harlow et al, J. Electrochem. Soc. 166 (13) A3031-A3044 (2019).
      Adjust the values in the boxes below and press 'Go!' to calculate the results."),
    actionButton('calculate', "Go!"),
    hr(),
    fluidRow(
        column(2,
               h4("Cell type"),
               hr(),
               radioButtons("celltype", "", choices = c("18650", "21700"))
               ),
        column(2,
               h4("+ve"),
               hr(),
               numericInput("p.specific.cap", "mAh/g", value = 175),
               numericInput("p.areal.cap", "mAh/cm2", value = 3.73),
               numericInput("p.massfrac", "mass fraction", value = 0.94),
               numericInput("p.density", "density, g/cc", value = 3.5),
               numericInput("p.nominalV", "average E, V", value = 3.75),
               numericInput("p.arealR", "R, Ohm cm2", value = 7),
               numericInput("p.firstloss", "first cyc. loss %", value = 10)
               ),
        column(2,
               h4("separator"),
               hr(),
               numericInput("s.thickness", "thickness / um", value = 16),
               numericInput("s.porosity", "porosity", value = 0.44),
               numericInput("s.bulkdensity", "bulk density, g/cc", value = 0.855),
               hr(),
               h4("electrolyte"),
               hr(),
               numericInput("e.ratio", "el'yte vol, mL/Ah", value = 1.8),
               numericInput("e.density", "density, g/cc", value = 1.22)
               ),
        column(2,
               h4("-ve"),
               hr(),
               numericInput("n.specific.cap", "mAh/g", value = 350),
               numericInput("n.areal.cap", "mAh/cm2", value = 4.13),
               numericInput("n.massfrac", "mass fraction", value = 0.954),
               numericInput("n.density", "density, g/cc", value = 1.55),
               numericInput("n.nominalV", "average E, V", value = 0.1),
               numericInput("n.arealR", "R, Ohm cm2", value = 5),
               numericInput("n.firstloss", "first cyc. loss %", value = 8)
               ),
        column(2,
               h4("current coll."),
               hr(),
               numericInput("p.cc.thickness", "+ve thickness / µm", value = 15),
               hr(),
               radioButtons("n.cc.type", "-ve c.c.", choices = c("Cu", "Al")),
               numericInput("n.cc.thickness", "-ve thickness / µm", value = 8)
               )

    ),

    hr(),

    h2("Results"),
    h3(textOutput("cellenergy")),
    textOutput("cellcapacity"),
    textOutput("firstcycleloss"),
    textOutput("cellresistance"),
    textOutput("jellyroll"),
    textOutput("electrodes"),
    hr(),
    h3("Mass breakdown"),
    plotOutput("massplot", height = "100%", width = "100%"),
    h3("Cross-section schematic"),
    plotOutput("crosssec", height = "100%", width = "100%")

)

# Define server logic
server <- function(input, output) {

    calculated <- eventReactive(input$calculate, {

        if(input$celltype == "18650") {
           cell <- list(
                diameter = 1.8, #in cm
                height = 6.5, # in cm
                can.thickness = 0.017, # in cm
                can.density = 7.9, # 7.9 for typical steel
                void.diameter = 0.15, # empty gap in the centre of the can, in cm
                headspace = 0.5, # headspace between top of jelly roll and top of can
                extra.mass = 2 # grams
            )
        } else {
            cell <- list(
                diameter = 2.1, #in cm
                height = 7.0, # in cm
                can.thickness = 0.017, # in cm
                can.density = 7.9, # 7.9 for typical steel
                void.diameter = 0.2, # empty gap in the centre of the can, in cm
                headspace = 0.5, # headspace between top of jelly roll and top of can
                extra.mass = 3 # grams
            )
        }

        if(input$n.cc.type == "Al") {
            n.cc.density <- 2.7
            } else {
                n.cc.density <- 8.95
            }

        out <- cyl_calculate(p.specific.cap = input$p.specific.cap, p.areal.cap = input$p.areal.cap, p.massfrac = input$p.massfrac, p.density = input$p.density, p.nominalV = input$p.nominalV, p.firstloss = input$p.firstloss, p.arealR = input$p.arealR,
                             s.thickness = input$s.thickness, s.porosity = input$s.porosity, s.bulkdensity = input$s.bulkdensity,
                             e.ratio = input$e.ratio, e.density = input$e.density,
                             n.specific.cap = input$n.specific.cap, n.areal.cap = input$n.areal.cap, n.massfrac = input$n.massfrac, n.density = input$n.density, n.nominalV = input$n.nominalV, n.firstloss = input$n.firstloss, n.arealR = input$n.arealR,
                             p.cc.thickness = input$p.cc.thickness, n.cc.thickness = input$n.cc.thickness, p.cc.density = 2.7, n.cc.density = n.cc.density,
                             cell.diameter = cell$diameter, cell.height = cell$height, cell.can.thickness = cell$can.thickness,
                             cell.can.density = cell$can.density, cell.void.diameter = cell$void.diameter, cell.headspace = cell$headspace,
                             cell.extra.mass = cell$extra.mass)

        return(out)
    })

    output$cellcapacity <- renderText({
        paste("Cell capacity:", signif(calculated()$cell$capacity, 3), "Ah | ", signif(calculated()$cell$mass, 3), "g")
    })

    output$cellenergy <- renderText({
        paste("Cell energy:", signif(calculated()$cell$energy, 3), "Wh | ", signif(calculated()$cell$energy.grav, 3), "Wh/kg | ", signif(calculated()$cell$energy.vol, 3), "Wh/L")
    })

    output$firstcycleloss <- renderText({
      paste("First cycle capacity loss:", signif(calculated()$cell$firstcycleloss * 100, 3), "%")
    })

    output$cellresistance <- renderText({
        paste("Estimated cell resistance:", signif(1000 * (calculated()$p$arealR + calculated()$n$arealR) / calculated()$jellyroll$area, 3), "mΩ")
    })

    output$jellyroll <- renderText({
        paste("Jellyroll:", round(calculated()$jellyroll$turns, 1), "turns; length: ", signif(calculated()$jellyroll$length, 3), "cm; total electrode area: ", signif(2 * calculated()$jellyroll$area, 3), "cm^2")
    })

    output$electrodes <- renderText({
        paste("+ve electrode:", signif(calculated()$p$comp.thickness * 10000, 3), "µm thick | -ve electrode:", signif(calculated()$n$comp.thickness * 10000, 3), "µm | n/p ratio:", signif(calculated()$n$areal.cap / calculated()$p$areal.cap, 3))
    })

    output$massplot <- renderPlot({
        calculated()$mass.summary %>%
            mutate(component = factor(component,
                                      levels = c("positive c.c.", "positive electrode",
                                                 "separator", "electrolyte", "negative electrode", "negative c.c.", "can"))) %>%
            ggplot(aes(x = "", y = 100 * value / sum(value))) +
            geom_bar(stat = "identity", aes(fill = component)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "", y = "mass fraction / %") +
            coord_flip() +
            theme_Lacey2() +
            scale_y_continuous(breaks = seq(0, 100, 5)) +
            scale_fill_manual(values = c("#b72900", "#ff3900", "#00813c", "#00d864", "#0789ce", "#0347d4", "grey")) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(),
                  legend.position = "top")

    }, height = 150, width = 600)

    output$crosssec <- renderPlot({
        calculated()$jellyroll$df %>%
            ggplot(aes(x = 10 * r * cos(theta), y = -10 * r * sin(theta))) +
            geom_path(aes(x = 10 * calculated()$cell$diameter / 2 * cos(theta), y = -10 * calculated()$cell$diameter / 2 * sin(theta))) +
            geom_path(aes(x = 10 * (calculated()$cell$diameter - 2 * calculated()$cell$can.thickness) / 2 * cos(theta), y = -10 * (calculated()$cell$diameter - 2 * calculated()$cell$can.thickness) / 2 * sin(theta))) +
            geom_path(color = "red") +
            coord_fixed() +
            theme_Lacey2() +
            scale_x_continuous(breaks = seq(-12, 12, 2)) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
            labs(x = "mm")
    }, height = 400, width = 400)

}

# Run the application
shinyApp(ui = ui, server = server)
