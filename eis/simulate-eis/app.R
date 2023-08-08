#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(pracma)

source("utils.R")
source("circuit_elements.R")

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("EIS Equivalent circuit simulator"),

    p("Create your equivalent circuit in the text box below and press 'Simulate' to refresh the plots.
      Build your equivalent circuit using '+' to add in series, and link elements in parallel with 'para()'.
      Equivalent circuits accepted (with their arguments in brackets): R(R), C(C), L(L), Q(Q, n), W(sigma),
      FLW(Z0, tau), FSW(Z0, tau), G(Z0, k), FLG(Z0, k, tau), trans_line(R, U, n)"),

    tags$div(HTML("Please consult <a>http://lacey.se/science/eis/equivalent-circuit-simulation</a> for detailed instructions.")),
    hr(),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("f.range", "log frequency range:",
                        min = -3, max = 7, value = c(-2,5), step = 0.2),
            hr(),
            checkboxInput("autoscale", "Autoscale Nyquist plot", TRUE),
            fluidRow(
                column(width = 6,
                       numericInput('xmin', label = "min. x", value = 0),
                       numericInput('ymin', label = "min. y", value = 0)
                ),
                column(width = 6,
                       numericInput('xmax', label = "max. x", value = 50),
                       numericInput('ymax', label = "max. y", value = 50)
                )
            ),
            hr(),
            actionButton('simulate', "Simulate"),
            hr(),
            downloadButton("downloadData", "Download as .txt")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textInput("eis.expr", "Equivalent circuit:", value = "R(5) + para(R(10), C(1E-5))", width = "90%"),
           h3("Nyquist plot"),
           plotOutput('nyquistplot', width = "80%"),
           hr(),
           h3("Bode plot"),
           plotOutput('bodeplot', width = "70%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    omega <- eventReactive(input$simulate, {
        # Set range of frequencies
        omega_range(low.logf = input$f.range[1], high.logf = input$f.range[2], p.per.dec = 10)
    })

    #omega.range <- omega.range()

    df <- eventReactive(input$simulate, {

        omega.range <<- omega()

        # Evaluate text expression
        z <- parse(text = input$eis.expr) %>% eval

        # Convert to data frame for plotting
        out <- Z_to_df(z) %>%
            mutate(f = omega.range / (2 * pi)) %>%
            select(f, Re, Im) %>%
            mutate(f = signif(f, 4), Re = signif(Re, 4), Im = signif(Im, 4))

        return(out)

    })

    output$nyquistplot <- renderPlot({

        df <- df()

        if(input$autoscale == FALSE) {
            scalemin.x <- input$xmin
            scalemax.x <- input$xmax
            scalemin.y <- input$ymin
            scalemax.y <- input$ymax
        } else {
            scalemin.x <- min(min(df$Re), min(df$Im * -1), 0)
            scalemin.y <- min(min(df$Re), min(df$Im * -1), 0)
            scalemax.x <- max(max(df$Re), max(df$Im * -1))
            scalemax.y <- max(max(df$Re), max(df$Im * -1))
        }

        ggplot(df, aes(x = Re, y = -Im)) +
            geom_point(shape = 21, size = 3) +
            geom_path(size = 1) +
            coord_fixed(xlim = c(scalemin.x, scalemax.x), ylim = c(scalemin.y, scalemax.y)) +
            xlab("Z' / "~Omega~"") +
            ylab("-Z'' / "~Omega~"") +
            theme_Lacey2(base_size = 20)

    })

    output$bodeplot <- renderPlot({

        df <- df()

        df %>% mutate(Im = abs(Im)) %>%
            gather(key = "key", value = "value", -f) %>%
            ggplot(aes(x = f, y = value, color = key)) +
            geom_path() +
            scale_x_log10(labels=trans_format('log10',math_format(10^.x))) +
            scale_y_log10(labels=trans_format('log10',math_format(10^.x))) +
            scale_color_brewer("", palette = "Set1", labels = c("-Z''", "Z'")) +
            labs(x = "f / Hz", y = "Z', -Z'' /"~Omega~"") +
            theme_Lacey2(base_size = 20)

    })

    output$downloadData <- downloadHandler(

        filename = "exported_sim.txt",
        content = function(file) {
            write.table(df(), file, row.names = FALSE, quote = FALSE, sep = "\t")
        }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
