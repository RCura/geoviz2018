# Global
library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
    fluidRow(
        column(4, sliderInput(inputId = "input_a", label = "Nb Observations",
                              min = 1, max = 150, value = 150)),
        column(4, sliderInput(inputId = "input_b", label = "Nb barres",
                              min = 2, max = 30, value = 20)),
        column(4, selectInput(inputId = "input_c", label = "Couleur",
                              choices = c("Grey", "Red", "Blue", "Green")))
    ),
    fluidRow(
        column(6, dataTableOutput(outputId = "output_d")),
        column(6, plotOutput(outputId = "output_e"))
    )
)

# Server
server <- function(input, output) {
    table_filtree <- reactive({
        iris %>%
            sample_n(input$input_a) %>%
            arrange(Species, Sepal.Length)
    }) 
    output$output_d <- renderDataTable({
        table_filtree()
    })
    output$output_e <- renderPlot({
        ggplot(table_filtree()) +
            geom_histogram(aes(x = Sepal.Length),
                           bins = input$input_b,
                           fill = input$input_c)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
