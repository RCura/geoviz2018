## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(stringr)
library(shiny)

## ---- eval = FALSE-------------------------------------------------------
## #############
## # Partie UI #
## #############
## sliderInput(inputId = "monSlider1", label = "Slider 1",
##             min = 0, max = 50, value = 25, step = 5),
## sliderInput(inputId = "monSlider2", label = "Slider 2",
##             min = -100, max = 100, value = 0, step = 10),
## selectInput(inputId = "maSelection", label = "Choisir un élément",
##             choices = c("Pomme", "Poire", "Papaye"), multiple = FALSE)
## #################
## # Partie server #
## #################
## input$monSlider1
## # > 25
## input$monSlider2
## # > 0
## input$maSelection
## # > "Pomme"

## ---- eval = FALSE-------------------------------------------------------
## #############
## # Partie UI #
## #############
## 
## plotOutput(outputId = "graphique1"),
## plotOutput(outputId = "graphique2"),
## textOutput(outputId = "texte1")
## 
## # [...]
## 
## #################
## # Partie server #
## #################
## output$texte1 <- renderText({print(c(1:50))})
## output$graphique1 <- renderPlot({plot(1:50)})
## output$graphique2 <- renderPlot({plot(50:1)})

## ---- echo = FALSE, fig.hold = TRUE, fig.width = 2.4, fig.height= 3------
plot(c(1:50))
plot(c(50:1))

## ---- echo = FALSE-------------------------------------------------------
print(c(1:50))

## ---- eval = FALSE-------------------------------------------------------
## fluidPage(
##   titlePanel("Ceci est un titlePanel", ),
##   fluidRow(column(width = 6, "Première colonne de largeur 6"),
##            column(width = 2, "Deuxième colonne de largeur 2"),
##            column(width = 4, "Troisième colonne de largeur 4")
##            ),
##   fluidRow(
##     column(width = 6, "Colonne de largeur 6"),
##     column(width = 6, "Colonne de largeur 6")
##   )
## )

## ---- eval = FALSE-------------------------------------------------------
## fluidPage(
##   titlePanel("Ceci est un titlePanel", ),
##   fluidRow(column(width = 6, "Première colonne de largeur 6"),
##            column(width = 2, "Deuxième colonne de largeur 2"),
##            column(width = 4, "Troisième colonne de largeur 4")
##            ),
##   fluidRow(
##     column(width = 6, "Colonne de largeur 6"),
##     column(width = 6, "Colonne de largeur 6")
##   )
## )

## ---- eval = FALSE-------------------------------------------------------
## # Global
## library(shiny)
## library(tidyverse)
## 
## # UI
## ui <- fluidPage(
##     fluidRow(
##         column(4, sliderInput(inputId = "input_a", label = "Nb Observations",
##                               min = 1, max = 150, value = 150)),
##         column(4, sliderInput(inputId = "input_b", label = "Nb barres",
##                               min = 2, max = 30, value = 20)),
##         column(4, selectInput(inputId = "input_c", label = "Couleur",
##                               choices = c("Grey", "Red", "Blue", "Green")))
##     ),
##     fluidRow(
##         column(6, dataTableOutput(outputId = "output_d")),
##         column(6, plotOutput(outputId = "output_e"))
##     )
## )
## 
## # Server
## server <- function(input, output) {
## output$output_d <- renderDataTable({
##     table_filtree <- iris %>%
##         sample_n(input$input_a) %>%
##         arrange(Species, Sepal.Length)
##     })
## output$output_e <- renderPlot({
##     table_filtree <- iris %>%
##         sample_n(input$input_a) %>%
##         arrange(Species, Sepal.Length)
##     ggplot(table_filtree) +
##         geom_histogram(aes(x = Sepal.Length), bins = input$input_b,
##                        fill = input$input_c)
## })
## }

## ---- eval = FALSE-------------------------------------------------------
## # Server
## server <- function(input, output) {
## output$output_d <- renderDataTable({
##     table_filtree <- iris %>%
##         sample_n(input$input_a) %>% #<<
##         arrange(Species, Sepal.Length)
##     })
## output$output_e <- renderPlot({
##     table_filtree <- iris %>%
##         sample_n(input$input_a) %>% #<<
##         arrange(Species, Sepal.Length)
##     ggplot(table_filtree) +
##         geom_histogram(aes(x = Sepal.Length),
##                        bins = input$input_b,
##                        fill = input$input_c)
## })
## }

## ---- eval = FALSE-------------------------------------------------------
## # Server
## server <- function(input, output) {
## output$output_d <- renderDataTable({
##     table_filtree <- iris %>%
##         sample_n(input$input_a) %>%
##         arrange(Species, Sepal.Length)
##     })
## output$output_e <- renderPlot({
##     table_filtree <- iris %>%
##         sample_n(input$input_a) %>%
##         arrange(Species, Sepal.Length)
##     ggplot(table_filtree) +
##         geom_histogram(aes(x = Sepal.Length),
##                        bins = input$input_b,
##                        fill = input$input_c)
## })
## }

## ---- eval = FALSE-------------------------------------------------------
## # Server
## server <- function(input, output) {
## 
## table_filtree <- reactive({ #<<
##   iris %>% #<<
##     sample_n(input$input_a) %>% #<<
##     arrange(Species, Sepal.Length) #<<
## })  #<<
## output$output_d <- renderDataTable({
##     table_filtree() #<<
##     })
## output$output_e <- renderPlot({
##     ggplot(table_filtree()) + #<<
##         geom_histogram(aes(x = Sepal.Length),
##                        bins = input$input_b,
##                        fill = input$input_c)
## })
## }

## ---- eval = FALSE-------------------------------------------------------
## # Declaration
## abc <- reactive(mean(iris$Sepal.Length))
## # Utilisation
## abc()
## #> 5.843333

## ---- eval=FALSE---------------------------------------------------------
## # Server
## server <- function(input, output, session) {
## # [...]
## 
## observe({
##   if (input$input_a > 30){
##   updateSliderInput(inputId = "input_b",
##                     session = session,
##                     min = 2,
##                     max = 30)
##   } else {
##   updateSliderInput(inputId = "input_b",
##                     session = session,
##                     min = 2,
##                     max = input$input_a,
##                     value = input$input_a/2)
##   }
## })
## 
## }

## ---- eval = FALSE-------------------------------------------------------
## # UI
## fluidRow(
##   column(width = 4,textInput("word1",
##                              "First word",
##                              "Hello")),
##   column(width = 4,textInput("word2",
##                              "Second word",
##                              "Master")),
##   column(width = 4,textOutput("combi"))
## )
## # Server
## output$combi <- renderText({
##   paste(input$word1,isolate(input$word2))
##   })

