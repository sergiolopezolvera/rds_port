

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(readr)
library(stringr)
library(shiny)
# Data import
#read csv as tibble
getwd()
aforo_mp <- read_csv("./content/blog/2022-07-02-water-availability/Aforo_MedioPixquiac.csv")

aforo_soco <- read_csv("./content/blog/2022-07-02-water-availability/Aforo_Socoyolapan.csv")

aforo_cp <- read_csv("./content/blog/2022-07-02-water-availability/Aforo_CincoPalos.csv")

aforo_mc <- read_csv("./content/blog/2022-07-02-water-availability/Aforo_Manantiales_Cofre.csv")


# Data cleaning and wrangling

tidy <- function(x){
  x %>%
    mutate(Dam = recode(Presa,
                        "Pixquiac" = "Dam 1",
                        "Socoyolapan" = "Dam 2",
                        "Cinco Palos" = "Dam 3",
                        "Cofre de Perote" = "Spring"),
           Stream = recode(Aguas,
                           "Arriba" = "Upstream",
                           "Abajo" = "Downstream"),
           Año = as.character(Año),
           Mes = as.character(Mes),
           Dia = "01") %>%
    select(Dam, Stream, Gasto, Año, Mes, Dia) %>%
    unite(Fecha, Año:Dia, sep = "/") %>%
    mutate(Fecha = as.Date(Fecha))
}

aforo_mp_tidy <- tidy(aforo_mp)
aforo_soco_tidy <- tidy(aforo_soco)
aforo_cp_tidy <- tidy(aforo_cp)

ui <- fluidPage(
  titlePanel("Water quantity and quality in a watershed"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dam", "Dam",
                  choices = c("Dam 1" = "aforo_mp_tidy",
                              "Dam 2" = "aforo_soco_tidy",
                              "Dam 3" = "aforo_cp_tidy")),
      tableOutput("av_hover"),
      tableOutput("ext_hover"),
      width = 2
    ),
    mainPanel(
      plotOutput("availability", hover = "availability_hover"),
      plotOutput("extraction", hover = "extraction_hover"),
      width = 10
    )
  )
)

server <- function(input, output, session) {
  output$availability <- renderPlot({
    get(input$dam) %>%
      ggplot(aes(Fecha, Gasto, color = Stream)) +
      geom_line() +
      labs(title = "Water volume",
           subtitle = "From 2018 to 2020") +
      scale_y_continuous(name = "Flux (l/s)") +
      scale_x_date(name = "Date")
  }
  )
  
  output$extraction <- renderPlot({
    get(input$dam) %>%
      pivot_wider(names_from = Stream, values_from = Gasto) %>%
      mutate(Gasto = Upstream - Downstream) %>%
      ggplot(aes(Fecha, Gasto)) +
      geom_line() +
      geom_point() +
      labs(title = "Volume of extracted water from each dam",
           subtitle = "From 2018 to 2020") +
      scale_y_continuous(name = "Flux (l/s)") +
      scale_x_date(name = "Date")
  })
  
  output$av_hover <- renderTable({
    req(input$availability_hover)
    nearPoints(get(input$dam), input$availability_hover)
  })
  
  output$ext_hover <- renderTable({
    req(input$extraction_hover)
    nearPoints((output$extraction), input$extraction_hover)
  })
}

shinyApp(ui, server)

