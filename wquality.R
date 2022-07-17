

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

?selectInput
gasto <- function(x){
  x %>% pivot_wider(names_from = Stream, values_from = Gasto) %>%
    mutate(Gasto = Upstream - Downstream) %>%
    select(Dam, Fecha, Gasto)
}

gasto_mp <- gasto(aforo_mp_tidy) 
gasto_cp <- gasto(aforo_cp_tidy) 
gasto_soco <- gasto(aforo_soco_tidy) 

ui <- fluidPage(
  titlePanel("Water quantity and quality in a watershed"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dam", "Dam",
                  choices = c("Medio Pixquiac",
                              "Cinco Palos",
                              "Socoyolapan"))
    ),
    mainPanel(
      plotOutput("availability"),
      plotOutput("extraction")
    )
  )
)

server <- function(input, output, session) {
  output$availability <- renderPlot({
    if(input$dam == "Medio Pixquiac"){
      aforo_mp_tidy %>%
        ggplot(aes(Fecha, Gasto, color = Stream)) +
        geom_line() +
        labs(title = "Water volume",
             subtitle = "From 2018 to 2020") +
        scale_y_continuous(name = "Flux (l/s)") +
        scale_x_date(name = "Date")
    } else {
      if(input$dam == "Cinco Palos"){
        aforo_cp_tidy %>%
          ggplot(aes(Fecha, Gasto, color = Stream)) +
          geom_line() +
          labs(title = "Water volume",
               subtitle = "From 2018 to 2020") +
          scale_y_continuous(name = "Flux (l/s)") +
          scale_x_date(name = "Date")
      } else {
        aforo_soco_tidy %>%
          ggplot(aes(Fecha, Gasto, color = Stream)) +
          geom_line() +
          labs(title = "Water volume",
               subtitle = "From 2018 to 2020") +
          scale_y_continuous(name = "Flux (l/s)") +
          scale_x_date(name = "Date")
      
    }
  }
  })
  output$extraction <- renderPlot({
    if(input$dam == "Medio Pixquiac"){
      gasto_mp %>%
        ggplot(aes(Fecha, Gasto)) +
        geom_line() +
        labs(title = "Volume of extracted water from each dam",
             subtitle = "From 2018 to 2020") +
        scale_y_continuous(name = "Flux (l/s)") +
        scale_x_date(name = "Date")
    } else {
      if(input$dam == "Cinco Palos"){
        gasto_cp %>%
          ggplot(aes(Fecha, Gasto)) +
          geom_line() +
          labs(title = "Volume of extracted water from each dam",
               subtitle = "From 2018 to 2020") +
          scale_y_continuous(name = "Flux (l/s)") +
          scale_x_date(name = "Date")
      } else {
        gasto_soco %>%
          ggplot(aes(Fecha, Gasto)) +
          geom_line() +
          labs(title = "Volume of extracted water from each dam",
               subtitle = "From 2018 to 2020") +
          scale_y_continuous(name = "Flux (l/s)") +
          scale_x_date(name = "Date")
      }
    }
  })
}

shinyApp(ui, server)

