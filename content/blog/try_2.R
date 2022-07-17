library(tidyverse)
library(rvest)
library(purrr)
library(corrplot)
library(ggcorrplot)
library(leaps)
library(bestglm)

# URL 
# "https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-1/v1c1293l12120p2?pr=0,&ba=10&be=1&pa=grage&si=105,"

map_df(1:19, function(i) {
  
  # Progress indicator
  print(i)
  
  #String for consecutive pages
  page = paste0("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-", i, "/v1c1293l12120p", i,"?pr=0,&ba=10&be=1&pa=grage&si=105,")
  
  precio <- read_html(page) %>% html_nodes(".ad-price") %>% html_text() %>%.[-1]
  recamaras <- read_html(page) %>% html_nodes(".re-bedroom") %>% html_text() %>%.[-1]
  banos <- read_html(page) %>% html_nodes(".re-bathroom") %>% html_text() 
  autos <- read_html(page) %>% html_nodes(".car-parking") %>% html_text() 
  superficie <- read_html(page) %>% html_nodes(".chiplets-inline-block:nth-child(8)") %>% html_text() 
  ubi <- read_html(page) %>% html_nodes("#srpContent b") %>% html_text() %>% .[-1] 
  desc <- read_html(page) %>% html_nodes(".tile-title-text") %>% html_text() %>% .[-1] 
  
  #binding resulting dataframes
  data.frame(precio, recamaras, banos, autos, superficie, ubi, desc)

  #Assigning to an object
}) -> casas

#Assigning columns names
colnames(casas) <- c("Precio", "Recamaras", "Banos", "Cochera", "Superficie", "Ubicacion", "Descripcion")

#Wrangling and cleaning the data
casas <- casas %>%
  filter(!is.na(Recamaras) & Ubicacion %in% c("22 De Septiembre", "Bernardo Casals O Nueva Nestle",
                                              "Campo Viejo", "Centro", "Jardines De Coatepec",
                                              "Jardines De Pastoresa", "La Mata",
                                              "Rafael Hernandez Ochoa")) %>%
  mutate(Precio = str_trim(Precio, side = "both"),
         Precio = str_remove(Precio, ","),
         Precio = str_remove(Precio, ","),
         Precio = str_remove(Precio,"\\$"),
         Precio = as.numeric(Precio)/1000000,
         Recamaras = str_remove(Recamaras, "\\+"),
         Recamaras = as.numeric(Recamaras),
         Banos = str_remove(Banos, "\\+"),
         Banos = as.numeric(Banos),
         Cochera = str_remove(Cochera, "\\+"),
         Cochera = as.numeric(Cochera),
         Ubicacion = str_to_title(Ubicacion),
         Ubicacion = recode(Ubicacion, "Bernardo Casals O Nueva Nestle" = "Nueva Nestle"),
         Descripcion = str_to_title(Descripcion),
         Superficie = str_extract(Superficie, "\\d{3,4}"),
         Superficie = as.numeric(Superficie)) %>%
  filter(Superficie < 500)

#Visualizing relation between price and number of bedrooms
casas %>% ggplot(aes(Recamaras, Precio, group = Recamaras)) +
  geom_boxplot() +
  geom_point(aes(color = Ubicacion)) +
  scale_x_continuous(breaks = c(2, 3, 4, 5)) +
  labs(title = "Relación entre precio y número de recámaras",
       subtitle = "De las casas en venta en Coatepec, Veracruz",
       y = "Precio en millones",
       x = "Número de recámaras")

#Visualizing relation between price and number of bathrooms
casas %>% ggplot(aes(Banos, Precio, group = Banos)) +
  geom_boxplot() +
  geom_point(aes(color = Ubicacion)) +
  labs(title = "Relación entre precio y número de baños",
       subtitle = "De las casas en venta en Coatepec, Veracruz",
       y = "Precio en millones",
       x = "Número de baños")

#Visualizing relation between price and number of cars space in garage
casas %>% ggplot(aes(Cochera, Precio, group = Cochera)) +
  geom_boxplot() +
  geom_point(aes(color = Ubicacion)) +
  labs(title = "Relación entre precio y número de autos que caben en la cochera",
       subtitle = "De las casas en venta en Coatepec, Veracruz",
       y = "Precio en millones",
       x= "Autos en cochera")

#Visualizing relation between price and land area
casas %>%
  ggplot(aes(Superficie, Precio)) +
  geom_point() +
  labs(title = "Relación entre precio y superficie",
       subtitle = "De las casas en venta en Coatepec, Veracruz",
       y = "Precio en millones") +
  geom_smooth(method = "lm")

#Visualizing relation between price and location
casas %>% ggplot(aes(Ubicacion, Precio, group = Ubicacion)) +
  geom_boxplot() +
  geom_point(aes(color = Ubicacion)) +
  labs(title = "Relación entre precio y ubicación",
       subtitle = "De las casas en venta en Coatepec, Veracruz",
       y = "Precio en millones")

#Visualizing relation between price, area, location and number of bedrooms
casas %>%
  ggplot(aes(Superficie, Precio)) +
  geom_point(aes(size = Recamaras, alpha = 0.5, color = Ubicacion)) +
  labs(title = "Relación entre precio, superficie, recámaras y ubicación",
       subtitle = "De las casas en venta en Coatepec, Veracruz",
       y = "Precio en millones")

#Variable Dummy para Ubicación del 0 al 5 en orden de precio promedio
casas %>%
  group_by(Ubicacion) %>%
  summarize("Precio promedio" = mean(Precio)) %>%
  arrange("Precio promedio")

casas_dummy <- casas %>%
  mutate(Ubi_Dummy = recode(Ubicacion,
                            "Campo Viejo" = "0",
                            "Centro" = "1",
                            "La Mata" = "2",
                            "Nueva Nestle" = "3",
                            "Rafael Hernandez Ochoa" = "4"),
         Ubi_Dummy = as.numeric(Ubi_Dummy)) %>%
  select(Precio, Recamaras, Banos, Cochera, Superficie, Ubi_Dummy)

#Analyzing correlation among variables
cor1 <- casas_dummy %>% cor()

#Visualizing correlation among variables
ggcorrplot(cor1)

#Selecting best regressors for a lineal model

regsubsets1 <- regsubsets(Precio ~ Recamaras + Banos + Cochera + Superficie + Ubi_Dummy,
                           data = casas_dummy, nbest = 1, nvmax = NULL, force.in = NULL,
                           force.out = NULL, method = "exhaustive")

#Visualizing best regressors subsets
summary.out <- summary(regsubsets1)

as.data.frame(summary.out$outmat)

plot(regsubsets1, scale = "adjr2", main = "R^2 ajustada")

which.max(summary.out$adjr2)

#Linear regression model
summary(lm(Precio ~ Recamaras + Banos + Superficie + Ubi_Dummy, data = casas_dummy))

#Precio = -0.903748 + 0.612049(Recamaras) + 0.261302(Baños) + 0.002974(Superficie) + 0.0704(Ubicación)

modelo <- lm(Precio ~ Recamaras + Banos + Superficie + Ubi_Dummy, data = casas_dummy)

summary(modelo)$coefficients %>% as.data.frame()
str(modelo)







precio

read_html("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-7/v1c1293l12120p7?pr=0,&ba=10&be=1&pa=grage&si=105,") %>%
  html_nodes(".ad-price") %>% html_text() %>%.[-1]

recamaras

read_html("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-7/v1c1293l12120p7?pr=0,&ba=10&be=1&pa=grage&si=105,") %>%
  html_nodes(".re-bedroom") %>% html_text() %>%.[-1]

banos

read_html("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-7/v1c1293l12120p7?pr=0,&ba=10&be=1&pa=grage&si=105,") %>%
  html_nodes(".re-bathroom") %>% html_text()

autos

read_html("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-7/v1c1293l12120p7?pr=0,&ba=10&be=1&pa=grage&si=105,") %>%
  html_nodes(".car-parking") %>% html_text() 

superficie

read_html("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-1/v1c1293l12120p1?pr=0,&ba=10&be=1&pa=grage&si=105,") %>%
  html_nodes(".chiplets-inline-block:nth-child(8)") %>% html_text() %>% .[-1]

ubi

read_html("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-1/v1c1293l12120p1?pr=0,&ba=10&be=1&pa=grage&si=105,") %>%
  html_nodes("#srpContent b") %>% html_text() %>% .[-1] 

desc

read_html("https://www.vivanuncios.com.mx/s-casas-en-venta/coatepec-ver/page-7/v1c1293l12120p7?pr=0,&ba=10&be=1&pa=grage&si=105,") %>%
  html_nodes(".tile-title-text") %>% html_text() %>% .[-1] 
