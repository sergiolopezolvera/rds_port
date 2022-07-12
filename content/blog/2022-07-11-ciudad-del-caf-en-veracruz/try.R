library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(readr)
library(gridExtra)
library(raster)
library(rgdal)
library(ggmap)
library(broom)
library(RColorBrewer)
library(rgeos)
library(sp)
library(sf)

DENUE_uno <- DENUE %>%
  select(ID, "Nombre" = Nombre.de.la.Unidad.Económica,
         "Actividad" = Nombre.de.clase.de.la.actividad,
         "Personal_ocupado" =  Descripcion.estrato.personal.ocupado,
         Municipio, Latitud, Longitud,
         "Fecha_DENUE" = Fecha.de.incorporación.al.DENUE) %>%
  filter(Actividad %in% c("Elaboración de café tostado y molido",
                          "Elaboración de café instantáneo",
                          "Beneficio del café"))
          
DENUE_dos <- DENUE %>%
  select(ID, "Nombre" = Nombre.de.la.Unidad.Económica,
         "Actividad" = Nombre.de.clase.de.la.actividad,
         "Personal_ocupado" =  Descripcion.estrato.personal.ocupado,
         Municipio, Latitud, Longitud,
         "Fecha_DENUE" = Fecha.de.incorporación.al.DENUE) %>%
  filter(Actividad %in% c("Fabricación de maquinaria y equipo para la industria alimentaria y de las bebidas",
                          "Elaboración de otras bebidas destiladas",
                          "Comercio al por mayor de otros alimentos",
                          "Comercio al por menor de otros alimentos",
                          "Cafeterías, fuentes de sodas, neverías, refresquerías y similares") &
           str_detect(Nombre, "CAFE")) %>% #Recodificando nombres para simplificarlos
  mutate(Actividad = recode(Actividad, "Fabricación de maquinaria y equipo para la industria alimentaria y de las bebidas" = "Fabricación de maquinaria y equipo",
                            "Cafeterías, fuentes de sodas, neverías, refresquerías y similares" = "Cafeterías y similares",
                            "Elaboración de otras bebidas destiladas" = "Cafeterías y similares",
                            "Comercio al por mayor de otros alimentos" = "Comercializadora",
                            "Comercio al por menor de otros alimentos" = "Comercializadora"))

DENUE_tres <- rbind(DENUE_uno, DENUE_dos)

DENUE_tres %>%
  group_by(Actividad) %>%
  summarise(Numero = n()) %>%
  ggplot(aes(Numero, Actividad, fill = Actividad)) + 
  geom_col() +
  theme(legend.position = "none") +
  labs(title = "Actividades relacionadas al café",
       subtitle = "Unidades económicas en el estado de Veracruz",
       x = "Número") +
  scale_y_discrete(limits = c("Fabricación de maquinaria y equipo",
                              "Elaboración de café instantáneo",
                              "Beneficio del café",
                              "Elaboración de café tostado y molido",
                              "Comercializadora",
                              "Cafeterías y similares"))
head(DENUE_tres)

first <- SpatialPoints(c(DENUE_tres[6],DENUE_tres[7]))

second <- SpatialPointsDataFrame(c(DENUE_tres[6],DENUE_tres[7]), DENUE_tres)
str(first)

DENUE_coords <- DENUE %>%
  select(Latitud, Longitud) %>%
  mutate(Latitud = as.numeric(Latitud), Longitud = as.numeric(Longitud)) 

third <- st_point(x = c(DENUE_coords[2], DENUE_tres[7]), dim = "XY")

                  
plot(second)
sp_third



sp_third %>%
  ggplot(aes()) +
  geom_sf()
