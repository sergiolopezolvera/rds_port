
library(tidyverse)

movies <- read.csv("MoviesOnStreamingPlatforms.csv") %>%
  mutate(Rotten.Tomatoes = str_remove(Rotten.Tomatoes, "/100"),
         Calificación = as.numeric(Rotten.Tomatoes),
         Prime = Prime.Video,
         Disney = Disney.,
         Título = Title,
         Año = Year,
         Edad = Age,
         Edad = recode(Edad, "all" = "Apta para toda la familia")) %>%
  select(Título, Año, Edad, Calificación, Netflix, Hulu, Prime, Disney)

head(movies)

#Películas con puntaje igual o mayor a 90/100 y dónde encontrarlas

over_90 <- movies %>%
  mutate(Netflix = as.character(Netflix),
         Netflix = str_replace(Netflix, "1", "X"),
         Netflix = str_replace(Netflix, "0", ""),
         Hulu = as.character(Hulu),
         Hulu = str_replace(Hulu, "1", "X"),
         Hulu = str_replace(Hulu, "0", ""),
         Prime = as.character(Prime),
         Prime = str_replace(Prime, "1", "X"),
         Prime = str_replace(Prime, "0", ""),
         Disney = as.character(Disney),
         Disney = str_replace(Disney, "1", "X"),
         Disney = str_replace(Disney, "0", ""),) %>%
  arrange(desc(Calificación)) %>%
  filter(Calificación >= 90)


age_80 <- movies %>%
  filter(Calificación >= 80 & Edad != "") %>%
  group_by(Edad) %>% summarise(Netflix = sum(Netflix),
                                       Hulu = sum(Hulu),
                                       Prime = sum(Prime),
                                       Disney = sum(Disney))

movies %>%
  filter(Calificación >= 80 & Age != "") %>%
  group_by(Edad) %>%
  summarise(Netflix = sum(Netflix),
            Hulu = sum(Hulu),
            Prime = sum(Prime),
            Disney = sum(Disney)) %>%
  pivot_longer(Netflix:Disney, names_to = "Plataforma", values_to = "Peliculas")

# 10 mejores películas en cada plataforma

Netflix_10 <- movies %>%
  filter(Netflix == 1) %>%
  select(Título, Calificación) %>%
  arrange(desc(Calificación)) %>%
  .[1:10,] %>%
  mutate(Netflix = Título) %>%
  select(Netflix)

Hulu_10 <- movies %>%
  filter(Hulu == 1) %>%
  select(Título, Calificación) %>%
  arrange(desc(Calificación)) %>%
  .[1:10,] %>%
  mutate(Hulu = Título) %>%
  select(Hulu)

Prime_10 <- movies %>%
  filter(Prime == 1) %>%
  select(Título, Calificación) %>%
  arrange(desc(Calificación)) %>%
  .[1:10,] %>%
  mutate(Prime = Título) %>%
  select(Prime)

Disney_10 <- movies %>%
  filter(Disney == 1) %>%
  select(Título, Calificación) %>%
  arrange(desc(Calificación)) %>%
  .[1:10,] %>%
  mutate(Disney = Título) %>%
  select(Disney)

rank_10 <- data.frame(Lugar = 1:10)

cbind(rank_10, Netflix_10, Hulu_10, Prime_10, Disney_10)

imdb <- read.csv("imdb_top_1000.csv") %>%
  mutate(Título = Series_Title, Género = Genre, Calificación_IMDB = IMDB_Rating, Reseña = Overview, Poster = Poster_Link) %>%
  select(Título, Calificación_IMDB, Género, Reseña, Poster)

head(imdb)

join <- movies %>%
  inner_join(imdb) %>%
  mutate(Rotten_Tomatoes = Calificación, IMDB = Calificación_IMDB) %>%
  select(Título, Año, Edad, Rotten_Tomatoes, IMDB, Género, Reseña, Poster, Netflix, Hulu, Prime, Disney)

head(join)
str(join)

join %>%
  group_by(Género) %>%
  summarise(n()) %>%
  print(n = 95)

"Action", "Adventure", "Comedy", "Drama", "Crime", "Biography", "Thriller", "Sci-Fi"

géneros <- join$Género %>% str_split(",", simplify = TRUE) %>% str_trim(side = "both") %>% unique() %>% .[-12]
