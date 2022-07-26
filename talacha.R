#Required packages
library(tidyverse)
#Reading data
instagram <- read_csv("content/blog/2022-07-25-qu-influencers-dominan-las-redes-sociales/instagram_june_2022.csv")
tiktok <- read_csv("content/blog/2022-07-25-qu-influencers-dominan-las-redes-sociales/tiktok_june_2022.csv")
youtube <- read_csv("content/blog/2022-07-25-qu-influencers-dominan-las-redes-sociales/youtube_june_2022.csv")
#Wrangling data
instagram <- instagram %>%
  mutate(Cuenta = `instagram name`,
         Nombre = `influencer name`,
         Subscribers_M = str_remove(`Subscribers count`, "M"),
         Suscriptores_Millones = as.integer(Subscribers_M),
         Likes_proxy = str_replace(`Likes avg`, "K", " 1000"),
         Likes_proxy = str_replace(Likes_proxy, "M", " 1000000"),
         Likes_proxy = str_split(Likes_proxy, " ", simplify = TRUE),
         Likes = as.integer(Likes_proxy[,1]),
         Likes_q = as.integer(Likes_proxy[,2]),
         Likes_promedio_Miles = (Likes*Likes_q)/1000,
         Comments_proxy = str_replace(`Comments avg.`, "K", " 1000"),
         Comments_proxy = str_replace(Comments_proxy, "M", " 1000000"),
         Comments_proxy = str_split(Comments_proxy, " ", simplify = TRUE),
         Comments = as.integer(Comments_proxy[,1]),
         Comments_q = as.integer(Comments_proxy[,2]),
         Comentarios_promedio_Miles = (Comments*Comments_q)/1000,
         País_fan1 = `Views avg.`,
         Categoría = Category_1) %>%
  select(Cuenta, Nombre, Suscriptores_Millones, Likes_promedio_Miles, Comentarios_promedio_Miles, País_fan1, Categoría)

tiktok <- tiktok %>%
  mutate(Cuenta = Tiktoker,
         Nombre = `influencer name`,
         Subscribers_M = str_remove(`Subscribers count`, "M"),
         Suscriptores_Millones = as.integer(Subscribers_M),
         Likes_proxy = str_replace(`Likes avg`, "K", " 1000"),
         Likes_proxy = str_replace(Likes_proxy, "M", " 1000000"),
         Likes_proxy = str_split(Likes_proxy, " ", simplify = TRUE),
         Likes = as.integer(Likes_proxy[,1]),
         Likes_q = as.integer(Likes_proxy[,2]),
         Likes_promedio_Miles = (Likes*Likes_q)/1000,
         Comments_proxy = str_replace(`Comments avg.`, "K", " 1000"),
         Comments_proxy = str_replace(Comments_proxy, "M", " 1000000"),
         Comments_proxy = str_split(Comments_proxy, " ", simplify = TRUE),
         Comments = as.integer(Comments_proxy[,1]),
         Comments_q = as.integer(Comments_proxy[,2]),
         Comentarios_promedio_Miles = (Comments*Comments_q)/1000,
         País_fan1 = `Views avg.`) %>%
  select(Cuenta, Nombre, Suscriptores_Millones, Likes_promedio_Miles, Comentarios_promedio_Miles, País_fan1)

#instagram analysis
#Top 20 Subscribers
instagram %>%
  arrange(desc(Suscriptores_Millones)) %>%
  .[2:11,] %>%
  ggplot(aes(Suscriptores_Millones, Cuenta, fill = Suscriptores_Millones)) +
  geom_col() +
  labs(x = "Millones de seguidores",
       y = "Cuenta en Instagram",
       title = "Influencers con más seguidores en Instagram",
       subtitle = "En junio del 2022",
         ) +
  scale_y_discrete(limits = c("justinbieber", "khloekardashian", "beyonce", "arianagrande", "kimkardashian",
                              "therock", "selenagomez", "leomessi", "kyliejenner", "cristiano")) +
  theme(legend.position = "Null")

#Más comentarios y likes en el top 10
instagram %>%
  arrange(desc(Suscriptores_Millones)) %>%
  .[2:21,] %>%
  ggplot(aes(Likes_promedio_Miles, Comentarios_promedio_Miles, color = Suscriptores_Millones)) +
  geom_label(aes(label = Cuenta)) +
  labs(x = "Miles de likes por post",
       y = "Miles de comentarios por post",
       title = "Influencers con más likes y comentarios en Instagram",
       subtitle = "En junio del 2022") +
  theme(legend.position = "Null")

#Categorías con más influencers en el top 20
instagram %>%
  arrange(desc(Suscriptores_Millones)) %>%
  .[2:21,] %>%
  select(Cuenta, Suscriptores_Millones, Categoría) %>%
  group_by(Categoría) %>%
  summarise(Mill_sus = sum(Suscriptores_Millones)) %>%
  arrange(desc(Mill_sus)) %>%
  ggplot(aes(Mill_sus, Categoría, fill = Mill_sus)) +
  geom_col() +
  labs(title = "Categorías más populares en el top 20 de influencers en instagram",
       subtitle = "En junio del 2022",
       caption = "Número de seguidores de las 20 cuentas más grandes agrupados por categoría",
       x = "Millones de suscriptores") +
  scale_y_discrete(limits = c("Lifestyle", "Fitness & Gym", "Nature & landscapes",
                              "Modeling", "Clothing & Outfits", "Cinema & Actors/actresses",
                              "Fashion", "Sports with a ball", "Music")) +
  theme(legend.position = "Null")
  
#Tiktok
#Top 20 Subscribers
tiktok %>%
  arrange(desc(Suscriptores_Millones)) %>%
  .[1:10,] %>%
  ggplot(aes(Suscriptores_Millones, Cuenta, fill = Suscriptores_Millones)) +
  geom_col() +
  labs(x = "Millones de seguidores",
       y = "Cuenta en TikTok",
       title = "Influencers con más seguidores en TikTok",
       subtitle = "En junio del 2022") +
  theme(legend.position = "Null") +
  scale_y_discrete(limits = c("ox_zung", "therock", "domelipa", "cznburak", "kimberly.loaiza",
                              "zachking", "addisonre", "bellapoarch", "charlidamelio", "khaby.lame"))

#Más comentarios y likes en el top 10
tiktok %>%
  arrange(desc(Suscriptores_Millones)) %>%
  .[1:20,] %>%
  ggplot(aes(Likes_promedio_Miles, Comentarios_promedio_Miles, color = Suscriptores_Millones)) +
  geom_label(aes(label = Cuenta)) +
  labs(x = "Miles de likes por post",
       y = "Miles de comentarios por post",
       title = "Influencers con más likes y comentarios en Instagram",
       subtitle = "En junio del 2022") +
  theme(legend.position = "Null")

#Influencers top en ambas redes

rank_instagram <- instagram %>%
  arrange(desc(Suscriptores_Millones)) %>%
  .[2:21,] %>%
  mutate(Rank_Instagram = c(1:20)) %>%
  select(Rank_Instagram, Cuenta)

rank_tiktok <- tiktok %>%
  arrange(desc(Suscriptores_Millones)) %>%
  .[2:21,] %>%
  mutate(Rank_TikTok = c(1:20)) %>%
  select(Rank_TikTok, Cuenta)

rank_vs <-inner_join(rank_instagram, rank_tiktok) %>%
  select(Cuenta, Rank_Instagram, Rank_TikTok)
