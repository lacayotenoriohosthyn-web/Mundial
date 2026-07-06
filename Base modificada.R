install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("purrr")
install.packages("furrr")
install.packages("future")
install.packages("goalmodel")
install.packages("caret")
install.packages("yardstick")

library(purrr)
library(tidyverse)
library(janitor)
library(lubridate)
#library(goalmodel)
library(readxl)
library(tidyverse)
library(worldfootballR)
library(dplyr)



datos <- read_excel("Roblecitos_partidos_basedepurada.xlsx")
datos <- datos %>% #creación de nuevas variables
  mutate(
    goal_diff = home_score - away_score,
    home_win = ifelse(home_score > away_score, 1, 0),
    draw = ifelse(home_score == away_score, 1, 0)
  )

#Convertir la fecha a formato Date
datos <- datos %>%
  mutate(
    date = as.Date(date)
  )

#victorias visitantes.
datos <- datos %>%
  mutate(
    resultado = case_when(
      home_score > away_score ~ "Home",
      home_score < away_score ~ "Away",
      TRUE ~ "Draw"
    )
  )

#Verificar el efecto de localía
datos %>%
  group_by(neutral) %>%
  summarise(
    pct_local_gana = mean(home_win)
  )
#generamos nueva base
datos_historicos <- datos %>%
  filter(!is.na(home_score))
nrow(datos)
nrow(datos_historicos)


#eliminar datos que nos generan error
datos_historicos <- datos %>%
  filter(!is.na(home_score))

#recalculamos las variables sin esos datos que nos dan problemas
datos_historicos <- datos_historicos %>%
  mutate(
    goal_diff = home_score - away_score,
    home_win = ifelse(home_score > away_score, 1, 0),
    draw = ifelse(home_score == away_score, 1, 0),
    resultado = case_when(
      home_score > away_score ~ "Home",
      home_score < away_score ~ "Away",
      TRUE ~ "Draw"
    )
  )
datos_historicos %>%
  group_by(neutral) %>%
  summarise(
    pct_local_gana = mean(home_win)
  )

#Distribucion de datos
datos_historicos %>%
  count(resultado) %>%
  mutate(prop = n/sum(n))

#segmentacion de paises de interes
equipos_mundial <- c(
  "United States","Mexico","Canada",
  "Argentina","Brazil","Uruguay","Colombia","Ecuador","Paraguay",
  "Panama","Haiti","Curaçao",
  "Germany","Austria","Belgium","Bosnia and Herzegovina","Croatia","Czech Republic",
  "England","France","Netherlands","Norway","Portugal","Scotland","Spain",
  "Sweden","Switzerland","Turkey",
  "Algeria","Cape Verde","Ivory Coast","Egypt","Ghana","Morocco","Senegal",
  "South Africa","Tunisia","DR Congo",
  "Australia","Iran","Japan","Jordan","South Korea","Qatar",
  "Saudi Arabia","Uzbekistan","Iraq",
  "New Zealand"
)
# información para locales
ataque_local <- datos_historicos %>%
  group_by(home_team) %>%
  summarise(
    goles_favor = mean(home_score)
  )

defensa_local <- datos_historicos %>%
  group_by(home_team) %>%
  summarise(
    goles_contra = mean(away_score)
  )

#informacion para visitantes
ataque_visitante <- datos_historicos %>%
  group_by(away_team) %>%
  summarise(
    goles_favor = mean(away_score)
  )

defensa_visitante <- datos_historicos %>%
  group_by(away_team) %>%
  summarise(
    goles_contra = mean(home_score)
  )

#prueba de modelo
datos_largos <- bind_rows(
  datos_historicos %>%
    transmute(
      equipo = home_team,
      goles_favor = home_score,
      goles_contra = away_score
    ),
  datos_historicos %>%
    transmute(
      equipo = away_team,
      goles_favor = away_score,
      goles_contra = home_score
    )
)

fortaleza <- datos_largos %>%
  filter(equipo %in% equipos_mundial) %>%
  group_by(equipo) %>%
  summarise(
    goles_favor = mean(goles_favor),
    goles_contra = mean(goles_contra),
    partidos = n()
  ) %>%
  arrange(desc(goles_favor))

#fuerza relativa de cada equipo
fortaleza <- fortaleza %>%
  mutate(
    rating = goles_favor - goles_contra
  ) %>%
  arrange(desc(rating))
#Donde: rating=Ataque−Defensa 
#Los equipos con valores altos marcan mucho y reciben poco.

mean(fortaleza$goles_favor)

#fuerza relativa de ataque
fortaleza <- fortaleza %>%
  mutate(
    ataque_rel = goles_favor / mean(goles_favor),
    defensa_rel = goles_contra / mean(goles_contra)
  )

#verifiquemos fortalezas una vez mas
fortaleza %>%
  select(equipo, rating, partidos) %>%
  arrange(desc(rating))

fortaleza %>%
  arrange(partidos)

#Hay que darle un peso diferente a cada selecicon segun su nivel
partidos_rival <- datos_historicos %>%
  transmute(
    equipo = home_team,
    rival = away_team,
    goles_favor = home_score,
    goles_contra = away_score
  ) %>%
  bind_rows(
    datos_historicos %>%
      transmute(
        equipo = away_team,
        rival = home_team,
        goles_favor = away_score,
        goles_contra = home_score
      )
  )
fortaleza <- fortaleza %>%
  mutate(
    confianza = case_when(
      partidos >= 300 ~ "Alta",
      partidos >= 100 ~ "Media",
      TRUE ~ "Baja"
    )
  )
#veamos que obtenemos
fortaleza %>%
  filter(equipo %in% c("Argentina","Japan"))

#creamos una nueva base porque chat lo demanda
partidos_rival <- datos_historicos %>%
  transmute(
    equipo = home_team,
    rival = away_team,
    goles_favor = home_score,
    goles_contra = away_score
  ) %>%
  bind_rows(
    datos_historicos %>%
      transmute(
        equipo = away_team,
        rival = home_team,
        goles_favor = away_score,
        goles_contra = home_score
      )
  )



fortaleza %>%
  select(equipo, rating)
partidos_rival <- partidos_rival %>%
  left_join(
    fortaleza %>%
      select(rival = equipo,
             rating_rival = rating),
    by = "rival"
  )
partidos_rival <- partidos_rival %>%
  mutate(
    goles_ponderados =
      goles_favor * (1 + rating_rival)
  )
#normalicemos la ponderizacion para evitar problemas
min_rating <- min(fortaleza$rating)

partidos_rival <- partidos_rival %>%
  mutate(
    peso_rival = rating_rival - min_rating + 0.1
  )

#comprobacion de que no haya problemas
fortaleza_ponderada <- partidos_rival %>%
  filter(equipo %in% equipos_mundial) %>%
  group_by(equipo) %>%
  summarise(
    goles_ponderados = mean(goles_ponderados),
    partidos = n()
  ) %>%
  arrange(desc(goles_ponderados))
#comparemos si las variable rating es util vs la ponderada
fortaleza %>%
  select(equipo, rating) %>%
  arrange(desc(rating))
fortaleza_ponderada

#seguimos revisando que la metodologia sea adecuada
summary(partidos_rival$rating_rival)
range(partidos_rival$rating_rival)

#empecemos con el modelo de poisson

partidos_rival <- datos_historicos %>%
  transmute(
    equipo = home_team,
    rival = away_team,
    goles_favor = home_score,
    goles_contra = away_score,
    neutral = neutral
  ) %>%
  bind_rows(
    datos_historicos %>%
      transmute(
        equipo = away_team,
        rival = home_team,
        goles_favor = away_score,
        goles_contra = home_score,
        neutral = neutral
      )
  )

#modelo 1
modelo_poisson1 <- glm(
  goles_favor ~ equipo + rival + neutral,
  family = poisson(link = "log"),
  data = partidos_rival
)

#agregamos esto para corregir un error para el modelo2
partidos_rival <- partidos_rival %>%
  left_join(
    fortaleza %>%
      select(rival = equipo,
             rating_rival = rating),
    by = "rival"
  )
glimpse(partidos_rival)

#modelo 2
modelo_poisson2 <- glm(
  goles_favor ~ rating_rival + neutral,
  family = poisson(link = "log"),
  data = partidos_rival
)

#comparemos ambosmodelos
AIC(modelo_poisson1, modelo_poisson2)
#no se trabajó con los mismo datos


#revisemos el modelo1
summary(modelo_poisson1)
deviance(modelo_poisson1)
df.residual(modelo_poisson1)
deviance(modelo_poisson1) /
  df.residual(modelo_poisson1)





#Prueba simulación cr vs engl V1
nuevo_partido <- data.frame(
  equipo = "England",
  rival = "Costa Rica",
  neutral = 1
)
predict(
  modelo_poisson1,
  newdata = nuevo_partido,
  type = "response"
)
nuevo_partido2 <- data.frame(
  equipo = "Costa Rica",
  rival = "England",
  neutral = 1
)

predict(
  modelo_poisson1,
  newdata = nuevo_partido2,
  type = "response"
)
data.frame(
  goles = 0:8,
  probabilidad = dpois(0:8, lambda)
)

#Prueba simulación cr vs engl V2
lambda_ENG <- predict(
  modelo_poisson1,
  newdata = data.frame(
    equipo = "England",
    rival = "Costa Rica",
    neutral = 1
  ),
  type = "response"
)

lambda_CR <- predict(
  modelo_poisson1,
  newdata = data.frame(
    equipo = "Costa Rica",
    rival = "England",
    neutral = 1
  ),
  type = "response"
)

#Crear la matriz de probabilidades
goles <- 0:8

matriz_prob <- outer(
  dpois(goles, lambda_ENG),
  dpois(goles, lambda_CR),
  "*"
)

rownames(matriz_prob) <- paste0("ENG_", goles)
colnames(matriz_prob) <- paste0("CR_", goles)

round(matriz_prob, 4)

#Probabilidades de victoria, empate y derrota

prob_empate <- sum(diag(matriz_prob))

prob_ENG_gana <- sum(
  matriz_prob[row(matriz_prob) > col(matriz_prob)]
)

prob_CR_gana <- sum(
  matriz_prob[row(matriz_prob) < col(matriz_prob)]
)

c(
  Inglaterra = prob_ENG_gana,
  Empate = prob_empate,
  Costa_Rica = prob_CR_gana
)

#Marcador más probable
indice <- which(
  matriz_prob == max(matriz_prob),
  arr.ind = TRUE
)

goles_ENG <- goles[indice[1]]
goles_CR  <- goles[indice[2]]

cat(
  "Marcador más probable:",
  goles_ENG, "-",
  goles_CR, "\n"
)
#mapa de calor
library(ggplot2)

df_heat <- expand.grid(
  ENG = goles,
  CR = goles
)

df_heat$probabilidad <- as.vector(matriz_prob)

ggplot(
  df_heat,
  aes(
    x = CR,
    y = ENG,
    fill = probabilidad
  )
) +
  geom_tile() +
  labs(
    x = "Goles Costa Rica",
    y = "Goles Inglaterra",
    fill = "Prob."
  ) +
  theme_minimal()




#Revisamos una vez mas un hipotesis nulo
# 1
anova(modelo_nulo, modelo_poisson1, test="Chisq")

# 2
pseudo_R2 <- 1 - modelo_poisson1$deviance /
  modelo_poisson1$null.deviance

# 3
summary(modelo_poisson1)$deviance /
  summary(modelo_poisson1)$df.residual

modelo_poisson1$null.deviance
modelo_poisson1$deviance
