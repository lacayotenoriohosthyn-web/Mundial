library(purrr)
library(tidyverse)
library(janitor)
library(lubridate)
#library(goalmodel)
library(readxl)
library(tidyverse)
library(worldfootballR)
library(dplyr)
library(writexl)
rm(list = ls())

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
#eliminar datos antes del 2022
datos <- datos %>%
  filter(date >= as.Date("2022-01-01"))
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

# Etiquetas de equipos
siglas <- c(
  "Argentina"="ARG",
  "Portugal"="POR",
  "Spain"="ESP",
  "Norway"="NOR",
  "Morocco"="MAR",
  "Netherlands"="NED",
  "England"="ENG",
  "Haiti"="HAI",
  "France"="FRA",
  "Senegal"="SEN",
  "Brazil"="BRA",
  "Colombia"="COL",
  "Uzbekistan"="UZB",
  "New Zealand"="NZL",
  "Uruguay"="URU",
  "Austria"="AUT",
  "United States"="USA",
  "Japan"="JPN",
  "Turkey"="TUR",
  "Mexico"="MEX",
  "South Africa"="RSA",
  "Canada"="CAN",
  "Switzerland"="SUI",
  "Ecuador"="ECU",
  "Sweden"="SWE",
  "Germany"="GER",
  "Curaçao"="CUW",
  "Egypt"="EGY",
  "Panama"="PAN",
  "Scotland"="SCO",
  "Algeria"="ALG",
  "Australia"="AUS",
  "Croatia"="CRO",
  "Saudi Arabia"="KSA",
  "Qatar"="QAT",
  "Tunisia"="TUN",
  "Iran"="IRN",
  "Paraguay"="PAR",
  "Belgium"="BEL",
  "Ivory Coast"="CIV",
  "Iraq"="IRQ",
  "South Korea"="KOR",
  "DR Congo"="COD",
  "Cape Verde"="CPV",
  "Czech Republic"="CZE",
  "Jordan"="JOR",
  "Ghana"="GHA",
  "Bosnia and Herzegovina"="BIH"
)

# ── Paso 1: extraer niveles del modelo ──────────────────────────────────────
niveles_equipo <- modelo_poisson1$xlevels[["local"]]
niveles_rival  <- modelo_poisson1$xlevels[["rival"]]

# ── Paso 2: función corregida ────────────────────────────────────────────────
simular_partido <- function(equipo1, equipo2, modelo, siglas) {
  
  sigla1 <- siglas[equipo1]
  sigla2 <- siglas[equipo2]
  
  niv_eq <- modelo$xlevels[["equipo"]]  # ← corregido
  niv_rv <- modelo$xlevels[["rival"]]
  
  hacer_pred <- function(eq, riv) {
    predict(modelo,
            newdata = data.frame(
              equipo  = factor(eq,  levels = niv_eq),
              rival   = factor(riv, levels = niv_rv),
              neutral = 1
            ),
            type = "response")
  }
  
  lambda_1 <- hacer_pred(equipo1, equipo2)
  lambda_2 <- hacer_pred(equipo2, equipo1)
  
  goles <- 0:8
  
  matriz_prob <- outer(
    dpois(goles, lambda_1),
    dpois(goles, lambda_2),
    "*"
  )
  
  rownames(matriz_prob) <- paste0(sigla1, "_", goles)
  colnames(matriz_prob) <- paste0(sigla2, "_", goles)
  
  prob_empate <- sum(diag(matriz_prob))
  prob_1      <- sum(matriz_prob[row(matriz_prob) > col(matriz_prob)])
  prob_2      <- sum(matriz_prob[row(matriz_prob) < col(matriz_prob)])
  
  indice  <- arrayInd(which.max(as.vector(matriz_prob)), dim(matriz_prob))
  goles_1 <- goles[indice[1, 1]]
  goles_2 <- goles[indice[1, 2]]
  
  data.frame(
    equipo_local      = equipo1,
    equipo_visitante  = equipo2,
    sigla_local       = sigla1,
    sigla_visitante   = sigla2,
    lambda_local      = round(lambda_1, 3),
    lambda_visitante  = round(lambda_2, 3),
    marcador_probable = paste(goles_1, "-", goles_2),
    prob_marcador     = round(max(matriz_prob), 4),
    prob_local        = round(prob_1, 4),
    prob_empate       = round(prob_empate, 4),
    prob_visitante    = round(prob_2, 4)
  )
}
# ── Paso 3: lista de partidos (redefinir porque se perdió con rm) ────────────
partidos <- list(
  c("Mexico","South Korea"), c("Mexico","Czech Republic"), c("Mexico","South Africa"),
  c("South Korea","Czech Republic"), c("South Korea","South Africa"), c("Czech Republic","South Africa"),
  c("Canada","Switzerland"), c("Canada","Bosnia and Herzegovina"), c("Canada","Qatar"),
  c("Switzerland","Bosnia and Herzegovina"), c("Switzerland","Qatar"), c("Bosnia and Herzegovina","Qatar"),
  c("Brazil","Morocco"), c("Brazil","Scotland"), c("Brazil","Haiti"),
  c("Morocco","Scotland"), c("Morocco","Haiti"), c("Scotland","Haiti"),
  c("United States","Paraguay"), c("United States","Australia"), c("United States","Turkey"),
  c("Paraguay","Australia"), c("Paraguay","Turkey"), c("Australia","Turkey"),
  c("Germany","Ecuador"), c("Germany","Ivory Coast"), c("Germany","Curaçao"),
  c("Ecuador","Ivory Coast"), c("Ecuador","Curaçao"), c("Ivory Coast","Curaçao"),
  c("Netherlands","Japan"), c("Netherlands","Sweden"), c("Netherlands","Tunisia"),
  c("Japan","Sweden"), c("Japan","Tunisia"), c("Sweden","Tunisia"),
  c("Belgium","Iran"), c("Belgium","Egypt"), c("Belgium","New Zealand"),
  c("Iran","Egypt"), c("Iran","New Zealand"), c("Egypt","New Zealand"),
  c("Spain","Uruguay"), c("Spain","Saudi Arabia"), c("Spain","Cape Verde"),
  c("Uruguay","Saudi Arabia"), c("Uruguay","Cape Verde"), c("Saudi Arabia","Cape Verde"),
  c("France","Senegal"), c("France","Iraq"), c("France","Norway"),
  c("Senegal","Iraq"), c("Senegal","Norway"), c("Iraq","Norway"),
  c("Argentina","Austria"), c("Argentina","Jordan"), c("Argentina","Algeria"),
  c("Austria","Jordan"), c("Austria","Algeria"), c("Jordan","Algeria"),
  c("Portugal","Colombia"), c("Portugal","Uzbekistan"), c("Portugal","DR Congo"),
  c("Colombia","Uzbekistan"), c("Colombia","DR Congo"), c("Uzbekistan","DR Congo"),
  c("England","Croatia"), c("England","Ghana"), c("England","Panama"),
  c("Croatia","Ghana"), c("Croatia","Panama"), c("Ghana","Panama")
)

# ── Paso 4: simular todos los partidos ───────────────────────────────────────
resultados <- do.call(rbind, lapply(partidos, function(p) {
  simular_partido(p[1], p[2], modelo_poisson1, siglas)
}))

saveRDS(resultados, "resultados_mundial.rds")
print(resultados)
