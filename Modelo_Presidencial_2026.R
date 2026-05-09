####################################################################
##  Modelo Presidencial 2026 - Colombia                           ##
##  Versión pública para proyectos de transparencia               ##
##  by @PoliticaConDato                                           ##
####################################################################
##
##  Este script reproduce el modelo electoral presidencial 2026 a
##  partir de archivos CSV bundleados en el repositorio. NO hace
##  llamadas a APIs externas — todos los datos se leen localmente
##  para garantizar reproducibilidad.
##
##  Cómo correrlo:
##    1. Clonar el repo
##    2. Abrir RStudio en la carpeta del repo (working directory)
##    3. Instalar las librerías listadas abajo si hace falta
##    4. source("Modelo_Presidencial_2026.R")
##
##  Entradas (en la carpeta Polls/):
##    polls2026.csv                              -> Encuestas
##    presidential_trends_intent_Presidente.csv  -> Google Trends - "Presidente"
##    presidential_trends_intent_2026.csv        -> Google Trends - "2026"
##    presidential_trends_intent_Propuestas.csv  -> Google Trends - "Propuestas"
##    Presidential_trends_topics_raw.csv         -> Google Trends - tópicos persona
##    Polymarket Price Data Colombia Election 2025-2026.csv
##    Kalshi Price History Colombia Election 2026.csv
##
##  Salida (en la raíz del repo):
##    Modelo_Tabla.png   -> Tabla del ensemble final con IC 90% por candidato
##
##  Metodología:
##    * Encuestas: promedio diario ponderado (rating x error x recencia,
##      ventana de 60 días, decay lineal). Renormaliza excluyendo indecisos.
##    * Tendencias: 4 sub-modelos (3 de palabra-clave + 1 de tópicos),
##      promedio ponderado (1/1/1/3 — el tópico de persona pesa más).
##    * Mercados: Polymarket + Kalshi, suavizado 7d/30d, normalizado por
##      fuente, luego promediado entre fuentes.
##    * Ensemble: 75% Encuestas + 10% Tendencias + 15% Mercados.
##    * Voto en blanco: prior fijo (1.7%).
##

####################################################################
##  Librerías                                                    ##
####################################################################
library(lubridate)    # parseo de fechas
library(reshape2)     # melt() para pasar a formato largo
library(tidyr)        # pivot_longer/pivot_wider
library(dplyr)        # group_by, summarise, mutate
library(data.table)   # frollmean (medias móviles para mercados)
library(stringr)      # limpieza de texto en mercados
library(stringi)      # transliteración Latin-ASCII en mercados
library(readr)        # read_csv para los CSVs de mercados
library(ggplot2)      # margin() para padding de la tabla
library(kableExtra)   # tabla HTML (para reportes R Markdown)
library(png)          # readPNG (logo en la tabla)
library(gridExtra)    # tableGrob
library(grid)         # viewport, gpar, etc.

####################################################################
##  Constantes y configuración                                    ##
####################################################################

# Voto en blanco - benchmark histórico
blanco <- 0.017

# Pesos del ensemble (deben sumar 1)
w_polls   <- 0.75
w_trends  <- 0.10
w_markets <- 0.15

# Candidatos rastreados
target_candidates <- c("Cepeda", "Abelardo", "Fajardo", "Lopez", "Paloma")

# Logo PoliData (se usa solamente en la tabla final)
logo_url  <- "https://raw.githubusercontent.com/PoliticaConDato/Elecciones-2022/main/data_2nda/PoliData.png"
logo_path <- file.path(tempdir(), "PoliData.png")
download.file(logo_url, logo_path, mode = "wb", quiet = TRUE)

####################################################################
##  1. MODELO DE ENCUESTAS                                        ##
####################################################################

# Cargar el CSV de encuestas
polls <- read.csv("Polls/polls2026.csv")

# Limpieza básica
polls <- polls[, !names(polls) %in% c("Start.Date", "End.Date", "Methodology")]
polls$Date <- mdy(polls$Date)
polls[is.na(polls)] <- 0
polls$Decided <- 1 - polls$Undecided

# Largo: una fila por (encuesta, candidato)
polls <- reshape2::melt(
  polls,
  id = c("ID", "Date", "Pollster", "Rating", "Sample", "Decided", "Error"),
  variable.name = "Candidate"
)
polls$value.norm <- polls$value / polls$Decided

# Promediar duplicados (e.g., escenarios alternativos)
polls <- polls %>%
  group_by(ID, Date, Pollster, Rating, Sample, Error, Candidate) %>%
  summarise(value = mean(value), value.norm = mean(value.norm), .groups = "drop")

# Pesos
polls$rating.weight <- polls$Rating / 10
polls$error.weight  <- 1 - polls$Error * 3

# Construir grilla de fechas x candidatos
start.date <- min(polls$Date)
end.date   <- max(polls$Date)
date.vec   <- seq(start.date, end.date, 1)
cand.vec   <- unique(as.character(polls$Candidate))

model.df <- merge(date.vec, cand.vec)
colnames(model.df) <- c("Date", "Candidate")

# Promedio diario ponderado por (rating x error x recencia)
weighted.values <- function(x) {
  model.date    <- ymd(x[1])
  reduced.polls <- polls[polls$Candidate == x[2], ]

  reduced.polls$days        <- model.date - reduced.polls$Date
  reduced.polls$date.weight <- ifelse(
    reduced.polls$days < 0, 0,
    ifelse(reduced.polls$days > 60, 0,
           1.2 * (1 - reduced.polls$days / 60))
  )
  reduced.polls <- reduced.polls[rev(order(reduced.polls$Pollster, reduced.polls$Date)), ]
  reduced.polls <- reduced.polls[reduced.polls$date.weight > 0, ]
  reduced.polls <- reduced.polls[!duplicated(reduced.polls$Pollster), ]
  reduced.polls$weight <- reduced.polls$rating.weight *
                          reduced.polls$error.weight *
                          reduced.polls$date.weight
  reduced.polls$weighted.value      <- reduced.polls$weight * reduced.polls$value
  reduced.polls$weighted.value.norm <- reduced.polls$weight * reduced.polls$value.norm

  total.weight <- sum(reduced.polls$weight)
  if (!is.finite(total.weight) || total.weight <= 0) {
    return(c(weighted.value = NA_real_, weighted.value.norm = NA_real_))
  }

  vote.cols <- reduced.polls[, c("weighted.value", "weighted.value.norm")]
  vote.cols <- colSums(vote.cols)
  vote.cols / total.weight
}

model.df <- cbind(model.df, t(apply(model.df, 1, weighted.values)))

# Snapshot final del modelo de encuestas (último día disponible)
poll.model <- model.df[model.df$Date == max(model.df$Date), ]
poll.model <- poll.model[, -c(1, 3)]
colnames(poll.model) <- c("Candidate", "Polls")
poll.model$Polls <- poll.model$Polls * (1 - blanco)
poll.model <- rbind(poll.model, data.frame(Candidate = "Blanco", Polls = blanco))

####################################################################
##  2. MODELO DE GOOGLE TRENDS                                    ##
####################################################################
##
##  En esta versión pública leemos directamente los archivos CSV
##  precomputados (no se llama a la API de Google Trends).
##  Cada CSV trae columnas: date, Candidate, avg
##  donde 'avg' es ya la participación relativa diaria por candidato
##  (promedio de medias móviles 7d y 30d).
##

# Helper: leer un CSV de tendencias y etiquetarlo con un sufijo
tag_trend <- function(path, tag) {
  read.csv(path) %>%
    mutate(date = ymd(date),
           Candidate = as.character(Candidate)) %>%
    select(date, Candidate, avg) %>%
    rename(!!paste0("v_", tag) := avg)
}

t1 <- tag_trend("Polls/presidential_trends_intent_Presidente.csv", "k1")
t2 <- tag_trend("Polls/presidential_trends_intent_2026.csv",       "k2")
t3 <- tag_trend("Polls/presidential_trends_intent_Propuestas.csv", "k3")
t4 <- tag_trend("Polls/Presidential_trends_topics_raw.csv",        "person")

# Merge de las 4 fuentes por (fecha, candidato)
trends <- Reduce(function(x, y) merge(x, y, all = TRUE, by = c("date", "Candidate")),
                 list(t1, t2, t3, t4))

# Promedio ponderado: el tópico de persona pesa 3, las palabras-clave 1 cada una
val_cols <- c("v_k1", "v_k2", "v_k3", "v_person")
weights  <- c(1, 1, 1, 3)

weighted_row_mean <- function(mat, wts) {
  num <- rowSums(mat * rep(wts, each = nrow(mat)), na.rm = TRUE)
  den <- rowSums(!is.na(mat) * rep(wts, each = nrow(mat)))
  ifelse(den > 0, num / den, NA_real_)
}

trends$value <- weighted_row_mean(as.matrix(trends[, val_cols]), weights)
trends <- trends[, c("date", "Candidate", "value")]

# Renormalizar diariamente, escalando a (1 - otros - blanco)
otros <- max(min(as.numeric(poll.model$Polls[poll.model$Candidate == "Otros"]), 1), 0)

trends <- trends %>%
  mutate(value = ifelse(is.finite(value), value, NA_real_),
         value = pmax(value, 0)) %>%
  group_by(date) %>%
  mutate(day_total = sum(value, na.rm = TRUE),
         value = ifelse(day_total > 0,
                        value / day_total * (1 - otros - blanco), 0),
         value = pmax(0, pmin(value, 1 - otros - blanco))) %>%
  ungroup() %>%
  select(date, Candidate, value)

# Snapshot final
trends.model <- trends[trends$date == max(trends$date), c("Candidate", "value")]
colnames(trends.model) <- c("Candidate", "Trends")
trends.model <- rbind(trends.model, data.frame(Candidate = "Blanco", Trends = blanco))

####################################################################
##  3. MODELO DE MERCADOS DE PREDICCIÓN (Polymarket + Kalshi)     ##
####################################################################

polymarket_file <- "Polls/Polymarket Price Data Colombia Election 2025-2026.csv"
kalshi_file     <- "Polls/Kalshi Price History Colombia Election 2026.csv"

# Mapeo de nombres del mercado -> ID interno
candidate_map <- c(
  "ivan cepeda castro"        = "Cepeda",
  "ivan cepeda"               = "Cepeda",
  "abelardo de la espriella"  = "Abelardo",
  "abelardo"                  = "Abelardo",
  "sergio fajardo (dc)"       = "Fajardo",
  "sergio fajardo"            = "Fajardo",
  "claudia lopez (ind)"       = "Lopez",
  "claudia lópez (ind)"       = "Lopez",
  "claudia lopez"             = "Lopez",
  "claudia lópez"             = "Lopez",
  "lopez"                     = "Lopez",
  "paloma valencia"           = "Paloma",
  "paloma"                    = "Paloma"
)

normalize_name <- function(x) {
  x %>% stri_trans_general("Latin-ASCII") %>% str_to_lower() %>% str_squish()
}

safe_share <- function(num, den) ifelse(den > 0, num / den, 0)

# Polymarket: precios en escala 0-1
read_polymarket <- function(path) {
  pm <- read_csv(path, show_col_types = FALSE)
  date_col <- names(pm)[1]
  x <- as.character(pm[[date_col]]) %>%
    str_squish() %>%
    str_replace_all("\\s*\\(UTC\\)\\s*$", "") %>%
    str_replace_all("\\s*UTC\\s*$", "")
  dt <- suppressWarnings(parse_date_time(
    x, orders = c("Y-m-d H:M:S","Y-m-d H:M","Y-m-d","m/d/Y H:M:S","m/d/Y H:M","m/d/Y"),
    tz = "UTC", exact = FALSE
  ))
  pm %>%
    mutate(date = as.Date(dt)) %>%
    select(-all_of(date_col)) %>%
    pivot_longer(-date, names_to = "candidate_raw", values_to = "prob_raw") %>%
    mutate(candidate_key = normalize_name(candidate_raw),
           Candidate = unname(candidate_map[candidate_key]),
           prob = pmax(0, pmin(as.numeric(prob_raw), 1))) %>%
    filter(!is.na(date), !is.na(Candidate), Candidate %in% target_candidates) %>%
    group_by(date, Candidate) %>%
    summarise(prob = mean(prob, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "polymarket")
}

# Kalshi: precios en cents (0-100), dividir por 100
read_kalshi <- function(path) {
  k <- read_csv(path, show_col_types = FALSE)
  if (!("timestamp" %in% names(k))) stop("Kalshi: falta columna 'timestamp'")
  k %>%
    mutate(ts = as.numeric(timestamp),
           date = as.Date(as.POSIXct(ifelse(ts > 1e12, ts / 1000, ts),
                                     origin = "1970-01-01", tz = "UTC"))) %>%
    select(-timestamp, -ts) %>%
    pivot_longer(-date, names_to = "candidate_raw", values_to = "prob_raw") %>%
    mutate(candidate_key = normalize_name(candidate_raw),
           Candidate = unname(candidate_map[candidate_key]),
           prob = as.numeric(prob_raw) / 100) %>%
    filter(!is.na(Candidate), Candidate %in% target_candidates) %>%
    select(date, Candidate, prob) %>%
    mutate(source = "kalshi")
}

# Suavizado y normalización por fuente, luego combinación
compute_market_shares <- function(df, smooth_days_1 = 7, smooth_days_2 = 30) {
  eps <- 1e-9
  df %>%
    arrange(date) %>%
    group_by(source, Candidate) %>%
    mutate(m1 = data.table::frollmean(prob, smooth_days_1, align = "right", fill = NA),
           m2 = data.table::frollmean(prob, smooth_days_2, align = "right", fill = NA)) %>%
    ungroup() %>%
    mutate(m1 = ifelse(is.na(m1), prob, m1),
           m2 = ifelse(is.na(m2), prob, m2),
           m1 = pmax(0, pmin(m1, 1)),
           m2 = pmax(0, pmin(m2, 1))) %>%
    group_by(source, date) %>%
    mutate(total1 = sum(m1, na.rm = TRUE),
           total2 = sum(m2, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total1 = ifelse(total1 < eps, eps, total1),
           total2 = ifelse(total2 < eps, eps, total2),
           s1 = safe_share(m1, total1),
           s2 = safe_share(m2, total2),
           share = (s1 + s2) / 2,
           share = share * (1 - otros - blanco),
           share = pmax(0, pmin(share, 1 - otros - blanco))) %>%
    select(date, Candidate, source, share)
}

combine_sources <- function(df_shares, w_polymarket = 1, w_kalshi = 1) {
  df_shares %>%
    mutate(weight = case_when(source == "polymarket" ~ w_polymarket,
                              source == "kalshi"     ~ w_kalshi,
                              TRUE ~ 1)) %>%
    group_by(date, Candidate) %>%
    summarise(value_raw = sum(share * weight, na.rm = TRUE) /
                          sum(weight[!is.na(share)], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(value_raw = ifelse(is.finite(value_raw), value_raw, NA_real_),
           value_raw = pmax(value_raw, 0)) %>%
    group_by(date) %>%
    mutate(day_total = sum(value_raw, na.rm = TRUE),
           value = ifelse(day_total > 0,
                          value_raw / day_total * (1 - otros - blanco),
                          NA_real_),
           value = pmax(0, pmin(value, 1 - otros - blanco))) %>%
    ungroup() %>%
    filter(!is.na(value)) %>%
    select(date, Candidate, value)
}

# Ejecutar pipeline de mercados
pm_long <- read_polymarket(polymarket_file)
k_long  <- read_kalshi(kalshi_file)

markets_long   <- bind_rows(pm_long, k_long)
markets_shares <- compute_market_shares(markets_long)
markets.model  <- combine_sources(markets_shares, w_polymarket = 1, w_kalshi = 1)

# Snapshot final
market.model <- markets.model[markets.model$date ==
                                min(max(k_long$date), max(pm_long$date)), ]
market.model <- market.model[, -c(1)]
colnames(market.model) <- c("Candidate", "Market")
market.model <- rbind(market.model, data.frame(Candidate = "Blanco", Market = blanco))

####################################################################
##  4. ENSEMBLE                                                   ##
####################################################################

ensemble.model <- poll.model[poll.model$Candidate != "Undecided", ]
ensemble.model <- merge(ensemble.model, trends.model, by = "Candidate", all.x = TRUE)
ensemble.model <- merge(ensemble.model, market.model, by = "Candidate", all.x = TRUE)

# Para "Otros" y "Blanco" donde no hay señal de Trends/Mercados, usar Polls
ensemble.model$Trends[ensemble.model$Candidate == "Otros"]  <- ensemble.model$Polls[ensemble.model$Candidate == "Otros"]
ensemble.model$Market[ensemble.model$Candidate == "Otros"]  <- ensemble.model$Polls[ensemble.model$Candidate == "Otros"]
ensemble.model$Trends[ensemble.model$Candidate == "Blanco"] <- blanco
ensemble.model$Market[ensemble.model$Candidate == "Blanco"] <- blanco

ensemble.model$Ensemble <-
  ensemble.model$Polls   * w_polls +
  ensemble.model$Trends  * w_trends +
  ensemble.model$Market  * w_markets

####################################################################
##  5. INTERVALOS DE CONFIANZA (90%)                              ##
####################################################################

model.date    <- max(polls$Date)
reduced.polls <- polls
reduced.polls$days        <- model.date - reduced.polls$Date
reduced.polls$date.weight <- ifelse(
  reduced.polls$days < 0, 0,
  ifelse(reduced.polls$days > 60, 0,
         1.2 * (1 - reduced.polls$days / 60))
)
reduced.polls <- reduced.polls[rev(order(reduced.polls$Pollster, reduced.polls$Date)), ]
reduced.polls <- reduced.polls[reduced.polls$date.weight > 0, ]
reduced.polls <- reduced.polls[!duplicated(reduced.polls$Pollster), ]
reduced.polls$weight <- reduced.polls$rating.weight *
                        reduced.polls$error.weight *
                        reduced.polls$date.weight
total.weight <- sum(reduced.polls$weight)
reduced.polls$weight <- reduced.polls$weight / total.weight
reduced.polls <- reduced.polls[, c("ID", "weight")]

new.polls <- merge(polls, reduced.polls, by = "ID")
new.polls <- new.polls[, c("Sample", "value.norm", "weight", "Candidate")]

candidates <- setdiff(sort(unique(new.polls$Candidate)), "Undecided")

set.seed(20260111)

# Simulación Beta-posterior por candidato
simulate_candidate <- function(poll_loop, ndraw = 50000) {
  poll_loop <- poll_loop %>% filter(is.finite(value.norm), Sample > 0, weight > 0)
  if (nrow(poll_loop) == 0) return(rep(NA_real_, ndraw))
  idx <- sample(seq_len(nrow(poll_loop)), size = ndraw, replace = TRUE,
                prob = poll_loop$weight)
  n <- poll_loop$Sample[idx]
  p <- pmin(pmax(poll_loop$value.norm[idx], 0), 1)
  rbeta(ndraw, 1 + p * n, 1 + (1 - p) * n)
}

# Generar vectores de simulación
for (cn in candidates) {
  vec <- simulate_candidate(new.polls %>% filter(Candidate == cn), ndraw = 50000)
  assign(paste0("vec.", cn), vec)
}

# Intervalo del 90% para el ensemble (combinando incertidumbre de las 3 fuentes)
for (cn in candidates) {
  poll.vec    <- get(paste0("vec.", cn))
  poll.point  <- ensemble.model$Polls[ensemble.model$Candidate == cn]
  trend.point <- ensemble.model$Trends[ensemble.model$Candidate == cn]
  market.point<- ensemble.model$Market[ensemble.model$Candidate == cn]

  trend.sd  <- abs(trend.point)  * 0.15
  market.sd <- abs(market.point) * 0.10
  trend.vec  <- pmax(0, pmin(1, rnorm(50000, trend.point,  trend.sd)))
  market.vec <- pmax(0, pmin(1, rnorm(50000, market.point, market.sd)))

  ensemble.vec <- (poll.vec * (1 - blanco)) * w_polls +
                  trend.vec  * w_trends +
                  market.vec * w_markets
  q <- quantile(ensemble.vec, c(0.05, 0.95), na.rm = TRUE)
  ensemble.model$Inter[ensemble.model$Candidate == cn] <-
    paste0(round(q[[1]] * 100, 1), "-", round(q[[2]] * 100, 1))
}

# IC histórico para Blanco (no hay datos de encuestas)
ensemble.model$Inter[ensemble.model$Candidate == "Blanco"] <- "0.5-3.5"

####################################################################
##  6. SALIDA — TABLA DEL ENSEMBLE                                ##
####################################################################

# Convertir a porcentajes y ordenar por valor del ensemble
ensemble.model[, c("Polls", "Trends", "Market", "Ensemble")] <-
  ensemble.model[, c("Polls", "Trends", "Market", "Ensemble")] * 100
ensemble.model <- arrange(ensemble.model, desc(Ensemble))

# Tabla en HTML (kable) — útil si se quiere renderizar en R Markdown
kable(ensemble.model, "html",
      digits = 1,
      caption = "Modelo de la elección presidencial de Colombia 2026 (% de votos)") %>%
  kable_styling(full_width = FALSE) %>%
  footnote(number = c(
    "Cocinero: PoliData",
    paste0("Fecha pronóstico: ", format(Sys.Date(), "%Y-%m-%d")),
    "Herramienta IA usada: Claude (Anthropic) — refactor de código y documentación",
    "Social: @PoliticaConDato"
  ))

# Versión PNG con tema oscuro
table.df <- ensemble.model[, c("Candidate", "Polls", "Trends", "Market", "Ensemble", "Inter")]
colnames(table.df) <- c("Candidato", "Encuestas", "Google Trends", "Mercados", "Ensemble", "IC 90%")

table.df$Encuestas       <- sprintf("%.1f", table.df$Encuestas)
table.df$`Google Trends` <- sprintf("%.1f", table.df$`Google Trends`)
table.df$Mercados        <- sprintf("%.1f", table.df$Mercados)
table.df$Ensemble        <- sprintf("%.1f", table.df$Ensemble)

table_png <- tableGrob(
  table.df,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 16,
    base_colour = "#E8E8E8",
    core    = list(bg_params = list(fill = "#2a2a2a"),
                   padding   = margin(2, 16, 2, 16)),
    colhead = list(bg_params = list(fill = "#404040"),
                   fg_params = list(col = "#E8E8E8", fontface = "bold"),
                   padding   = margin(4, 16, 4, 16)),
    rowhead = list(bg_params = list(fill = "#2a2a2a"),
                   fg_params = list(col = "#E8E8E8"))
  )
)

png("Modelo_Tabla.png", width = 1200, height = 750, res = 120)
grid.newpage()
grid.rect(gp = gpar(fill = "#1a1a1a", col = NA))

table_width  <- 0.80
table_height <- 0.45  # tighter to leave room for title (above) + 4-line footer (below)
title_y      <- 0.93
subtitle_y   <- 0.88
table_y      <- 0.58
footer_y     <- 0.20

grid.text("Modelo de la elección presidencial de Colombia 2026",
          x = 0.5, y = title_y,
          gp = gpar(fontsize = 22, fontface = "bold", col = "#E8E8E8"))
grid.text("(% de votos)",
          x = 0.5, y = subtitle_y,
          gp = gpar(fontsize = 16, col = "#E8E8E8"))

pushViewport(viewport(x = 0.5, y = table_y, width = table_width, height = table_height))
grid.draw(table_png)
popViewport()

## Footer en formato del concurso: 4 notas numeradas
grid.text("(1)  Cocinero: PoliData",
          x = 0.5, y = footer_y,
          gp = gpar(fontsize = 10, col = "#E8E8E8"))
grid.text(paste0("(2)  Fecha pronóstico: ", format(Sys.Date(), "%Y-%m-%d")),
          x = 0.5, y = footer_y - 0.030,
          gp = gpar(fontsize = 10, col = "#E8E8E8"))
grid.text("(3)  Herramienta IA usada: Claude (Anthropic) — refactor de código y documentación",
          x = 0.5, y = footer_y - 0.060,
          gp = gpar(fontsize = 10, col = "#E8E8E8"))
grid.text("(4)  Social: @PoliticaConDato",
          x = 0.5, y = footer_y - 0.090,
          gp = gpar(fontsize = 10, col = "#E8E8E8"))
grid.text("IC 90% = Intervalo de confianza al 90% para el modelo Ensemble",
          x = 0.5, y = footer_y - 0.130,
          gp = gpar(fontsize = 9, col = "#999999"))

logo_img <- readPNG(logo_path)
grid.raster(logo_img, x = 0.95, y = footer_y - 0.130, width = 0.035,
            just = c("right", "center"))
dev.off()

####################################################################
##  Resumen en consola                                            ##
####################################################################

cat("\n=== Modelo Presidencial 2026 — Resumen ===\n")
cat(sprintf("  Encuestas hasta: %s (%d encuestas)\n",
            format(max(polls$Date)), length(unique(polls$ID))))
cat(sprintf("  Voto en blanco (prior): %.1f%%\n", blanco * 100))
cat(sprintf("  Pesos del ensemble: Encuestas=%.0f%%  Trends=%.0f%%  Mercados=%.0f%%\n",
            w_polls * 100, w_trends * 100, w_markets * 100))
cat("\n  Pronóstico final (% de votos):\n")
print(ensemble.model[, c("Candidate", "Polls", "Trends", "Market", "Ensemble", "Inter")],
      row.names = FALSE)
cat("\n  Imagen generada: Modelo_Tabla.png\n")
