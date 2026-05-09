# Modelo Presidencial Colombia 2026 — Receta para Concurso de Pronósticos

**Cocinero:** [@PoliticaConDato](https://twitter.com/PoliticaConDato) / PoliData
**Concurso:** [Recetas Electorales — 1ra vuelta 2026](https://recetas-electorales.netlify.app/elecciones/2026-colombia/2026-05-03-concurso/2026-concurso-1era.html)
**Fecha de la elección:** 31 de mayo de 2026

## Resumen

Este modelo combina tres fuentes de información para producir un pronóstico de la primera vuelta presidencial colombiana:

| Fuente | Peso | Descripción |
|---|---:|---|
| **Encuestas** | 75% | Promedio rodante diario ponderado por calidad de encuestadora, error reportado y recencia (ventana 60 días). |
| **Google Trends** | 10% | Interés relativo de búsqueda (4 temas, medias móviles 7 y 30 días). |
| **Mercados de predicción** | 15% | Polymarket + Kalshi, normalizados por fuente y combinados. |

El modelo produce una sola salida: `Modelo_Tabla.png` — la tabla del ensamble final con intervalo de confianza al 90% por candidato.

## Cómo correr

Requisitos: R 4.0+ con los paquetes listados al inicio de [`Modelo_Presidencial_2026.R`](Modelo_Presidencial_2026.R) (`lubridate`, `dplyr`, `tidyr`, `data.table`, `kableExtra`, `gridExtra`, `ggplot2`, etc.).

```bash
Rscript Modelo_Presidencial_2026.R
```

El script lee todos los datos desde la carpeta `Polls/` (incluida en este repositorio) y genera `Modelo_Tabla.png` en el directorio raíz. **No se hacen llamadas a APIs externas** — todos los datos están bundleados para garantizar reproducibilidad.

## Datos

Toda la información usada por el modelo está en [`Polls/`](Polls/):

| Archivo | Contenido |
|---|---|
| `polls2026.csv` | Encuestas presidenciales 2026 (encuestadora, fecha, muestra, error, calificación, % por candidato) |
| `Polymarket Price Data Colombia Election 2025-2026.csv` | Precios diarios de Polymarket por candidato |
| `Kalshi Price History Colombia Election 2026.csv` | Precios diarios de Kalshi |
| `presidential_trends_intent_Presidente.csv` | Google Trends — tema "intención presidente" |
| `presidential_trends_intent_2026.csv` | Google Trends — tema "intención 2026" |
| `presidential_trends_intent_Propuestas.csv` | Google Trends — tema "propuestas" |
| `Presidential_trends_topics_raw.csv` | Google Trends — interés agregado por entidades |

### Fuentes públicas

- **Encuestas:** agregación manual de publicaciones de cada encuestadora (todas verificables en sus respectivos sitios)
- **Polymarket:** [gamma-api.polymarket.com](https://gamma-api.polymarket.com)
- **Kalshi:** [kalshi.com](https://kalshi.com)
- **Google Trends:** [trends.google.com](https://trends.google.com) (geo=CO, ventana 90 días)

## Metodología

### 1. Modelo de encuestas

Para cada día *d*, se calcula un promedio ponderado de la encuesta más reciente por encuestadora dentro de una ventana de 60 días, donde el peso de la encuesta *p* es:

```
peso(p, d) = peso_calificación × peso_error × peso_fecha
peso_calificación = Calificación / 10
peso_error        = 1 − 3 × Error
peso_fecha        = 1.2 × (1 − días/60)   si 0 ≤ días ≤ 60, sino 0
```

### 2. Calificación de encuestadoras

Las calificaciones (0–10) en `polls2026.csv` provienen de un análisis histórico de **9 carreras** (1ra y 2da vuelta 2018, 1ra y 2da vuelta 2022, 5 alcaldías 2023). Por cada (encuestadora, carrera) se computan hasta 6 métricas:

1. RMSE entre predicción y resultado oficial
2. Predicción correcta del ganador (binaria)
3. Predicción correcta del top-2 (binaria, solo carreras multi-candidato)
4. Error en margen 1°-2°
5. Error en margen 2°-3° (solo carreras multi-candidato)
6. RMSE ajustado por tamaño de muestra (cuando *n* está disponible)

Cada métrica se puntúa con un esquema híbrido (50% absoluto + 50% rank dentro de la carrera). Los puntajes por ciclo se agregan con pesos de recencia (2018=0.625, 2022=1.25, 2023=1.25 con ronda R1=ronda RO dentro de cada ciclo) y se aplica suavizado bayesiano (k=3) para encuestadoras con pocas carreras. La calificación final se mapea a 0–10 vía percentil de la distribución observada.

### 3. Modelo de Google Trends

Para cada tema:
- Media móvil de **7 y 30 días** sobre los hits crudos
- Renormalización a una participación diaria (% del interés total entre los candidatos rastreados)
- Promedio de las dos ventanas (7 y 30 días)

Los 4 temas se combinan con pesos relativos `1:1:1:3` (el tema `topics` con interés por entidades pesa 3× más que cada keyword). El resultado se reescala por `(1 − blanco − otros)` para mantener consistencia con el modelo de encuestas.

### 4. Modelo de mercados

- **Polymarket** (escala decimal 0–1) y **Kalshi** (escala 0–100, dividida por 100)
- Suavizado por **media móvil de 7 y 30 días**, calculado **dentro de cada fuente independientemente**
- Cada fuente se normaliza a una participación de mercado por candidato por día
- Las dos fuentes se combinan con peso `1:1`, con normalización NA-aware (si Polymarket falta para una fecha, Kalshi pasa solo)

### 5. Ensamble final

```
Ensamble = 0.75 × Encuestas + 0.10 × Google Trends + 0.15 × Mercados
```

### 6. Intervalos de confianza

Los intervalos al 90% por candidato se calculan vía **simulación Monte Carlo (50,000 muestras)** usando una distribución Beta posterior por encuesta y aproximaciones normales (~15% para Trends, ~10% para mercados) para las otras fuentes.

## Reconocimientos

El núcleo del modelo (modelo de encuestas, ensamble, simulación de probabilidad) está basado en la versión que usé para las elecciones presidenciales de **2022**, hecha antes de la era de los modelos de lenguaje grandes.

Para esta versión 2026 utilicé asistencia de IA (**Claude**, de Anthropic) para:

- Refactorizar y limpiar el código original (versión 2022)
- Documentación, comentarios y este README
- Organización de archivos y reproducibilidad
- Análisis cuantitativo del sistema de calificación de encuestadoras (incluyendo validación cruzada leave-one-race-out)

La metodología, decisiones de modelado, selección de fuentes, y todos los juicios sustantivos son del autor (PoliticaConDato).

## Licencia

[Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE) — alineada con la licencia del sitio del concurso. Puedes usar, modificar y redistribuir libremente, dando crédito al autor.
