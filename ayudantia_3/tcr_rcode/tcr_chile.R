library(ggplot2)

tema <- function() {
  theme_minimal(base_family = "Palatino", base_size = 16) +
    theme(
      plot.title    = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 15),
      axis.title    = element_text(size = 15),
      legend.text   = element_text(size = 14),
      legend.position = "bottom"
    )
}

# Tipo de cambio real de Chile
# Fuente: BIS, WS_EER, tipo de cambio real efectivo (real, broad), frecuencia mensual.
# Convención BIS: un alza del REER = APRECIACIÓN real.
# El curso usa q = e P*/P (alza = DEPRECIACIÓN, como el índice del BCCh), así que
# invertimos el REER y lo rebasamos a 2020 = 100 para que un alza signifique depreciación.
url <- "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_EER/1.0/M.R.B.CL?format=csv"

bruto <- read.csv(url, stringsAsFactors = FALSE)

datos <- data.frame(
  fecha = as.Date(paste0(bruto$TIME_PERIOD, "-01")),
  reer  = as.numeric(bruto$OBS_VALUE)
)
datos <- datos[!is.na(datos$reer), ]
datos <- datos[order(datos$fecha), ]
datos <- datos[datos$fecha <= as.Date("2023-12-01"), ]

# Inversión y rebase a 2020 = 100, de modo que alza = depreciación real (convención q = e P*/P).
base_2020 <- mean(datos$reer[format(datos$fecha, "%Y") == "2020"])
datos$tcr <- 100 * base_2020 / datos$reer

anio_ini <- format(min(datos$fecha), "%Y")
anio_fin <- format(max(datos$fecha), "%Y")

grafico_tcr <- ggplot(datos, aes(x = fecha, y = tcr)) +
  geom_line(color = "#1a3a8f", linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40") +
  labs(
    title    = sprintf("Tipo de Cambio Real de Chile (%s–%s)", anio_ini, anio_fin),
    subtitle = "Índice multilateral, 2020 = 100 (un alza = depreciación real)",
    x        = NULL,
    y        = "Índice (2020 = 100)",
    caption  = "Fuente: elaboración propia a partir del REER (real, broad) del BIS, WS_EER"
  ) +
  tema()

ggsave("outputs/tcr_chile.png", grafico_tcr, width = 8, height = 5, dpi = 150)

cat(sprintf("TCR: %s a %s, %d observaciones\n", anio_ini, anio_fin, nrow(datos)))
