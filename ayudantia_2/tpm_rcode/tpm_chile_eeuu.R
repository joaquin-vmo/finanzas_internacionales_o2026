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

# Tasa de Política Monetaria: Chile vs EE.UU.
# Fuente: BIS, WS_CBPOL, frecuencia mensual.
url <- "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_CBPOL/1.0/M.CL+US?format=csv"

bruto <- read.csv(url, stringsAsFactors = FALSE)

datos <- data.frame(
  fecha = as.Date(paste0(bruto$TIME_PERIOD, "-01")),
  pais  = ifelse(bruto$REF_AREA == "CL", "Chile", "Estados Unidos"),
  tpm   = bruto$OBS_VALUE
)
datos <- datos[!is.na(datos$tpm) & datos$fecha >= as.Date("2000-01-01"), ]

# Figura 1: niveles de la TPM 
grafico_tpm <- ggplot(datos, aes(x = fecha, y = tpm, color = pais)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Chile"          = "#1a3a8f",
                                "Estados Unidos" = "#c0392b")) +
  labs(
    title    = "Tasa de Política Monetaria: Chile y EE.UU. (2000–2026)",
    subtitle = "TPM del Banco Central de Chile y de la Reserva Federal",
    x        = NULL,
    y        = "% anual",
    color    = NULL,
    caption  = "Fuente: Bank for International Settlements (BIS), Central bank policy rates"
  ) +
  tema()

ggsave("outputs/tpm_chile_eeuu.png", grafico_tpm, width = 8, height = 5, dpi = 150)

# Figura 2: diferencial de tasas (Chile − EE.UU.) 
ancho <- reshape(datos, idvar = "fecha", timevar = "pais",
                 direction = "wide")
names(ancho) <- c("fecha", "Chile", "EEUU")
ancho <- ancho[complete.cases(ancho), ]
ancho$dif <- ancho$Chile - ancho$EEUU

grafico_dif <- ggplot(ancho, aes(x = fecha, y = dif)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(color = "#1a3a8f", linewidth = 0.8) +
  labs(
    title    = "Diferencial de Tasas de Política: Chile − EE.UU. (2000–2026)",
    subtitle = "Compensa la depreciación esperada del peso y el riesgo país",
    x        = NULL,
    y        = "Puntos porcentuales",
    caption  = "Fuente: Bank for International Settlements (BIS), Central bank policy rates"
  ) +
  tema()

ggsave("outputs/diferencial_tpm.png", grafico_dif, width = 8, height = 5, dpi = 150)

# Tipo de cambio CLP/USD (BIS WS_XRU) y depreciación interanual del peso
url_tc <- "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_XRU/1.0/M.CL.CLP.A?format=csv"
tc_bruto <- read.csv(url_tc, stringsAsFactors = FALSE)

tc <- data.frame(
  fecha  = as.Date(paste0(tc_bruto$TIME_PERIOD, "-01")),
  clpusd = tc_bruto$OBS_VALUE
)
tc <- tc[order(tc$fecha), ]
tc <- tc[tc$fecha >= as.Date("1999-01-01"), ]
# variación interanual: (e_t / e_{t-12} - 1) * 100
tc$deprec <- c(rep(NA, 12), (tc$clpusd[-(1:12)] / head(tc$clpusd, -12) - 1) * 100)

# Figura 3: diferencial de tasas frente a la depreciación del peso
comp <- merge(ancho[, c("fecha", "dif")], tc[, c("fecha", "deprec")], by = "fecha")
comp <- comp[!is.na(comp$deprec), ]

largo <- rbind(
  data.frame(fecha = comp$fecha, valor = comp$dif,
             serie = "Diferencial de tasas (Chile − EE.UU.)"),
  data.frame(fecha = comp$fecha, valor = comp$deprec,
             serie = "Depreciación interanual del peso")
)

grafico_comp <- ggplot(largo, aes(x = fecha, y = valor, color = serie)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Diferencial de tasas (Chile − EE.UU.)" = "#1a3a8f",
                                "Depreciación interanual del peso"       = "#c0392b")) +
  labs(
    title    = "Diferencial de Tasas y Depreciación del Peso (2000–2026)",
    subtitle = "El mayor interés local convive con un peso que tiende a depreciarse",
    x        = NULL,
    y        = "% anual / puntos porcentuales",
    color    = NULL,
    caption  = "Fuente: BIS, Central bank policy rates y Exchange rates (WS_XRU)"
  ) +
  tema()

ggsave("outputs/diferencial_vs_depreciacion.png", grafico_comp, width = 8, height = 5, dpi = 150)

cat(sprintf("Diferencial promedio Chile - EE.UU. (2000-2026): %.2f pp\n", mean(ancho$dif)))
cat(sprintf("Último dato (%s): Chile %.2f%%, EE.UU. %.2f%%, diferencial %.2f pp\n",
            format(max(ancho$fecha), "%Y-%m"),
            ancho$Chile[which.max(ancho$fecha)],
            ancho$EEUU[which.max(ancho$fecha)],
            ancho$dif[which.max(ancho$fecha)]))
cat(sprintf("Depreciación promedio anual del peso (2000-2026): %.2f %%\n", mean(comp$deprec)))
cat(sprintf("Premio por riesgo implícito = diferencial - depreciación (promedio): %.2f pp\n",
            mean(comp$dif - comp$deprec)))
