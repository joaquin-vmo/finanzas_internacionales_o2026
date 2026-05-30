library(WDI)
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

# ── Cuenta corriente ─────────────────────────────────────────────────────────
# BN.CAB.XOKA.GD.ZS: cuenta corriente como % del PIB
datos_cc <- WDI(
  country   = "CL",
  indicator = "BN.CAB.XOKA.GD.ZS",
  start     = 1990,
  end       = 2023
)
names(datos_cc)[names(datos_cc) == "BN.CAB.XOKA.GD.ZS"] <- "cc"

grafico_cc <- ggplot(datos_cc, aes(x = year, y = cc)) +
  geom_line(color = "#1a3a8f", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title    = "Cuenta Corriente de Chile (1990–2023)",
    subtitle = "Como porcentaje del PIB",
    x        = NULL,
    y        = "% del PIB",
    caption  = "Fuente: World Development Indicators, Banco Mundial"
  ) +
  tema()

ggsave("outputs/cc_chile.png", grafico_cc, width = 8, height = 5, dpi = 150)

# ── Ahorro nacional e inversión ──────────────────────────────────────────────
# NY.GNS.ICTR.ZS: ahorro bruto nacional como % del PIB
# NE.GDI.TOTL.ZS: formación bruta de capital (inversión) como % del PIB
datos_si <- WDI(
  country   = "CL",
  indicator = c("NY.GNS.ICTR.ZS", "NE.GDI.TOTL.ZS"),
  start     = 1990,
  end       = 2023
)
names(datos_si)[names(datos_si) == "NY.GNS.ICTR.ZS"] <- "ahorro"
names(datos_si)[names(datos_si) == "NE.GDI.TOTL.ZS"] <- "inversion"

grafico_si <- ggplot(datos_si, aes(x = year)) +
  geom_line(aes(y = ahorro,    color = "Ahorro nacional"),  linewidth = 0.8) +
  geom_line(aes(y = inversion, color = "Inversión"),        linewidth = 0.8) +
  scale_color_manual(values = c("Ahorro nacional" = "#1a3a8f",
                                "Inversión"       = "#c0392b")) +
  labs(
    title    = "Ahorro Nacional e Inversión en Chile (1990–2023)",
    subtitle = "Como porcentaje del PIB",
    x        = NULL,
    y        = "% del PIB",
    color    = NULL,
    caption  = "Fuente: World Development Indicators, Banco Mundial"
  ) +
  tema()

ggsave("outputs/ahorro_inversion_chile.png", grafico_si, width = 8, height = 5, dpi = 150)
