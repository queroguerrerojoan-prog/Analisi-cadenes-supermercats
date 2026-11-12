# =============================================================================
#  04 - Anàlisi NPS (Net Promoter Score)
# -----------------------------------------------------------------------------
#  Classifica els clients en Detractors (0-6), Neutrals (7-8) i Promotors
#  (9-10) i calcula l'NPS = % Promotors - % Detractors, amb interval de
#  confiança, total, per cadena i per cadena i grup d'edat.
#  Requereix: data_responses, cadenes, out_path (de 00_main.R, 01 i 03)
# =============================================================================


# ---- 1. Paràmetres ----------------------------------------------------------

# Variable de puntuació. L'NPS estàndard es calcula sobre la pregunta
# "Amb quina probabilitat ens recomanaries?". Si el qüestionari la té,
# posa-la aquí; amb Ph1.Satisfaccion és un índex "tipus NPS".
var_nps <- "Ph1.Satisfaccion"

nivells_nps <- c("Detractors", "Neutrals", "Promotors")
colors_nps  <- c("Detractors" = "#CA0020",
                 "Neutrals"   = "#BDBDBD",
                 "Promotors"  = "#1B7837")

# Mida mínima de mostra per considerar fiable un NPS per subgrup
n_minim <- 30


# ---- 2. Preparació de les dades ---------------------------------------------
# as.numeric(as.character()) evita comparar text ("10" <= "6" és TRUE).

dades_nps <- data_responses[
  Establecimiento %in% cadenes,
  .(Establecimiento,
    C1.Edad,
    puntuacio = as.numeric(as.character(get(var_nps))))
][!is.na(puntuacio)]

dades_nps[, categoria := factor(
  fcase(puntuacio <= 6, "Detractors",
        puntuacio <= 8, "Neutrals",
        default = "Promotors"),
  levels = nivells_nps
)]


# ---- 3. Càlcul de l'NPS -----------------------------------------------------
# Interval de confiança del 95% per a una diferència de proporcions
# de la mateixa mostra: Var(NPS) = [pP + pD - (pP - pD)^2] / n

calcular_nps <- function(dt, by = NULL) {
  dt[, {
    p_prom <- mean(categoria == "Promotors")
    p_detr <- mean(categoria == "Detractors")
    nps    <- 100 * (p_prom - p_detr)
    se     <- 100 * sqrt((p_prom + p_detr - (p_prom - p_detr)^2) / .N)
    .(n              = .N,
      pct_promotors  = 100 * p_prom,
      pct_neutrals   = 100 * (1 - p_prom - p_detr),
      pct_detractors = 100 * p_detr,
      nps            = nps,
      ic_inf         = nps - 1.96 * se,
      ic_sup         = nps + 1.96 * se)
  }, by = by]
}

nps_total       <- calcular_nps(dades_nps)
nps_cadena      <- calcular_nps(dades_nps, by = "Establecimiento")[order(-nps)]
nps_cadena_edat <- calcular_nps(dades_nps, by = c("Establecimiento", "C1.Edad"))[
  order(Establecimiento, C1.Edad)]
nps_cadena_edat[, fiable := n >= n_minim]


# ---- 4. Distribució de puntuacions ------------------------------------------

dist_total <- dades_nps[, .N, by = .(puntuacio, categoria)][
  , pct := 100 * N / sum(N)][order(puntuacio)]

dist_cadena <- dades_nps[, .N, by = .(Establecimiento, puntuacio, categoria)][
  , pct := 100 * N / sum(N), by = Establecimiento][order(Establecimiento, puntuacio)]


# ---- 5. Gràfics -------------------------------------------------------------

# 5.1 Distribució global de puntuacions, acolorida per categoria
grafic_dist_total <- ggplot(dist_total,
                            aes(x = puntuacio, y = pct, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.3, size = 3.2) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = colors_nps) +
  labs(title = "Distribució de les puntuacions",
       subtitle = sprintf("NPS global: %.1f (IC 95%%: %.1f a %.1f) · n = %d",
                          nps_total$nps, nps_total$ic_inf,
                          nps_total$ic_sup, nps_total$n),
       x = "Puntuació", y = "Percentatge (%)", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

# 5.2 Distribució de puntuacions per cadena
grafic_dist_cadena <- ggplot(dist_cadena,
                             aes(x = puntuacio, y = pct, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.0f%%", pct)), vjust = -0.3, size = 2.8) +
  facet_wrap(~ Establecimiento, ncol = 1, labeller = as_labeller(etiqueta)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = colors_nps) +
  labs(title = "Distribució de les puntuacions per cadena",
       x = "Puntuació", y = "Percentatge (%)", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

# 5.3 Composició Detractors/Neutrals/Promotors i NPS per cadena
composicio_cadena <- melt(
  nps_cadena,
  id.vars       = c("Establecimiento", "nps"),
  measure.vars  = c("pct_detractors", "pct_neutrals", "pct_promotors"),
  variable.name = "categoria",
  value.name    = "pct"
)
composicio_cadena[, categoria := factor(
  categoria,
  levels = c("pct_detractors", "pct_neutrals", "pct_promotors"),
  labels = nivells_nps
)]

grafic_composicio <- ggplot(composicio_cadena,
                            aes(x = reorder(Establecimiento, nps), y = pct,
                                fill = categoria)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            position = position_stack(vjust = 0.5), size = 3.2) +
  geom_text(data = nps_cadena,
            aes(x = Establecimiento, y = 102,
                label = sprintf("NPS %+.1f", nps)),
            inherit.aes = FALSE, hjust = 0, fontface = "bold", size = 3.5) +
  coord_flip() +
  scale_x_discrete(labels = etiqueta) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, 25),
                     expand = c(0, 0)) +
  scale_fill_manual(values = colors_nps) +
  labs(title = "Promotors, neutrals i detractors per cadena",
       x = NULL, y = "%", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

# 5.4 NPS per cadena amb interval de confiança
grafic_nps_cadena <- ggplot(nps_cadena,
                            aes(x = nps, y = reorder(Establecimiento, nps))) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.2) +
  geom_point(size = 3.5, colour = palette_main["primary"]) +
  geom_text(aes(label = sprintf("%.1f", nps)), vjust = -1.2, size = 3.5) +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_discrete(labels = etiqueta) +
  labs(title = "NPS per cadena",
       subtitle = "Interval de confiança del 95%",
       x = "NPS", y = NULL) +
  theme_minimal()

# 5.5 NPS per grup d'edat i cadena
# Els subgrups amb n < n_minim es mostren buits: resultat poc fiable.
grafic_nps_edat <- ggplot(nps_cadena_edat,
                          aes(x = C1.Edad, y = nps)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.2,
                colour = "grey50") +
  geom_point(aes(shape = fiable), size = 3, colour = palette_main["primary"]) +
  facet_wrap(~ Establecimiento, labeller = as_labeller(etiqueta)) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                     labels = c(`TRUE`  = paste0("n ≥ ", n_minim),
                                `FALSE` = paste0("n < ", n_minim))) +
  labs(title = "NPS per grup d'edat i cadena",
       subtitle = "Interval de confiança del 95%",
       x = "Grup d'edat", y = "NPS", shape = NULL) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))


# ---- 6. Exportació ----------------------------------------------------------

ggsave(file.path(out_path, "nps_distribucio.png"),
       grafic_dist_total, width = 8, height = 5, dpi = 150)
ggsave(file.path(out_path, "nps_distribucio_cadena.png"),
       grafic_dist_cadena, width = 8, height = 9, dpi = 150)
ggsave(file.path(out_path, "nps_composicio_cadena.png"),
       grafic_composicio, width = 9, height = 4, dpi = 150)
ggsave(file.path(out_path, "nps_cadena.png"),
       grafic_nps_cadena, width = 8, height = 4, dpi = 150)
ggsave(file.path(out_path, "nps_edat_cadena.png"),
       grafic_nps_edat, width = 12, height = 5, dpi = 150)

fwrite(nps_cadena, file.path(out_path, "taula_nps_cadena.csv"),
       sep = ";", dec = ",")
fwrite(nps_cadena_edat, file.path(out_path, "taula_nps_cadena_edat.csv"),
       sep = ";", dec = ",")
