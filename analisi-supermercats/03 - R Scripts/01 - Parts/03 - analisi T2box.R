# =============================================================================
#  03 - Anàlisi Top-2-Box (T2B)
# -----------------------------------------------------------------------------
#  Classifica les valoracions (escala 1-10) en quatre categories i calcula,
#  per a cada cadena i atribut, el percentatge de clients satisfets (T2B).
#  Compara les cadenes entre si.
#  Requereix: data_responses, vars_t2box, out_path (de 00_main.R i 01)
# =============================================================================


# ---- 1. Paràmetres ----------------------------------------------------------

# (cadenes es defineix a 00_main.R)

# Ordre de les categories (de millor a pitjor), usat a taules i gràfics
nivells_valoracio <- c("Excel·lent", "Notable", "Suficient", "Insuficient")

# Categories que compten com a T2B (Excel·lent 9-10 + Notable 7-8)
categories_t2b <- c("Excel·lent", "Notable")

colors_valoracio <- c(
  "Excel·lent"  = "#1B7837",
  "Notable"     = "#7FBF7B",
  "Suficient"   = "#F4A582",
  "Insuficient" = "#CA0020"
)


# ---- 2. Funcions ------------------------------------------------------------

# Converteix una puntuació 1-10 en categoria.
# as.numeric(as.character()) evita comparar text ("10" >= "9" és FALSE).
classificar <- function(x) {
  x <- as.numeric(as.character(x))
  cat <- cut(x,
             breaks = c(-Inf, 5, 7, 9, Inf),
             right  = FALSE,                  # [9, Inf) = Excel·lent, etc.
             labels = rev(nivells_valoracio))
  factor(cat, levels = nivells_valoracio)
}


# ---- 3. Satisfacció mitjana per cadena --------------------------------------

satisfaccio_mitjana <- data_responses[
  Establecimiento %in% cadenes,
  .(n = .N,
    satisfaccio_mitjana = mean(as.numeric(as.character(Ph1.Satisfaccion)),
                               na.rm = TRUE)),
  by = Establecimiento
][order(-satisfaccio_mitjana)]


# ---- 4. Classificació de les valoracions (format llarg) ---------------------
# Totes les cadenes alhora: una fila per resposta i atribut.

dades_t2b <- melt(
  data_responses[Establecimiento %in% cadenes],
  id.vars       = c("#", "Establecimiento", "C1.Edad", "C4.CompraMedia"),
  measure.vars  = vars_t2box,
  variable.name = "atribut",
  value.name    = "puntuacio"
)
dades_t2b[, atribut := as.character(atribut)]
dades_t2b[, valoracio := classificar(puntuacio)]


# ---- 5. Percentatges per cadena, atribut i categoria ------------------------

taula_valoracio <- dades_t2b[!is.na(valoracio),
                             .N, by = .(Establecimiento, atribut, valoracio)]

# Completar combinacions sense respostes (p. ex. cap "Insuficient") amb 0
taula_valoracio <- taula_valoracio[
  CJ(Establecimiento = cadenes,
     atribut         = vars_t2box,
     valoracio       = factor(nivells_valoracio, levels = nivells_valoracio)),
  on = .(Establecimiento, atribut, valoracio)
]
taula_valoracio[is.na(N), N := 0L]
taula_valoracio[, pct := 100 * N / sum(N), by = .(Establecimiento, atribut)]

# T2B per cadena i atribut
taula_t2b <- taula_valoracio[
  , .(t2b = sum(pct[valoracio %in% categories_t2b])),
  by = .(Establecimiento, atribut)
][order(Establecimiento, -t2b)]

taula_valoracio[taula_t2b, t2b := i.t2b, on = .(Establecimiento, atribut)]

# Taula ampla per consultar/exportar: una fila per atribut, una columna per cadena
taula_t2b_wide <- dcast(taula_t2b, atribut ~ Establecimiento, value.var = "t2b")


# ---- 6. Gràfics per cadena --------------------------------------------------

# Distribució completa de valoracions, ordenada per T2B
grafic_valoracio <- function(cadena) {
  ggplot(taula_valoracio[Establecimiento == cadena],
         aes(x = reorder(atribut, t2b), y = pct, fill = valoracio)) +
    geom_col(width = 0.8) +
    coord_flip() +
    scale_fill_manual(values = colors_valoracio) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = paste("Valoració dels atributs –", etiqueta(cadena)),
         subtitle = "Ordenats per T2B (% de valoracions de 7 a 10)",
         x = NULL, y = "%", fill = NULL) +
    theme_minimal() +
    theme(legend.position = "top", axis.text.y = element_text(size = 9))
}

# Els n atributs millor i pitjor valorats segons T2B
grafic_top_bottom <- function(cadena, n = 5) {
  t <- taula_t2b[Establecimiento == cadena][order(-t2b)]
  t <- rbind(
    copy(head(t, n))[, grup := "Millor valorats"],
    copy(tail(t, n))[, grup := "Pitjor valorats"]
  )

  ggplot(t, aes(x = reorder(atribut, t2b), y = t2b, fill = grup)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.1f", t2b)), hjust = -0.15, size = 3) +
    coord_flip() +
    facet_wrap(~ grup, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c("Millor valorats" = "#1B7837",
                                 "Pitjor valorats" = "#CA0020")) +
    scale_y_continuous(limits = c(0, 100),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = paste("Atributs amb més i menys T2B –", etiqueta(cadena)),
         x = NULL, y = "T2B (%)") +
    theme_minimal()
}


# ---- 7. Gràfics comparatius entre cadenes -----------------------------------

# Distribució completa, una faceta per cadena
grafic_valoracio_comparatiu <- ggplot(
  taula_valoracio,
  aes(x = reorder(atribut, t2b), y = pct, fill = valoracio)
) +
  geom_col(width = 0.8) +
  coord_flip() +
  facet_wrap(~ Establecimiento, labeller = as_labeller(etiqueta)) +
  scale_fill_manual(values = colors_valoracio) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Valoració dels atributs per cadena",
       x = NULL, y = "%", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 8))

# T2B de les tres cadenes a la mateixa línia: les diferències es veuen d'un cop d'ull
grafic_t2b_comparatiu <- ggplot(
  taula_t2b,
  aes(x = t2b, y = reorder(atribut, t2b), colour = Establecimiento)
) +
  geom_line(aes(group = atribut), colour = "grey80", linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_viridis_d(end = 0.85, labels = etiqueta) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(title = "T2B per atribut i cadena",
       subtitle = "% de valoracions de 7 a 10",
       x = "T2B (%)", y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 9))


# ---- 8. Exportació ----------------------------------------------------------

for (cad in cadenes) {
  ggsave(file.path(out_path, paste0("t2b_valoracio_", cad, ".png")),
         grafic_valoracio(cad), width = 9, height = 8, dpi = 150)
  ggsave(file.path(out_path, paste0("t2b_top_bottom_", cad, ".png")),
         grafic_top_bottom(cad), width = 8, height = 6, dpi = 150)
}

ggsave(file.path(out_path, "t2b_valoracio_comparatiu.png"),
       grafic_valoracio_comparatiu, width = 14, height = 8, dpi = 150)
ggsave(file.path(out_path, "t2b_comparatiu.png"),
       grafic_t2b_comparatiu, width = 9, height = 9, dpi = 150)

fwrite(taula_t2b_wide, file.path(out_path, "taula_t2b.csv"), sep = ";", dec = ",")
fwrite(satisfaccio_mitjana, file.path(out_path, "satisfaccio_mitjana.csv"),
       sep = ";", dec = ",")
