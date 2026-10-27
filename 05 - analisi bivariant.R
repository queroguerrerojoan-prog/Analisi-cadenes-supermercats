# =============================================================================
#  05 - Anàlisi bivariant: mitjanes i Gap Analysis (Importància-Satisfacció)
# -----------------------------------------------------------------------------
#  1. Compara la valoració mitjana de cada atribut entre cadenes i respecte
#     a la mitjana del mercat, per detectar punts forts i febles.
#  2. Gap Analysis: creua la importància de cada atribut (correlació amb la
#     satisfacció global) amb la satisfacció mitjana i el classifica en
#     quatre quadrants d'acció.
#  Requereix: data_responses, vars_t2box, cadenes, out_path
# =============================================================================


# ---- 1. Paràmetres ----------------------------------------------------------

var_global    <- "Ph1.Satisfaccion"                 # variable de resultat
vars_atributs <- setdiff(vars_t2box, var_global)    # possibles motors

colors_quadrant <- c(
  "Prioritat de millora"    = "#CA0020",
  "Fortalesa a mantenir"    = "#1B7837",
  "Possible excés d'esforç" = "#F4A582",
  "Baixa prioritat"         = "#BDBDBD"
)

dades_mercat <- data_responses[Establecimiento %in% cadenes]


# ---- 2. Mitjanes per cadena i diferència respecte al mercat -----------------

mitjanes_cadena <- melt(
  dades_mercat[, lapply(.SD, mean, na.rm = TRUE),
               by = Establecimiento, .SDcols = vars_t2box],
  id.vars         = "Establecimiento",
  variable.name   = "atribut",
  value.name      = "mitjana",
  variable.factor = FALSE
)

mitjanes_total <- melt(
  dades_mercat[, lapply(.SD, mean, na.rm = TRUE), .SDcols = vars_t2box],
  measure.vars    = vars_t2box,
  variable.name   = "atribut",
  value.name      = "mitjana_mercat",
  variable.factor = FALSE
)

# Diferència de cada cadena respecte a la mitjana del mercat:
# valors negatius = punts febles relatius de la cadena
mitjanes_cadena[mitjanes_total, dif_mercat := mitjana - i.mitjana_mercat,
                on = "atribut"]


# ---- 3. Gràfics de mitjanes -------------------------------------------------

# Mitjana per atribut: les tres cadenes a la mateixa línia
grafic_mitjanes_cadena <- ggplot(
  mitjanes_cadena,
  aes(x = mitjana, y = reorder(atribut, mitjana), colour = Establecimiento)
) +
  geom_line(aes(group = atribut), colour = "grey80", linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_viridis_d(end = 0.85) +
  labs(title = "Valoració mitjana per atribut i cadena",
       x = "Mitjana (escala 1-10)", y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 9))

# Mapa de calor: on cada cadena està per sobre o per sota del mercat
grafic_dif_mercat <- ggplot(
  mitjanes_cadena,
  aes(x = Establecimiento, y = reorder(atribut, mitjana), fill = dif_mercat)
) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%+.2f", dif_mercat)), size = 3) +
  scale_fill_gradient2(low = "#CA0020", mid = "white", high = "#1B7837",
                       midpoint = 0) +
  labs(title = "Diferència respecte a la mitjana del mercat",
       subtitle = "Punts sobre 10. Vermell = per sota del mercat",
       x = NULL, y = NULL, fill = "Diferència") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9), panel.grid = element_blank())


# ---- 4. Gap Analysis --------------------------------------------------------
# Importància derivada = correlació de Pearson entre l'atribut i la
# satisfacció global, calculada dins del mateix conjunt de dades.
# Els talls dels quadrants són les mitjanes d'importància i satisfacció.

calcular_gap <- function(dt) {
  taula <- data.table(
    atribut     = vars_atributs,
    importancia = sapply(vars_atributs, \(x)
                         cor(dt[[x]], dt[[var_global]],
                             use = "pairwise.complete.obs")),
    satisfaccio = sapply(vars_atributs, \(x) mean(dt[[x]], na.rm = TRUE))
  )
  imp_tall <- mean(taula$importancia, na.rm = TRUE)
  sat_tall <- mean(taula$satisfaccio, na.rm = TRUE)

  taula[, quadrant := factor(fcase(
    importancia >= imp_tall & satisfaccio <  sat_tall, "Prioritat de millora",
    importancia >= imp_tall & satisfaccio >= sat_tall, "Fortalesa a mantenir",
    importancia <  imp_tall & satisfaccio >= sat_tall, "Possible excés d'esforç",
    default = "Baixa prioritat"
  ), levels = names(colors_quadrant))]
  taula[]
}

gap_total  <- calcular_gap(dades_mercat)
gap_cadena <- rbindlist(lapply(cadenes, \(cad)
  calcular_gap(dades_mercat[Establecimiento == cad])[, Establecimiento := cad]
))

# Resum accionable: atributs a millorar per cadena, de més a menys importants
prioritats_millora <- gap_cadena[quadrant == "Prioritat de millora",
                                 .(Establecimiento, atribut,
                                   importancia = round(importancia, 2),
                                   satisfaccio = round(satisfaccio, 2))][
  order(Establecimiento, -importancia)]


# ---- 5. Gràfics de Gap Analysis ---------------------------------------------

grafic_gap <- function(taula, titol) {
  ggplot(taula, aes(x = importancia, y = satisfaccio,
                    colour = quadrant, label = atribut)) +
    geom_vline(xintercept = mean(taula$importancia, na.rm = TRUE),
               linetype = "dashed", colour = "grey60") +
    geom_hline(yintercept = mean(taula$satisfaccio, na.rm = TRUE),
               linetype = "dashed", colour = "grey60") +
    geom_point(size = 3) +
    ggrepel::geom_text_repel(size = 3, show.legend = FALSE,
                             max.overlaps = Inf, seed = 1) +
    scale_colour_manual(values = colors_quadrant, drop = FALSE) +
    labs(title = titol,
         subtitle = "Importància derivada: correlació amb la satisfacció global",
         x = "Importància (correlació r)",
         y = "Satisfacció mitjana (1-10)",
         colour = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
}

grafic_gap_total <- grafic_gap(gap_total, "Gap Analysis – Total mercat")


# ---- 6. Exportació ----------------------------------------------------------

ggsave(file.path(out_path, "mitjanes_cadena.png"),
       grafic_mitjanes_cadena, width = 9, height = 9, dpi = 150)
ggsave(file.path(out_path, "mitjanes_dif_mercat.png"),
       grafic_dif_mercat, width = 8, height = 9, dpi = 150)
ggsave(file.path(out_path, "gap_total.png"),
       grafic_gap_total, width = 10, height = 8, dpi = 150)

for (cad in cadenes) {
  ggsave(file.path(out_path, paste0("gap_", cad, ".png")),
         grafic_gap(gap_cadena[Establecimiento == cad],
                    paste("Gap Analysis –", cad)),
         width = 10, height = 8, dpi = 150)
}

fwrite(gap_cadena, file.path(out_path, "taula_gap_cadena.csv"),
       sep = ";", dec = ",")
fwrite(prioritats_millora, file.path(out_path, "prioritats_millora.csv"),
       sep = ";", dec = ",")
