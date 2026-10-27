# =============================================================================
#  07 - Anàlisi de components principals (ACP) amb rotació varimax
# -----------------------------------------------------------------------------
#  Redueix els 26 atributs a unes poques dimensions latents interpretables
#  (p. ex. "servei", "botiga", "rapidesa"...), compara les cadenes en
#  aquestes dimensions i mesura quina pesa més en la satisfacció global.
#   1. Adequació de les dades (KMO, Bartlett)
#   2. Nombre de components (Kaiser, colze, anàlisi paral·lela)
#   3. Matriu de correlacions
#   4. ACP amb rotació varimax i interpretació de les càrregues
#   5. Posició de les cadenes en cada dimensió
#   6. Pes de cada dimensió en la satisfacció global
#  Requereix: data_responses, vars_atributs, var_global, cadenes, out_path
# =============================================================================


# ---- 1. Dades ---------------------------------------------------------------
# S'exclou la satisfacció global (var_global): és el resultat que volem
# explicar, no un atribut. Així les dimensions es poden usar com a motors.

vars_acp <- vars_atributs

dades_acp <- na.omit(
  data_responses[Establecimiento %in% cadenes,
                 c("Establecimiento", var_global, vars_acp), with = FALSE]
)
matriu_acp <- dades_acp[, ..vars_acp]
cor_acp    <- cor(matriu_acp)


# ---- 2. Adequació per a l'anàlisi factorial ---------------------------------
# KMO: proporció de variància compartida entre variables.
#      > 0,8 molt bo · > 0,7 bo · > 0,6 acceptable · < 0,5 inadequat
# Bartlett: contrasta que la matriu de correlacions no és la identitat.

kmo_acp      <- psych::KMO(cor_acp)
bartlett_acp <- psych::cortest.bartlett(cor_acp, n = nrow(matriu_acp))

if (kmo_acp$MSA < 0.6) warning("KMO global < 0,6: dades poc adequades per a l'ACP")

# Variables amb KMO individual baix: candidates a excloure
vars_kmo_baix <- names(kmo_acp$MSAi)[kmo_acp$MSAi < 0.5]


# ---- 3. Nombre de components ------------------------------------------------

acp_inicial <- prcomp(matriu_acp, scale. = TRUE)

valors_propis <- data.table(
  component  = seq_along(acp_inicial$sdev),
  eigenvalue = acp_inicial$sdev^2
)
valors_propis[, var_pct  := 100 * eigenvalue / sum(eigenvalue)]
valors_propis[, var_acum := cumsum(var_pct)]

# Criteri de Kaiser: components amb valor propi > 1
n_kaiser <- valors_propis[eigenvalue > 1, .N]

# Anàlisi paral·lela: compara amb valors propis de dades aleatòries
# (criteri més rigorós que Kaiser, que tendeix a sobreestimar)
paralela <- psych::fa.parallel(matriu_acp, fa = "pc", plot = FALSE)
valors_propis[, eigen_aleatori := paralela$pc.sim]
n_paralela <- paralela$ncomp

# Decisió final: Kaiser i l'anàlisi paral·lela en suggereixen 3, però amb 4
# la "rapidesa i esperes" se separa de l'atenció i dona dimensions més
# interpretables i útils per a negoci (65% de variància explicada).
n_comp <- 4

grafic_sedimentacio <- ggplot(valors_propis, aes(x = component)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_line(aes(y = eigen_aleatori, linetype = "Dades aleatòries"),
            colour = "grey50") +
  geom_line(aes(y = eigenvalue, linetype = "Dades observades"),
            colour = palette_main["primary"]) +
  geom_point(aes(y = eigenvalue,
                 shape = component <= n_comp),
             colour = palette_main["primary"], size = 2.5) +
  geom_text(data = valors_propis[component <= n_comp],
            aes(y = eigenvalue, label = sprintf("%.0f%%", var_acum)),
            vjust = -1, size = 3) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  scale_linetype_manual(values = c("Dades observades" = "solid",
                                   "Dades aleatòries" = "dotted")) +
  labs(title = "Gràfic de sedimentació",
       subtitle = sprintf(paste0("Kaiser: %d · Anàlisi paral·lela: %d · ",
                                 "Escollits: %d (%.0f%% de variància acumulada)"),
                          n_kaiser, n_paralela, n_comp,
                          valors_propis[component == n_comp, var_acum]),
       x = "Component", y = "Valor propi", linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "top")


# ---- 4. Matriu de correlacions ----------------------------------------------
# Ordenada per clústers jeràrquics: els blocs anticipen els components.

png(file.path(out_path, "matriu_correlacions.png"),
    width = 1800, height = 1800, res = 150)
corrplot(
  cor_acp,
  method      = "color",
  order       = "hclust",
  addrect     = n_comp,
  rect.col    = "black",
  col         = colorRampPalette(c("#2166AC", "white", "#CA0020"))(200),
  tl.col      = "black",
  tl.cex      = 0.7,
  title       = "Matriu de correlacions entre atributs",
  mar         = c(0, 0, 2, 0)
)
dev.off()


# ---- 5. ACP amb rotació varimax ---------------------------------------------
# Sense rotació, el primer component sol ser un factor "general" on tot
# carrega i els altres són difícils d'interpretar. La rotació varimax
# fa que cada atribut carregui sobretot en un sol component.

acp_rot <- psych::principal(matriu_acp, nfactors = n_comp,
                            rotate = "varimax", scores = TRUE)
components <- colnames(acp_rot$loadings)

carregues <- as.data.table(unclass(acp_rot$loadings), keep.rownames = "atribut")
carregues[, comunalitat := acp_rot$communality[atribut]]

carregues_long <- melt(carregues[, !"comunalitat"],
                       id.vars = "atribut", variable.name = "component",
                       value.name = "carrega", variable.factor = FALSE)
carregues_long[, dominant := abs(carrega) == max(abs(carrega)), by = atribut]

# Noms automàtics: els 3 atributs que més carreguen en cada component
etiquetes_auto <- carregues_long[dominant == TRUE][
  order(-abs(carrega)),
  .(desc = paste(head(atribut, 3), collapse = ", ")),
  by = component]

noms_dimensions <- setNames(components, components)
noms_dimensions[etiquetes_auto$component] <- paste0(etiquetes_auto$component,
                                                    ": ", etiquetes_auto$desc)

# Noms interpretatius, assignats després de revisar la taula de càrregues.
# Si canvien les dades, cal tornar a revisar-los (l'ordre pot variar).
noms_dimensions[c("RC1", "RC2", "RC3", "RC4")] <- c(
  "Atenció i servei al client",   # Actitud, Atenció, Servei, Consultes
  "Ambient de la botiga",         # Il·luminació, Ordre i neteja, Climatització
  "Rapidesa i esperes",           # Espera a caixa, Espera d'atenció
  "Oferta i assessorament"        # Varietat, Competitivitat, Assessorament
)

ajustar_text <- function(x, amplada = 18) {
  sapply(x, \(s) paste(strwrap(s, amplada), collapse = "\n"))
}

# Mapa de càrregues: atributs agrupats pel seu component dominant
ordre_atributs <- carregues_long[dominant == TRUE][
  order(match(component, components), -abs(carrega)), atribut]
carregues_long[, atribut := factor(atribut, levels = rev(ordre_atributs))]
carregues_long[, component := factor(component, levels = components)]

grafic_carregues <- ggplot(carregues_long,
                           aes(x = component, y = atribut, fill = carrega)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", carrega),
                fontface = ifelse(dominant, "bold", "plain")),
            size = 2.8) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#CA0020",
                       midpoint = 0, limits = c(-1, 1)) +
  scale_x_discrete(labels = \(x) ajustar_text(noms_dimensions[x])) +
  labs(title = "Càrregues factorials (rotació varimax)",
       subtitle = "En negreta: component on cada atribut carrega més",
       x = NULL, y = NULL, fill = "Càrrega") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 8))


# ---- 6. Posició de les cadenes en cada dimensió -----------------------------
# Les puntuacions estan estandarditzades (mitjana 0, desviació 1):
# valors positius = la cadena està per sobre de la mitjana del mercat.

puntuacions <- as.data.table(acp_rot$scores)
puntuacions[, Establecimiento := dades_acp$Establecimiento]

puntuacions_long <- melt(puntuacions, id.vars = "Establecimiento",
                         variable.name = "component", value.name = "score",
                         variable.factor = FALSE)

mitjanes_dimensions <- puntuacions_long[
  , .(mitjana = mean(score), se = sd(score) / sqrt(.N)),
  by = .(Establecimiento, component)]
mitjanes_dimensions[, `:=`(ic_inf = mitjana - 1.96 * se,
                           ic_sup = mitjana + 1.96 * se)]
mitjanes_dimensions[, component := factor(component, levels = rev(components))]

grafic_dimensions_cadena <- ggplot(
  mitjanes_dimensions,
  aes(x = component, y = mitjana, ymin = ic_inf, ymax = ic_sup,
      colour = Establecimiento)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_x_discrete(labels = \(x) ajustar_text(noms_dimensions[x], 30)) +
  scale_colour_viridis_d(end = 0.85, labels = etiqueta) +
  labs(title = "Posició de cada cadena en les dimensions",
       subtitle = "Puntuació mitjana estandarditzada, IC 95%",
       x = NULL, y = "Puntuació (0 = mitjana del mercat)", colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

# Mapa perceptual: individus, el·lipses i centroides per cadena
grafic_mapa <- function(cx, cy) {
  centroides <- puntuacions[, lapply(.SD, mean), by = Establecimiento,
                            .SDcols = c(cx, cy)]
  ggplot(puntuacions, aes(x = .data[[cx]], y = .data[[cy]],
                          colour = Establecimiento)) +
    geom_hline(yintercept = 0, colour = "grey80") +
    geom_vline(xintercept = 0, colour = "grey80") +
    geom_point(alpha = 0.15, size = 1) +
    stat_ellipse(level = 0.68, linewidth = 0.8) +
    geom_point(data = centroides, size = 5, shape = 18) +
    scale_colour_viridis_d(end = 0.85, labels = etiqueta) +
    labs(x = noms_dimensions[cx], y = noms_dimensions[cy], colour = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

grafic_mapes <- (grafic_mapa(components[1], components[2]) |
                   grafic_mapa(components[2], components[3])) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Mapa perceptual de les cadenes",
                  subtitle = "Rombe = centroide · El·lipse = 68% dels clients") &
  theme(legend.position = "bottom")


# ---- 7. Pes de cada dimensió en la satisfacció global -----------------------
# Com que els components rotats són (gairebé) independents entre si, la
# regressió evita el problema de multicol·linealitat de la Gap Analysis.

dades_model <- cbind(puntuacions[, ..components],
                     satisfaccio = dades_acp[[var_global]])
model_dimensions <- lm(satisfaccio ~ ., data = dades_model)
r2_model <- summary(model_dimensions)$r.squared

pes_dimensions <- as.data.table(summary(model_dimensions)$coefficients,
                                keep.rownames = "component")[
  component != "(Intercept)"]
setnames(pes_dimensions, c("component", "beta", "se", "t", "p"))
pes_dimensions[, significatiu := p < alpha]

grafic_pes_dimensions <- ggplot(pes_dimensions,
                                aes(x = beta, y = reorder(component, beta),
                                    fill = significatiu)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", beta)), hjust = -0.2, size = 3.5) +
  scale_y_discrete(labels = \(x) ajustar_text(noms_dimensions[x], 30)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c(`TRUE` = palette_main[["primary"]],
                               `FALSE` = "grey70"),
                    labels = c(`TRUE` = "Significatiu",
                               `FALSE` = "No significatiu")) +
  labs(title = "Què pesa més en la satisfacció global?",
       subtitle = sprintf(paste0("Coeficients de regressió: punts de satisfacció ",
                                 "per +1 desviació en cada dimensió · R² = %.2f"),
                          r2_model),
       x = "Coeficient", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")


# ---- 8. Resum de resultats --------------------------------------------------

resum_acp <- c(
  "=== Adequació ===",
  sprintf("KMO global: %.3f", kmo_acp$MSA),
  sprintf("Bartlett: χ² = %.1f, gl = %d, p = %.2e",
          bartlett_acp$chisq, bartlett_acp$df, bartlett_acp$p.value),
  sprintf("Variables amb KMO < 0,5: %s",
          if (length(vars_kmo_baix)) paste(vars_kmo_baix, collapse = ", ") else "cap"),
  "",
  "=== Nombre de components ===",
  sprintf("Kaiser: %d · Anàlisi paral·lela: %d · Escollits: %d",
          n_kaiser, n_paralela, n_comp),
  sprintf("Variància acumulada: %.1f%%",
          valors_propis[component == n_comp, var_acum]),
  "",
  "=== Dimensions ===",
  paste0("  ", noms_dimensions),
  "",
  "=== Model de satisfacció ===",
  sprintf("R² = %.3f", r2_model),
  pes_dimensions[, sprintf("  %-6s beta = %+.3f, p = %.4f", component, beta, p)]
)
writeLines(resum_acp)


# ---- 9. Exportació ----------------------------------------------------------

writeLines(resum_acp, file.path(out_path, "resultats_acp.txt"))
fwrite(carregues, file.path(out_path, "taula_carregues.csv"), sep = ";", dec = ",")
fwrite(valors_propis, file.path(out_path, "taula_valors_propis.csv"),
       sep = ";", dec = ",")

ggsave(file.path(out_path, "acp_sedimentacio.png"),
       grafic_sedimentacio, width = 8, height = 5, dpi = 150)
ggsave(file.path(out_path, "acp_carregues.png"),
       grafic_carregues, width = 9, height = 9, dpi = 150)
ggsave(file.path(out_path, "acp_dimensions_cadena.png"),
       grafic_dimensions_cadena, width = 9, height = 5, dpi = 150)
ggsave(file.path(out_path, "acp_mapa_perceptual.png"),
       grafic_mapes, width = 13, height = 6, dpi = 150)
ggsave(file.path(out_path, "acp_pes_dimensions.png"),
       grafic_pes_dimensions, width = 9, height = 4, dpi = 150)
