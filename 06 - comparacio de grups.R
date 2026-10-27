# =============================================================================
#  06 - Comparació estadística entre cadenes
# -----------------------------------------------------------------------------
#  Contrasta si les diferències observades entre cadenes són estadísticament
#  significatives:
#   1. ANOVA + Tukey sobre la satisfacció global, amb comprovació de
#      supòsits i alternatives robustes (Welch, Kruskal-Wallis).
#   2. Els mateixos contrastos per a tots els atributs, amb correcció per
#      comparacions múltiples i mida de l'efecte.
#   3. Khi quadrat sobre un atribut d'exemple (Pc1.Actitud).
#  Requereix: data_responses, vars_t2box, cadenes, classificar(),
#             colors_valoracio, out_path
# =============================================================================


# ---- 1. Paràmetres i dades --------------------------------------------------

alpha       <- 0.05
var_exemple <- "Pc1.Actitud"

dades_test <- data_responses[Establecimiento %in% cadenes]
dades_test[, Establecimiento := factor(Establecimiento, levels = cadenes)]

# "casa_ametller" -> "Casa Ametller"
etiqueta <- function(x) tools::toTitleCase(gsub("_", " ", x))

# Mida de l'efecte (eta quadrat) segons els llindars de Cohen
interpretar_eta2 <- function(e) {
  fcase(e < 0.01, "Negligible",
        e < 0.06, "Petit",
        e < 0.14, "Mitjà",
        default = "Gran")
}


# ---- 2. ANOVA sobre la satisfacció global -----------------------------------

anova_sat  <- aov(Ph1.Satisfaccion ~ Establecimiento, data = dades_test)
taula_aov  <- summary(anova_sat)[[1]]
eta2_sat   <- taula_aov[["Sum Sq"]][1] / sum(taula_aov[["Sum Sq"]])

# Supòsits: l'escala 1-10 és ordinal i acostuma a no ser normal, i les
# variàncies poden diferir entre cadenes. Per això es contrasten
# alternatives que no depenen d'aquests supòsits.
fligner_sat <- fligner.test(Ph1.Satisfaccion ~ Establecimiento, data = dades_test)  # homogeneïtat de variàncies
welch_sat   <- oneway.test(Ph1.Satisfaccion ~ Establecimiento, data = dades_test,
                           var.equal = FALSE)                                       # ANOVA de Welch
kruskal_sat <- kruskal.test(Ph1.Satisfaccion ~ Establecimiento, data = dades_test)  # no paramètric


# ---- 3. Tukey: quines parelles de cadenes es diferencien --------------------

tukey_sat <- as.data.table(TukeyHSD(anova_sat)$Establecimiento,
                           keep.rownames = "comparacio")
setnames(tukey_sat, "p adj", "p_adj")

# Etiquetes generades a partir dels noms reals (no escrites a mà)
tukey_sat[, comparacio := sapply(strsplit(comparacio, "-"),
                                 \(p) paste(etiqueta(p), collapse = " vs "))]
tukey_sat[, significatiu := p_adj < alpha]

# Una diferència és significativa si el seu interval NO inclou el 0
grafic_tukey <- ggplot(tukey_sat,
                       aes(x = diff, y = comparacio, xmin = lwr, xmax = upr,
                           colour = significatiu)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(size = 0.6) +
  geom_text(aes(label = sprintf("p = %.3f", p_adj)), vjust = -1.2,
            size = 3.2, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "#CA0020", `FALSE` = "grey50"),
                      labels = c(`TRUE` = "Significativa",
                                 `FALSE` = "No significativa")) +
  labs(title = "Diferència de mitjanes en la satisfacció global",
       subtitle = "Test de Tukey, interval de confiança del 95%",
       x = "Diferència de mitjanes (punts sobre 10)", y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top")


# ---- 4. Contrastos per a tots els atributs ----------------------------------
# Amb 27 contrastos, alguns sortirien significatius per atzar: es corregeixen
# els p-valors (Benjamini-Hochberg). La mida de l'efecte indica si la
# diferència, a més de significativa, és rellevant.

test_atribut <- function(var) {
  f <- reformulate("Establecimiento", response = var)
  d <- dades_test[!is.na(get(var))]
  a <- summary(aov(f, data = d))[[1]]
  list(
    atribut   = var,
    eta2      = a[["Sum Sq"]][1] / sum(a[["Sum Sq"]]),
    p_welch   = oneway.test(f, data = d, var.equal = FALSE)$p.value,
    p_kruskal = kruskal.test(f, data = d)$p.value
  )
}

tests_atributs <- rbindlist(lapply(vars_t2box, test_atribut))
tests_atributs[, `:=`(
  p_welch_adj   = p.adjust(p_welch,   method = "BH"),
  p_kruskal_adj = p.adjust(p_kruskal, method = "BH"),
  efecte        = interpretar_eta2(eta2)
)]
# Significatiu només si ho és amb els dos mètodes (resultat robust)
tests_atributs[, significatiu := p_welch_adj < alpha & p_kruskal_adj < alpha]
setorder(tests_atributs, -eta2)

grafic_tests_atributs <- ggplot(tests_atributs,
                                aes(x = eta2, y = reorder(atribut, eta2),
                                    fill = significatiu)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = c(0.01, 0.06), linetype = "dotted",
             colour = "grey50") +
  scale_fill_manual(values = c(`TRUE` = "#CA0020", `FALSE` = "grey70"),
                    labels = c(`TRUE` = "Diferència significativa",
                               `FALSE` = "No significativa")) +
  labs(title = "On es diferencien més les cadenes?",
       subtitle = paste0("Mida de l'efecte (η²) per atribut. Línies: efecte ",
                         "petit (0,01) i mitjà (0,06). p-valors corregits (BH)"),
       x = "η² (proporció de variància explicada per la cadena)",
       y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 9))


# ---- 5. Khi quadrat: atribut d'exemple --------------------------------------
# Amb les 10 puntuacions per separat moltes cel·les tindrien freqüències
# esperades < 5 i el test no seria vàlid. S'agrupen en les 4 categories
# del T2B (classificar(), definida a la part 03).

valoracio_exemple <- classificar(dades_test[[var_exemple]])
taula_contingencia <- table(Cadena = dades_test$Establecimiento,
                            Valoracio = valoracio_exemple)
taula_contingencia <- taula_contingencia[, colSums(taula_contingencia) > 0,
                                         drop = FALSE]

chi_exemple <- chisq.test(taula_contingencia)
if (any(chi_exemple$expected < 5)) {
  warning("Khi quadrat: hi ha cel·les amb freqüència esperada < 5")
}

# V de Cramér: intensitat de l'associació (0 = cap, 1 = total)
cramer_v <- sqrt(as.numeric(chi_exemple$statistic) /
                   (sum(taula_contingencia) * (min(dim(taula_contingencia)) - 1)))

# Residus estandarditzats: quines cel·les expliquen la diferència
# (|residu| > 2 = més o menys respostes de les esperades)
residus <- as.data.table(as.table(chi_exemple$stdres))
setnames(residus, c("Cadena", "Valoracio", "residu"))
residus[, Valoracio := factor(Valoracio, levels = nivells_valoracio)]

grafic_residus <- ggplot(residus,
                         aes(x = Valoracio, y = Cadena, fill = residu)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%+.1f", residu)), size = 3.5) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#CA0020",
                       midpoint = 0) +
  scale_y_discrete(labels = etiqueta) +
  labs(title = paste("Khi quadrat –", var_exemple),
       subtitle = sprintf("χ² = %.1f, p = %.4f, V de Cramér = %.2f. |residu| > 2: diferència rellevant",
                          chi_exemple$statistic, chi_exemple$p.value, cramer_v),
       x = NULL, y = NULL, fill = "Residu\nestandarditzat") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Distribució de les puntuacions de l'atribut per cadena
dist_exemple <- dades_test[!is.na(get(var_exemple)),
                           .N, by = .(Establecimiento, puntuacio = get(var_exemple))][
  , pct := N / sum(N), by = Establecimiento]

grafic_dist_exemple <- ggplot(dist_exemple,
                              aes(x = puntuacio, y = pct,
                                  fill = Establecimiento)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85, labels = etiqueta) +
  labs(title = paste("Distribució de les puntuacions –", var_exemple),
       x = "Puntuació", y = "% dins de cada cadena", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")


# ---- 6. Boxplot -------------------------------------------------------------
# Els punts (amb soroll horitzontal) mostren on es concentren les respostes,
# cosa que el boxplot sol amaga en una escala discreta d'1 a 10.

grafic_boxplot <- ggplot(dades_test,
                         aes(x = Establecimiento, y = .data[[var_exemple]])) +
  geom_jitter(width = 0.2, height = 0.15, alpha = 0.15,
              colour = "grey40", size = 1) +
  geom_boxplot(fill = NA, outlier.shape = NA, width = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
               colour = palette_main["primary"]) +
  scale_x_discrete(labels = etiqueta) +
  scale_y_continuous(breaks = 1:10) +
  labs(title = paste("Valoració", var_exemple, "per cadena"),
       subtitle = "Rombe = mitjana",
       x = NULL, y = "Valoració (1-10)") +
  theme_minimal()


# ---- 7. Resum de resultats --------------------------------------------------

resum_tests <- c(
  "=== Satisfacció global per cadena ===",
  sprintf("ANOVA:          F = %.2f, p = %.4f, η² = %.3f (%s)",
          taula_aov[["F value"]][1], taula_aov[["Pr(>F)"]][1],
          eta2_sat, interpretar_eta2(eta2_sat)),
  sprintf("Fligner:        p = %.4f (%s)", fligner_sat$p.value,
          ifelse(fligner_sat$p.value < alpha,
                 "variàncies diferents: prioritzar Welch",
                 "variàncies homogènies")),
  sprintf("Welch:          F = %.2f, p = %.4f",
          welch_sat$statistic, welch_sat$p.value),
  sprintf("Kruskal-Wallis: H = %.2f, p = %.4f",
          kruskal_sat$statistic, kruskal_sat$p.value),
  "",
  "=== Atributs amb diferències significatives entre cadenes (BH) ===",
  if (any(tests_atributs$significatiu)) {
    tests_atributs[significatiu == TRUE,
                   sprintf("%-20s η² = %.3f (%s)", atribut, eta2, efecte)]
  } else "Cap",
  "",
  sprintf("=== Khi quadrat %s ===", var_exemple),
  sprintf("χ² = %.2f, gl = %d, p = %.4f, V de Cramér = %.2f",
          chi_exemple$statistic, chi_exemple$parameter,
          chi_exemple$p.value, cramer_v)
)
writeLines(resum_tests)


# ---- 8. Exportació ----------------------------------------------------------

writeLines(resum_tests, file.path(out_path, "resultats_tests.txt"))
fwrite(tukey_sat, file.path(out_path, "taula_tukey.csv"), sep = ";", dec = ",")
fwrite(tests_atributs, file.path(out_path, "taula_tests_atributs.csv"),
       sep = ";", dec = ",")

ggsave(file.path(out_path, "tukey_satisfaccio.png"),
       grafic_tukey, width = 8, height = 4, dpi = 150)
ggsave(file.path(out_path, "tests_atributs.png"),
       grafic_tests_atributs, width = 9, height = 9, dpi = 150)
ggsave(file.path(out_path, "chi_residus.png"),
       grafic_residus, width = 8, height = 4, dpi = 150)
ggsave(file.path(out_path, "distribucio_exemple.png"),
       grafic_dist_exemple, width = 9, height = 5, dpi = 150)
ggsave(file.path(out_path, "boxplot_exemple.png"),
       grafic_boxplot, width = 7, height = 5, dpi = 150)
