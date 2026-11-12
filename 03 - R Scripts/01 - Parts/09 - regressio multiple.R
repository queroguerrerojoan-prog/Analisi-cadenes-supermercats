# =============================================================================
#  09 - Regressió múltiple: motors de la satisfacció global
# -----------------------------------------------------------------------------
#  Explica la satisfacció global a partir dels 26 atributs:
#   1. Model complet i selecció de variables per AIC (stepwise)
#   2. Diagnòstic: multicol·linealitat, heteroscedasticitat, normalitat,
#      autocorrelació i errors estàndard robustos
#   3. Validació creuada (capacitat predictiva fora de mostra)
#   4. Efecte cadena: queda alguna diferència un cop controlats els atributs?
#   5. Importància relativa (LMG): quin percentatge de l'R² aporta cada atribut
#  Requereix: data_responses, vars_atributs, var_global, cadenes, etiqueta(),
#             alpha, out_path
# =============================================================================


# ---- 1. Dades ---------------------------------------------------------------

dades_reg <- na.omit(
  data_responses[Establecimiento %in% cadenes,
                 c("Establecimiento", var_global, vars_atributs), with = FALSE]
)
dades_reg[, Establecimiento := factor(Establecimiento, levels = cadenes)]

formula_completa <- reformulate(vars_atributs, response = var_global)

# Bloc temàtic de cada atribut (prefix), per acolorir els gràfics
bloc_atribut <- function(x) {
  fcase(startsWith(x, "Pc1"), "Comprensió",
        startsWith(x, "Pd1"), "Informació",
        startsWith(x, "Pe1"), "Experiència",
        startsWith(x, "Pf1"), "Productes",
        startsWith(x, "Pg1"), "Centres",
        startsWith(x, "Ph1"), "Valoració global")
}
colors_bloc <- c("Comprensió"       = "#1B9E77",
                 "Informació"       = "#D95F02",
                 "Experiència"      = "#7570B3",
                 "Productes"        = "#E7298A",
                 "Centres"          = "#66A61E",
                 "Valoració global" = "#E6AB02")


# ---- 2. Model complet i selecció per AIC -------------------------------------
# step() elimina (i reincorpora) variables mentre l'AIC millori. L'AIC
# equilibra ajust i complexitat: pot conservar variables amb p > 0,05 si
# milloren la predicció. Per això NO s'eliminen variables a mà per p-valor
# després del stepwise: serien dos criteris de selecció diferents barrejats.

model_complet <- lm(formula_completa, data = dades_reg)
model_aic     <- step(model_complet, direction = "both", trace = 0)

vars_model <- attr(terms(model_aic), "term.labels")
vars_eliminades <- setdiff(vars_atributs, vars_model)


# ---- 3. Diagnòstic ------------------------------------------------------------

# 3.1 Multicol·linealitat: VIF > 5 seria preocupant (> 10, greu)
vif_model <- car::vif(model_aic)

# 3.2 Heteroscedasticitat (Breusch-Pagan): p < 0,05 = la variància dels
#     errors no és constant -> els errors estàndard clàssics no són fiables
bp_model <- lmtest::bptest(model_aic)

# 3.3 Autocorrelació (Durbin-Watson): DW proper a 2 i p > 0,05 = sense
#     autocorrelació. En dades d'enquesta (no temporals) el resultat depèn
#     de l'ordre de les files, de manera que és un control secundari.
dw_model <- lmtest::dwtest(model_aic)
# Les files estan ordenades per cadena: si hi ha "autocorrelació", pot ser
# només l'efecte de l'ordre. Es repeteix el test amb les files en ordre aleatori.
set.seed(1234)
dw_aleatori <- lmtest::dwtest(model_aic, order.by = sample(nrow(dades_reg)))

# 3.4 Errors estàndard robustos (HC3): vàlids encara que hi hagi
#     heteroscedasticitat. Són els que s'usen per interpretar el model.
coef_robustos <- lmtest::coeftest(model_aic,
                                  vcov. = sandwich::vcovHC(model_aic, type = "HC3"))
taula_coef <- as.data.table(unclass(coef_robustos), keep.rownames = "variable")
setnames(taula_coef, c("variable", "beta", "se_robust", "t", "p"))
taula_coef[, `:=`(ic_inf = beta - 1.96 * se_robust,
                  ic_sup = beta + 1.96 * se_robust,
                  significatiu = p < alpha)]
taula_coef <- taula_coef[variable != "(Intercept)"]
taula_coef[, vif := vif_model[variable]]
taula_coef[, bloc := bloc_atribut(variable)]
setorder(taula_coef, -beta)

# 3.5 Gràfics de residus
dades_residus <- data.table(ajustat = fitted(model_aic),
                            residu  = rstandard(model_aic))

grafic_residus_ajust <- ggplot(dades_residus, aes(x = ajustat, y = residu)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              colour = palette_main[["primary"]]) +
  labs(title = "Residus vs valors ajustats",
       subtitle = "Les franges diagonals són degudes a l'escala discreta (1-10)",
       x = "Satisfacció predita", y = "Residu estandarditzat") +
  theme_minimal()

grafic_qq <- ggplot(dades_residus, aes(sample = residu)) +
  stat_qq(alpha = 0.3, size = 1) +
  stat_qq_line(colour = palette_main[["primary"]]) +
  labs(title = "Normalitat dels residus (Q-Q)",
       x = "Quantils teòrics", y = "Residus estandarditzats") +
  theme_minimal()

grafic_diagnostic <- (grafic_residus_ajust | grafic_qq) +
  plot_annotation(title = "Diagnòstic del model")


# ---- 4. Validació creuada (10 particions) -------------------------------------
# L'R² dins de la mostra sempre és optimista. La validació creuada estima
# com prediu el model clients que no ha vist.

set.seed(1234)
particions <- sample(rep(1:10, length.out = nrow(dades_reg)))
formula_aic <- formula(model_aic)

r2_cv <- sapply(1:10, \(f) {
  entrenament <- dades_reg[particions != f]
  prova       <- dades_reg[particions == f]
  pred <- predict(lm(formula_aic, data = entrenament), newdata = prova)
  obs  <- prova[[var_global]]
  1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
})


# ---- 5. Efecte cadena ---------------------------------------------------------
# Si un cop controlats els atributs la cadena encara explica satisfacció,
# hi ha factors que el qüestionari no recull (marca, preus, ubicació...).

model_cadena <- update(model_aic, . ~ . + Establecimiento)
anova_cadena <- anova(model_aic, model_cadena)

coef_cadena <- as.data.table(
  unclass(lmtest::coeftest(model_cadena,
                           vcov. = sandwich::vcovHC(model_cadena, type = "HC3"))),
  keep.rownames = "variable"
)[startsWith(variable, "Establecimiento")]
setnames(coef_cadena, c("variable", "beta", "se_robust", "t", "p"))
coef_cadena[, variable := etiqueta(sub("Establecimiento", "", variable))]

# Diferència bruta (sense controlar) vs neta (controlant els atributs),
# respecte a la cadena de referència (la primera de `cadenes`)
dif_bruta <- dades_reg[, .(mitjana = mean(get(var_global))), by = Establecimiento]
dif_bruta[, dif := mitjana - mitjana[Establecimiento == cadenes[1]]]


# ---- 6. Sensibilitat: model sense la relació qualitat-preu --------------------
# Ph1.CalidadPrecio és també una valoració global, molt propera a la
# satisfacció. Es comprova quant explica el model sense aquesta variable.

model_sense_qp <- lm(reformulate(setdiff(vars_model, "Ph1.CalidadPrecio"),
                                 response = var_global), data = dades_reg)


# ---- 7. Importància relativa (LMG) --------------------------------------------
# Amb predictors correlacionats, els coeficients no indiquen bé la
# importància: una variable pot tenir beta petita perquè una altra ja
# recull el seu efecte. El mètode LMG reparteix l'R² entre els predictors
# fent la mitjana de la seva aportació en tots els ordres d'entrada possibles.
# rela = TRUE: percentatges que sumen 100% de l'R².

relimp <- relaimpo::calc.relimp(model_aic, type = "lmg", rela = TRUE)

importancia <- data.table(variable    = names(relimp@lmg),
                          importancia = 100 * as.numeric(relimp@lmg))
importancia[, bloc := bloc_atribut(variable)]
setorder(importancia, -importancia)

importancia_bloc <- importancia[, .(importancia = sum(importancia)),
                                by = bloc][order(-importancia)]

grafic_importancia <- ggplot(importancia,
                             aes(x = importancia,
                                 y = reorder(variable, importancia),
                                 fill = bloc)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", importancia)),
            hjust = -0.15, size = 3) +
  scale_fill_manual(values = colors_bloc) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Importància relativa dels atributs en la satisfacció global",
       subtitle = sprintf("Mètode LMG · %% de l'R² explicat (R² = %.2f)",
                          summary(model_aic)$r.squared),
       x = "Importància relativa (%)", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 9))

# Coeficients amb IC robust: efecte de +1 punt en l'atribut, mantenint fixos
# els altres
grafic_coeficients <- ggplot(taula_coef,
                             aes(x = beta, y = reorder(variable, beta),
                                 colour = significatiu)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = ic_inf, xmax = ic_sup), size = 0.4) +
  scale_colour_manual(values = c(`TRUE` = palette_main[["primary"]],
                                 `FALSE` = "grey60"),
                      labels = c(`TRUE` = "p < 0,05", `FALSE` = "p ≥ 0,05")) +
  labs(title = "Coeficients del model (selecció per AIC)",
       subtitle = "Efecte de +1 punt en l'atribut sobre la satisfacció · IC 95% amb errors robustos (HC3)",
       x = "Coeficient", y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 9))


# ---- 8. Resum de resultats ----------------------------------------------------

s_complet <- summary(model_complet)
s_aic     <- summary(model_aic)

resum_regressio <- c(
  "=== Ajust ===",
  sprintf("Model complet:  %d variables, R² = %.3f, R² ajustat = %.3f, AIC = %.1f",
          length(vars_atributs), s_complet$r.squared, s_complet$adj.r.squared,
          AIC(model_complet)),
  sprintf("Model AIC:      %d variables, R² = %.3f, R² ajustat = %.3f, AIC = %.1f",
          length(vars_model), s_aic$r.squared, s_aic$adj.r.squared,
          AIC(model_aic)),
  sprintf("Variables eliminades: %s", paste(vars_eliminades, collapse = ", ")),
  sprintf("Validació creuada (10 particions): R² = %.3f (rang %.3f-%.3f)",
          mean(r2_cv), min(r2_cv), max(r2_cv)),
  sprintf("Sense Ph1.CalidadPrecio: R² = %.3f",
          summary(model_sense_qp)$r.squared),
  "",
  "=== Diagnòstic ===",
  sprintf("VIF màxim: %.2f (%s)", max(vif_model), names(which.max(vif_model))),
  sprintf("Breusch-Pagan: BP = %.1f, p = %.2e (%s)", bp_model$statistic,
          bp_model$p.value,
          ifelse(bp_model$p.value < alpha,
                 "heteroscedasticitat: s'usen errors robustos HC3",
                 "variància constant")),
  sprintf("Durbin-Watson: DW = %.3f, p = %.3f (%s)", dw_model$statistic,
          dw_model$p.value,
          ifelse(dw_model$p.value < alpha, "possible autocorrelació",
                 "sense autocorrelació")),
  sprintf("Durbin-Watson amb ordre aleatori: DW = %.3f, p = %.3f",
          dw_aleatori$statistic, dw_aleatori$p.value),
  "",
  "=== Coeficients (errors robustos HC3) ===",
  taula_coef[, sprintf("%-34s beta = %+.3f  p = %.4f  VIF = %.2f",
                       variable, beta, p, vif)],
  "",
  "=== Efecte cadena (controlant atributs) ===",
  sprintf("F = %.2f, p = %.2e", anova_cadena$F[2], anova_cadena$`Pr(>F)`[2]),
  coef_cadena[, sprintf("%-14s vs %s: beta = %+.3f, p = %.4f",
                        variable, etiqueta(cadenes[1]), beta, p)],
  dif_bruta[Establecimiento != cadenes[1],
            sprintf("%-14s diferència bruta = %+.3f", etiqueta(Establecimiento), dif)],
  "",
  "=== Importància relativa (LMG, % de l'R²) ===",
  importancia[, sprintf("%-34s %5.1f%%", variable, importancia)],
  "",
  "Per bloc:",
  importancia_bloc[, sprintf("  %-18s %5.1f%%", bloc, importancia)]
)
writeLines(resum_regressio)


# ---- 9. Exportació ----------------------------------------------------------

writeLines(resum_regressio, file.path(out_path, "resultats_regressio.txt"))
fwrite(taula_coef, file.path(out_path, "taula_coeficients.csv"),
       sep = ";", dec = ",")
fwrite(importancia, file.path(out_path, "taula_importancia_relativa.csv"),
       sep = ";", dec = ",")

ggsave(file.path(out_path, "regressio_importancia.png"),
       grafic_importancia, width = 9, height = 7, dpi = 150)
ggsave(file.path(out_path, "regressio_coeficients.png"),
       grafic_coeficients, width = 9, height = 7, dpi = 150)
ggsave(file.path(out_path, "regressio_diagnostic.png"),
       grafic_diagnostic, width = 11, height = 4.5, dpi = 150)
