# =============================================================================
#  02 - Anàlisi descriptiva
# -----------------------------------------------------------------------------
#  Distribució dels ingressos mensuals i relació entre ingressos i edat.
#  Requereix: data_responses, out_path (de 00_main.R i 01)
# =============================================================================


# ---- 1. Distribució dels ingressos ------------------------------------------

taula_ingressos <- data_responses[, .(n = .N), by = C5.IngMes][
  , pct := 100 * n / sum(n)][
  order(C5.IngMes)]

grafic_ingressos <- ggplot(taula_ingressos, aes(x = C5.IngMes, y = pct)) +
  geom_col(fill = palette_main["primary"]) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.4, size = 3) +
  labs(
    title = "Distribució dels ingressos mensuals",
    subtitle = paste0("n = ", nrow(data_responses)),
    x = "Ingressos mensuals (€)",
    y = "Percentatge (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---- 2. Taula creuada: ingressos per grup d'edat ----------------------------
# Percentatges per columna: dins de cada grup d'edat, com es reparteixen
# els ingressos (cada grup d'edat suma 100%).

taula_ingressos_edats <- data_responses[, .(n = .N), by = .(C1.Edad, C5.IngMes)][
  , pct := 100 * n / sum(n), by = C1.Edad][
  order(C1.Edad, C5.IngMes)]

# Versió en format ample, per consultar o exportar la taula
taula_ingressos_edats_wide <- dcast(
  taula_ingressos_edats,
  C5.IngMes ~ C1.Edad,
  value.var = "pct",
  fill = 0            # combinacions sense observacions = 0%, no NA
)


# ---- 3. Gràfics de la taula creuada -----------------------------------------

# Barres apilades al 100%: la comparació més llegible entre grups d'edat
grafic_edats_ingressos <- ggplot(taula_ingressos_edats,
                                 aes(x = C1.Edad, y = pct, fill = C5.IngMes)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(direction = -1) +
  labs(
    title = "Ingressos mensuals per grup d'edat",
    x = "Grup d'edat",
    y = "Percentatge dins del grup d'edat",
    fill = "Ingressos mensuals (€)"
  ) +
  theme_minimal()

# Una faceta per grup d'edat: útil per veure la forma de cada distribució
grafic_edats_ingressos_facet <- ggplot(taula_ingressos_edats,
                                       aes(x = C5.IngMes, y = pct, fill = C5.IngMes)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ C1.Edad) +
  scale_fill_viridis_d(direction = -1) +
  labs(
    title = "Distribució dels ingressos dins de cada grup d'edat",
    x = "Ingressos mensuals (€)",
    y = "Percentatge (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---- 4. Exportació ----------------------------------------------------------
# Guardar els gràfics permet mostrar-los al README del repositori.

ggsave(file.path(out_path, "ingressos_distribucio.png"),
       grafic_ingressos, width = 8, height = 5, dpi = 150)
ggsave(file.path(out_path, "ingressos_per_edat.png"),
       grafic_edats_ingressos, width = 8, height = 5, dpi = 150)
ggsave(file.path(out_path, "ingressos_per_edat_facet.png"),
       grafic_edats_ingressos_facet, width = 10, height = 6, dpi = 150)

fwrite(taula_ingressos_edats_wide,
       file.path(out_path, "taula_ingressos_edats.csv"),
       sep = ";", dec = ",")
