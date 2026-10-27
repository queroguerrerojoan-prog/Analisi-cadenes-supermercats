# =============================================================================
#  08 - Segmentació de clients (anàlisi clúster)
# -----------------------------------------------------------------------------
#  Agrupa els clients en segments homogenis segons com valoren els atributs:
#   1. Clúster jeràrquic (Ward) com a exploració
#   2. Elecció del nombre de segments (colze, silueta)
#   3. K-means: segments definitius, amb nom assignat segons la satisfacció
#   4. Validació: concordança amb el jeràrquic, satisfacció i NPS per segment
#   5. Composició dels segments per cadena i perfil sociodemogràfic
#  Requereix: data_responses, vars_atributs, var_global, cadenes, etiqueta(),
#             alpha, out_path
# =============================================================================


# ---- 1. Dades ---------------------------------------------------------------
# Variables de segmentació: els 26 atributs, sense la satisfacció global.
# Així la satisfacció serveix per validar els segments: si surt diferent
# entre segments, la segmentació té sentit de negoci.
# No s'estandarditza: totes les variables estan en la mateixa escala (1-10),
# i estandarditzar donaria el mateix pes a atributs amb poca variabilitat.

vars_cluster <- vars_atributs

dades_cluster <- na.omit(
  data_responses[Establecimiento %in% cadenes,
                 c("#", "Establecimiento", "C1.Edad", "C2.Sexo", "C5.IngMes",
                   var_global, vars_cluster), with = FALSE]
)
matriu_cluster <- as.matrix(dades_cluster[, ..vars_cluster])

k_max <- 8


# ---- 2. Clúster jeràrquic (exploratori) -------------------------------------

dist_cluster <- dist(matriu_cluster, method = "euclidean")
hc_cluster   <- hclust(dist_cluster, method = "ward.D2")


# ---- 3. Nombre de segments --------------------------------------------------
# - Colze: variància intra-grup (WSS); se cerca on deixa de baixar molt.
# - Silueta: com de ben assignat està cada client al seu grup (de -1 a 1).
# Es combinen amb la interpretabilitat: pocs segments i ben diferenciats.

set.seed(1234)
criteris_k <- rbindlist(lapply(2:k_max, \(k) {
  km  <- kmeans(matriu_cluster, centers = k, nstart = 25, iter.max = 50)
  sil <- cluster::silhouette(km$cluster, dist_cluster)
  data.table(k = k,
             wss = km$tot.withinss,
             var_explicada = 100 * km$betweenss / km$totss,
             silueta = mean(sil[, "sil_width"]))
}))

# Decisió: la silueta és màxima amb k = 2, però només separa satisfets i
# insatisfets. Amb k = 4 s'obté una gradació més útil per a negoci, i a
# partir de k = 4 la variància explicada creix poc (el colze).
# La silueta és baixa en tots els casos (< 0,3): els clients formen un
# continu més que no pas grups naturals ben separats.
k_final <- 4

grafic_colze <- ggplot(criteris_k, aes(x = k, y = var_explicada)) +
  geom_line(colour = "grey50") +
  geom_point(aes(colour = k == k_final), size = 3) +
  scale_colour_manual(values = c(`TRUE` = palette_main[["primary"]],
                                 `FALSE` = "grey50"), guide = "none") +
  scale_x_continuous(breaks = 2:k_max) +
  labs(title = "Mètode del colze",
       x = "Nombre de segments (k)",
       y = "Variància explicada entre grups (%)") +
  theme_minimal()

grafic_silueta <- ggplot(criteris_k, aes(x = k, y = silueta)) +
  geom_line(colour = "grey50") +
  geom_point(aes(colour = k == k_final), size = 3) +
  scale_colour_manual(values = c(`TRUE` = palette_main[["primary"]],
                                 `FALSE` = "grey50"), guide = "none") +
  scale_x_continuous(breaks = 2:k_max) +
  labs(title = "Silueta mitjana",
       x = "Nombre de segments (k)", y = "Silueta (més alt = millor)") +
  theme_minimal()

grafic_eleccio_k <- (grafic_colze | grafic_silueta) +
  plot_annotation(title = "Elecció del nombre de segments",
                  subtitle = paste("Segments escollits:", k_final))

# Dendrograma amb els k_final grups marcats
png(file.path(out_path, "cluster_dendrograma.png"),
    width = 1800, height = 1000, res = 150)
plot(hc_cluster, labels = FALSE, hang = -1, sub = NA,
     main = "Dendrograma (Ward.D2)", xlab = "Clients", ylab = "Distància")
rect.hclust(hc_cluster, k = k_final, border = palette_main[["primary"]])
dev.off()


# ---- 4. K-means ---------------------------------------------------------------
# nstart = 25: es prova amb 25 inicialitzacions aleatòries i es queda la
# millor. Amb una sola (opció per defecte) el resultat pot canviar molt.

set.seed(1234)
km_final <- kmeans(matriu_cluster, centers = k_final, nstart = 25, iter.max = 50)
dades_cluster[, cluster := km_final$cluster]

# Noms dels segments: ordenats per valoració mitjana dels atributs.
# El número que retorna kmeans() és arbitrari i canvia d'una execució a una
# altra, per això els noms NO es poden assignar a mà per número de clúster.
noms_segments <- c("Molt crítics", "Crítics", "Satisfets", "Entusiastes")
stopifnot(length(noms_segments) == k_final)

ordre_clusters <- order(rowMeans(km_final$centers))
dades_cluster[, segment := factor(noms_segments[match(cluster, ordre_clusters)],
                                  levels = noms_segments)]

colors_segment <- setNames(c("#CA0020", "#F4A582", "#92C5DE", "#0571B0"),
                           noms_segments)


# ---- 5. Validació -------------------------------------------------------------

# 5.1 Concordança amb el clúster jeràrquic: si dos mètodes diferents
# troben grups semblants, la segmentació és estable.
dades_cluster[, cluster_hc := cutree(hc_cluster, k = k_final)]
taula_concordanca <- table(Kmeans = dades_cluster$segment,
                           Jerarquic = dades_cluster$cluster_hc)
# Concordança: proporció de clients en la cel·la majoritària de cada fila
concordanca <- sum(apply(taula_concordanca, 1, max)) / sum(taula_concordanca)

# 5.2 Perfil de cada segment: mida, satisfacció global i NPS
perfil_segments <- dades_cluster[, .(
  n             = .N,
  pct           = 100 * .N / nrow(dades_cluster),
  mitjana_atrib = mean(rowMeans(.SD)),
  satisfaccio   = mean(get(var_global)),
  nps           = 100 * (mean(get(var_global) >= 9) - mean(get(var_global) <= 6))
), by = segment, .SDcols = vars_cluster][order(segment)]

# La satisfacció global no s'ha fet servir per segmentar: si difereix
# entre segments, els grups són rellevants per a negoci.
kruskal_segment <- kruskal.test(reformulate("segment", var_global),
                                data = dades_cluster)

# 5.3 Mitjana de cada atribut per segment
mitjanes_segment <- melt(
  dades_cluster[, lapply(.SD, mean), by = segment, .SDcols = vars_cluster],
  id.vars = "segment", variable.name = "atribut", value.name = "mitjana",
  variable.factor = FALSE
)

# Ordre dels atributs segons la diferència entre el segment més i el menys
# satisfet: a dalt, els atributs que més separen els segments
ordre_atrib <- mitjanes_segment[, .(rang = max(mitjana) - min(mitjana)),
                                by = atribut][order(rang), atribut]
mitjanes_segment[, atribut := factor(atribut, levels = ordre_atrib)]

grafic_perfil_segments <- ggplot(mitjanes_segment,
                                 aes(x = mitjana, y = atribut,
                                     colour = segment)) +
  geom_line(aes(group = atribut), colour = "grey85", linewidth = 1) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = colors_segment) +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(title = "Perfil dels segments",
       subtitle = "Valoració mitjana per atribut. A dalt, els atributs que més separen els segments",
       x = "Mitjana (1-10)", y = NULL, colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.y = element_text(size = 9))

grafic_satisfaccio_segment <- ggplot(perfil_segments,
                                     aes(x = segment, y = satisfaccio,
                                         fill = segment)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f\nNPS %+.0f", satisfaccio, nps)),
            vjust = -0.3, size = 3.3, lineheight = 0.9) +
  scale_fill_manual(values = colors_segment) +
  scale_y_continuous(limits = c(0, 11), breaks = seq(0, 10, 2)) +
  labs(title = "Satisfacció global per segment",
       subtitle = "Variable no utilitzada per segmentar: valida els grups",
       x = NULL, y = "Satisfacció mitjana (1-10)") +
  theme_minimal()


# ---- 6. Composició per cadena -----------------------------------------------
# Percentatge de clients de cada cadena en cada segment (cada cadena suma 100%)

segment_cadena <- dades_cluster[, .N, by = .(Establecimiento, segment)][
  , pct := 100 * N / sum(N), by = Establecimiento]

chi_segment_cadena <- chisq.test(table(dades_cluster$Establecimiento,
                                       dades_cluster$segment))

grafic_segment_cadena <- ggplot(segment_cadena,
                                aes(x = Establecimiento, y = pct,
                                    fill = segment)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(pct >= 4, sprintf("%.0f%%", pct), "")),
            position = position_stack(vjust = 0.5), size = 3.3) +
  coord_flip() +
  scale_x_discrete(labels = etiqueta) +
  scale_fill_manual(values = colors_segment) +
  labs(title = "Segments de clients per cadena",
       subtitle = sprintf("χ² = %.1f, p < 0,001",
                          chi_segment_cadena$statistic),
       x = NULL, y = "% de clients de la cadena", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")


# ---- 7. Perfil sociodemogràfic ------------------------------------------------
# Comprova si els segments s'associen amb l'edat (khi quadrat + V de Cramér)

taula_edat <- table(dades_cluster$segment, dades_cluster$C1.Edad)
chi_segment_edat <- chisq.test(taula_edat)
cramer_edat <- sqrt(as.numeric(chi_segment_edat$statistic) /
                      (sum(taula_edat) * (min(dim(taula_edat)) - 1)))

segment_edat <- dades_cluster[, .N, by = .(C1.Edad, segment)][
  , pct := 100 * N / sum(N), by = C1.Edad]

grafic_segment_edat <- ggplot(segment_edat,
                              aes(x = C1.Edad, y = pct, fill = segment)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = colors_segment) +
  labs(title = "Segments per grup d'edat",
       subtitle = sprintf("χ² = %.1f, p = %.4f, V de Cramér = %.2f",
                          chi_segment_edat$statistic,
                          chi_segment_edat$p.value, cramer_edat),
       x = "Grup d'edat", y = "% del grup d'edat", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")


# ---- 8. Resum de resultats ----------------------------------------------------

resum_cluster <- c(
  "=== Nombre de segments ===",
  criteris_k[, sprintf("k = %d  variància explicada = %.1f%%  silueta = %.3f",
                       k, var_explicada, silueta)],
  sprintf("Escollits: %d", k_final),
  "",
  "=== Perfil dels segments ===",
  perfil_segments[, sprintf("%-13s n = %4d (%4.1f%%)  mitjana atributs = %.2f  satisfacció = %.2f  NPS = %+.0f",
                            segment, n, pct, mitjana_atrib, satisfaccio, nps)],
  sprintf("Kruskal-Wallis satisfacció ~ segment: H = %.1f, p = %.2e",
          kruskal_segment$statistic, kruskal_segment$p.value),
  "",
  "=== Validació ===",
  sprintf("Concordança K-means / jeràrquic: %.1f%%", 100 * concordanca),
  "",
  "=== Segments per cadena (%) ===",
  capture.output(print(dcast(segment_cadena, Establecimiento ~ segment,
                             value.var = "pct", fill = 0), digits = 3)),
  sprintf("Khi quadrat: χ² = %.1f, p = %.2e",
          chi_segment_cadena$statistic, chi_segment_cadena$p.value),
  "",
  "=== Segments per edat ===",
  sprintf("χ² = %.1f, p = %.4f, V de Cramér = %.2f",
          chi_segment_edat$statistic, chi_segment_edat$p.value, cramer_edat)
)
writeLines(resum_cluster)


# ---- 9. Exportació ----------------------------------------------------------

writeLines(resum_cluster, file.path(out_path, "resultats_cluster.txt"))
fwrite(perfil_segments, file.path(out_path, "taula_perfil_segments.csv"),
       sep = ";", dec = ",")
fwrite(dcast(mitjanes_segment, atribut ~ segment, value.var = "mitjana"),
       file.path(out_path, "taula_mitjanes_segment.csv"), sep = ";", dec = ",")
fwrite(dades_cluster[, .(`#`, Establecimiento, segment)],
       file.path(out_path, "assignacio_segments.csv"), sep = ";", dec = ",")

ggsave(file.path(out_path, "cluster_eleccio_k.png"),
       grafic_eleccio_k, width = 11, height = 4.5, dpi = 150)
ggsave(file.path(out_path, "cluster_perfil.png"),
       grafic_perfil_segments, width = 9, height = 9, dpi = 150)
ggsave(file.path(out_path, "cluster_satisfaccio.png"),
       grafic_satisfaccio_segment, width = 7, height = 5, dpi = 150)
ggsave(file.path(out_path, "cluster_cadena.png"),
       grafic_segment_cadena, width = 9, height = 4, dpi = 150)
ggsave(file.path(out_path, "cluster_edat.png"),
       grafic_segment_edat, width = 9, height = 5, dpi = 150)
