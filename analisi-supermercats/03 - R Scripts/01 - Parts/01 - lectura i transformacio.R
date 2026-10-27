# =============================================================================
#  01 - Lectura i transformació
# -----------------------------------------------------------------------------
#  Llegeix les respostes i les metadades, assigna noms de columna,
#  converteix les variables sociodemogràfiques a factors i identifica
#  les variables d'escala 1-10 per a la transformació Top-2-Box.
#  Requereix: data_path (definit a 00_main.R)
# =============================================================================


# ---- 1. Lectura -------------------------------------------------------------

data_responses <- fread(
  file.path(data_path, "data_responses.csv"),
  sep = ";",
  dec = ",",
  encoding = "UTF-8"
)

metadata <- fread(
  file.path(data_path, "metadata.csv"),
  header = TRUE,
  sep = ";",
  dec = ",",
  encoding = "Latin-1"
)

# Noms de columna a partir de les metadades (col_desc: noms curts, p. ex.
# "Establecimiento"; col_detall conté el text de la pregunta en alguns casos)
stopifnot(ncol(data_responses) == nrow(metadata),
          all(names(data_responses) == metadata$col_value))
setnames(data_responses, metadata$col_desc)


# ---- 2. Factors sociodemogràfics --------------------------------------------
# Cada element: nivells en l'ordre correcte + si la variable és ordinal.

factor_spec <- list(
  C1.Edad = list(
    levels  = c("de 18 a 24", "de 25 a 35", "de 35 a 44",
                "de 45 a 54", "de 55 a 64", "mas de 64"),
    ordered = TRUE
  ),
  C2.Sexo = list(
    levels  = c("hombre", "mujer"),
    ordered = FALSE
  ),
  C3.EstadoCivil = list(
    levels  = c("soltero/a", "unido/a", "casado/a", "separado/a", "viudo/a"),
    ordered = FALSE
  ),
  C4.CompraMedia = list(
    levels  = c("menos de 15", "entre 15 y 30", "entre 31 y 45",
                "entre 46 y 60", "entre 61 y 75", "entre 76 y 90",
                "entre 91 y 120", "mas de 120"),
    ordered = TRUE
  ),
  C5.IngMes = list(
    levels  = c("menos de 600", "entre 601 y 1000", "entre 1001 y 1500",
                "entre 1501 y 2000", "entre 2001 y 3000",
                "entre 3001 y 4500", "mas de 4500"),
    ordered = TRUE
  )
)

for (v in names(factor_spec)) {
  spec <- factor_spec[[v]]

  # Avís si hi ha valors a les dades que no són a la llista de nivells
  # (factor() els convertiria en NA sense dir res)
  unknown <- setdiff(unique(na.omit(data_responses[[v]])), spec$levels)
  if (length(unknown) > 0) {
    warning(v, ": valors no previstos -> ", paste(unknown, collapse = ", "))
  }

  set(data_responses, j = v,
      value = factor(data_responses[[v]],
                     levels  = spec$levels,
                     ordered = spec$ordered))
}


# ---- 3. Control de qualitat -------------------------------------------------

na_counts <- colSums(is.na(data_responses))
stopifnot("Hi ha valors perduts a les dades" = all(na_counts == 0))


# ---- 4. Variables d'escala 1-10 (per a Top-2-Box) ---------------------------

vars_t2box <- metadata[tipus == "1al10", col_desc]
# 27 variables: Pc1.* (atenció), Pd1.* (assessorament), Pe1.* (experiència),
# Pf1.* (oferta), Pg1.* (botiga), Ph1.* (valoració global)

# Les escales han de ser numèriques per calcular mitjanes i correlacions
# (as.character primer per si vénen com a text o factor)
data_responses[, (vars_t2box) := lapply(.SD, \(x) as.numeric(as.character(x))),
               .SDcols = vars_t2box]
