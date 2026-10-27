# =============================================================================
#  Anàlisi supermercats – Estudi de satisfacció de clients
# -----------------------------------------------------------------------------
#  Autor:       JQ
#  Descripció:  Script principal. Carrega llibreries, defineix rutes i
#               paràmetres i executa les parts de l'anàlisi en ordre.
#  Execució:    Obrir el projecte (.Rproj) i executar aquest fitxer.
# =============================================================================


# ---- 1. Llibreries ----------------------------------------------------------

library(data.table)   # lectura i manipulació de dades
library(ggplot2)      # visualització
library(viridis)      # paletes de color
library(patchwork)    # combinació de gràfics
library(corrplot)     # matriu de correlacions
# També s'usen amb paquet::funció: here, scales, ggrepel, psych


# ---- 2. Rutes ---------------------------------------------------------------
# here::here() fa que les rutes funcionin des de l'arrel del projecte,
# independentment del directori de treball.

root_path        <- here::here()
data_path        <- file.path(root_path, "01 - Data")
out_path         <- file.path(root_path, "02 - Output")
script_path      <- file.path(root_path, "03 - R Scripts")
script_parts_path <- file.path(script_path, "01 - Parts")
inci_path        <- file.path(root_path, "04 - Incidències")


# ---- 3. Paràmetres gràfics --------------------------------------------------

palette_main <- c(
  primary   = "#DD2048",
  secondary = "#BE2649",
  accent    = "#EE365D"
)


# ---- 4. Paràmetres de l'estudi ----------------------------------------------

cadenes <- c("bonpreu", "casa_ametller", "consum")

# Nom llegible per als gràfics: "casa_ametller" -> "Casa Ametller"
etiqueta <- function(x) tools::toTitleCase(gsub("_", " ", x))


# ---- 5. Execució de les parts -----------------------------------------------
# La lectura de dades es fa a la part 01.

dir.create(out_path, showWarnings = FALSE)

source(file.path(script_parts_path, "01 - lectura i transformacio.R"))
source(file.path(script_parts_path, "02 - analisi descriptiu.R"))
source(file.path(script_parts_path, "03 - analisi T2box.R"))
source(file.path(script_parts_path, "04 - analisi NPS.R"))
source(file.path(script_parts_path, "05 - analisi bivariant.R"))
source(file.path(script_parts_path, "06 - comparacio de grups.R"))
source(file.path(script_parts_path, "07 - analisi factorial.R"))
source(file.path(script_parts_path, "08 - analisi cluster.R"))
source(file.path(script_parts_path, "09 - regressio multiple.R"))
