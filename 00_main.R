# =============================================================================
#  Anàlisi Eufòria – Estudi setmanal de supermercats
# -----------------------------------------------------------------------------
#  Autor:       JQ
#  Descripció:  Script principal. Carrega llibreries, defineix rutes i
#               paràmetres, llegeix les dades i executa les parts de l'anàlisi.
#  Execució:    Obrir el projecte (.Rproj) i executar aquest fitxer.
# =============================================================================


# ---- 1. Llibreries ----------------------------------------------------------

library(data.table)   # lectura i manipulació de dades
library(dplyr)        # transformació de dades
library(chron)        # gestió de dates/hores
library(ggplot2)      # visualització
library(hrbrthemes)   # temes de ggplot
library(viridis)      # paletes de color
library(patchwork)    # combinació de gràfics
library(corrplot)     # matrius de correlació
library(FactoMineR)   # anàlisi multivariant (ACP, ACM...)


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


# ---- 5. Execució de les parts -----------------------------------------------
# La lectura de dades es fa a la part 01.

dir.create(out_path, showWarnings = FALSE)

source(file.path(script_parts_path, "01 - lectura i transformacio.R"))
source(file.path(script_parts_path, "02 - analisi descriptiu.R"))
source(file.path(script_parts_path, "03 - analisi T2box.R"))
source(file.path(script_parts_path, "04 - analisi NPS.R"))
