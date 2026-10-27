# 🛒 Anàlisi Eufòria – Estudi de satisfacció en supermercats

Anàlisi en **R** d'una enquesta de satisfacció a clients de tres cadenes de supermercats catalanes: **Bonpreu**, **Casa Ametller** i **Consum**.

L'estudi combina una anàlisi descriptiva del perfil dels clients, una anàlisi **Top-2-Box (T2B)** de 27 atributs de servei i botiga, i el càlcul del **Net Promoter Score (NPS)** per cadena i per grup d'edat.

---

## 📋 Índex

1. [Objectius](#-objectius)
2. [Dades](#-dades)
3. [Metodologia](#-metodologia)
4. [Resultats](#-resultats)
5. [Estructura del repositori](#-estructura-del-repositori)
6. [Com executar-ho](#️-com-executar-ho)
7. [Tecnologies](#-tecnologies)

---

## 🎯 Objectius

- Descriure el perfil sociodemogràfic dels clients enquestats.
- Identificar els **punts forts i febles** de cada cadena a partir de la valoració de 27 atributs.
- Comparar la **fidelitat dels clients** entre cadenes mitjançant l'NPS.
- Detectar diferències de percepció segons el **grup d'edat**.

---

## 📊 Dades

| Fitxer | Descripció |
|---|---|
| `data_responses.csv` | Una fila per resposta: variables sociodemogràfiques, cadena i valoracions (escala 1–10) |
| `metadata.csv` | Diccionari de variables: nom de columna, descripció i tipus (`tipus = "1al10"` per a les escales) |

**Variables sociodemogràfiques:** edat, sexe, estat civil, despesa mitjana per compra i ingressos mensuals.

**Atributs valorats (escala 1–10), agrupats en blocs:**

| Bloc | Prefix | Exemples |
|---|---|---|
| Atenció al client | `Pc1` | Actitud, atenció ràpida, servei |
| Assessorament | `Pd1` | Assessorament, informació completa |
| Experiència de compra | `Pe1` | Coneixement de la gamma, temps d'espera a caixa |
| Oferta | `Pf1` | Competitivitat, varietat |
| Botiga | `Pg1` | Il·luminació, ordre i neteja, senyalització |
| Valoració global | `Ph1` | Relació qualitat-preu, satisfacció |

> ⚠️ Les dades originals no s'inclouen al repositori per motius de confidencialitat.

---

## 🔬 Metodologia

### 1. Preparació de les dades
- Assignació de noms de columna a partir del diccionari de metadades.
- Conversió de les variables sociodemogràfiques a **factors** (ordinals quan escau: edat, despesa, ingressos).
- Validació automàtica: avís si apareixen categories no previstes i aturada si hi ha valors perduts.

### 2. Anàlisi descriptiva
Distribució dels ingressos i taula creuada **ingressos × edat**, amb percentatges calculats dins de cada grup d'edat.

### 3. Top-2-Box (T2B)
Cada valoració es classifica en quatre categories:

| Categoria | Puntuació |
|---|---|
| Excel·lent | 9–10 |
| Notable | 7–8 |
| Suficient | 5–6 |
| Insuficient | 1–4 |

El **T2B** és el percentatge de valoracions *Excel·lent + Notable* (7–10) per a cada atribut i cadena. Permet identificar els 5 atributs millor i pitjor valorats de cada cadena i comparar-les entre si.

### 4. Net Promoter Score (NPS)

| Categoria | Puntuació |
|---|---|
| Detractors | 0–6 |
| Neutrals | 7–8 |
| Promotors | 9–10 |

**NPS = % Promotors − % Detractors** (rang de −100 a +100)

Cada NPS s'acompanya del seu **interval de confiança del 95%**:

$$SE_{NPS} = 100 \cdot \sqrt{\frac{p_P + p_D - (p_P - p_D)^2}{n}}$$

Els subgrups amb menys de 30 respostes es marquen com a poc fiables.

---

## 📈 Resultats

> Els gràfics es generen automàticament a la carpeta `02 - Output/` en executar `00_main.R`.

### Perfil dels clients

![Ingressos per grup d'edat](02%20-%20Output/ingressos_per_edat.png)

<!-- Conclusió: ... -->

### Top-2-Box: comparativa entre cadenes

![T2B per atribut i cadena](02%20-%20Output/t2b_comparatiu.png)

<!-- Conclusió: quina cadena destaca en quins atributs -->

### Punts forts i febles per cadena

| Bonpreu | Casa Ametller | Consum |
|---|---|---|
| ![](02%20-%20Output/t2b_top_bottom_bonpreu.png) | ![](02%20-%20Output/t2b_top_bottom_casa_ametller.png) | ![](02%20-%20Output/t2b_top_bottom_consum.png) |

<!-- Conclusió: ... -->

### NPS per cadena

![Composició NPS per cadena](02%20-%20Output/nps_composicio_cadena.png)

![NPS per cadena amb IC](02%20-%20Output/nps_cadena.png)

<!-- Conclusió: ... -->

### NPS per grup d'edat

![NPS per edat i cadena](02%20-%20Output/nps_edat_cadena.png)

<!-- Conclusió: ... -->

### 💡 Conclusions principals

<!-- Escriu aquí 3-5 conclusions clau de negoci. Exemple de format:
- **Bonpreu** obté el millor NPS (+XX), impulsat per ...
- L'atribut pitjor valorat a totes les cadenes és ..., una oportunitat de millora comuna.
- Els clients de menys de 35 anys són ...
-->

---

## 📁 Estructura del repositori

```
.
├── 00_main.R                         # Script principal: executa tot el flux
├── 01 - Data/                        # Dades (no incloses al repositori)
├── 02 - Output/                      # Gràfics i taules generats
├── 03 - R Scripts/
│   └── 01 - Parts/
│       ├── 01 - lectura i transformacio.R
│       ├── 02 - analisi descriptiu.R
│       ├── 03 - analisi T2box.R
│       └── 04 - analisi NPS.R
└── README.md
```

---

## ▶️ Com executar-ho

1. Clona el repositori:
   ```bash
   git clone https://github.com/<usuari>/<repositori>.git
   ```
2. Obre el projecte a RStudio (`.Rproj`).
3. Instal·la les dependències:
   ```r
   install.packages(c("data.table", "dplyr", "ggplot2", "viridis",
                      "patchwork", "scales", "here"))
   ```
4. Col·loca els fitxers de dades a `01 - Data/`.
5. Executa `00_main.R`.

---

## 🛠 Tecnologies

![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![data.table](https://img.shields.io/badge/data.table-1E5A8E?style=flat)
![ggplot2](https://img.shields.io/badge/ggplot2-E05A4F?style=flat)

- **data.table** – manipulació eficient de dades
- **ggplot2** + **viridis** – visualització
- **here** – rutes reproduïbles

---

## 👤 Autor

**JQ** · [LinkedIn](https://www.linkedin.com/in/<perfil>) · [GitHub](https://github.com/<usuari>)
