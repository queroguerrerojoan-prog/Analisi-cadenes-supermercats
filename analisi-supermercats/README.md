# 🛒 Anàlisi supermercats – Estudi de satisfacció de clients

Anàlisi en **R** d'una enquesta de satisfacció a **2.040 clients** de tres cadenes de supermercats catalanes: **Bonpreu**, **Casa Ametller** i **Consum**.

L'estudi respon tres preguntes de negoci:

1. **Com valoren els clients cada cadena?** → Top-2-Box i Net Promoter Score
2. **Les diferències són reals o fruit de l'atzar?** → ANOVA, Tukey, Kruskal-Wallis i khi quadrat
3. **On ha d'actuar cada cadena per millorar?** → Gap Analysis i anàlisi de components principals
4. **Quins tipus de clients hi ha?** → Segmentació amb clúster jeràrquic i K-means
5. **Què explica la satisfacció?** → Regressió múltiple i importància relativa

---

## 📋 Índex

1. [Resultats principals](#-resultats-principals)
2. [Dades](#-dades)
3. [Metodologia](#-metodologia)
4. [Resultats en detall](#-resultats-en-detall)
5. [Estructura del repositori](#-estructura-del-repositori)
6. [Com executar-ho](#️-com-executar-ho)

---

## 💡 Resultats principals

| | Bonpreu | Casa Ametller | Consum |
|---|---|---|---|
| **Satisfacció mitjana** (1–10) | 5,74 | 6,67 | **7,37** |
| **NPS** | −55,8 | −28,8 | **+0,4** |
| **% detractors** | 63% | 41% | 26% |
| **Punt fort** | Relació qualitat-preu | Ambient de la botiga | Atenció i rapidesa |
| **Prioritat de millora** | Botiga (neteja, senyalització, decoració) | Personal (interès, caixa, nombre d'empleats) | Personal de caixa i varietat |

- **Consum lidera clarament.** Té la satisfacció i l'NPS més alts i està per sobre de la mitjana del mercat en els 27 atributs. El seu avantatge més gran és el **personal**: atenció, empleats de caixa i rapidesa.
- **Bonpreu té un problema generalitzat.** Està per sota del mercat en **tots** els atributs i té un 63% de detractors. Els dèficits més grans són a la **botiga física**: amplitud (−1,46 punts), ordre i neteja (−1,32) i decoració (−1,15).
- **Casa Ametller té dues cares.** Lidera l'ambient de la botiga (amplitud +1,03, varietat +0,83), però en atenció i esperes està a la mitjana o per sota. El seu marge de millora és el **personal**.
- **Les diferències són estadísticament robustes.** Totes les parelles de cadenes es diferencien en satisfacció global (Tukey, p < 0,001), i els 27 atributs mostren diferències significatives després de corregir per comparacions múltiples.
- **L'ambient de la botiga és el que més pesa en la satisfacció.** Les 4 dimensions de l'ACP expliquen el 51% de la variància de la satisfacció global. L'ambient de la botiga hi té el pes més gran, justament la dimensió on Bonpreu està més per sota (−0,6 desviacions).
- **Les diferències entre cadenes s'expliquen pels atributs, no per la marca.** Els atributs expliquen el 57% de la variància de la satisfacció. Un cop controlats, la cadena ja no hi afegeix res (p = 0,22). Si Bonpreu igualés els atributs de Consum, s'eliminaria la major part de la distància de 1,6 punts. Els motors principals són la **relació qualitat-preu** (25% de l'R²), la **competitivitat**, l'**ordre i neteja** i el **servei al client**.
- **La segmentació confirma la distància entre cadenes.** Hi ha 4 segments de clients, de *Molt crítics* (NPS −93) a *Entusiastes* (NPS +34). A Consum, el 45% dels clients són entusiastes. A Bonpreu, el 63% són crítics o molt crítics.
- **A Bonpreu, els joves són els més crítics.** El seu NPS va de −62 entre els 18 i els 24 anys a −27 entre els majors de 64. A Consum, en canvi, l'NPS és estable per edats.

---

## 📊 Dades

| Fitxer | Descripció |
|---|---|
| `data_responses.csv` | 2.040 respostes × 38 columnes: cadena, 27 valoracions (escala 1–10) i variables sociodemogràfiques |
| `metadata.csv` | Diccionari de variables: codi de columna, nom, bloc temàtic i tipus (`1al10`, `sociodemografica`...) |

**Mostra per cadena:** Bonpreu 719 · Casa Ametller 617 · Consum 704

**Variables sociodemogràfiques:** edat, sexe, estat civil, despesa mitjana per compra i ingressos mensuals.

**Atributs valorats (escala 1–10):**

| Bloc | Prefix | Atributs |
|---|---|---|
| Comprensió | `Pc1` | Actitud, atenció, atenció instantània, servei al client |
| Informació | `Pd1` | Assessorament, informació completa |
| Experiència | `Pe1` | Experiència, consultes a caixa, gamma, resolució, interès, esperes, empleats de caixa |
| Productes | `Pf1` | Competitivitat, varietat |
| Centres | `Pg1` | Nombre d'empleats, decoració, il·luminació, climatització, ordre i neteja, amplitud, senyalització, identitat de la cadena |
| Valoració global | `Ph1` | Relació qualitat-preu, **satisfacció global** |

---

## 🔬 Metodologia

### 1. Preparació de les dades
- **Noms de columna.** S'assignen a partir del diccionari de metadades, amb una comprovació que l'ordre de les columnes coincideix.
- **Factors.** Les variables sociodemogràfiques es converteixen a factors, ordinals quan escau (edat, despesa i ingressos).
- **Validació automàtica.** Avís si apareixen categories no previstes i aturada si hi ha valors perduts.

### 2. Top-2-Box (T2B)

| Categoria | Puntuació |
|---|---|
| Excel·lent | 9–10 |
| Notable | 7–8 |
| Suficient | 5–6 |
| Insuficient | 1–4 |

El **T2B** és el percentatge de valoracions *Excel·lent + Notable* (7–10). Sobre una escala de 10 punts, el T2B estricte seria només 9–10. Aquí es fa servir la definició ampliada perquè discrimina millor entre cadenes. Es pot canviar amb un paràmetre.

### 3. Net Promoter Score (NPS)

| Categoria | Puntuació |
|---|---|
| Detractors | 1–6 |
| Neutrals | 7–8 |
| Promotors | 9–10 |

**NPS = % Promotors − % Detractors** (de −100 a +100), amb **interval de confiança del 95%**:

$$SE_{NPS} = 100 \cdot \sqrt{\frac{p_P + p_D - (p_P - p_D)^2}{n}}$$

> El qüestionari no inclou la pregunta clàssica de recomanació, de manera que l'índex es calcula sobre la **satisfacció global**. És un índex de tipus NPS.

### 4. Gap Analysis (Importance-Performance Analysis)
- **Importància derivada.** És la correlació de Pearson entre cada atribut i la satisfacció global, calculada dins de cada cadena.
- **Rendiment.** És la valoració mitjana de l'atribut.
- **Quadrants.** Els talls són les mitjanes dels dos eixos:

| | Satisfacció baixa | Satisfacció alta |
|---|---|---|
| **Importància alta** | 🔴 Prioritat de millora | 🟢 Fortalesa a mantenir |
| **Importància baixa** | ⚪ Baixa prioritat | 🟠 Possible excés d'esforç |

### 5. Contrastos estadístics

| Test | Ús |
|---|---|
| **ANOVA** + **Tukey HSD** | Diferències de mitjanes en la satisfacció global i entre quines parelles de cadenes |
| **Fligner-Killeen** | Homogeneïtat de variàncies |
| **ANOVA de Welch** / **Kruskal-Wallis** | Alternatives robustes, perquè l'escala 1–10 és ordinal i les variàncies no són iguales |
| **Khi quadrat** + **V de Cramér** | Associació entre cadena i categoria de valoració, amb residus estandarditzats |

- **Correcció per comparacions múltiples.** Els contrastos es repeteixen per als 27 atributs amb correcció de Benjamini-Hochberg. Un atribut es considera significatiu només si ho és amb Welch **i** amb Kruskal-Wallis.
- **Mida de l'efecte.** Es reporta la **η²**, perquè amb mostres grans una diferència pot ser significativa però poc rellevant.

### 6. Anàlisi de components principals (ACP)
- **Adequació.** Es comprova amb el KMO (0,967, excel·lent) i el test de Bartlett (p < 0,001).
- **Nombre de components.** Es decideix amb el criteri de Kaiser, el gràfic de sedimentació i l'**anàlisi paral·lela**.
- **Rotació varimax.** Fa que cada atribut carregui en una sola dimensió i les dimensions siguin interpretables.
- **Regressió.** Es fa una regressió de la satisfacció global sobre les dimensions. Com que les dimensions són independents entre si, se supera la limitació de multicol·linealitat de la Gap Analysis.

### 7. Segmentació de clients (anàlisi clúster)
- **Variables.** Es fan servir els 26 atributs sense estandarditzar, perquè tots tenen la mateixa escala (1–10). La **satisfacció global queda fora** de la segmentació i serveix per validar-la.
- **Exploració.** Primer es fa un clúster jeràrquic (Ward.D2) per veure l'estructura de les dades.
- **Nombre de segments.** Es tria amb el mètode del colze i la silueta mitjana.
- **K-means.** Es fa amb 25 inicialitzacions aleatòries (`nstart = 25`) perquè el resultat sigui estable.
- **Noms dels segments.** S'assignen segons la valoració mitjana. El número de clúster que retorna `kmeans()` és arbitrari, de manera que no es poden assignar noms a mà per número.
- **Validació.** Es comprova la concordança amb el clúster jeràrquic i les diferències de satisfacció entre segments (Kruskal-Wallis).

### 8. Regressió múltiple
- **Model.** Satisfacció global en funció dels 26 atributs.
- **Selecció de variables.** Es fa per **AIC** (stepwise en les dues direccions). No es barreja amb eliminacions manuals per p-valor.
- **Diagnòstic:**
  - **VIF** per a la multicol·linealitat.
  - **Breusch-Pagan** per a la heteroscedasticitat. Com que n'hi ha, els coeficients s'interpreten amb **errors estàndard robustos HC3**.
  - **Durbin-Watson** per a l'autocorrelació, repetit amb ordre aleatori de les files.
  - **Q-Q plot** per a la normalitat dels residus.
- **Validació creuada (10 particions).** Estima l'R² fora de mostra.
- **Efecte cadena.** Es contrasta si la cadena explica satisfacció un cop controlats els atributs.
- **Importància relativa (LMG).** Reparteix l'R² entre els predictors fent la mitjana de la seva aportació en tots els ordres d'entrada possibles. Amb predictors correlacionats és més fiable que comparar coeficients.

---

## 📈 Resultats en detall

### Satisfacció i NPS per cadena

![Composició NPS per cadena](02%20-%20Output/nps_composicio_cadena.png)

Consum és l'única cadena amb un NPS neutre (+0,4). Bonpreu té **9 detractors per cada promotor**.

![NPS per edat i cadena](02%20-%20Output/nps_edat_cadena.png)

A Bonpreu l'NPS millora clarament amb l'edat: els clients joves són els més crítics. A Casa Ametller i a Consum no hi ha una tendència clara, i els intervals dels majors de 64 anys són amples perquè la mostra és més petita.

### Top-2-Box per atribut

![T2B per atribut i cadena](02%20-%20Output/t2b_comparatiu.png)

L'ordre de les cadenes es manté en gairebé tots els atributs. Les excepcions són l'**amplitud**, la **varietat** i la **senyalització**, on Casa Ametller supera Consum. Els atributs pitjor valorats a tot el mercat són les **esperes** (a caixa i per ser atès).

### Posició de cada cadena respecte al mercat

![Diferència respecte al mercat](02%20-%20Output/mitjanes_dif_mercat.png)

### Gap Analysis: on actuar

| Bonpreu | Casa Ametller | Consum |
|---|---|---|
| ![](02%20-%20Output/gap_bonpreu.png) | ![](02%20-%20Output/gap_casa_ametller.png) | ![](02%20-%20Output/gap_consum.png) |

| Cadena | Prioritats de millora (importància alta, satisfacció baixa) |
|---|---|
| **Bonpreu** | Ordre i neteja, senyalització, atenció instantània, decoració, atenció, varietat, amplitud |
| **Casa Ametller** | Interès del personal, empleats de caixa, nombre d'empleats, servei al client, gamma, rapidesa de resolució |
| **Consum** | Empleats de caixa, varietat, assessorament, informació completa, espera d'atenció |

### Les diferències són significatives?

![Tukey satisfacció global](02%20-%20Output/tukey_satisfaccio.png)

- **Satisfacció global.** Les tres parelles de cadenes es diferencien significativament. La cadena explica el 12,4% de la variància de la satisfacció (η², efecte mitjà).
- **Robustesa.** Welch i Kruskal-Wallis confirmen el resultat. És important perquè les variàncies no són homogènies (Fligner, p < 0,001).

![Mida de l'efecte per atribut](02%20-%20Output/tests_atributs.png)

On més es diferencien les cadenes és en els **atributs físics de la botiga**: amplitud (η² = 0,27), ordre i neteja (0,22) i decoració (0,17).

### Dimensions latents (ACP)

![Càrregues factorials](02%20-%20Output/acp_carregues.png)

Els 26 atributs es resumeixen en **4 dimensions**, que expliquen el 65% de la variància: **Atenció i servei al client**, **Ambient de la botiga**, **Rapidesa i esperes** i **Oferta i assessorament**.

![Posició de les cadenes en les dimensions](02%20-%20Output/acp_dimensions_cadena.png)

![Pes de cada dimensió en la satisfacció](02%20-%20Output/acp_pes_dimensions.png)

- **Pes de les dimensions.** Totes quatre són significatives. L'**ambient de la botiga** és la que pesa més en la satisfacció global, i les esperes la que menys.
- **Posició de les cadenes.** Cada cadena té un perfil diferent:
  - **Consum** lidera en atenció i rapidesa.
  - **Casa Ametller** lidera en ambient i oferta.
  - **Bonpreu** està per sota en totes quatre, i sobretot en ambient.

### Segments de clients

![Elecció del nombre de segments](02%20-%20Output/cluster_eleccio_k.png)

S'han escollit **4 segments**. La silueta és màxima amb k = 2, però amb només dos grups se separen satisfets i insatisfets. A partir de k = 4 la variància explicada creix poc. El K-means i el clúster jeràrquic coincideixen en el **79%** dels clients.

| Segment | % clients | Mitjana atributs | Satisfacció | NPS |
|---|---|---|---|---|
| Molt crítics | 9,5% | 3,56 | 3,89 | −93 |
| Crítics | 26,6% | 5,27 | 5,55 | −76 |
| Satisfets | 36,2% | 6,56 | 6,85 | −24 |
| Entusiastes | 27,8% | 7,81 | 8,13 | +34 |

![Satisfacció per segment](02%20-%20Output/cluster_satisfaccio.png)

![Segments per cadena](02%20-%20Output/cluster_cadena.png)

- **Validació amb la satisfacció.** La satisfacció global no s'ha fet servir per segmentar i, tot i així, augmenta de manera clara d'un segment al següent (Kruskal-Wallis, p < 0,001). Els segments tenen sentit de negoci.
- **Repartiment per cadena.** Gairebé la meitat dels clients de Consum són *entusiastes*. A Bonpreu, el 22% són *molt crítics*: és la cadena que concentra gairebé tots els clients d'aquest segment.
- **Edat.** L'associació amb l'edat és significativa però molt feble (V de Cramér = 0,07). Els segments no corresponen a perfils demogràfics.

![Perfil dels segments](02%20-%20Output/cluster_perfil.png)

**Limitació.** Les línies del perfil són gairebé paral·leles: els segments es diferencien sobretot pel **nivell general** de valoració (efecte halo), més que per perfils diferents. La silueta baixa (0,13) ho confirma, perquè els clients formen un continu. Els atributs que més separen els segments són els **empleats de caixa**, l'**ordre i neteja** i la **decoració**, els mateixos on Bonpreu té més dèficit. Una extensió seria segmentar sobre el perfil relatiu de cada client (atributs centrats per fila) per trobar grups que prioritzen coses diferents.

### Motors de la satisfacció (regressió múltiple)

| Model | Variables | R² | R² ajustat | R² validació creuada |
|---|---|---|---|---|
| Complet | 26 | 0,570 | 0,565 | – |
| **Selecció per AIC** | **12** | **0,568** | **0,566** | **0,555** |
| AIC sense qualitat-preu | 11 | 0,492 | – | – |

- **Simplificació.** La selecció per AIC elimina 14 atributs pràcticament sense perdre capacitat explicativa.
- **Predicció fora de mostra.** La validació creuada (0,555) és gairebé igual a l'R² dins de la mostra, de manera que **no hi ha sobreajust**.

![Importància relativa](02%20-%20Output/regressio_importancia.png)

- **El motor principal és la relació qualitat-preu.** Aporta una quarta part del poder explicatiu. Com que és també una valoració global, s'ha comprovat el model sense aquesta variable: l'R² baixa només de 0,57 a 0,49.
- **Per blocs, la botiga física i l'experiència de compra hi pesen gairebé igual.** Centres aporten el 20,5% i Experiència el 19,8%, més que el tracte personal (Comprensió, 12,9%) i que la informació (5,4%).

![Coeficients](02%20-%20Output/regressio_coeficients.png)

- **Lectura dels coeficients.** Cada coeficient indica l'efecte de +1 punt en l'atribut, mantenint fixos els altres. Per exemple, +1 punt en relació qualitat-preu equival a +0,39 punts de satisfacció.
- **Variables no significatives.** Quatre atributs no són significatius individualment amb errors robustos, però l'AIC els conserva perquè milloren la predicció en conjunt.

**Efecte cadena.** Sense controlar res, Consum supera Bonpreu en 1,63 punts de satisfacció i Casa Ametller en 0,93. **Un cop controlats els atributs, aquestes diferències desapareixen** (F = 1,51, p = 0,22). La satisfacció no depèn de la marca sinó de com es valoren els atributs. És una conclusió accionable: si Bonpreu millora els atributs, pot tancar la distància.

![Diagnòstic](02%20-%20Output/regressio_diagnostic.png)

**Diagnòstic:**
- **Multicol·linealitat.** No n'hi ha (VIF màxim de 2,44).
- **Heteroscedasticitat.** N'hi ha (Breusch-Pagan, p = 0,003), i per això s'usen errors robustos.
- **Autocorrelació.** El Durbin-Watson inicial (p = 0,017) és un artefacte de l'ordre de les files, que estan ordenades per cadena. Amb ordre aleatori desapareix (p = 0,17).
- **Normalitat.** Els residus tenen la cua esquerra pesant: hi ha clients molt insatisfets que els atributs no expliquen del tot. Amb n = 2.040 això no invalida la inferència, però suggereix que hi ha motius d'insatisfacció que el qüestionari no recull.

**Limitacions.** La satisfacció és una variable ordinal (1–10) tractada com a contínua, i els p-valors després d'una selecció stepwise són optimistes. Una extensió seria una regressió logística ordinal.

---

## 📁 Estructura del repositori

```
.
├── 00_main.R                         # Script principal: executa tot el flux
├── analisi-supermercats.Rproj
├── 01 - Data/
│   ├── data_responses.csv
│   └── metadata.csv
├── 02 - Output/                      # Gràfics (.png), taules (.csv) i resums (.txt)
├── 03 - R Scripts/
│   └── 01 - Parts/
│       ├── 01 - lectura i transformacio.R
│       ├── 02 - analisi descriptiu.R
│       ├── 03 - analisi T2box.R
│       ├── 04 - analisi NPS.R
│       ├── 05 - analisi bivariant.R
│       ├── 06 - comparacio de grups.R
│       ├── 07 - analisi factorial.R
│       ├── 08 - analisi cluster.R
│       └── 09 - regressio multiple.R
└── README.md
```

Cada part llegeix els objectes creats per les anteriors. Per tant, s'han d'executar en ordre a través de `00_main.R`.

---

## ▶️ Com executar-ho

1. Clona el repositori:
   ```bash
   git clone https://github.com/<usuari>/analisi-supermercats.git
   ```
2. Obre `analisi-supermercats.Rproj` a RStudio.
3. Instal·la les dependències:
   ```r
   install.packages(c("data.table", "ggplot2", "viridis", "patchwork",
                      "corrplot", "psych", "ggrepel", "scales", "here",
                      "cluster", "car", "lmtest", "sandwich", "relaimpo"))
   ```
4. Executa `00_main.R`. Tots els gràfics i taules es generen a `02 - Output/`.

---

## 🛠 Tecnologies

![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![data.table](https://img.shields.io/badge/data.table-1E5A8E?style=flat)
![ggplot2](https://img.shields.io/badge/ggplot2-E05A4F?style=flat)
![psych](https://img.shields.io/badge/psych-6A4C93?style=flat)

- **data.table** – manipulació de dades
- **ggplot2**, **patchwork**, **ggrepel**, **viridis**, **corrplot** – visualització
- **psych** – KMO, anàlisi paral·lela i ACP amb rotació
- **cluster** – silueta per validar la segmentació
- **car**, **lmtest**, **sandwich**, **relaimpo** – diagnòstic de la regressió, errors robustos i importància relativa
- **here** – rutes reproduïbles

---

## 👤 Autor

**Joan Quero Guerrero** · [LinkedIn](https://www.linkedin.com/in/<perfil>) · [GitHub](https://github.com/<usuari>)
