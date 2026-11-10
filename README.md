# Dietro la classificazione settoriale: affinità e differenze tra le imprese europee. Cluster analysis sulle società non finanziarie dello STOXX Europe 600

Codice R utilizzato per la tesi di laurea triennale di Nicolò Bozzola (relatore prof. Luca Rossini), Università degli Studi di Milano.

La tesi confronta la classificazione settoriale ICB (Industry Classification Benchmark) con gruppi di società costruiti tramite cluster analysis su 11 indicatori economico-finanziari e di mercato. Il campione comprende le 445 società non finanziarie dello STOXX Europe 600 al 30 giugno 2026. I cluster sono ottenuti con il k-means sulle variabili standardizzate e sono confrontati con i 16 supersettori ICB.

## Dati

I dati provengono da Bloomberg e non sono inclusi nella repository. Il file atteso e le sue colonne sono descritti in [`dati/LEGGIMI.md`](dati/LEGGIMI.md).


## Script

| Script | Contenuto | Capitolo |
|---|---|---|
| `01_lettura_e_controlli.R` | Lettura del dataset e selezione del campione | 2 |
| `02_descrittive.R` | Variabili candidate e statistiche descrittive | 2 |
| `03_correlazioni.R` | Matrici di correlazione di Pearson e Spearman | 2 |
| `04_variabili_scelte.R` | Le 11 variabili del clustering, correlazioni residue, valori estremi | 2 |
| `05_distribuzioni.R` | Dato mancante e forma delle distribuzioni | 2 |
| `06_trattamento_anomali.R` | Winsorizzazione e trasformazioni logaritmiche | 2 |
| `07_standardizzazione_finale.R` | Standardizzazione e dataset finale | 2 |
| `08_scelta_k.R` | Scelta del numero di cluster (gomito, Calinski-Harabasz, silhouette) | 3, 4 |
| `09_cluster_principale.R` | Partizioni k-means a K = 2, 4 e 6 | 3, 4 |
| `10_pca.R` | Analisi delle componenti principali | 3, 4 |
| `11_ward_e_pesi.R` | Controlli con il metodo di Ward e con i pesi per dimensione | 3, 4 |
| `funzioni_confronto_icb.R` | Funzioni per il confronto tra cluster e gruppi ICB | 4 |
| `12_confronto_icb.R` | Confronto con i supersettori ICB | 4 |
| `13_profili_casi.R` | Profili dei cluster e casi particolari | 4 |
| `14_robustezza.R` | Controlli di robustezza | 4 |
| `15_figure_cap4.R` | Figure del capitolo 4 | 4 |

## Struttura

```
tesi_stoxx600/
├── tesi_stoxx600.Rproj
├── R/          script di analisi
├── dati/       file dei dati (non incluso)
└── output/     tabelle (CSV), figure (PNG) e file intermedi (RDS), creati dagli script
```
