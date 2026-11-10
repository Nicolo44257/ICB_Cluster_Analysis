# Dati

Il file dei dati non è incluso nella repository: i dati provengono da Bloomberg e le condizioni d'uso non ne consentono la ridistribuzione.

Gli script leggono il file `dati/DATASET_v5_valori.xlsx`, foglio `DATASET`, con una riga per ciascuna delle 600 società dello STOXX Europe 600 al 30 giugno 2026. I dati di bilancio si riferiscono all'ultimo esercizio annuale disponibile a quella data (valori in euro, non rettificati).

## Colonne usate dagli script

| Colonna | Contenuto |
|---|---|
| `Ticker`, `Name` | Ticker Bloomberg e nome della società |
| `Nome industria ICB`, `ICB Supersct Nm`, `Nome settore ICB` | Industria, supersettore e settore ICB |
| `Campione` | `Incluso`, oppure il motivo dell'esclusione (società finanziarie, Real Estate, società senza attività operativa) |
| `EBITDA / Vend nette:Y`, `Mrg op:Y`, `Prg ut:Y` | Margine EBITDA, margine operativo, margine netto (%) |
| `ROA:Y`, `ROE:Y`, `ROIC:Y` | Redditività (%) |
| `Vend-Cr 1an:Y` | Crescita dei ricavi rispetto all'anno precedente (%) |
| `Debito/Attivi:Y`, `Net Debt to EBITDA:Y` | Struttura finanziaria |
| `EBIT:Y`, `Int pass:Y` | EBIT e interessi passivi |
| `Liq corr:Y`, `Quick Ratio:Y` | Current ratio e quick ratio |
| `CAPEX/Sales:Y`, `Ast TO:Y` | Capex/vendite (%) e rotazione dell'attivo |
| colonna che inizia con `Volt` | Volatilità: deviazione standard annualizzata dei rendimenti giornalieri negli ultimi 260 giorni di borsa (%) |
| `Beta:Y-1`, `Raw Beta:Y-1` | Beta rettificato e non rettificato |
| `Cap mercato` | Capitalizzazione al 30/06/2026 |
| `EV (calc)` | Cap mercato + Debito netto:Y + (Tot Eqty:Y − Tot CE:Y) |
| `EBITDA/EV (calc)` | EBITDA:Y / EV × 100 |
| `Book-to-price (calc)` | Tot CE:Y / Cap mercato |
| `Dividend yield (calc)` | dividendi pagati (Div pag:Y, in valore assoluto) / Cap mercato × 100 |
| `R&S/vendite (calc)` | Spes R&D:Y / Ricavi:Y × 100 |

Il suffisso `:Y` indica il dato dell'ultimo esercizio annuale. La costruzione del campione e le variabili sono descritte nel capitolo 2 della tesi.
