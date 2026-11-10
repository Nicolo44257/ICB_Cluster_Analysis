# =====================================================================
# 01_lettura_e_controlli.R
# Lettura del dataset e selezione delle 445 società del campione
# (capitolo 2, sezione 2.1): dimensioni, dati mancanti e composizione
# per industria e supersettore ICB.
# =====================================================================

# 1. Pacchetti ---------------------------------------------------------
library(readxl)
library(dplyr)

dir.create("output", showWarnings = FALSE)

# 2. Lettura del dataset completo (600 società dello STOXX Europe 600) --
dati_tutti <- read_excel("dati/DATASET_v5_valori.xlsx", sheet = "DATASET")

dim(dati_tutti)

# 3. Esito della colonna Campione (incluse ed escluse con il motivo) ----
table(dati_tutti$Campione)

# 4. Campione di analisi: solo le società incluse ----------------------
campione <- dati_tutti %>%
  filter(Campione == "Incluso")

dim(campione)

# 5. Controllo: nessun ticker ripetuto ---------------------------------
stopifnot(n_distinct(campione$Ticker) == nrow(campione))

# 6. Dati mancanti per colonna (solo le colonne con almeno un vuoto) ---
mancanti <- colSums(is.na(campione))
mancanti[mancanti > 0]

# 7. Composizione per industria e supersettore ICB ----------------------
table(campione$`Nome industria ICB`)
table(campione$`ICB Supersct Nm`)
