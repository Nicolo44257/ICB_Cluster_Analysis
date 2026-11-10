# =====================================================================
# 04_variabili_scelte.R
# Le 11 variabili scelte per il clustering: dati mancanti, correlazioni
# residue e valori estremi (capitolo 2, sezioni 2.4 e 2.5).
# =====================================================================

# 1. Dati: variabili candidate e matrici di correlazione (script 03) ---
source("R/03_correlazioni.R")

# 2. Le 11 variabili del clustering, raggruppate per dimensione --------
var_cluster <- c(
  "marg_op", "roa",                          # redditività
  "cresc_ricavi",                            # crescita
  "debito_attivi",                           # struttura finanziaria
  "current",                                 # liquidità
  "capex_vendite", "rot_attivo",             # investimenti
  "ebitda_ev", "book_price", "div_yield",    # valutazione
  "volatilita"                               # rischio
)

# 3. Tabella per il clustering ------------------------------------------
dati_cluster <- variabili %>%
  select(ticker, nome, supersettore, all_of(var_cluster))

dim(dati_cluster)

# 4. Dati mancanti -------------------------------------------------------
colSums(is.na(dati_cluster))

dati_cluster %>%
  filter(!complete.cases(dati_cluster)) %>%
  select(nome, supersettore)

# 5. Correlazioni residue tra le 11 variabili -----------------------------
sp11 <- cor_spearman[var_cluster, var_cluster]
diag(sp11) <- NA
max(abs(sp11), na.rm = TRUE)
coppie_forti(sp11, soglia = 0.5)

# 6. Valori estremi: le 3 società più basse e più alte per variabile ------
estremi <- function(dati, v, k = 3) {
  ordinati <- dati %>%
    filter(!is.na(.data[[v]])) %>%
    arrange(.data[[v]])
  bind_rows(
    head(ordinati, k) %>% mutate(lato = "minimi"),
    tail(ordinati, k) %>% mutate(lato = "massimi")
  ) %>%
    transmute(variabile = v, lato, nome, supersettore,
              valore = round(.data[[v]], 2))
}

tab_estremi <- bind_rows(lapply(var_cluster, function(v) estremi(dati_cluster, v)))
tab_estremi

# 7. Salvataggio -----------------------------------------------------------
write.csv2(tab_estremi, "output/tab_estremi.csv", row.names = FALSE)
