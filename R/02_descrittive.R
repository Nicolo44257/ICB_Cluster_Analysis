# =====================================================================
# 02_descrittive.R
# Tabella delle variabili candidate (con nomi brevi) e relative
# statistiche descrittive (capitolo 2, sezioni 2.2 e 2.4).
# =====================================================================

# 1. Dati: campione delle 445 società (script 01) ----------------------
source("R/01_lettura_e_controlli.R")

# 2. Variabili candidate con nomi brevi ----------------------------------
variabili <- campione %>%
  select(
    ticker        = Ticker,
    nome          = Name,
    supersettore  = `ICB Supersct Nm`,
    # Redditività
    marg_ebitda   = `EBITDA / Vend nette:Y`,
    marg_op       = `Mrg op:Y`,
    marg_netto    = `Prg ut:Y`,
    roa           = `ROA:Y`,
    roe           = `ROE:Y`,
    roic          = `ROIC:Y`,
    # Crescita
    cresc_ricavi  = `Vend-Cr 1an:Y`,
    # Struttura finanziaria
    debito_attivi = `Debito/Attivi:Y`,
    nd_ebitda     = `Net Debt to EBITDA:Y`,
    ebit          = `EBIT:Y`,
    int_pass      = `Int pass:Y`,
    # Liquidità
    current       = `Liq corr:Y`,
    quick         = `Quick Ratio:Y`,
    # Investimenti
    capex_vendite = `CAPEX/Sales:Y`,
    rot_attivo    = `Ast TO:Y`,
    rs_vendite    = `R&S/vendite (calc)`,
    # Valutazione
    ebitda_ev     = `EBITDA/EV (calc)`,
    book_price    = `Book-to-price (calc)`,
    div_yield     = `Dividend yield (calc)`,
    # Rischio
    volatilita    = starts_with("Volt"),
    beta_rett     = `Beta:Y-1`,
    beta_raw      = `Raw Beta:Y-1`,
    # Dimensione
    cap           = `Cap mercato`
  )

# 3. Variabili calcolate -------------------------------------------------
# Copertura degli interessi = EBIT / interessi passivi; non definita
# quando gli interessi passivi sono nulli.
# Capitalizzazione espressa in miliardi di euro.
variabili <- variabili %>%
  mutate(
    cop_interessi = ifelse(int_pass > 0, ebit / int_pass, NA),
    cap_mld       = cap / 1e9
  ) %>%
  select(-ebit, -int_pass, -cap)

dim(variabili)

# 4. Indice di asimmetria ---------------------------------------------------
# Media dei cubi degli scarti standardizzati: 0 = distribuzione simmetrica,
# positivo = coda lunga a destra, negativo = coda lunga a sinistra.
asimmetria <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}

# 5. Tabella delle statistiche descrittive ---------------------------------
num <- variabili %>% select(where(is.numeric))

descrittive <- data.frame(
  variabile  = names(num),
  n          = sapply(num, function(x) sum(!is.na(x))),
  mancanti   = sapply(num, function(x) sum(is.na(x))),
  media      = sapply(num, mean,     na.rm = TRUE),
  dev_std    = sapply(num, sd,       na.rm = TRUE),
  minimo     = sapply(num, min,      na.rm = TRUE),
  q1         = sapply(num, quantile, probs = 0.25, na.rm = TRUE),
  mediana    = sapply(num, median,   na.rm = TRUE),
  q3         = sapply(num, quantile, probs = 0.75, na.rm = TRUE),
  massimo    = sapply(num, max,      na.rm = TRUE),
  asimmetria = sapply(num, asimmetria),
  row.names  = NULL
)

descrittive[, -1] <- round(descrittive[, -1], 2)
descrittive

# 6. Salvataggio --------------------------------------------------------------
# I CSV usano il punto e virgola come separatore e la virgola decimale.
write.csv2(descrittive, "output/tab_descrittive.csv", row.names = FALSE)
