# =====================================================================
# 06_trattamento_anomali.R
# Trattamento dei valori anomali (capitolo 2, sezioni 2.3 e 2.5):
#   1) EBITDA/EV di BMW non significativo (EV negativo): mediana;
#   2) winsorizzazione al 1° e al 99° percentile delle 11 variabili;
#   3) logaritmo per current ratio e capex/vendite.
# Confronto prima/dopo e nuove matrici di correlazione.
# =====================================================================

# 1. Dati: le 11 variabili con il dato mancante sostituito (script 05) --
source("R/05_distribuzioni.R")

# 2. EBITDA/EV non significativo quando l'EV è negativo -------------------
ev <- campione %>%
  select(ticker = Ticker, ev = `EV (calc)`)

dati_cluster <- dati_cluster %>%
  left_join(ev, by = "ticker")

dati_cluster %>%
  filter(ev <= 0) %>%
  select(nome, ev, ebitda_ev)

# Il valore diventa mancante e viene sostituito con la mediana del campione,
# come per gli altri dati mancanti.
dati_cluster <- dati_cluster %>%
  mutate(
    ev_non_signif = ev <= 0,
    ebitda_ev     = ifelse(ev_non_signif, NA, ebitda_ev)
  )

mediana_ebitda_ev <- median(dati_cluster$ebitda_ev, na.rm = TRUE)
mediana_ebitda_ev

dati_cluster <- dati_cluster %>%
  mutate(ebitda_ev = ifelse(is.na(ebitda_ev), mediana_ebitda_ev, ebitda_ev))

sum(dati_cluster$ev_non_signif)
stopifnot(sum(is.na(dati_cluster[, var_cluster])) == 0)

# 3. Winsorizzazione ----------------------------------------------------------
# I valori sotto il percentile p sono portati al percentile p, quelli sopra
# il percentile 1 - p al percentile 1 - p.
winsorizza <- function(x, p = 0.01) {
  lim <- quantile(x, c(p, 1 - p))
  pmin(pmax(x, lim[1]), lim[2])
}

# 4. Limiti di winsorizzazione e numero di valori modificati -------------------
tab_winsor <- data.frame(
  variabile    = var_cluster,
  lim_basso    = sapply(var_cluster, function(v) quantile(dati_cluster[[v]], 0.01)),
  lim_alto     = sapply(var_cluster, function(v) quantile(dati_cluster[[v]], 0.99)),
  n_modificati = sapply(var_cluster, function(v) {
    x <- dati_cluster[[v]]
    sum(x < quantile(x, 0.01) | x > quantile(x, 0.99))
  }),
  row.names = NULL
)

# 5. Dataset trattato ----------------------------------------------------------
# dati_cluster conserva i valori originali; il risultato va in dati_tratt.
# Per capex/vendite si usa log(1 + x) perché la variabile contiene uno zero.
dati_tratt <- dati_cluster %>%
  mutate(across(all_of(var_cluster), winsorizza)) %>%
  mutate(
    current       = log(current),
    capex_vendite = log1p(capex_vendite)
  )

dim(dati_tratt)
stopifnot(sum(is.na(dati_tratt[, var_cluster])) == 0)

# 6. Asimmetria prima e dopo il trattamento ------------------------------------
tab_winsor$asim_prima <- sapply(var_cluster, function(v) asimmetria(dati_cluster[[v]]))
tab_winsor$asim_dopo  <- sapply(var_cluster, function(v) asimmetria(dati_tratt[[v]]))
tab_winsor[, -1] <- round(tab_winsor[, -1], 2)
tab_winsor

write.csv2(tab_winsor, "output/tab_trattamento.csv", row.names = FALSE)

# 7. Grafici prima/dopo per ogni variabile ---------------------------------------
# Istogramma originale, istogramma trattato, Q-Q plot dei dati trattati.
for (v in var_cluster) {
  png(paste0("output/fig_tratt_", v, ".png"),
      width = 3000, height = 1000, res = 300)
  par(mfrow = c(1, 3))

  hist(dati_cluster[[v]], breaks = 30, col = "grey80",
       main = paste("Prima:", v), xlab = "")
  hist(dati_tratt[[v]], breaks = 30, col = "grey60",
       main = paste("Dopo:", v), xlab = "")
  qqnorm(dati_tratt[[v]], main = "Q-Q plot dopo")
  qqline(dati_tratt[[v]], col = "red", lwd = 2)

  dev.off()
}
par(mfrow = c(1, 1))

# 8. Matrici di correlazione sui dati trattati ---------------------------------------
cor_p_tratt <- cor(dati_tratt[, var_cluster], method = "pearson")
cor_s_tratt <- cor(dati_tratt[, var_cluster], method = "spearman")

coppie_forti(cor_p_tratt, soglia = 0.5)
coppie_forti(cor_s_tratt, soglia = 0.5)

disegna(cor_p_tratt)

png("output/fig_correlazioni_pearson_tratt.png", width = 2400, height = 2400, res = 300)
disegna(cor_p_tratt)
dev.off()

png("output/fig_correlazioni_spearman_tratt.png", width = 2400, height = 2400, res = 300)
disegna(cor_s_tratt)
dev.off()

write.csv2(round(cor_p_tratt, 2), "output/tab_cor_pearson_tratt.csv")
write.csv2(round(cor_s_tratt, 2), "output/tab_cor_spearman_tratt.csv")

# 9. Salvataggio del dataset trattato -------------------------------------------
write.csv2(dati_tratt, "output/dati_trattati.csv", row.names = FALSE)
