# =====================================================================
# 07_standardizzazione_finale.R
# Chiusura della preparazione dei dati (capitolo 2):
#   1) composizione dell'indice e del campione (Tabelle 1.2 e 2.1);
#   2) statistiche delle 11 variabili dopo il trattamento;
#   3) standardizzazione (sezione 2.6);
#   4) dataset finale 445 x 11 (sezione 2.7), salvato per i capitoli 3 e 4.
# =====================================================================

# 1. Dati: variabili trattate (script 06) --------------------------------
source("R/06_trattamento_anomali.R")

# 2. Esito della selezione del campione ------------------------------------
table(dati_tutti$Campione)

# 3. Composizione dei 600 per industria e supersettore (Tabella 1.2) --------
tab_12 <- dati_tutti %>%
  count(industria = `Nome industria ICB`, supersettore = `ICB Supersct Nm`)
tab_12
sum(tab_12$n)
write.csv2(tab_12, "output/tab_composizione_600.csv", row.names = FALSE)

# 4. Statistiche delle 11 variabili dopo il trattamento ---------------------
# Scala originale, prima della standardizzazione; current ratio e
# capex/vendite sono già in logaritmo.
tab_descr_tratt <- data.frame(
  variabile = var_cluster,
  media     = sapply(var_cluster, function(v) mean(dati_tratt[[v]])),
  dev_std   = sapply(var_cluster, function(v) sd(dati_tratt[[v]])),
  minimo    = sapply(var_cluster, function(v) min(dati_tratt[[v]])),
  mediana   = sapply(var_cluster, function(v) median(dati_tratt[[v]])),
  massimo   = sapply(var_cluster, function(v) max(dati_tratt[[v]])),
  row.names = NULL
)
tab_descr_tratt[, -1] <- round(tab_descr_tratt[, -1], 2)
tab_descr_tratt
write.csv2(tab_descr_tratt, "output/tab_descrittive_trattate.csv", row.names = FALSE)

# 5. Standardizzazione (z-score) ----------------------------------------------
# Ogni variabile ha media 0 e deviazione standard 1, così che nessuna pesi
# di più nelle distanze per effetto della sola unità di misura.
standardizza <- function(x) (x - mean(x)) / sd(x)

dati_std <- dati_tratt %>%
  mutate(across(all_of(var_cluster), standardizza))

round(sapply(var_cluster, function(v) mean(dati_std[[v]])), 3)
round(sapply(var_cluster, function(v) sd(dati_std[[v]])), 3)

# Valori minimi e massimi in deviazioni standard
tab_z <- data.frame(
  variabile = var_cluster,
  z_min     = sapply(var_cluster, function(v) min(dati_std[[v]])),
  z_max     = sapply(var_cluster, function(v) max(dati_std[[v]])),
  row.names = NULL
)
tab_z[, -1] <- round(tab_z[, -1], 2)
tab_z

# 6. Grafico prima/dopo la standardizzazione ------------------------------------
disegna_std <- function() {
  par(mfrow = c(2, 1), mar = c(7, 4, 3, 1))
  boxplot(as.data.frame(dati_tratt[, var_cluster]), las = 2, cex.axis = 0.8,
          col = "grey80", main = "Prima della standardizzazione")
  boxplot(as.data.frame(dati_std[, var_cluster]), las = 2, cex.axis = 0.8,
          col = "grey60", main = "Dopo la standardizzazione (z-score)")
  abline(h = 0, col = "red", lty = 2)
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
}

disegna_std()

png("output/fig_standardizzazione.png", width = 2400, height = 2400, res = 300)
disegna_std()
dev.off()

# 7. Dataset finale ----------------------------------------------------------------
# Industria, supersettore e settore ICB sono conservati per il confronto
# del capitolo 4, ma non entrano nel clustering.
icb <- campione %>%
  select(ticker = Ticker, industria = `Nome industria ICB`, settore = `Nome settore ICB`)

dati_finali <- dati_std %>%
  left_join(icb, by = "ticker") %>%
  select(ticker, nome, industria, supersettore, settore, all_of(var_cluster))

dim(dati_finali)
stopifnot(sum(is.na(dati_finali)) == 0)

# 8. Composizione per supersettore prima e dopo le esclusioni (Tabella 2.1) --------
comp_449 <- dati_tutti %>%
  filter(!`Nome industria ICB` %in% c("Financials", "Real Estate")) %>%
  count(supersettore = `ICB Supersct Nm`, name = "n_449")

comp_445 <- dati_finali %>%
  count(supersettore, name = "n_445")

tab_comp <- comp_449 %>%
  left_join(comp_445, by = "supersettore") %>%
  mutate(differenza = n_445 - n_449)
tab_comp
write.csv2(tab_comp, "output/tab_composizione_finale.csv", row.names = FALSE)

table(dati_finali$industria)

# 9. Salvataggio del dataset finale ---------------------------------------------
# Il file RDS è il punto di partenza degli script dei capitoli 3 e 4.
write.csv2(dati_finali, "output/dati_finali.csv", row.names = FALSE)
saveRDS(dati_finali, "output/dati_finali.rds")
