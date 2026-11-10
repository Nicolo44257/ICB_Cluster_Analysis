# =====================================================================
# 12_confronto_icb.R
# Confronto tra i cluster e i 16 supersettori ICB (capitolo 4, sezioni
# 4.3 e 4.4), per la partizione principale (K = 4) e per K = 2:
# tabella di contingenza, rapporti, categorie dei supersettori, purezza
# dei cluster, ARI e scenario di riferimento.
# =====================================================================

# 1. Funzioni di confronto (caricano anche dplyr e mclust) ----------------
source("R/funzioni_confronto_icb.R")

cartella_out <- "output/capitolo_4"
dir.create(cartella_out, showWarnings = FALSE)

# 2. Dati: assegnazioni ai cluster (script 09) ---------------------------
assegnazioni <- readRDS("output/capitolo_3/cluster_assegnazioni.rds")

dim(assegnazioni)
table(assegnazioni$cluster_K4)
table(assegnazioni$cluster_K2)
stopifnot(sum(is.na(assegnazioni$supersettore)) == 0)
n_distinct(assegnazioni$supersettore)

sort(table(assegnazioni$supersettore))

# 3. Confronto a K = 4 (partizione principale) ---------------------------
# Il cluster 1 è il nucleo (profilo vicino alla media): i supersettori
# concentrati nel nucleo sono segnalati a parte.
conf_K4 <- confronta_con_icb(cluster        = assegnazioni$cluster_K4,
                             gruppi         = assegnazioni$supersettore,
                             cluster_nucleo = 1)

stopifnot(sum(conf_K4$contingenza) == nrow(assegnazioni))

conf_K4$contingenza
round(conf_K4$rapporti, 2)
conf_K4$categorie
table(conf_K4$categorie$categoria)
conf_K4$purezza
conf_K4$ari

# 4. Confronto a K = 2 ------------------------------------------------------
conf_K2 <- confronta_con_icb(cluster = assegnazioni$cluster_K2,
                             gruppi  = assegnazioni$supersettore)

conf_K2$contingenza
conf_K2$categorie
table(conf_K2$categorie$categoria)
conf_K2$purezza
conf_K2$ari

# 5. Salvataggio --------------------------------------------------------
# CSV con le quote in percentuale e un file RDS con i valori non
# arrotondati, usato dagli script 13, 14 e 15.
salva_confronto(conf_K4, cartella_out, "k4")
salva_confronto(conf_K2, cartella_out, "k2")

tab_ari <- bind_rows(cbind(K = 4, conf_K4$ari),
                     cbind(K = 2, conf_K2$ari)) %>%
  mutate(across(c(ARI_osservato, ARI_scenario), ~ round(.x, 3)))
tab_ari
write.csv2(tab_ari, file.path(cartella_out, "tab_ari_icb.csv"), row.names = FALSE)

saveRDS(list(K4 = conf_K4, K2 = conf_K2),
        file.path(cartella_out, "confronto_icb.rds"))
