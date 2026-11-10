# =====================================================================
# 08_scelta_k.R
# Scelta del numero di cluster K (capitolo 3, sezione 3.4; capitolo 4,
# sezione 4.1):
#   1) k-means per K da 2 a 15 (100 ripartenze, seme fisso);
#   2) per ogni K: devianza entro i gruppi (gomito), indice di
#      Calinski-Harabasz (CH) e silhouette media;
#   3) applicazione della regola di scelta: K principale e K più fine;
#   4) controllo di stabilità con semi diversi.
# La classificazione ICB non entra nei calcoli.
# =====================================================================

# 1. Pacchetti ---------------------------------------------------------
library(dplyr)
library(cluster)   # silhouette()
library(mclust)    # adjustedRandIndex()

dir.create("output/capitolo_3", showWarnings = FALSE)

# 2. Dati: le 11 variabili standardizzate (script 07) -------------------
dati_finali <- readRDS("output/dati_finali.rds")
dim(dati_finali)

var_cluster <- c("marg_op", "roa", "cresc_ricavi", "debito_attivi",
                 "current", "capex_vendite", "rot_attivo",
                 "ebitda_ev", "book_price", "div_yield", "volatilita")

# Matrice 445 x 11: unico input del clustering
X <- as.matrix(dati_finali[, var_cluster])
n <- nrow(X)

# Controlli: nessun mancante, medie 0, deviazioni standard 1
stopifnot(sum(is.na(X)) == 0)
round(colMeans(X), 3)
round(apply(X, 2, sd), 3)

# 3. Distanze euclidee tra le società (per la silhouette) ----------------
D <- dist(X, method = "euclidean")

# 4. Impostazioni del k-means ---------------------------------------------
K_valori     <- 2:15
n_ripartenze <- 100    # nstart: si tiene la migliore di 100 partenze casuali
n_iter       <- 100    # iter.max
seme         <- 123

# 5. K-means per ogni K e indici di valutazione ----------------------------
# - devianza entro i gruppi (tot.withinss): metodo del gomito;
# - CH = [devianza tra / (K - 1)] / [devianza entro / (n - K)]
#   (Calinski e Harabasz, 1974, formula (2), p. 10);
# - silhouette media (Rousseeuw, 1987).
# Il seme è reimpostato per ogni K, così il risultato di un K non dipende
# dagli altri.
risultati <- data.frame()

for (K in K_valori) {
  set.seed(seme)
  km <- kmeans(X, centers = K, nstart = n_ripartenze, iter.max = n_iter)

  ch  <- (km$betweenss / (K - 1)) / (km$tot.withinss / (n - K))
  sil <- mean(silhouette(km$cluster, D)[, "sil_width"])

  risultati <- rbind(risultati, data.frame(
    K                   = K,
    devianza_entro      = km$tot.withinss,
    CH                  = ch,
    silhouette_media    = sil,
    cluster_piu_piccolo = min(km$size),
    cluster_piu_grande  = max(km$size)
  ))
}

# Riduzione della devianza entro i gruppi passando da K - 1 a K
risultati <- risultati %>%
  mutate(calo_devianza = lag(devianza_entro) - devianza_entro) %>%
  relocate(calo_devianza, .after = devianza_entro)

risultati

# 6. Regola di scelta del K principale -----------------------------------
ch_val  <- risultati$CH
K_ch    <- risultati$K[which.max(ch_val)]

# Massimi locali interni del CH (esclusi gli estremi dell'intervallo)
interni <- 2:(length(ch_val) - 1)
massimi_locali <- risultati$K[interni][ch_val[interni] > ch_val[interni - 1] &
                                       ch_val[interni] > ch_val[interni + 1]]
massimi_locali

ch_sempre_decrescente <- all(diff(ch_val) < 0)
ch_sempre_decrescente

if (K_ch > 2) {
  # Caso 1: massimo del CH all'interno dell'intervallo.
  K_principale <- K_ch
  ramo <- "massimo del CH"
} else if (ch_sempre_decrescente) {
  # Caso 2: CH massimo a K = 2 e poi sempre decrescente (Calinski e
  # Harabasz, 1974, pp. 11-12): si sceglie il K con la silhouette media
  # più alta (Rousseeuw, 1987, p. 59).
  K_principale <- risultati$K[which.max(risultati$silhouette_media)]
  ramo <- "CH decrescente da K = 2: massimo della silhouette"
} else {
  # Caso 3: CH massimo a K = 2 con massimi locali successivi; la regola
  # non individua un K.
  K_principale <- NA
  ramo <- "caso non previsto dalla regola"
}

ramo
K_principale

# K più fine (controllo di robustezza): tra i K maggiori del principale,
# quello con la silhouette media più alta.
if (!is.na(K_principale) && K_principale < max(K_valori)) {
  oltre  <- risultati %>% filter(K > K_principale)
  K_fine <- oltre$K[which.max(oltre$silhouette_media)]
} else {
  K_fine <- NA
}
K_fine

# 7. Controllo di stabilità -----------------------------------------------
# Per il K principale e il K più fine il k-means viene ripetuto con 5 semi
# diversi e ogni partizione è confrontata con quella del seme 123 tramite
# l'ARI (1 = stessa partizione) e la devianza entro i gruppi.
stabilita <- data.frame()

for (K in na.omit(c(K_principale, K_fine))) {
  set.seed(seme)
  km_rif <- kmeans(X, centers = K, nstart = n_ripartenze, iter.max = n_iter)
  for (s in c(1, 2, 3, 4, 5)) {
    set.seed(s)
    km_s <- kmeans(X, centers = K, nstart = n_ripartenze, iter.max = n_iter)
    stabilita <- rbind(stabilita, data.frame(
      K                  = K,
      seme               = s,
      ARI_con_seme_123   = adjustedRandIndex(km_rif$cluster, km_s$cluster),
      devianza_seme_123  = km_rif$tot.withinss,
      devianza_seme      = km_s$tot.withinss
    ))
  }
}

stabilita

# 8. Salvataggio ------------------------------------------------------------
tab_scelta_k <- risultati %>%
  mutate(across(c(devianza_entro, calo_devianza, CH), ~ round(.x, 1)),
         silhouette_media = round(silhouette_media, 3))
write.csv2(tab_scelta_k, "output/capitolo_3/tab_scelta_k.csv", row.names = FALSE)

write.csv2(stabilita %>% mutate(across(where(is.double), ~ round(.x, 3))),
           "output/capitolo_3/tab_stabilita_k.csv", row.names = FALSE)

# K scelti e impostazioni del k-means, letti dagli script successivi
saveRDS(list(K_principale = K_principale, K_fine = K_fine, ramo = ramo,
             seme = seme, n_ripartenze = n_ripartenze, n_iter = n_iter),
        "output/capitolo_3/scelta_k.rds")

# 9. Grafico: gomito, CH e silhouette ----------------------------------------
# Linea rossa: K principale; linea grigia: K più fine.
disegna_k <- function() {
  par(mfrow = c(1, 3), mar = c(5, 5, 3, 1))
  plot(risultati$K, risultati$devianza_entro, type = "b", pch = 19,
       xlab = "Numero di cluster (K)", ylab = "Devianza entro i gruppi",
       main = "Gomito")
  abline(v = c(K_principale, K_fine), col = c("red", "grey50"), lty = 2)
  plot(risultati$K, risultati$CH, type = "b", pch = 19,
       xlab = "Numero di cluster (K)", ylab = "Indice CH",
       main = "Calinski-Harabasz")
  abline(v = c(K_principale, K_fine), col = c("red", "grey50"), lty = 2)
  plot(risultati$K, risultati$silhouette_media, type = "b", pch = 19,
       xlab = "Numero di cluster (K)", ylab = "Silhouette media",
       main = "Silhouette")
  abline(v = c(K_principale, K_fine), col = c("red", "grey50"), lty = 2)
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
}

disegna_k()

png("output/capitolo_3/fig_scelta_k.png", width = 3600, height = 1300, res = 300)
disegna_k()
dev.off()
