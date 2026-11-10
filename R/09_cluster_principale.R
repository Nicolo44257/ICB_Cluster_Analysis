# =====================================================================
# 09_cluster_principale.R
# Partizioni k-means ai tre K usati nella tesi (capitolo 4, sezioni 4.1
# e 4.2):
#   - K principale (4): partizione su cui si basa il confronto con l'ICB;
#   - K = 2: prima rappresentazione della struttura dei dati;
#   - K più fine (6): controllo di robustezza.
# Per ogni K: dimensioni dei cluster, centroidi, silhouette di ogni
# società e legame tra le partizioni.
# Le colonne ICB non entrano nei calcoli: sono solo riportate nel file
# delle assegnazioni usato dagli script del capitolo 4.
# =====================================================================

# 1. Pacchetti ---------------------------------------------------------
library(dplyr)
library(cluster)   # silhouette()
library(mclust)    # adjustedRandIndex()

# 2. Dati e K scelti (script 08) -----------------------------------------
dati_finali <- readRDS("output/dati_finali.rds")
scelta_k    <- readRDS("output/capitolo_3/scelta_k.rds")
scelta_k

var_cluster <- c("marg_op", "roa", "cresc_ricavi", "debito_attivi",
                 "current", "capex_vendite", "rot_attivo",
                 "ebitda_ev", "book_price", "div_yield", "volatilita")

X <- as.matrix(dati_finali[, var_cluster])
D <- dist(X, method = "euclidean")

K_scelti <- c(K2 = 2, K4 = scelta_k$K_principale, K6 = scelta_k$K_fine)
K_scelti

# 3. K-means con le impostazioni dello script 08 ---------------------------
# I numeri dei cluster restituiti da kmeans() sono arbitrari: i cluster
# vengono rinumerati per dimensione (1 = il più grande).
esegui_kmeans <- function(K) {
  set.seed(scelta_k$seme)
  km <- kmeans(X, centers = K, nstart = scelta_k$n_ripartenze,
               iter.max = scelta_k$n_iter)
  dimensioni <- sort(table(km$cluster), decreasing = TRUE)
  vecchi     <- as.integer(names(dimensioni))
  list(
    km        = km,
    cluster   = match(km$cluster, vecchi),
    centroidi = km$centers[vecchi, , drop = FALSE]
  )
}

risultati <- lapply(K_scelti, esegui_kmeans)

sapply(risultati, function(r) round(r$km$tot.withinss, 1))

# 4. Silhouette di ogni società ---------------------------------------
# Per ogni società: cluster, cluster vicino (neighbor) e s(i) (sil_width).
sil <- lapply(risultati, function(r) silhouette(r$cluster, D))

sapply(sil, function(s) round(mean(s[, "sil_width"]), 3))

# 5. Dimensioni e qualità di ogni cluster ------------------------------
# Numero di società, quota del campione, silhouette media del cluster
# (Rousseeuw, 1987) e quota di società con s(i) negativa.
tab_cluster <- bind_rows(lapply(names(K_scelti), function(nome) {
  s <- sil[[nome]]
  data.frame(cluster = s[, "cluster"], sil = s[, "sil_width"]) %>%
    group_by(cluster) %>%
    summarise(n_societa       = n(),
              quota_campione  = round(n() / nrow(X) * 100, 1),
              silhouette_media = round(mean(sil), 3),
              quota_s_negativa = round(mean(sil < 0) * 100, 1),
              .groups = "drop") %>%
    mutate(K = K_scelti[[nome]], .before = cluster)
}))

tab_cluster
min(tab_cluster$n_societa)

write.csv2(tab_cluster, "output/capitolo_3/tab_cluster.csv", row.names = FALSE)

# 6. Centroidi (medie delle variabili standardizzate per cluster) ---------
# Valori in deviazioni standard dalla media del campione.
tab_centroidi <- bind_rows(lapply(names(K_scelti), function(nome) {
  data.frame(K = K_scelti[[nome]],
             cluster = seq_len(K_scelti[[nome]]),
             round(risultati[[nome]]$centroidi, 2),
             row.names = NULL)
}))

tab_centroidi

write.csv2(tab_centroidi, "output/capitolo_3/tab_centroidi.csv", row.names = FALSE)

# 7. Legame tra le partizioni ------------------------------------------------
# Tabelle incrociate tra i cluster a K = 2, 4 e 6 e ARI tra le partizioni.
incrocio_2_4 <- table(K2 = risultati$K2$cluster, K4 = risultati$K4$cluster)
incrocio_4_6 <- table(K4 = risultati$K4$cluster, K6 = risultati$K6$cluster)
incrocio_2_4
incrocio_4_6

ari_tra_K <- data.frame(
  confronto = c("K2 - K4", "K4 - K6"),
  ARI = round(c(adjustedRandIndex(risultati$K2$cluster, risultati$K4$cluster),
                adjustedRandIndex(risultati$K4$cluster, risultati$K6$cluster)), 3)
)
ari_tra_K

write.csv2(as.data.frame.matrix(incrocio_2_4),
           "output/capitolo_3/tab_incrocio_k2_k4.csv")
write.csv2(as.data.frame.matrix(incrocio_4_6),
           "output/capitolo_3/tab_incrocio_k4_k6.csv")
write.csv2(ari_tra_K, "output/capitolo_3/tab_ari_tra_k.csv", row.names = FALSE)

# 8. File delle assegnazioni ------------------------------------------------------
# Una riga per società: cluster ai tre K, s(i) e cluster vicino.
assegnazioni <- dati_finali %>%
  select(ticker, nome, industria, supersettore, settore) %>%
  mutate(cluster_K2 = risultati$K2$cluster,
         cluster_K4 = risultati$K4$cluster,
         cluster_K6 = risultati$K6$cluster,
         sil_K4     = round(sil$K4[, "sil_width"], 3),
         vicino_K4  = sil$K4[, "neighbor"],
         sil_K6     = round(sil$K6[, "sil_width"], 3))

dim(assegnazioni)
write.csv2(assegnazioni, "output/capitolo_3/cluster_assegnazioni.csv",
           row.names = FALSE)
saveRDS(assegnazioni, "output/capitolo_3/cluster_assegnazioni.rds")

# 9. Grafico dei centroidi a K = 4 ----------------------------------------------
disegna_centroidi <- function(centroidi) {
  K <- nrow(centroidi)
  par(mfrow = c(ceiling(K / 2), 2), mar = c(4, 9, 3, 1))
  lim <- range(c(centroidi, -1, 1))
  for (k in seq_len(K)) {
    n_k <- sum(risultati$K4$cluster == k)
    barplot(rev(centroidi[k, ]), horiz = TRUE, las = 1, cex.names = 0.8,
            xlim = lim, col = ifelse(rev(centroidi[k, ]) > 0, "grey40", "grey75"),
            main = paste0("Cluster ", k, " (", n_k, " societa')"),
            xlab = "Media standardizzata")
    abline(v = 0, lty = 2)
  }
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
}

disegna_centroidi(risultati$K4$centroidi)

png("output/capitolo_3/fig_centroidi_k4.png", width = 2400, height = 2400, res = 300)
disegna_centroidi(risultati$K4$centroidi)
dev.off()

# 10. Grafico della silhouette a K = 4 (Rousseeuw, 1987) ------------------------
png("output/capitolo_3/fig_silhouette_k4.png", width = 2400, height = 2400, res = 300)
plot(sil$K4, border = NA, col = "grey50",
     main = "Silhouette delle societa' (K = 4)")
dev.off()

plot(sil$K4, border = NA, col = "grey50",
     main = "Silhouette delle societa' (K = 4)")
