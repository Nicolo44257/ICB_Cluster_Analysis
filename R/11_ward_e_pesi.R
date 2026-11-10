# =====================================================================
# 11_ward_e_pesi.R
# Due controlli di robustezza sulla costruzione dei cluster (capitolo 3,
# sezione 3.6; capitolo 4, sezione 4.6):
#   1) metodo di Ward allo stesso K del k-means;
#   2) k-means con pesi 1/radice(n) per dimensione, perché la valutazione
#      è rappresentata da 3 variabili su 11.
# Ogni alternativa è confrontata con la partizione principale tramite
# l'ARI, ai K = 4 e K = 6.
# La classificazione ICB non entra nei calcoli.
# =====================================================================

# 1. Pacchetti ---------------------------------------------------------
library(dplyr)
library(cluster)   # silhouette()
library(mclust)    # adjustedRandIndex()

# 2. Dati, K scelti e partizioni k-means (script 08 e 09) ----------------
dati_finali  <- readRDS("output/dati_finali.rds")
scelta_k     <- readRDS("output/capitolo_3/scelta_k.rds")
assegnazioni <- readRDS("output/capitolo_3/cluster_assegnazioni.rds")

var_cluster <- c("marg_op", "roa", "cresc_ricavi", "debito_attivi",
                 "current", "capex_vendite", "rot_attivo",
                 "ebitda_ev", "book_price", "div_yield", "volatilita")

X <- as.matrix(dati_finali[, var_cluster])
D <- dist(X, method = "euclidean")
n <- nrow(X)

stopifnot(all(dati_finali$ticker == assegnazioni$ticker))

K_controllo <- c(K4 = scelta_k$K_principale, K6 = scelta_k$K_fine)
K_controllo

# 3. Funzioni di supporto ------------------------------------------------
# Indice CH per una partizione qualsiasi (stessa formula dello script 08)
indice_ch <- function(M, cl) {
  K   <- length(unique(cl))
  tot <- sum(scale(M, scale = FALSE)^2)                  # devianza totale
  entro <- sum(sapply(unique(cl), function(k) {
    sum(scale(M[cl == k, , drop = FALSE], scale = FALSE)^2)
  }))
  ((tot - entro) / (K - 1)) / (entro / (nrow(M) - K))
}

# Rinumera i cluster per dimensione (1 = il più grande)
rinumera <- function(cl) {
  dimensioni <- sort(table(cl), decreasing = TRUE)
  match(cl, as.integer(names(dimensioni)))
}

round(indice_ch(X, assegnazioni$cluster_K4), 1)

# 4. Controllo 1: metodo di Ward ------------------------------------------
# "ward.D2" è la versione del metodo di Ward per le distanze euclidee
# (Charrad et al., 2014, p. 21). Il metodo è deterministico.
albero_ward <- hclust(D, method = "ward.D2")

ward <- lapply(K_controllo, function(K) rinumera(cutree(albero_ward, k = K)))

# 5. Controllo 2: pesi per dimensione ----------------------------------------
# Ogni variabile riceve peso 1 / radice(n), con n = numero di variabili
# della sua dimensione (sezione 1.6): nelle distanze al quadrato ogni
# dimensione conta come una sola variabile.
dimensione <- c(marg_op = "Redditivita'", roa = "Redditivita'",
                cresc_ricavi = "Crescita",
                debito_attivi = "Struttura finanziaria",
                current = "Liquidita'",
                capex_vendite = "Investimenti", rot_attivo = "Investimenti",
                ebitda_ev = "Valutazione", book_price = "Valutazione",
                div_yield = "Valutazione",
                volatilita = "Rischio")

n_per_dim <- table(dimensione)
n_per_dim

pesi <- 1 / sqrt(as.numeric(n_per_dim[dimensione[var_cluster]]))
names(pesi) <- var_cluster
round(pesi, 3)

X_pesi <- sweep(X, 2, pesi, `*`)
D_pesi <- dist(X_pesi, method = "euclidean")

# K-means con le stesse impostazioni dello script 08
km_pesi <- lapply(K_controllo, function(K) {
  set.seed(scelta_k$seme)
  km <- kmeans(X_pesi, centers = K, nstart = scelta_k$n_ripartenze,
               iter.max = scelta_k$n_iter)
  rinumera(km$cluster)
})

# 6. Tabella riassuntiva dei confronti ------------------------------------
# Per ogni K: ARI con la partizione principale, CH e silhouette media.
# Ogni partizione è valutata sui dati con cui è stata costruita (dati
# pesati per la variante con pesi).
principale <- list(K4 = assegnazioni$cluster_K4, K6 = assegnazioni$cluster_K6)

riga <- function(nome_K, metodo, cl, M, Dm) {
  data.frame(
    K                 = K_controllo[[nome_K]],
    partizione        = metodo,
    ARI_con_principale = adjustedRandIndex(principale[[nome_K]], cl),
    CH                = indice_ch(M, cl),
    silhouette_media  = mean(silhouette(cl, Dm)[, "sil_width"]),
    dimensioni        = paste(sort(table(cl), decreasing = TRUE), collapse = " / ")
  )
}

tab_robustezza <- bind_rows(lapply(names(K_controllo), function(nome_K) {
  bind_rows(
    riga(nome_K, "k-means (principale)", principale[[nome_K]], X, D),
    riga(nome_K, "Ward",                 ward[[nome_K]],       X, D),
    riga(nome_K, "k-means con pesi per dimensione", km_pesi[[nome_K]], X_pesi, D_pesi)
  )
})) %>%
  mutate(ARI_con_principale = round(ARI_con_principale, 3),
         CH = round(CH, 1), silhouette_media = round(silhouette_media, 3))

tab_robustezza

write.csv2(tab_robustezza, "output/capitolo_3/tab_robustezza_costruzione.csv",
           row.names = FALSE)

# 7. Tabelle incrociate a K = 4 ------------------------------------------------
# Righe: cluster del k-means principale; colonne: cluster dell'alternativa.
incrocio_ward <- table(kmeans = principale$K4, ward = ward$K4)
incrocio_pesi <- table(kmeans = principale$K4, pesi = km_pesi$K4)
incrocio_ward
incrocio_pesi

write.csv2(as.data.frame.matrix(incrocio_ward),
           "output/capitolo_3/tab_incrocio_kmeans_ward_k4.csv")
write.csv2(as.data.frame.matrix(incrocio_pesi),
           "output/capitolo_3/tab_incrocio_kmeans_pesi_k4.csv")

# 8. Profili dei cluster alternativi a K = 4 ---------------------------------
# Medie delle 11 variabili standardizzate (senza pesi) per cluster.
profili <- function(cl, metodo) {
  data.frame(partizione = metodo, cluster = sort(unique(cl)),
             n_societa = as.vector(table(cl)),
             round(apply(X, 2, function(v) tapply(v, cl, mean)), 2),
             row.names = NULL)
}

tab_profili_alt <- bind_rows(profili(ward$K4, "Ward"),
                             profili(km_pesi$K4, "k-means con pesi per dimensione"))
tab_profili_alt

write.csv2(tab_profili_alt, "output/capitolo_3/tab_profili_alternativi_k4.csv",
           row.names = FALSE)

# 9. Salvataggio delle partizioni alternative (usate nello script 14) ----------
alternative <- assegnazioni %>%
  select(ticker, nome) %>%
  mutate(ward_K4 = ward$K4, ward_K6 = ward$K6,
         pesi_K4 = km_pesi$K4, pesi_K6 = km_pesi$K6)

saveRDS(alternative, "output/capitolo_3/cluster_alternativi.rds")
write.csv2(alternative, "output/capitolo_3/cluster_alternativi.csv", row.names = FALSE)
