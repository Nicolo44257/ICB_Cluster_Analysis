# =====================================================================
# 10_pca.R
# Analisi delle componenti principali (PCA) come supporto alla lettura
# dei cluster (capitolo 3, sezione 3.5): non è un input del clustering.
#   1) autovalori e varianza spiegata (scree plot);
#   2) loadings delle variabili;
#   3) società sul piano PC1-PC2 per cluster (K = 4) e loadings sullo
#      stesso piano.
# Riferimento: James et al. (2013), par. 10.2.
# La classificazione ICB non entra nei calcoli.
# =====================================================================

# 1. Pacchetti ---------------------------------------------------------
library(dplyr)

# 2. Dati -----------------------------------------------------------------
dati_finali  <- readRDS("output/dati_finali.rds")
assegnazioni <- readRDS("output/capitolo_3/cluster_assegnazioni.rds")

var_cluster <- c("marg_op", "roa", "cresc_ricavi", "debito_attivi",
                 "current", "capex_vendite", "rot_attivo",
                 "ebitda_ev", "book_price", "div_yield", "volatilita")

# Etichette per tabelle e grafici
etichette <- c(marg_op = "Margine operativo", roa = "ROA",
               cresc_ricavi = "Crescita dei ricavi", debito_attivi = "Debito/Attivi",
               current = "Current ratio (log)", capex_vendite = "Capex/vendite (log)",
               rot_attivo = "Rotazione dell'attivo", ebitda_ev = "EBITDA/EV",
               book_price = "Book-to-price", div_yield = "Dividend yield",
               volatilita = "Volatilità")

X <- as.matrix(dati_finali[, var_cluster])

stopifnot(all(dati_finali$ticker == assegnazioni$ticker))

# 3. PCA -------------------------------------------------------------------
# Le variabili sono già standardizzate: la PCA lavora sulla matrice di
# correlazione (James et al., 2013, pp. 380-382).
pca <- prcomp(X, center = TRUE, scale. = TRUE)

# 4. Autovalori e varianza spiegata -------------------------------------
# Autovalore = varianza della componente. Con 11 variabili standardizzate
# la varianza totale è 11.
autovalori <- pca$sdev^2
sum(autovalori)

# Controllo: autovalori uguali a quelli della matrice di correlazione
stopifnot(isTRUE(all.equal(autovalori, eigen(cor(X))$values)))

tab_varianza <- data.frame(
  componente        = paste0("PC", seq_along(autovalori)),
  autovalore        = round(autovalori, 2),
  quota_spiegata    = round(autovalori / sum(autovalori) * 100, 1),
  quota_cumulata    = round(cumsum(autovalori) / sum(autovalori) * 100, 1)
)
tab_varianza

write.csv2(tab_varianza, "output/capitolo_3/tab_pca_varianza.csv", row.names = FALSE)

# 5. Loadings delle prime 5 componenti ----------------------------------------
# Pesi delle variabili in ogni componente (James et al., 2013, pp. 375-376).
# Il segno di una componente è arbitrario: conta il confronto tra le variabili.
tab_loadings <- data.frame(
  variabile = etichette[rownames(pca$rotation)],
  round(pca$rotation[, 1:5], 2),
  row.names = NULL
)
tab_loadings

write.csv2(tab_loadings, "output/capitolo_3/tab_pca_loadings.csv", row.names = FALSE)

# 6. Scree plot e varianza cumulata ---------------------------------------
disegna_scree <- function() {
  par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
  plot(seq_along(autovalori), autovalori, type = "b", pch = 19,
       xlab = "Componente principale", ylab = "Autovalore",
       main = "Scree plot", xaxt = "n")
  axis(1, at = seq_along(autovalori))
  plot(seq_along(autovalori), cumsum(autovalori) / sum(autovalori) * 100,
       type = "b", pch = 19, ylim = c(0, 100),
       xlab = "Componente principale", ylab = "Varianza spiegata cumulata (%)",
       main = "Varianza cumulata", xaxt = "n")
  axis(1, at = seq_along(autovalori))
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
}

disegna_scree()

png("output/capitolo_3/fig_pca_scree.png", width = 3000, height = 1300, res = 300)
disegna_scree()
dev.off()

# 7. Società sul piano PC1-PC2, per cluster (K = 4) -------------------------
# Colori e simboli diversi per cluster; il numero grande indica la media
# del cluster sul piano.
cl      <- assegnazioni$cluster_K4
colori  <- c("grey60", "#1b6ca8", "#d1495b", "#2e8b57")
simboli <- c(16, 17, 15, 18)
q1 <- tab_varianza$quota_spiegata[1]
q2 <- tab_varianza$quota_spiegata[2]

disegna_piano <- function() {
  par(mar = c(5, 5, 3, 1))
  plot(pca$x[, 1], pca$x[, 2], col = colori[cl], pch = simboli[cl], cex = 0.8,
       xlab = paste0("PC1 (", q1, "% della varianza)"),
       ylab = paste0("PC2 (", q2, "% della varianza)"),
       main = "Società sul piano delle prime due componenti (K = 4)")
  abline(h = 0, v = 0, lty = 3, col = "grey40")
  medie <- aggregate(pca$x[, 1:2], by = list(cluster = cl), FUN = mean)
  text(medie$PC1, medie$PC2, labels = medie$cluster, cex = 1.8, font = 2)
  legend("topright", legend = paste("Cluster", 1:4), col = colori,
         pch = simboli, bty = "n", cex = 0.9)
  par(mar = c(5, 4, 4, 2))
}

disegna_piano()

png("output/capitolo_3/fig_pca_cluster.png", width = 2400, height = 2200, res = 300)
disegna_piano()
dev.off()

# 8. Loadings sul piano PC1-PC2 ------------------------------------------------
# Ogni freccia indica la direzione verso cui spinge un valore alto della
# variabile.
disegna_loadings <- function() {
  par(mar = c(5, 5, 3, 1))
  L <- pca$rotation[, 1:2]
  plot(NA, xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.8), asp = 1,
       xlab = paste0("PC1 (", q1, "%)"), ylab = paste0("PC2 (", q2, "%)"),
       main = "Loadings delle variabili su PC1 e PC2")
  abline(h = 0, v = 0, lty = 3, col = "grey40")
  arrows(0, 0, L[, 1], L[, 2], length = 0.08, col = "grey30")
  text(L[, 1] * 1.15, L[, 2] * 1.15, labels = etichette[rownames(L)], cex = 0.75)
  par(mar = c(5, 4, 4, 2))
}

disegna_loadings()

png("output/capitolo_3/fig_pca_loadings.png", width = 2400, height = 2400, res = 300)
disegna_loadings()
dev.off()
