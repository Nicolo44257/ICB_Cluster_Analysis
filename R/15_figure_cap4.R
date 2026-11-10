# =====================================================================
# 15_figure_cap4.R
# Figure del capitolo 4, ottenute dagli output degli script 08-12:
#   Fig. 4.1 gomito, CH e silhouette per K da 2 a 15;
#   Fig. 4.2 centroidi dei 4 cluster;
#   Fig. 4.3 società sul piano PC1-PC2 con le frecce dei coefficienti;
#   Fig. 4.4 heatmap 16 supersettori x 4 cluster (conteggio e rapporto r).
# Le figure non hanno titolo: il titolo è nella didascalia della tesi.
# =====================================================================

# 1. Dati -----------------------------------------------------------------
cartella_out <- "output/capitolo_4"

tab_k         <- read.csv2("output/capitolo_3/tab_scelta_k.csv")
scelta_k      <- readRDS("output/capitolo_3/scelta_k.rds")
dati_finali   <- readRDS("output/dati_finali.rds")
assegnazioni  <- readRDS("output/capitolo_3/cluster_assegnazioni.rds")
confronto_icb <- readRDS(file.path(cartella_out, "confronto_icb.rds"))

stopifnot(all(dati_finali$ticker == assegnazioni$ticker))

var_cluster <- c("marg_op", "roa", "cresc_ricavi", "debito_attivi",
                 "current", "capex_vendite", "rot_attivo",
                 "ebitda_ev", "book_price", "div_yield", "volatilita")

# Etichette (current ratio e capex/vendite sono in logaritmo)
etichette <- c(marg_op = "Margine operativo", roa = "ROA",
               cresc_ricavi = "Crescita dei ricavi", debito_attivi = "Debito/Attivi",
               current = "Current ratio (log)", capex_vendite = "Capex/vendite (log)",
               rot_attivo = "Rotazione dell'attivo", ebitda_ev = "EBITDA/EV",
               book_price = "Book-to-price", div_yield = "Dividend yield",
               volatilita = "Volatilità")

# 2. Colori e simboli comuni a tutte le figure ----------------------------------
# Nucleo (cluster 1) in grigio chiaro; cluster 2-4 con colori distinguibili
# anche da chi ha difficoltà a distinguere i colori. Simboli diversi per
# la stampa in bianco e nero.
colori  <- c("#c8c7c0", "#2a78d6", "#eb6834", "#1baf7a")
simboli <- c(16, 17, 15, 18)          # cerchio, triangolo, quadrato, rombo
nomi_cluster <- c("1 Nucleo", "2 Capitale e debito",
                  "3 Valutazioni basse", "4 Redditività alta")
inchiostro <- "#0b0b0b"; inchiostro_2 <- "#52514e"; griglia <- "#e1e0d9"

# Salva una figura in PNG a 300 dpi
salva <- function(file, larghezza, altezza, disegno) {
  png(file.path(cartella_out, file), width = larghezza, height = altezza, res = 300)
  disegno()
  dev.off()
}

# 3. Fig. 4.1 - scelta di K ----------------------------------------------------
# Linea continua: K principale (4); linea tratteggiata: K di controllo (6).
fig_4_1 <- function() {
  par(mfrow = c(1, 3), mar = c(4.5, 4.5, 2.5, 1), mgp = c(2.8, 0.7, 0),
      las = 1, col.axis = inchiostro_2, cex.axis = 0.9)
  pannello <- function(y, titolo, etichetta_y, decimali) {
    plot(tab_k$K, y, type = "n", xlab = "Numero di cluster (K)", ylab = etichetta_y,
         main = titolo, font.main = 1, cex.main = 1.1, xaxt = "n", bty = "l",
         yaxt = "n")
    axis(1, at = seq(2, 14, by = 2))
    tacche <- pretty(y)
    axis(2, at = tacche, labels = formatC(tacche, format = "f", digits = decimali))
    abline(h = tacche, col = griglia, lwd = 0.8)
    abline(v = scelta_k$K_principale, col = colori[2], lwd = 1.5)
    abline(v = scelta_k$K_fine, col = "grey55", lty = 2, lwd = 1.2)
    lines(tab_k$K, y, lwd = 1.5, col = inchiostro)
    points(tab_k$K, y, pch = 19, cex = 0.9, col = inchiostro)
  }
  pannello(tab_k$devianza_entro, "Gomito: devianza entro i gruppi", "Devianza entro i gruppi", 0)
  pannello(tab_k$CH, "Indice di Calinski-Harabasz", "Indice CH", 0)
  pannello(tab_k$silhouette_media, "Silhouette media", "Silhouette media", 3)
  legend("topright", legend = c("K = 4 (principale)", "K = 6 (controllo)"),
         col = c(colori[2], "grey55"), lty = c(1, 2), lwd = 1.5, bty = "n", cex = 0.9)
}
fig_4_1()
salva("fig_4_1_scelta_k.png", 3600, 1300, fig_4_1)

# 4. Fig. 4.2 - centroidi a K = 4 ------------------------------------------------
# Barre = media standardizzata di ogni variabile nel cluster
# (0 = media del campione).
X  <- as.matrix(dati_finali[, var_cluster])
cl <- assegnazioni$cluster_K4
centroidi <- apply(X, 2, function(x) tapply(x, cl, mean))
round(centroidi, 2)
n_cluster <- as.vector(table(cl))

fig_4_2 <- function() {
  par(mfrow = c(2, 2), mar = c(4, 10.5, 2.5, 1), mgp = c(2.5, 0.6, 0),
      las = 1, col.axis = inchiostro_2, cex.axis = 0.85)
  limiti <- c(-1, 2)          # stessa scala per tutti i pannelli
  for (k in 1:4) {
    valori <- rev(centroidi[k, ])                    # prima variabile in alto
    posizioni <- barplot(valori, horiz = TRUE, xlim = limiti, plot = FALSE)
    plot(NA, xlim = limiti, ylim = range(posizioni) + c(-0.7, 0.7),
         yaxt = "n", xlab = "Deviazioni standard dalla media", ylab = "",
         main = paste0(nomi_cluster[k], " (", n_cluster[k], " società)"),
         font.main = 1, cex.main = 1, bty = "n")
    abline(v = seq(-1, 2, by = 0.5), col = griglia, lwd = 0.8)
    barplot(valori, horiz = TRUE, add = TRUE, col = colori[k], border = NA,
            names.arg = etichette[names(valori)], cex.names = 0.8, axes = FALSE)
    abline(v = 0, col = inchiostro_2, lwd = 1)
  }
}
fig_4_2()
salva("fig_4_2_centroidi.png", 3000, 2600, fig_4_2)

# 5. Fig. 4.3 - piano PC1-PC2 ---------------------------------------------------
# Stessa PCA dello script 10. Punti = società; frecce = coefficienti delle
# variabili, ingranditi per essere leggibili sul piano (conta la direzione,
# non la lunghezza); numero grande = posizione media del cluster.
pca <- prcomp(X, center = TRUE, scale. = TRUE)
quote <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
quote[1:2]; sum(quote[1:2])

# Il segno delle componenti è arbitrario: viene fissato in modo che il ROA
# abbia coefficiente positivo su PC1 e la rotazione dell'attivo su PC2.
segni  <- c(sign(pca$rotation["roa", 1]), sign(pca$rotation["rot_attivo", 2]))
punti  <- sweep(pca$x[, 1:2], 2, segni, "*")
frecce <- sweep(pca$rotation[, 1:2], 2, segni, "*")
scala  <- 7                                  # ingrandimento delle frecce

# Etichette delle frecce sulla punta, dal lato verso cui punta la freccia.
# Crescita dei ricavi e ROA hanno quasi la stessa direzione: l'etichetta
# della crescita è spostata sotto la freccia.
pos_etichette <- frecce * scala
lato <- ifelse(abs(pos_etichette[, 2]) > abs(pos_etichette[, 1]),
               ifelse(pos_etichette[, 2] > 0, 3, 1),      # 3 = sopra, 1 = sotto
               ifelse(pos_etichette[, 1] > 0, 4, 2))      # 4 = destra, 2 = sinistra
lato["cresc_ricavi"] <- 1
pos_etichette["cresc_ricavi", 2] <- pos_etichette["cresc_ricavi", 2] - 0.25

fig_4_3 <- function() {
  par(mar = c(4.5, 4.5, 1, 1), mgp = c(2.8, 0.7, 0), las = 1,
      col.axis = inchiostro_2, cex.axis = 0.9)
  ordine <- order(cl == 1, decreasing = TRUE)        # nucleo disegnato per primo
  plot(punti[ordine, 1], punti[ordine, 2], col = colori[cl[ordine]],
       pch = simboli[cl[ordine]], cex = 0.75, bty = "l",
       xlim = range(punti[, 1], pos_etichette[, 1] * 1.3),
       xlab = paste0("PC1 (", quote[1], "% della varianza)"),
       ylab = paste0("PC2 (", quote[2], "% della varianza)"))
  abline(h = 0, v = 0, lty = 3, col = "grey60")
  arrows(0, 0, frecce[, 1] * scala, frecce[, 2] * scala,
         length = 0.07, col = inchiostro_2, lwd = 1.2)
  text(pos_etichette[, 1], pos_etichette[, 2], labels = etichette[rownames(frecce)],
       cex = 0.7, col = inchiostro, pos = lato, offset = 0.3)
  medie <- aggregate(punti, by = list(cluster = cl), FUN = mean)
  text(medie[, 2], medie[, 3], labels = medie$cluster, cex = 1.6, font = 2, col = inchiostro)
  legend("bottomright", legend = nomi_cluster, col = colori, pch = simboli,
         bty = "n", cex = 0.8, pt.cex = 1)
}
fig_4_3()
salva("fig_4_3_pca.png", 2700, 2400, fig_4_3)

# 6. Fig. 4.4 - heatmap supersettori x cluster ----------------------------------
# Righe nell'ordine della Tabella 4.2. In ogni cella: numero di società e
# rapporto r. Colore: rapporto r su scala logaritmica centrata su 1
# (rosso = sovrarappresentato, blu = sottorappresentato).
ordine_righe <- confronto_icb$K4$categorie$gruppo
conteggi <- unclass(confronto_icb$K4$contingenza)[ordine_righe, ]
rapporti <- confronto_icb$K4$rapporti[ordine_righe, ]
stopifnot(sum(conteggi) == nrow(assegnazioni))

# Lato blu più tenue del rosso, per non far prevalere le celle vuote.
scala_colori <- colorRampPalette(c("#6da7ec", "#b7d3f6", "#f0efec", "#ef9a96", "#c0302f"))(101)
limite <- log2(5)                                     # r da 1/5 a 5
indice_colore <- function(r) {
  z <- pmin(pmax(log2(pmax(r, 1e-6)), -limite), limite)     # r = 0 -> estremo blu
  round((z + limite) / (2 * limite) * 100) + 1
}
# Testo bianco sulle celle scure, nero su quelle chiare
colore_testo <- function(hex) {
  rgb <- col2rgb(hex) / 255
  ifelse(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ] < 0.45, "white", inchiostro)
}

fig_4_4 <- function() {
  nr <- nrow(conteggi); nc <- ncol(conteggi)
  par(mar = c(5.5, 17, 4.2, 1), las = 1, xpd = NA)
  plot(NA, xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5), axes = FALSE,
       xlab = "", ylab = "")
  for (i in 1:nr) for (j in 1:nc) {
    y <- nr - i + 1                                     # prima riga in alto
    fill <- scala_colori[indice_colore(rapporti[i, j])]
    rect(j - 0.48, y - 0.46, j + 0.48, y + 0.46, col = fill, border = NA)
    txt <- colore_testo(fill)
    text(j, y + 0.13, conteggi[i, j], cex = 0.8, font = 2, col = txt)
    text(j, y - 0.18, paste0("r = ", formatC(rapporti[i, j], format = "f", digits = 2)),
         cex = 0.62, col = txt)
  }
  n_righe <- rowSums(conteggi)
  axis(2, at = nr:1, labels = paste0(ordine_righe, " (", n_righe, ")"),
       tick = FALSE, cex.axis = 0.72, col.axis = inchiostro)
  axis(3, at = 1:nc, labels = sub(" ", "\n", nomi_cluster), tick = FALSE,
       cex.axis = 0.72, col.axis = inchiostro, padj = 0.3)
  # Legenda del colore
  valori_legenda <- c(0.2, 0.5, 1, 2, 5)
  x0 <- seq(0.9, nc + 0.1, length.out = 5)
  for (h in seq_along(valori_legenda)) {
    rect(x0[h] - 0.12, -0.55, x0[h] + 0.12, -0.25,
         col = scala_colori[indice_colore(valori_legenda[h])], border = NA)
    text(x0[h] + 0.16, -0.4, formatC(valori_legenda[h], format = "f", digits = 1),
         pos = 4, cex = 0.65, col = inchiostro_2)
  }
  text(0.5, -0.95, "Colore: rapporto r = quota del supersettore nel cluster / quota del cluster nel campione (1 = come il campione)",
       pos = 4, cex = 0.62, col = inchiostro_2)
}
fig_4_4()
salva("fig_4_4_heatmap.png", 2700, 3000, fig_4_4)
