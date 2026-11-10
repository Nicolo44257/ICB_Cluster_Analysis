# =====================================================================
# 03_correlazioni.R
# Matrici di correlazione di Pearson e di Spearman delle variabili
# candidate, per individuare le variabili ridondanti (capitolo 2,
# sezione 2.4).
# =====================================================================

# 1. Dati: variabili candidate (script 02) ------------------------------
source("R/02_descrittive.R")

library(corrplot)

# 2. Solo le variabili numeriche ------------------------------------------
vars <- variabili %>% select(where(is.numeric))
dim(vars)

# 3. Matrici di correlazione -------------------------------------------------
# Pearson misura la relazione lineare ed è sensibile ai valori estremi;
# Spearman lavora sui ranghi ed è robusta ai valori estremi.
# Per ogni coppia si usano le società con entrambi i valori disponibili.
cor_pearson  <- cor(vars, use = "pairwise.complete.obs", method = "pearson")
cor_spearman <- cor(vars, use = "pairwise.complete.obs", method = "spearman")

# 4. Coppie di variabili con |r| >= soglia ----------------------------------
coppie_forti <- function(m, soglia = 0.7) {
  pos <- which(abs(m) >= soglia & upper.tri(m), arr.ind = TRUE)
  data.frame(
    var1 = rownames(m)[pos[, 1]],
    var2 = colnames(m)[pos[, 2]],
    r    = round(m[pos], 2)
  ) %>%
    arrange(desc(abs(r)))
}

coppie_forti(cor_pearson)
coppie_forti(cor_spearman)

# 5. Grafici delle matrici -----------------------------------------------------
disegna <- function(m) {
  corrplot(m, method = "color", type = "upper", diag = FALSE,
           tl.col = "black", tl.cex = 0.7,
           addCoef.col = "black", number.cex = 0.45)
}

disegna(cor_spearman)

png("output/fig_correlazioni_spearman.png", width = 2400, height = 2400, res = 300)
disegna(cor_spearman)
dev.off()

png("output/fig_correlazioni_pearson.png", width = 2400, height = 2400, res = 300)
disegna(cor_pearson)
dev.off()

# 6. Salvataggio delle matrici ------------------------------------------------
write.csv2(round(cor_pearson, 2),  "output/tab_cor_pearson.csv")
write.csv2(round(cor_spearman, 2), "output/tab_cor_spearman.csv")
