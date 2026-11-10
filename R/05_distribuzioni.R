# =====================================================================
# 05_distribuzioni.R
# Sostituzione del dato mancante (crescita dei ricavi di Zegona) con la
# mediana del campione e analisi della forma delle 11 variabili:
# istogrammi, boxplot, Q-Q plot, asimmetria, test di Shapiro-Wilk,
# conteggio dei valori anomali (capitolo 2, sezioni 2.3 e 2.5).
# =====================================================================

# 1. Dati: le 11 variabili scelte (script 04) ---------------------------
source("R/04_variabili_scelte.R")

# 2. Dato mancante: mediana dell'intero campione ---------------------------
mediana_cresc <- median(dati_cluster$cresc_ricavi, na.rm = TRUE)
mediana_cresc

dati_cluster <- dati_cluster %>%
  mutate(
    cresc_imputata = is.na(cresc_ricavi),
    cresc_ricavi   = ifelse(is.na(cresc_ricavi), mediana_cresc, cresc_ricavi)
  )

sum(dati_cluster$cresc_imputata)
stopifnot(sum(is.na(dati_cluster[, var_cluster])) == 0)

# 3. Valori anomali secondo la regola del boxplot ----------------------------
# Anomalo = sotto Q1 - k*IQR oppure sopra Q3 + k*IQR, con IQR = Q3 - Q1.
# k = 1,5: punti isolati del boxplot; k = 3: valori molto lontani.
conta_anomali <- function(x, k) {
  q   <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  sum(x < q[1] - k * iqr | x > q[2] + k * iqr)
}

# 4. Forma delle distribuzioni ------------------------------------------------
# Per ogni variabile: asimmetria, statistica W e p-value di Shapiro-Wilk,
# valori anomali e valori minori o uguali a zero (rilevanti per la scelta
# del logaritmo).
tab_distr <- data.frame(
  variabile    = var_cluster,
  asimmetria   = sapply(var_cluster, function(v) asimmetria(dati_cluster[[v]])),
  W            = sapply(var_cluster, function(v) shapiro.test(dati_cluster[[v]])$statistic),
  p_value      = sapply(var_cluster, function(v) shapiro.test(dati_cluster[[v]])$p.value),
  anomali_1.5  = sapply(var_cluster, function(v) conta_anomali(dati_cluster[[v]], 1.5)),
  anomali_3    = sapply(var_cluster, function(v) conta_anomali(dati_cluster[[v]], 3)),
  zero_o_neg   = sapply(var_cluster, function(v) sum(dati_cluster[[v]] <= 0)),
  row.names    = NULL
)

tab_distr$asimmetria <- round(tab_distr$asimmetria, 2)
tab_distr$W          <- round(tab_distr$W, 3)
tab_distr$p_value    <- signif(tab_distr$p_value, 3)
tab_distr

write.csv2(tab_distr, "output/tab_distribuzioni.csv", row.names = FALSE)

# 5. Istogramma, boxplot e Q-Q plot per ogni variabile ------------------------
for (v in var_cluster) {
  x <- dati_cluster[[v]]

  png(paste0("output/fig_distr_", v, ".png"),
      width = 3000, height = 1000, res = 300)
  par(mfrow = c(1, 3))

  hist(x, breaks = 30, col = "grey80",
       main = paste("Istogramma:", v), xlab = "")
  boxplot(x, horizontal = TRUE, col = "grey80",
          main = "Boxplot")
  qqnorm(x, main = "Q-Q plot normale")
  qqline(x, col = "red", lwd = 2)

  dev.off()
}

# 6. Esempio a schermo: margine operativo --------------------------------------
par(mfrow = c(1, 3))
hist(dati_cluster$marg_op, breaks = 30, col = "grey80",
     main = "Istogramma: marg_op", xlab = "")
boxplot(dati_cluster$marg_op, horizontal = TRUE, col = "grey80", main = "Boxplot")
qqnorm(dati_cluster$marg_op, main = "Q-Q plot normale")
qqline(dati_cluster$marg_op, col = "red", lwd = 2)
par(mfrow = c(1, 1))
