# =====================================================================
# 13_profili_casi.R
# Profili dei cluster e casi particolari (capitolo 4, sezioni 4.2 e 4.5):
#   1) profili dei 4 cluster in unità originali (mediane), con
#      capitalizzazione e R&S/vendite (Tabella 4.1);
#   2) società divergenti e loro variabili chiave (Tabella 4.4);
#   3) società di confine (silhouette negativa);
#   4) casi discussi nel capitolo 2 (case automobilistiche, Zegona,
#      Prosus, Kongsberg Gruppen, Iveco).
# =====================================================================

# 1. Pacchetti ---------------------------------------------------------
library(readxl)
library(dplyr)

cartella_out <- "output/capitolo_4"

var_cluster <- c("marg_op", "roa", "cresc_ricavi", "debito_attivi",
                 "current", "capex_vendite", "rot_attivo",
                 "ebitda_ev", "book_price", "div_yield", "volatilita")

# 2. Dati originali --------------------------------------------------------
# Valori prima di winsorizzazione e logaritmi, con le due sostituzioni
# della sezione 2.3 (crescita di Zegona ed EBITDA/EV di BMW).
campione <- read_excel("dati/DATASET_v5_valori.xlsx", sheet = "DATASET") %>%
  filter(Campione == "Incluso")
dim(campione)

originali <- campione %>%
  select(ticker        = Ticker,
         marg_op       = `Mrg op:Y`,
         roa           = `ROA:Y`,
         cresc_ricavi  = `Vend-Cr 1an:Y`,
         debito_attivi = `Debito/Attivi:Y`,
         current       = `Liq corr:Y`,
         capex_vendite = `CAPEX/Sales:Y`,
         rot_attivo    = `Ast TO:Y`,
         ebitda_ev     = `EBITDA/EV (calc)`,
         book_price    = `Book-to-price (calc)`,
         div_yield     = `Dividend yield (calc)`,
         volatilita    = starts_with("Volt"),
         rs_vendite    = `R&S/vendite (calc)`,
         cap           = `Cap mercato`,
         ev            = `EV (calc)`)

# Sostituzione 1 (script 05): crescita mancante -> mediana del campione
mediana_cresc <- median(originali$cresc_ricavi, na.rm = TRUE)
mediana_cresc

# Sostituzione 2 (script 06): EBITDA/EV con EV negativo -> mediana
sum(originali$ev <= 0)
originali <- originali %>%
  mutate(ebitda_ev = ifelse(ev <= 0, NA, ebitda_ev))
mediana_ebitda_ev <- median(originali$ebitda_ev, na.rm = TRUE)
mediana_ebitda_ev

originali <- originali %>%
  mutate(cresc_ricavi = ifelse(is.na(cresc_ricavi), mediana_cresc, cresc_ricavi),
         ebitda_ev    = ifelse(is.na(ebitda_ev), mediana_ebitda_ev, ebitda_ev),
         cap_mld      = cap / 1e9) %>%       # capitalizzazione in miliardi di euro
  select(-cap, -ev)

stopifnot(sum(is.na(originali[, var_cluster])) == 0)
sum(!is.na(originali$rs_vendite))        # società con il dato di R&S

# 3. Controllo: dai dati originali si ottengono i dati del clustering -------
# Winsorizzazione, logaritmi e standardizzazione (script 06 e 07) applicati
# ai dati originali devono restituire dati_finali.rds.
winsorizza   <- function(x, p = 0.01) {
  lim <- quantile(x, c(p, 1 - p))
  pmin(pmax(x, lim[1]), lim[2])
}
standardizza <- function(x) (x - mean(x)) / sd(x)

ricostruiti <- originali %>%
  mutate(across(all_of(var_cluster), winsorizza)) %>%
  mutate(current = log(current), capex_vendite = log1p(capex_vendite)) %>%
  mutate(across(all_of(var_cluster), standardizza))

dati_finali <- readRDS("output/dati_finali.rds")
riga <- match(dati_finali$ticker, ricostruiti$ticker)
stopifnot(max(abs(as.matrix(ricostruiti[riga, var_cluster]) -
                  as.matrix(dati_finali[, var_cluster]))) < 1e-10)

# 4. Unione con i cluster e con le categorie dei supersettori (script 12) ----
assegnazioni  <- readRDS("output/capitolo_3/cluster_assegnazioni.rds")
confronto_icb <- readRDS(file.path(cartella_out, "confronto_icb.rds"))

# cluster_casa = cluster prevalente del supersettore
casa <- confronto_icb$K4$categorie %>%
  transmute(supersettore = gruppo, categoria,
            cluster_casa = as.integer(cluster_prevalente))

dati <- assegnazioni %>%
  left_join(originali, by = "ticker") %>%
  left_join(casa, by = "supersettore")

dim(dati)
stopifnot(sum(is.na(dati$marg_op)) == 0)

# 5. Profili in unità originali (Tabella 4.1) ------------------------------------
# Per ogni cluster e per il campione: mediane delle 11 variabili, della
# capitalizzazione e di R&S/vendite (solo società con il dato), silhouette
# media e quota di società con silhouette negativa.
profilo <- function(d) {
  c(n_societa        = nrow(d),
    sapply(var_cluster, function(v) median(d[[v]])),
    cap_mld          = median(d$cap_mld),
    rs_vendite       = median(d$rs_vendite, na.rm = TRUE),
    n_con_rs         = sum(!is.na(d$rs_vendite)),
    silhouette_media = mean(d$sil_K4),
    quota_s_negativa = mean(d$sil_K4 < 0) * 100)
}

tab_profili <- cbind(sapply(1:4, function(k) profilo(dati[dati$cluster_K4 == k, ])),
                     profilo(dati))
colnames(tab_profili) <- c(paste0("cluster_", 1:4), "campione")
tab_profili <- round(tab_profili, 2)
tab_profili

write.csv2(tab_profili, file.path(cartella_out, "tab_profili_originali.csv"))

# 6. Società divergenti --------------------------------------------------------
# Società di un supersettore concentrato o intermedio assegnate a un
# cluster diverso da quello prevalente del supersettore, con silhouette
# positiva.
divergenti_tutte <- dati %>%
  filter(categoria %in% c("concentrato", "intermedio"),
         cluster_K4 != cluster_casa,
         sil_K4 > 0) %>%
  arrange(desc(sil_K4))

nrow(divergenti_tutte)
dplyr::count(divergenti_tutte, supersettore, categoria)

# Al massimo una società per supersettore (la silhouette più alta);
# le prime 4 per silhouette sono i casi della Tabella 4.4.
divergenti <- divergenti_tutte %>%
  group_by(supersettore) %>%
  slice_max(sil_K4, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(sil_K4)) %>%
  mutate(posizione = row_number(), scelta = posizione <= 4) %>%
  select(posizione, scelta, nome, ticker, supersettore, settore, categoria,
         cluster_casa, cluster_K4, vicino_K4, sil_K4)
divergenti

write.csv2(divergenti_tutte %>%
             select(nome, ticker, supersettore, settore, categoria,
                    cluster_casa, cluster_K4, vicino_K4, sil_K4),
           file.path(cartella_out, "tab_divergenti_tutte.csv"), row.names = FALSE)
write.csv2(divergenti, file.path(cartella_out, "tab_divergenti.csv"), row.names = FALSE)

# 7. Variabili chiave di una società ------------------------------------------------
# Per ogni variabile: valore della società e mediana del suo supersettore
# (società compresa), in unità originali e in deviazioni standard.
# Variabili chiave = le 3 con la distanza maggiore, in deviazioni
# standard, dalla mediana del supersettore.
z <- dati_finali %>% select(ticker, all_of(var_cluster))

variabili_caso <- function(tk) {
  soc   <- dati %>% filter(ticker == tk)
  pari  <- dati %>% filter(supersettore == soc$supersettore)
  z_soc <- z %>% filter(ticker == tk)
  z_pari <- z %>% filter(ticker %in% pari$ticker)
  data.frame(
    nome                   = soc$nome,
    supersettore           = soc$supersettore,
    variabile              = var_cluster,
    valore                 = unlist(soc[1, var_cluster]),
    mediana_supersettore   = sapply(var_cluster, function(v) median(pari[[v]])),
    z_societa              = unlist(z_soc[1, var_cluster]),
    z_mediana_supersettore = sapply(var_cluster, function(v) median(z_pari[[v]])),
    row.names = NULL
  ) %>%
    mutate(distanza_z = z_societa - z_mediana_supersettore,
           chiave     = rank(-abs(distanza_z), ties.method = "first") <= 3)
}

# 8. Società di confine (silhouette negativa) --------------------------------------
tab_confine <- dati %>%
  filter(sil_K4 < 0) %>%
  arrange(sil_K4) %>%
  select(nome, supersettore, settore, cluster_K4, vicino_K4, sil_K4)
nrow(tab_confine)
head(tab_confine, 12)

write.csv2(tab_confine, file.path(cartella_out, "tab_confine.csv"), row.names = FALSE)

# 9. Casi discussi nel capitolo 2 ----------------------------------------------------
# Per BMW l'EBITDA/EV e per Zegona la crescita dei ricavi sono i valori
# sostituiti con la mediana.
ticker_casi <- c("VOW3 GR Equity", "BMW GR Equity", "MBG GR Equity",
                 "ZEG LN Equity", "PRX NA Equity", "KOG NO Equity",
                 "IVG IM Equity")

tab_casi <- dati %>%
  filter(ticker %in% ticker_casi) %>%
  mutate(fuori_casa = cluster_K4 != cluster_casa) %>%
  select(nome, supersettore, settore, categoria, cluster_casa, cluster_K4,
         vicino_K4, sil_K4, fuori_casa, all_of(var_cluster), cap_mld)
tab_casi

write.csv2(tab_casi, file.path(cartella_out, "tab_casi_cap2.csv"), row.names = FALSE)

# 10. Variabili chiave dei casi divergenti scelti e dei casi del capitolo 2 -----------
tab_variabili_casi <- bind_rows(
  bind_rows(lapply(divergenti$ticker[divergenti$scelta], variabili_caso)) %>%
    mutate(tipo = "divergente", .before = 1),
  bind_rows(lapply(ticker_casi, variabili_caso)) %>%
    mutate(tipo = "cap. 2", .before = 1)
) %>%
  mutate(across(c(valore, mediana_supersettore, z_societa,
                  z_mediana_supersettore, distanza_z), ~ round(.x, 2)))

tab_variabili_casi %>% filter(chiave)

write.csv2(tab_variabili_casi, file.path(cartella_out, "tab_variabili_casi.csv"),
           row.names = FALSE)
