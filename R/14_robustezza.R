# =====================================================================
# 14_robustezza.R
# Controlli di robustezza (capitolo 4, sezione 4.6, Tabella 4.5).
# Si modifica una scelta alla volta, con K = 4 nelle varianti su dati e
# variabili:
#   - costruzione: Ward, pesi per dimensione, K = 6 (dallo script 11);
#   - dati: sola winsorizzazione (senza logaritmi); campione senza BMW
#     e Zegona (443 società);
#   - variabili: ROIC al posto del ROA;
#   - classificazione: industria (9 gruppi) al posto del supersettore.
# Per ogni variante: ARI con la partizione principale, ARI con i
# supersettori (e scenario di riferimento), numero di supersettori che
# restano nella stessa categoria.
# =====================================================================

# 1. Funzioni, pacchetti e risultati degli script precedenti -------------
source("R/funzioni_confronto_icb.R")
library(readxl)

cartella_out <- "output/capitolo_4"

var_cluster <- c("marg_op", "roa", "cresc_ricavi", "debito_attivi",
                 "current", "capex_vendite", "rot_attivo",
                 "ebitda_ev", "book_price", "div_yield", "volatilita")

scelta_k      <- readRDS("output/capitolo_3/scelta_k.rds")
dati_finali   <- readRDS("output/dati_finali.rds")
assegnazioni  <- readRDS("output/capitolo_3/cluster_assegnazioni.rds")
alternativi   <- readRDS("output/capitolo_3/cluster_alternativi.rds")
confronto_icb <- readRDS(file.path(cartella_out, "confronto_icb.rds"))

stopifnot(all(dati_finali$ticker == assegnazioni$ticker))

# 2. Dati originali (come nello script 13, con in più il ROIC) --------------
campione <- read_excel("dati/DATASET_v5_valori.xlsx", sheet = "DATASET") %>%
  filter(Campione == "Incluso")

originali <- campione %>%
  select(ticker        = Ticker,
         marg_op       = `Mrg op:Y`,
         roa           = `ROA:Y`,
         roic          = `ROIC:Y`,
         cresc_ricavi  = `Vend-Cr 1an:Y`,
         debito_attivi = `Debito/Attivi:Y`,
         current       = `Liq corr:Y`,
         capex_vendite = `CAPEX/Sales:Y`,
         rot_attivo    = `Ast TO:Y`,
         ebitda_ev     = `EBITDA/EV (calc)`,
         book_price    = `Book-to-price (calc)`,
         div_yield     = `Dividend yield (calc)`,
         volatilita    = starts_with("Volt"),
         ev            = `EV (calc)`)

# Le due sostituzioni della sezione 2.3 (Zegona e BMW)
mediana_cresc <- median(originali$cresc_ricavi, na.rm = TRUE)
originali <- originali %>% mutate(ebitda_ev = ifelse(ev <= 0, NA, ebitda_ev))
mediana_ebitda_ev <- median(originali$ebitda_ev, na.rm = TRUE)
originali <- originali %>%
  mutate(cresc_ricavi = ifelse(is.na(cresc_ricavi), mediana_cresc, cresc_ricavi),
         ebitda_ev    = ifelse(is.na(ebitda_ev), mediana_ebitda_ev, ebitda_ev)) %>%
  select(-ev)

stopifnot(sum(is.na(originali[, c(var_cluster, "roic")])) == 0)

# 3. Preparazione dei dati e k-means -------------------------------------------
winsorizza   <- function(x, p = 0.01) {
  lim <- quantile(x, c(p, 1 - p))
  pmin(pmax(x, lim[1]), lim[2])
}
standardizza <- function(x) (x - mean(x)) / sd(x)

# Trattamento del capitolo 2 (winsorizzazione, eventuale logaritmo,
# standardizzazione) applicato alle variabili indicate.
prepara <- function(d, variabili, con_log = TRUE) {
  d <- d %>% mutate(across(all_of(variabili), winsorizza))
  if (con_log) {
    d <- d %>% mutate(current = log(current), capex_vendite = log1p(capex_vendite))
  }
  d %>% mutate(across(all_of(variabili), standardizza))
}

# K-means con le impostazioni degli script 08-09, cluster rinumerati per
# dimensione (1 = il più grande).
esegui_kmeans <- function(d, variabili, K = 4) {
  set.seed(scelta_k$seme)
  km <- kmeans(as.matrix(d[, variabili]), centers = K,
               nstart = scelta_k$n_ripartenze, iter.max = scelta_k$n_iter)
  dimensioni <- sort(table(km$cluster), decreasing = TRUE)
  match(km$cluster, as.integer(names(dimensioni)))
}

# Controllo: le due funzioni riproducono i dati e la partizione principali
principale_dati <- prepara(originali, var_cluster)
riga <- match(dati_finali$ticker, principale_dati$ticker)
stopifnot(max(abs(as.matrix(principale_dati[riga, var_cluster]) -
                  as.matrix(dati_finali[, var_cluster]))) < 1e-10)

cl_controllo <- esegui_kmeans(dati_finali, var_cluster)
stopifnot(all(cl_controllo == assegnazioni$cluster_K4))

# 4. Varianti su dati e variabili ------------------------------------------------
# 4a. Sola winsorizzazione, senza logaritmo su current ratio e capex/vendite
dati_winsor <- prepara(originali, var_cluster, con_log = FALSE)
cl_winsor   <- esegui_kmeans(dati_winsor, var_cluster)

# 4b. Campione senza BMW e Zegona: percentili e medie sono ricalcolati
#     sul campione ridotto
dati_senza <- originali %>%
  filter(!ticker %in% c("BMW GR Equity", "ZEG LN Equity")) %>%
  prepara(var_cluster)
nrow(dati_senza)
cl_senza <- esegui_kmeans(dati_senza, var_cluster)

# 4c. ROIC al posto del ROA
var_roic   <- replace(var_cluster, var_cluster == "roa", "roic")
dati_roic  <- prepara(originali, var_roic)
cl_roic    <- esegui_kmeans(dati_roic, var_roic)

table(cl_winsor); table(cl_senza); table(cl_roic)

# 5. Tutte le partizioni in una tabella (una riga per società) ------------------
partizioni <- assegnazioni %>%
  select(ticker, nome, industria, supersettore, principale = cluster_K4, K6 = cluster_K6) %>%
  left_join(alternativi %>% select(ticker, ward = ward_K4, pesi = pesi_K4), by = "ticker") %>%
  left_join(data.frame(ticker = dati_winsor$ticker, sola_winsor = cl_winsor), by = "ticker") %>%
  left_join(data.frame(ticker = dati_senza$ticker, senza_bmw_zegona = cl_senza), by = "ticker") %>%
  left_join(data.frame(ticker = dati_roic$ticker, roic = cl_roic), by = "ticker")

colSums(is.na(partizioni))

# 6. Misure per ogni variante -----------------------------------------------------
# (1) ARI con la partizione principale (sulle società comuni);
# (2) ARI con i supersettori, scenario di riferimento e cluster usati;
# (3) supersettori, su 16, nella stessa categoria della partizione principale.
cat_principale <- confronto_icb$K4$categorie %>% select(gruppo, categoria)

varianti <- c("Ward a K = 4"                     = "ward",
              "Pesi per dimensione, K = 4"       = "pesi",
              "K = 6"                            = "K6",
              "Sola winsorizzazione"             = "sola_winsor",
              "Senza BMW e Zegona (443 societa')" = "senza_bmw_zegona",
              "ROIC al posto del ROA"            = "roic")

categorie_varianti <- list()   # categorie dei supersettori in ogni variante

misura <- function(etichetta, colonna) {
  d    <- partizioni %>% filter(!is.na(.data[[colonna]]))
  conf <- confronta_con_icb(d[[colonna]], d$supersettore)
  confronto_cat <- conf$categorie %>%
    select(gruppo, categoria_variante = categoria) %>%
    inner_join(cat_principale, by = "gruppo")
  categorie_varianti[[colonna]] <<- confronto_cat
  data.frame(variante           = etichetta,
             n_societa          = nrow(d),
             ari_principale     = adjustedRandIndex(d[[colonna]], d$principale),
             ari_supersettori   = conf$ari$ARI_osservato,
             ari_scenario       = conf$ari$ARI_scenario,
             n_cluster_scenario = conf$ari$n_cluster_scenario,
             stessa_categoria   = sum(confronto_cat$categoria_variante ==
                                      confronto_cat$categoria))
}

tab_robustezza <- bind_rows(
  # riga di riferimento: la partizione principale
  data.frame(variante = "Principale (k-means, K = 4)", n_societa = 445,
             ari_principale = 1,
             ari_supersettori   = confronto_icb$K4$ari$ARI_osservato,
             ari_scenario       = confronto_icb$K4$ari$ARI_scenario,
             n_cluster_scenario = confronto_icb$K4$ari$n_cluster_scenario,
             stessa_categoria   = 16),
  bind_rows(lapply(names(varianti), function(e) misura(e, varianti[[e]])))
)

# 7. Variante di classificazione: industria al posto del supersettore -------------
# Stessa partizione principale, confrontata con le 9 industrie ICB.
n_distinct(partizioni$industria)
conf_industria <- confronta_con_icb(partizioni$principale, partizioni$industria)
conf_industria$categorie

tab_robustezza <- bind_rows(
  tab_robustezza,
  data.frame(variante = "Industria (9 gruppi) al posto del supersettore",
             n_societa = 445, ari_principale = NA,
             ari_supersettori   = conf_industria$ari$ARI_osservato,   # ARI con le industrie
             ari_scenario       = conf_industria$ari$ARI_scenario,
             n_cluster_scenario = conf_industria$ari$n_cluster_scenario,
             stessa_categoria   = NA)
) %>%
  mutate(across(c(ari_principale, ari_supersettori, ari_scenario), ~ round(.x, 3)))

tab_robustezza

write.csv2(tab_robustezza, file.path(cartella_out, "tab_robustezza.csv"), row.names = FALSE)

# 8. Categorie dei supersettori in ogni variante ------------------------------------
# Una riga per supersettore, una colonna per variante.
tab_cat <- cat_principale %>% rename(supersettore = gruppo, principale = categoria)
for (v in varianti) {
  colonna <- categorie_varianti[[v]] %>% select(supersettore = gruppo, categoria_variante)
  names(colonna)[2] <- v
  tab_cat <- tab_cat %>% left_join(colonna, by = "supersettore")
}
tab_cat

write.csv2(tab_cat, file.path(cartella_out, "tab_robustezza_categorie.csv"), row.names = FALSE)
write.csv2(conf_industria$categorie %>%
             mutate(across(c(quota, quota_cluster), ~ round(.x * 100, 1)),
                    across(c(rapporto, tetto_rapporto), ~ round(.x, 2))),
           file.path(cartella_out, "tab_categorie_industrie.csv"), row.names = FALSE)

# 9. Profili delle varianti su dati e variabili -------------------------------------
# Centroidi = medie standardizzate per cluster, ciascuna sui propri dati.
# Incroci = cluster principali di provenienza delle società di ogni cluster.
centroidi <- function(d, cl, variabili, etichetta) {
  data.frame(variante = etichetta, cluster = sort(unique(cl)),
             n_societa = as.vector(table(cl)),
             round(apply(as.matrix(d[, variabili]), 2,
                         function(x) tapply(x, cl, mean)), 2),
             row.names = NULL)
}

tab_centroidi <- bind_rows(
  centroidi(dati_winsor, cl_winsor, var_cluster, "sola_winsor"),
  centroidi(dati_senza,  cl_senza,  var_cluster, "senza_bmw_zegona"),
  centroidi(dati_roic,   cl_roic,   var_roic,    "roic")
)
tab_centroidi

incroci <- bind_rows(lapply(c("sola_winsor", "senza_bmw_zegona", "roic"), function(v) {
  d <- partizioni %>% filter(!is.na(.data[[v]]))
  as.data.frame(table(principale = d$principale, variante = d[[v]])) %>%
    mutate(nome_variante = v, .before = 1)
}))
for (v in c("sola_winsor", "senza_bmw_zegona", "roic")) {
  print(v)
  print(table(principale = partizioni$principale, variante = partizioni[[v]]))
}

write.csv2(tab_centroidi, file.path(cartella_out, "tab_robustezza_centroidi.csv"),
           row.names = FALSE)
write.csv2(incroci, file.path(cartella_out, "tab_robustezza_incroci.csv"),
           row.names = FALSE)
