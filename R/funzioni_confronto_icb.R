# =====================================================================
# funzioni_confronto_icb.R
# Funzioni per confrontare una partizione in cluster con una
# classificazione ICB (supersettori o industrie), usate dagli script 12
# e 14 (capitolo 3, sezione 3.6; capitolo 4, sezioni 4.3, 4.4 e 4.6).
# Il file non va eseguito da solo: viene caricato con source().
#
# Definizioni:
#   - quota q    = quota delle società di un gruppo nel suo cluster
#                  prevalente
#   - rapporto r = q / quota di quel cluster nel campione (r > 1: gruppo
#                  sovrarappresentato nel cluster)
#   - categorie: concentrato q >= 2/3; intermedio 1/2 <= q < 2/3;
#                distribuito q < 1/2
#   - in caso di parità nel cluster prevalente si sceglie il cluster più
#     piccolo (rapporto più alto) e si riporta l'altro
#   - scenario di riferimento dell'ARI: ogni gruppo spostato per intero
#     nel suo cluster prevalente
# =====================================================================

library(dplyr)
library(mclust)   # adjustedRandIndex()

# 1. Tabella di contingenza ---------------------------------------------
# Righe = gruppi ICB, colonne = cluster.
tabella_contingenza <- function(cluster, gruppi) {
  table(gruppo = gruppi, cluster = cluster)
}

# 2. Rapporto r di ogni cella -------------------------------------------
# Quota del gruppo nel cluster divisa per la quota del cluster nel campione.
tabella_rapporti <- function(tab) {
  quota_nel_gruppo <- prop.table(tab, margin = 1)
  quota_cluster    <- colSums(tab) / sum(tab)
  unclass(sweep(quota_nel_gruppo, 2, quota_cluster, "/"))
}

# 3. Categorie dei gruppi -------------------------------------------------
# Per ogni gruppo: cluster prevalente, quota q, rapporto r, valore massimo
# del rapporto (1 / quota del cluster) e categoria.
# Le soglie 2/3 e 1/2 sono confrontate sui conteggi, per evitare errori
# di arrotondamento.
# cluster_nucleo: numero del cluster con profilo medio (NA se assente);
# i gruppi concentrati in quel cluster sono segnalati a parte.
categorie_gruppi <- function(tab, cluster_nucleo = NA) {
  dim_cluster   <- colSums(tab)
  quota_cluster <- dim_cluster / sum(tab)

  righe <- lapply(rownames(tab), function(g) {
    conti      <- tab[g, ]
    n_g        <- sum(conti)
    n_prev     <- max(conti)
    prevalenti <- names(conti)[conti == n_prev]
    scelto     <- prevalenti[which.min(dim_cluster[prevalenti])]
    data.frame(
      gruppo             = g,
      n_societa          = n_g,
      cluster_prevalente = scelto,
      parita_con         = paste(setdiff(prevalenti, scelto), collapse = ", "),
      n_nel_prevalente   = n_prev,
      quota              = n_prev / n_g,
      quota_cluster      = quota_cluster[[scelto]],
      rapporto           = (n_prev / n_g) / quota_cluster[[scelto]],
      tetto_rapporto     = 1 / quota_cluster[[scelto]],
      categoria          = if (3 * n_prev >= 2 * n_g) "concentrato"
                           else if (2 * n_prev >= n_g) "intermedio"
                           else "distribuito"
    )
  })

  bind_rows(righe) %>%
    mutate(nel_nucleo = !is.na(cluster_nucleo) &
                        categoria == "concentrato" &
                        cluster_prevalente == as.character(cluster_nucleo)) %>%
    arrange(desc(quota), desc(rapporto))
}

# 4. Purezza dei cluster ---------------------------------------------------
# Per ogni cluster: gruppo prevalente e purezza (quota del gruppo prevalente
# nel cluster), numero di gruppi presenti e gruppi sovrarappresentati
# (r > 1), ordinati per rapporto.
purezza_cluster <- function(tab) {
  rapp <- tabella_rapporti(tab)
  bind_rows(lapply(colnames(tab), function(k) {
    conti <- tab[, k]
    n_k   <- sum(conti)
    sovra <- rapp[, k] > 1
    ordine     <- order(rapp[sovra, k], decreasing = TRUE)
    nomi_sovra <- names(conti)[sovra][ordine]
    elenco <- paste0(nomi_sovra, " (", conti[sovra][ordine], "; r = ",
                     formatC(rapp[sovra, k][ordine], format = "f", digits = 2),
                     ")", collapse = "; ")
    data.frame(
      cluster              = k,
      n_societa            = n_k,
      quota_campione       = n_k / sum(tab),
      gruppo_prevalente    = paste(names(conti)[conti == max(conti)],
                                   collapse = " / "),
      n_prevalente         = max(conti),
      purezza              = max(conti) / n_k,
      n_gruppi_presenti    = sum(conti > 0),
      n_sovrarappresentati = sum(sovra),
      sovrarappresentati   = elenco
    )
  }))
}

# 5. ARI osservato e scenario di riferimento -------------------------------
# Nello scenario ogni società è assegnata al cluster prevalente del suo
# gruppo. Non è il massimo teorico dell'ARI, ma un termine di confronto
# costruito sulla stessa tabella di contingenza.
ari_con_scenario <- function(cluster, gruppi, categorie) {
  prevalente <- setNames(categorie$cluster_prevalente, categorie$gruppo)
  scenario   <- prevalente[as.character(gruppi)]
  data.frame(
    ARI_osservato      = adjustedRandIndex(cluster, gruppi),
    ARI_scenario       = adjustedRandIndex(scenario, gruppi),
    n_gruppi           = length(unique(gruppi)),
    n_cluster          = length(unique(cluster)),
    n_cluster_scenario = length(unique(scenario))  # cluster non vuoti nello scenario
  )
}

# 6. Confronto completo per una partizione ----------------------------------
confronta_con_icb <- function(cluster, gruppi, cluster_nucleo = NA) {
  tab       <- tabella_contingenza(cluster, gruppi)
  categorie <- categorie_gruppi(tab, cluster_nucleo)
  list(contingenza = tab,
       rapporti    = tabella_rapporti(tab),
       categorie   = categorie,
       purezza     = purezza_cluster(tab),
       ari         = ari_con_scenario(cluster, gruppi, categorie))
}

# 7. Salvataggio in CSV (quote in percentuale, arrotondate) -----------------
salva_confronto <- function(conf, cartella, suffisso) {
  percorso <- function(nome) file.path(cartella, paste0(nome, "_", suffisso, ".csv"))

  contingenza <- addmargins(conf$contingenza)
  dimnames(contingenza) <- lapply(dimnames(contingenza),
                                  function(x) sub("^Sum$", "Totale", x))
  write.csv2(as.data.frame.matrix(contingenza), percorso("tab_contingenza"))

  write.csv2(round(conf$rapporti, 2), percorso("tab_rapporti"))

  write.csv2(conf$categorie %>%
               mutate(across(c(quota, quota_cluster), ~ round(.x * 100, 1)),
                      across(c(rapporto, tetto_rapporto), ~ round(.x, 2))),
             percorso("tab_categorie_supersettori"), row.names = FALSE)

  write.csv2(conf$purezza %>%
               mutate(across(c(quota_campione, purezza), ~ round(.x * 100, 1))),
             percorso("tab_purezza_cluster"), row.names = FALSE)
}
