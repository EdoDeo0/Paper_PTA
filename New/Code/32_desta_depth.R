###########################################################
###### 32 — DESTA depth index per i PTA cinesi         ###
###########################################################
## Author: Edoardo Vitella
##
## Cosa fa: costruisce una misura alternativa di profondita' dei PTA cinesi
## basata sul DESTA depth index (Dur, Baccini & Elsig 2014), da usare come
## robustezza al posto di TotalDepth_nonEnv (che e' calcolato dalla stessa fonte
## WB che misura anche EP, generando correlazione meccanica).
##
## Il DESTA depth_index (0-7) conta quante delle 7 aree tematiche principali
## sono coperte dall'accordo (beni, servizi, investimenti, standard, appalti,
## concorrenza, IPR). Le environmental provisions NON rientrano nel conteggio.
##
## Input:  New/Data/External/DESTA/desta_*.csv   (DESTA v2.3, set. 2025)
##         Data/Country_Codes_Custom_Data.csv
## Output: New/Data/TotalDepth/desta_depth_country_year.csv
##         (Country, country_code, year, DESTA_depth_index, DESTA_depth_rasch)

## --- Setup -------------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)

DESTA_DIR <- here("New/Data/External/DESTA")
OUT_DIR   <- here("New/Data/TotalDepth")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Carica file DESTA -------------------------------------------------------
treaties <- fread(file.path(DESTA_DIR, "desta_list_of_treaties_02_03.csv"),
                  encoding = "Latin-1")
dyads    <- fread(file.path(DESTA_DIR, "desta_list_of_treaties_02_03_dyads.csv"),
                  encoding = "Latin-1")
indices  <- fread(file.path(DESTA_DIR, "desta_indices_version_02_03.csv"),
                  encoding = "Latin-1")

## --- Base_treaty IDs dei 14 accordi nel campione -----------------------------
# Corrispondenza verificata manualmente (vedi check_desta_coverage3.R e output):
# base_treaty DESTA = numero accordo nei dati WB/TREND del progetto
our_base_treaties <- c(62L, 67L, 100L, 199L, 220L, 221L, 222L, 224L,
                       227L, 804L, 840L, 862L, 909L, 955L)

## --- Mapping ISO numerico → codice e nome paese del progetto -----------------
# ISO numerico standard (da dyads$iso1/iso2) → country_code progetto
# verificato contro Data/Country_Codes_Custom_Data.csv
iso_map <- data.table(
  iso = c(
    # Asia
    50L, 96L, 116L, 360L, 418L, 458L, 104L, 608L, 702L, 764L, 704L,
    356L, 410L, 144L, 496L, 586L,
    # Americas
    152L, 188L, 604L,
    # Speciali
    344L, 446L,
    # Oceania
    36L, 554L,
    # Europa
    352L, 756L
  ),
  country_code = c(
    103L, 105L, 107L, 112L, 119L, 122L, 106L, 129L, 132L, 136L, 141L,
    111L, 133L, 134L, 124L, 127L,
    412L, 415L, 434L,
    110L, 121L,
    601L, 609L,
    322L, 331L
  ),
  Country = c(
    "Bangladesh", "Brunei", "Cambodia", "Indonesia", "Laos,PDR", "Malaysia",
    "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam",
    "India", "Korea Rep.", "Sri Lanka", "Mongolia", "Pakistan",
    "Chile", "Costa Rica", "Peru",
    "HongKong", "Macau",
    "Australia", "New Zealand",
    "Iceland", "Switzerland"
  )
)

## --- Filtra dyads: solo accordi cinesi nel campione --------------------------
CHINA_ISO <- 156L
d <- dyads[base_treaty %in% our_base_treaties & (iso1 == CHINA_ISO | iso2 == CHINA_ISO)]
d[, partner_iso := fifelse(iso1 == CHINA_ISO, iso2, iso1)]
d <- d[partner_iso != CHINA_ISO]  # rimuovi dyad Cina-Cina se esistono

## --- Aggiungi depth da indices (join su number) ------------------------------
idx <- indices[base_treaty %in% our_base_treaties,
               .(number, base_treaty, depth_index, depth_rasch)]
d <- merge(d, idx, by = c("number", "base_treaty"), all.x = TRUE)

# Fallback 1: se entryforceyear mancante nel dyads, usa year (anno di firma)
d[is.na(entryforceyear), entryforceyear := year]

# Fallback 2: accession entries (es. 62+1 Mongolia, 100+1 Bangkok) non hanno
# una riga propria in indices — usa il depth del base treaty corrispondente.
# Questo assicura che Mongolia (entra in APTA nel 2013 via 62+1) sia inclusa.
bt_depth <- indices[as.character(number) == as.character(base_treaty) &
                      base_treaty %in% our_base_treaties,
                    .(base_treaty, depth_bt = depth_index, depth_rasch_bt = depth_rasch)]
d <- merge(d, bt_depth, by = "base_treaty", all.x = TRUE)
d[is.na(depth_index), `:=`(depth_index = depth_bt, depth_rasch = depth_rasch_bt)]
d[, c("depth_bt", "depth_rasch_bt") := NULL]

cat("Righe con depth:", nrow(d[!is.na(depth_index)]),
    "| senza (post-2015 o non codificate):", nrow(d[is.na(depth_index)]), "\n")

## --- Aggiungi codice e nome paese --------------------------------------------
d <- merge(d, iso_map, by.x = "partner_iso", by.y = "iso", all.x = TRUE)

unmatched <- unique(d[is.na(country_code), partner_iso])
if (length(unmatched) > 0) {
  cat("[WARN] ISO senza mapping:", paste(unmatched, collapse = ", "), "\n")
  cat("       Questi paesi non saranno nell'output finale.\n")
}
d <- d[!is.na(country_code)]

## --- Espansione a country-year (2000-2015) con depth time-varying ------------
# Per ogni (country_code, base_treaty, year_panel):
# depth = max(depth_index) tra le versioni con entryforceyear <= year_panel
PANEL_YEARS <- 2000L:2015L

# Per ogni country-base_treaty: tutti i version-depth disponibili
versions <- d[!is.na(depth_index),
              .(partner_iso, country_code, Country, base_treaty,
                entryforceyear, depth_index, depth_rasch)]
setkey(versions, country_code, base_treaty, entryforceyear)

# Espandiamo: cross-join country-base_treaty x panel_years, poi filtro e max
pairs <- unique(versions[, .(country_code, Country, base_treaty)])
panel <- pairs[, .(year = PANEL_YEARS), by = .(country_code, Country, base_treaty)]

panel <- merge(panel, versions[, .(country_code, base_treaty, entryforceyear,
                                    depth_index, depth_rasch)],
               by = c("country_code", "base_treaty"), allow.cartesian = TRUE)

# Tieni solo versioni gia' in vigore nell'anno del panel
panel <- panel[entryforceyear <= year]

# Depth effettiva per quel year: max tra le versioni attive (di solito 1)
out <- panel[, .(depth_index = max(depth_index, na.rm = TRUE),
                 depth_rasch = max(depth_rasch, na.rm = TRUE)),
             by = .(country_code, Country, base_treaty, year)]

## --- Aggregazione a country-year (max tra tutti gli accordi attivi) ----------
final <- out[, .(DESTA_depth_index = max(depth_index),
                 DESTA_depth_rasch = max(depth_rasch)),
             by = .(country_code, Country, year)]

setorder(final, Country, year)

## --- Validazione rapida ------------------------------------------------------
cat("\nRiepilogo output:\n")
cat("  Country-year totali:", nrow(final), "\n")
cat("  Paesi unici:", uniqueN(final$Country), "\n")
cat("  Range anni:", min(final$year), "-", max(final$year), "\n")
cat("\nDistribuzione DESTA_depth_index:\n")
print(final[, .N, by = DESTA_depth_index][order(DESTA_depth_index)])

cat("\nDepth per paese (anno massimo nel panel):\n")
last <- final[, .SD[year == max(year)], by = Country]
print(last[order(Country), .(Country, year, DESTA_depth_index, DESTA_depth_rasch)])

cat("\n[INFO] East Timor: non coperto da DESTA dyads, escluso dall'output.\n")

## --- Salva ------------------------------------------------------------------
fwrite(final, file.path(OUT_DIR, "desta_depth_country_year.csv"))
cat("\n[OK] desta_depth_country_year.csv —", nrow(final), "country-year\n")
