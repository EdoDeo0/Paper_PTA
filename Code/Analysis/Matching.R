###########################################################################
######   CEM Matching — Robustness with Restricted Control Group    #######
###########################################################################
##
## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## ── OBIETTIVO ────────────────────────────────────────────────────────────
## I partner PTA della Cina non sono scelti casualmente: sono prevalentemente
## economie asiatiche, in via di sviluppo, con relazioni commerciali pre-
## esistenti intense. Il gruppo di controllo "naturale" (tutte le destinazioni
## senza PTA) include paesi sistematicamente diversi, compromettendo la
## validità del confronto trattati/controlli.
##
## Questo script implementa il Coarsened Exact Matching (CEM) per costruire
## un sotto-campione di destinazioni di controllo comparabili ai partner PTA
## cinesi lungo le dimensioni pre-trattamento rilevanti. Il campione matched
## viene poi usato come robustness check per OLS, PPML e TWFE DiD.
##
## ── STRUTTURA ────────────────────────────────────────────────────────────
## PARTE 1 — Costruzione dataset paese-livello con covariate pre-trattamento
##           (deve essere eseguita una volta, richiede dati esterni)
## PARTE 2 — CEM matching e costruzione del campione matched
## PARTE 3 — OLS (HDFE) sul campione matched
## PARTE 4 — PPML sul campione matched
## PARTE 5 — TWFE DiD sul campione matched
## PARTE 6 — Balance table e diagnostics
##
## ── OUTPUT ───────────────────────────────────────────────────────────────
## Data:
##   matched_countries.csv        → lista paesi matched con pesi CEM
##   data_cem_matched.fst         → dataset principale filtrato sui matched
## Tabelle:
##   CEM_Balance_Table.tex        → balance table pre/post matching
##   CEM_OLS_WB_*.tex             → OLS robustness (campione matched)
##   CEM_PPML_WB_*.tex            → PPML robustness (campione matched)
##   CEM_TWFE_DiD.tex             → TWFE DiD robustness (campione matched)
##
## ── DIPENDENZE ESTERNE ───────────────────────────────────────────────────
## Per la PARTE 1 servono dati paese-livello pre-trattamento. Le fonti
## raccomandate sono commentate nel codice. Se hai già accesso a questi dati
## (es. WDI via R, CEPII, BACI), aggiorna i percorsi nelle sezioni indicate.
## In alternativa, il pacchetto `WDI` scarica PIL e PIL pro capite direttamente.
##
## Pacchetti richiesti (oltre a quelli standard del progetto):
##   install.packages(c("MatchIt", "cobalt", "WDI"))
## ── MatchIt  → CEM implementation
## ── cobalt   → love plots e balance statistics
## ── WDI      → World Bank World Development Indicators (opzionale)

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(MatchIt) # CEM

library(cobalt) # Balance diagnostics

source(here("Code/Analysis/pta_functions.R"))

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir <- here("Output/Analysis/CEM_Robustness")
dirs <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))

# ─────────────────────────────────────────────────────────────────────
# PARTE 1 — DATASET PAESE-LIVELLO (covariate pre-trattamento)
# ─────────────────────────────────────────────────────────────────────
## Costruiamo un dataset con una riga per paese destinazione. Le covariate
## devono essere misurate PRIMA dell'entrata in vigore del PTA (anno 2000).
##
## OPZIONE A (automatica) — scarica PIL e PIL pro capite via pacchetto WDI.
##   Richiede connessione internet al momento della prima esecuzione.
##   L'output viene salvato in Data/Matching/wdi_data.csv per usi futuri.
##
## OPZIONE B (manuale) — carica un CSV già preparato con le covariate.
##   Formato atteso: una riga per paese, colonne descritte sotto.
##
## Covariate richieste nel dataset paese-livello:
##   country_code    : codice numerico dei customs data (key per il merge)
##   iso3c           : codice ISO3 (usato per WDI e CEPII)
##   country_name    : nome esteso (per output)
##   log_gdp_2000    : log PIL corrente USD anno 2000
##   log_gdppc_2000  : log PIL pro capite corrente USD anno 2000
##   log_dist        : log distanza da Pechino in km (fonte: CEPII GeoDist)
##   log_imports_2000: log importazioni totali dalla Cina anno 2000 (BACI/Comtrade)
##   mfn_tariff_2000 : tariffa MFN media applicata alla Cina anno 2000 (WTO/WITS)
##   asia_dummy      : 1 se paese in Asia (definizione propria o ONU)
##   treated         : 1 se paese ha un PTA attivo con la Cina nel periodo 2000-2015

## ── Lista paesi trattati (da README) ─────────────────────────────────
## Questi sono i partner PTA della Cina nel periodo 2000–2015.
## I country_code numerici vanno recuperati da Data/Country_Codes_Custom_Data.csv.
## Aggiorna il vettore con i codici effettivi del tuo dataset.

pta_partners_names <- c(
    # ASEAN
    "Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia",
    "Myanmar", "Philippines", "Singapore", "Thailand", "Vietnam",
    # APTA / Bangkok Agreement
    "Bangladesh", "India", "Sri Lanka",
    # Bilaterali
    "Australia", "Chile", "Costa Rica", "Hong Kong", "Iceland",
    "Korea, Rep.", "Macau", "New Zealand", "Pakistan", "Peru", "Switzerland"
)

## ── Caricamento country codes ─────────────────────────────────────────
country_codes_file <- here("Data/Country_Codes_Custom_Data.csv")
stopifnot("File country codes non trovato!" = file.exists(country_codes_file))

dt_codes <- fread(country_codes_file)
# Adatta i nomi delle colonne se necessario: il file deve avere
# una colonna con il nome del paese e una con il codice numerico.
# Rinomina qui se le colonne si chiamano diversamente:
setnames(dt_codes, old = c("country", "country_code"),
         new = c("country_name", "country_code"))

cat("Colonne in Country_Codes_Custom_Data.csv:\n")
print(names(dt_codes))

# ─────────────────────────────────────────────────────────────────────
# PARTE 1A — Download PIL/PIL pc via WDI (opzionale ma consigliato)
# ─────────────────────────────────────────────────────────────────────
wdi_cache_file <- here("Data/Matching/wdi_data.csv")

if (!file.exists(wdi_cache_file)) {
    if (!requireNamespace("WDI", quietly = TRUE)) {
        stop("Installa il pacchetto WDI: install.packages('WDI')")
    }
    library(WDI)

    cat("Scaricando dati WDI...\n")
    wdi_raw <- WDI(
        country = "all",
        indicator = c(
            "NY.GDP.MKTP.CD", # PIL corrente USD
            "NY.GDP.PCAP.CD" # PIL pro capite corrente USD
        ),
        start = 2000,
        end = 2000,
        extra = TRUE # include iso3c, region, ecc.
    )

    wdi_dt <- as.data.table(wdi_raw)
    wdi_dt <- wdi_dt[!is.na(NY.GDP.MKTP.CD) & !is.na(NY.GDP.PCAP.CD)]
    wdi_dt[, log_gdp_2000 := log(NY.GDP.MKTP.CD)]
    wdi_dt[, log_gdppc_2000 := log(NY.GDP.PCAP.CD)]

    dir.create(here("Data/Matching"), showWarnings = FALSE, recursive = TRUE)
    fwrite(
        wdi_dt[, .(iso3c, country, log_gdp_2000, log_gdppc_2000)],
        wdi_cache_file
    )
    cat("WDI salvato in:", wdi_cache_file, "\n")
} else {
    wdi_dt <- fread(wdi_cache_file)
    cat("WDI caricato da cache.\n")
}

# ─────────────────────────────────────────────────────────────────────
# PARTE 1B — Caricamento covariate aggiuntive (distanza, import, tariffe)
# ─────────────────────────────────────────────────────────────────────
## Per distanza e import, hai due opzioni:
##
## (1) CEPII GeoDist: scaricabile da http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6
##     Contiene distanza capitale-a-capitale e popolazione. Variabile: distcap o dist.
##
## (2) BACI: per import_2000 dalla Cina. Variabile: value aggregata su tutti i prodotti
##     per coppia (country_i = Cina, country_j = destinazione) nell'anno 2000.
##
## (3) WITS/WTO: per tariffe MFN 2000. Disponibile via https://wits.worldbank.org/
##
## Se non hai questi dati, puoi procedere con SOLO PIL e PIL pro capite + asia_dummy,
## impostando use_full_covariates = FALSE nella sezione matching sotto.

use_full_covariates <- TRUE # Imposta TRUE se hai distanza, import e tariffe

dir.create(here("Data/Matching"), showWarnings = FALSE, recursive = TRUE)

# ── 1. DISTANZA (CEPII GeoDist via pacchetto R) ──────────────────────
if (!requireNamespace("cepiigeodist", quietly = TRUE))
  install.packages("cepiigeodist")
library(cepiigeodist)

data("dist_cepii")
dt_dist_china <- as.data.table(dist_cepii)[
  iso_o == "CHN",
  .(iso3c = iso_d, log_dist = log(distcap))
]
fwrite(dt_dist_china, here("Data/Matching/cepii_dist.csv"))
cat("Distanza CEPII: OK —", nrow(dt_dist_china), "paesi\n")

# ── 2. IMPORTAZIONI DALLA CINA 2000 (BACI) ───────────────────────────
# Prerequisito: scarica BACI_HS92_V202401.zip da cepii.fr (registrazione
# gratuita) ed estrai i due file nella cartella Data/Matching/:
#   - BACI_HS92_Y2000_V202401.csv
#   - country_codes_V202401.csv

baci_file  <- here("Data/Matching/BACI_HS92_Y2000_V202601.csv")
baci_codes <- here("Data/Matching/country_codes_V202601.csv")

if (file.exists(baci_file) && file.exists(baci_codes)) {
  baci_2000 <- fread(baci_file)
  cc_baci   <- fread(baci_codes)
  
  baci_china <- baci_2000[i == 156,  # 156 = China in BACI
                          .(imports_from_china_2000 = sum(v, na.rm = TRUE)),
                          by = j]
  baci_china <- merge(
    baci_china,
    cc_baci[, .(j = country_code, iso3c = country_iso3)],
    by = "j"
  )
  baci_china[, log_imports_2000 := log(imports_from_china_2000 + 1)]
  fwrite(baci_china[, .(iso3c, imports_from_china_2000, log_imports_2000)],
         here("Data/Matching/baci_imports_from_china_2000.csv"))
  cat("BACI import 2000: OK —", nrow(baci_china), "paesi\n")
} else {
  cat("ATTENZIONE: file BACI non trovati in Data/Matching/.\n")
  cat("Scarica BACI_HS92_V202401.zip da https://www.cepii.fr (registrazione gratuita)\n")
  cat("ed estrai i file nella cartella Data/Matching/ prima di proseguire.\n")
}

# ── 3. TARIFFE MFN 2000 (World Bank WITS via wbstats) ────────────────
if (!requireNamespace("wbstats", quietly = TRUE))
  install.packages("wbstats")
library(wbstats)

mfn_raw <- wb_data(
  indicator   = "TM.TAX.MRCH.SM.AR.ZS",
  start_date  = 2000,
  end_date    = 2000,
  return_wide = TRUE
)
dt_mfn <- as.data.table(mfn_raw)[
  !is.na(TM.TAX.MRCH.SM.AR.ZS),
  .(iso3c = iso3c, mfn_tariff_2000 = TM.TAX.MRCH.SM.AR.ZS)
]
fwrite(dt_mfn, here("Data/Matching/mfn_tariffs_2000.csv"))
cat("Tariffe MFN 2000: OK —", nrow(dt_mfn), "paesi con dati\n")

# ── Caricamento in memoria per uso nel matching ───────────────────────
dt_dist_china <- fread(here("Data/Matching/cepii_dist.csv"))
dt_mfn        <- fread(here("Data/Matching/mfn_tariffs_2000.csv"))

imp_file <- here("Data/Matching/baci_imports_from_china_2000.csv")
dt_imp   <- if (file.exists(imp_file)) fread(imp_file) else NULL


# ─────────────────────────────────────────────────────────────────────
# PARTE 1C — Calcolo asia_dummy e costruzione dataset finale paesi
# ─────────────────────────────────────────────────────────────────────
## Asia dummy: paesi classificati come East Asia, Southeast Asia,
## South Asia o Central Asia secondo la classificazione ONU.
## Lista ISO3 costruita manualmente; aggiorna se necessario.

asia_iso3 <- c(
    # East Asia
    "CHN", "JPN", "KOR", "PRK", "MNG", "TWN", "HKG", "MAC",
    # Southeast Asia
    "BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP",
    "THA", "TLS", "VNM",
    # South Asia
    "AFG", "BGD", "BTN", "IND", "MDV", "NPL", "PAK", "LKA",
    # Central Asia
    "KAZ", "KGZ", "TJK", "TKM", "UZB"
)

## Costruiamo il dataset paese-livello a partire da WDI
dt_country <- copy(wdi_dt)
setnames(dt_country, "country", "country_name", skip_absent = TRUE)

dt_country[, asia_dummy := as.integer(iso3c %in% asia_iso3)]

## Aggiungi covariate esterne se disponibili
if (use_full_covariates) {
    if (!is.null(dt_dist_china)) {
        dt_country <- merge(dt_country, dt_dist_china, by = "iso3c", all.x = TRUE)
    }
    if (!is.null(dt_imp)) {
        dt_country <- merge(dt_country, dt_imp[, .(iso3c, log_imports_2000)],
            by = "iso3c", all.x = TRUE
        )
    }
    if (!is.null(dt_mfn)) {
        dt_country <- merge(dt_country, dt_mfn[, .(iso3c, mfn_tariff_2000)],
            by = "iso3c", all.x = TRUE
        )
    }
}

## Aggiungi country_code numerico (key del dataset customs)
## IMPORTANTE: questo merge richiede che dt_codes abbia una colonna iso3c o
## un identificatore comune. Adatta il codice al tuo file Country_Codes_Custom_Data.csv.
## Se dt_codes usa nomi di paese anziché ISO3, userai un merge per nome
## (meno affidabile — meglio aggiungere ISO3 manualmente al csv se non presente).

# Esempio con merge per nome (adatta se usi ISO3):
# dt_country <- merge(dt_country, dt_codes[, .(country_name, country_code)],
#                     by = "country_name", all.x = TRUE)

# Alternativa: costruisci manualmente la mappatura iso3c → country_code
# basandoti sui 14 partner e sulle destinazioni di controllo nel tuo dataset.
# Questo è il metodo più robusto dato che Country_Codes_Custom_Data.csv
# usa nomi propri non standardizzati.

## ── MAPPATURA MANUALE (da completare) ────────────────────────────────
## Aggiungi qui i country_code numerici corrispondenti agli iso3c.
## I codici numerici li trovi in Data/Country_Codes_Custom_Data.csv.
## Esempio:
manual_iso3_to_code <- data.table(
  iso3c = c(
    # ── PARTNER PTA (trattati) ──
    "BGD",  # Bangladesh
    "BRN",  # Brunei
    "MMR",  # Myanmar
    "KHM",  # Cambodia
    "HKG",  # HongKong
    "IND",  # India
    "IDN",  # Indonesia
    "LAO",  # Laos,PDR
    "MAC",  # Macau
    "MYS",  # Malaysia
    "PAK",  # Pakistan
    "PHL",  # Philippines
    "SGP",  # Singapore
    "KOR",  # Korea Rep.
    "LKA",  # Sri Lanka
    "THA",  # Thailand
    "VNM",  # Vietnam
    "TLS",  # East Timor
    "ISL",  # Iceland
    "CHE",  # Switzerland
    "CHL",  # Chile
    "CRI",  # Costa Rica
    "PER",  # Peru
    "AUS",  # Australia
    "NZL",  # New Zealand
    # ── CONTROLLI ASIA ──
    "AFG",  # Afghanistan
    "BTN",  # Bhutan
    "CYP",  # Cyprus
    "JPN",  # Japan
    "JOR",  # Jordan
    "KWT",  # Kuwait
    "LBN",  # Lebanon
    "MDV",  # Maldives
    "MNG",  # Mongolia
    "NPL",  # Nepal
    "OMN",  # Oman
    "QAT",  # Qatar
    "SAU",  # Saudi Arabia
    "SYR",  # Syrian
    "TUR",  # Turkey
    "ARE",  # United Arab Emirates
    "YEM",  # Republic of Yemen
    "KAZ",  # Kazakhstan
    "KGZ",  # Kirghizia
    "TJK",  # Tadzhikistan
    "TKM",  # Turkmenistan
    "UZB",  # Uzbekstan
    # ── CONTROLLI AFRICA ──
    "DZA",  # Algeria
    "AGO",  # Angola (NB: "Angora" nel CSV = errore tipografico)
    "BEN",  # Benin
    "BWA",  # Botswana
    "BDI",  # Burundi
    "CMR",  # Cameroon
    "CAF",  # Central African Rep.
    "TCD",  # Chad
    "COM",  # Comoros
    "COG",  # Congo
    "DJI",  # Djibouti
    "EGY",  # Egypt
    "GNQ",  # Eq.Guinea
    "ETH",  # Ethiopia
    "GAB",  # Gabon
    "GMB",  # Gambia
    "GHA",  # Ghana
    "GIN",  # Guinea
    "GNB",  # Guinea Bissau
    "CIV",  # Cote d'Ivoire
    "KEN",  # Kenya
    "LBR",  # Liberia
    "LBY",  # Libyan Arab Jm
    "MDG",  # Madagascar
    "MWI",  # Malawi
    "MLI",  # Mali
    "MRT",  # Mauritania
    "MUS",  # Mauritius
    "MAR",  # Morocco
    "MOZ",  # Mozambique
    "NAM",  # Namibia
    "NER",  # Niger
    "NGA",  # Nigeria
    "RWA",  # Rwanda
    "STP",  # Sao Tome & Principe
    "SEN",  # Senegal
    "SYC",  # Seychelles
    "SLE",  # Sierra Leone
    "SOM",  # Somalia
    "ZAF",  # S.Africa
    "SDN",  # Sudan
    "TZA",  # Tanzania
    "TGO",  # Togo
    "TUN",  # Tunisia
    "UGA",  # Uganda
    "BFA",  # Burkina Faso
    "COD",  # Congo,DR
    "ZMB",  # Zambia
    "ZWE",  # Zimbabwe
    "LSO",  # Lesotho
    "SWZ",  # Swaziland
    "ERI",  # Eritrea
    "SSD",  # Republic of South Sudan
    # ── CONTROLLI EUROPA ──
    "BEL",  # Bel+Lux
    "DNK",  # Denmark
    "GBR",  # United Kingdom
    "DEU",  # Germany
    "FRA",  # France
    "IRL",  # Ireland
    "ITA",  # Italy
    "NLD",  # Netherlands
    "GRC",  # Greece
    "PRT",  # Portugal
    "ESP",  # Spain
    "ALB",  # Albania
    "AND",  # Andorra
    "AUT",  # Austria
    "BGR",  # Bulgaria
    "FIN",  # Finland
    "HUN",  # Hungary
    "LIE",  # Liechtenstein
    "MLT",  # Malta
    "MCO",  # Monaco
    "NOR",  # Norway
    "POL",  # Poland
    "ROU",  # Romania
    "SWE",  # Sweden
    "EST",  # Estonia
    "LVA",  # Latvia
    "LTU",  # Lithuania
    "GEO",  # Georgia
    "ARM",  # Armenia
    "AZE",  # Azerbaijan
    "BLR",  # Belarus
    "MDA",  # Moldova
    "RUS",  # Russian Federation
    "UKR",  # Ukraine
    "SVN",  # Slovenia
    "HRV",  # Croatia
    "CZE",  # Czech Republic
    "SVK",  # Slovakia
    "MKD",  # Macedonia,FYR
    "BIH",  # Bosnia and Hercegovina
    "SRB",  # Serbia
    "MNE",  # Montenegro
    # ── CONTROLLI AMERICHE ──
    "ARG",  # Argentina
    "BLZ",  # Belize
    "BOL",  # Bolivia
    "BRA",  # Brazil
    "COL",  # Colombia
    "CUB",  # Cuba
    "DOM",  # Dominican Republic
    "ECU",  # Ecuador
    "GTM",  # Guatemala
    "GUY",  # Guyana
    "HTI",  # Haiti
    "HND",  # Honduras
    "JAM",  # Jamaica
    "MEX",  # Mexico
    "NIC",  # Nicaragua
    "PAN",  # Panama
    "PRY",  # Paraguay
    "SLV",  # El Salvador
    "SUR",  # Suriname
    "TTO",  # Trinidad and Tobago
    "URY",  # Uruguay
    "VEN",  # Venezuela
    # ── NORD AMERICA + OCEANIA ──
    "CAN",  # Canada
    "USA",  # United States
    "FJI",  # Fiji
    "PNG",  # Papua New Guinea
    "WSM",  # Samoa
    "TON",  # Tonga
    "SLB",  # Solomon Islands
    "VUT"   # Vanuatu
  ),
  country_code = c(
    # ── PARTNER PTA (trattati) ──
    103L, 105L, 106L, 107L, 110L, 111L, 112L, 119L,
    121L, 122L, 127L, 129L, 132L, 133L, 134L, 136L,
    141L, 144L, 322L, 331L, 412L, 415L, 434L, 601L, 609L,
    # ── CONTROLLI ASIA ──
    101L, 104L, 108L, 116L, 117L, 118L, 120L, 123L,
    124L, 125L, 126L, 130L, 131L, 135L, 137L, 138L,
    139L, 145L, 146L, 147L, 148L, 149L,
    # ── CONTROLLI AFRICA ──
    201L, 202L, 203L, 204L, 205L, 206L, 209L, 211L,
    212L, 213L, 214L, 215L, 216L, 217L, 218L, 219L,
    220L, 221L, 222L, 223L, 224L, 225L, 226L, 227L,
    228L, 229L, 230L, 231L, 232L, 233L, 234L, 235L,
    236L, 238L, 239L, 240L, 241L, 242L, 243L, 244L,
    246L, 247L, 248L, 249L, 250L, 251L, 252L, 253L,
    254L, 255L, 257L, 258L, 260L,
    # ── CONTROLLI EUROPA ──
    301L, 302L, 303L, 304L, 305L, 306L, 307L, 309L,
    310L, 311L, 312L, 313L, 314L, 315L, 316L, 318L,
    321L, 323L, 324L, 325L, 326L, 327L, 328L, 330L,
    334L, 335L, 336L, 337L, 338L, 339L, 340L, 343L,
    344L, 347L, 350L, 351L, 352L, 353L, 354L, 355L,
    358L, 359L,
    # ── CONTROLLI AMERICHE ──
    402L, 406L, 408L, 410L, 413L, 416L, 418L, 419L,
    423L, 424L, 425L, 426L, 427L, 429L, 431L, 432L,
    433L, 440L, 441L, 442L, 444L, 445L,
    # ── NORD AMERICA + OCEANIA ──
    501L, 502L, 603L, 611L, 617L, 614L, 613L, 608L
  )
)

## NOTA: compila i codici numerici sopra prima di eseguire lo script.
## I valori NA causeranno l'esclusione di quei paesi dal merge finale.

dt_country <- merge(dt_country, manual_iso3_to_code, by = "iso3c", all.x = TRUE)

## Flag paesi trattati
dt_country[, treated := as.integer(iso3c %in% c(
    "AUS", "BGD", "BRN", "KHM", "CHL", "CRI", "HKG", "ISL",
    "IDN", "IND", "KOR", "LAO", "MYS", "MAC", "MMR", "NZL",
    "PAK", "PHL", "PER", "SGP", "LKA", "CHE", "THA", "TLS", "VNM"
))]

cat(sprintf("\nPaesi trattati (PTA): %d\n", sum(dt_country$treated, na.rm = TRUE)))
cat(sprintf("Paesi di controllo (no PTA): %d\n", sum(!dt_country$treated, na.rm = TRUE)))
cat(sprintf(
    "Paesi con country_code disponibile: %d\n",
    sum(!is.na(dt_country$country_code))
))

# ─────────────────────────────────────────────────────────────────────
# PARTE 2 — COARSENED EXACT MATCHING (CEM)
# ─────────────────────────────────────────────────────────────────────
## Il CEM funziona così:
##   1. Discretizza le covariate continue in bins ("coarsening")
##   2. Fa matching esatto sui bins
##   3. Paesi trattati senza alcun controllo nello stesso stratum vengono
##      scartati ("pruned") — se molti vengono scartati, allargare i bins
##   4. Ai paesi matched vengono assegnati pesi proporzionali
##
## Covariate usate in base alla disponibilità:
covariates_base <- c("log_gdp_2000", "log_gdppc_2000", "asia_dummy")
covariates_full <- c(
    "log_gdp_2000", "log_gdppc_2000", "log_dist",
    "log_imports_2000", "mfn_tariff_2000", "asia_dummy"
)

#covariates_used <- if (use_full_covariates) covariates_full else covariates_base
covariates_used <- c("log_gdp_2000", "log_gdppc_2000", "log_dist",
                     "log_imports_2000", "mfn_tariff_2000")

## Rimuovi osservazioni con NA nelle covariate di matching
dt_match <- dt_country[complete.cases(dt_country[, ..covariates_used]) &
    !is.na(treated)]

cat(sprintf(
    "\nDataset per matching: %d paesi (%d trattati, %d controlli)\n",
    nrow(dt_match),
    sum(dt_match$treated),
    sum(!dt_match$treated)
))

## Formula di matching
match_formula <- as.formula(paste("treated ~", paste(covariates_used, collapse = " + ")))

## CEM con cutpoints automatici (Sturges rule per le variabili continue)
## L'argomento `cutpoints` può essere specificato manualmente per maggiore controllo.
## Esempio: cutpoints = list(log_gdp_2000 = 4) usa 4 bins per il PIL.
set.seed(42)
cem_out <- matchit(
    formula    = match_formula,
    data       = as.data.frame(dt_match),
    method     = "cem",
    estimand   = "ATT", # Average Treatment effect on the Treated
    cutpoints  = list(
      log_gdp_2000 = 3,
      log_gdppc_2000 = 3,
      log_dist = 3,
      log_imports_2000 = 3,
      mfn_tariff_2000 = 3
      #asia_dummy = c(0, 1) # dummy binaria
    ),
)

cat("\n=== SOMMARIO CEM ===\n")
print(summary(cem_out, un = TRUE))

## Salva il sommario in un file di testo
sink(file.path(out_dir, "CEM_Summary.txt"))
print(summary(cem_out, un = TRUE))
sink()

## Estrai dati matched con pesi
dt_matched <- as.data.table(match.data(cem_out))

cat(sprintf(
    "\nPaesi nel campione matched: %d (%d trattati, %d controlli)\n",
    nrow(dt_matched),
    sum(dt_matched$treated),
    sum(!dt_matched$treated)
))

## Lista dei country_code nel campione matched
matched_country_codes <- dt_matched[!is.na(country_code), unique(country_code)]
cat(sprintf(
    "Country codes matched con dati customs: %d\n",
    length(matched_country_codes)
))

## Salva la lista dei paesi matched
fwrite(
    dt_matched[, .(
        iso3c, country_name, country_code, treated,
        subclass, weights
    )],
    file.path(out_dir, "matched_countries.csv")
)

# ─────────────────────────────────────────────────────────────────────
# PARTE 2A — BALANCE TABLE (LaTeX)
# ─────────────────────────────────────────────────────────────────────
## La balance table mostra la differenza nelle covariate tra trattati e
## controlli PRIMA e DOPO il matching. Un matching riuscito riduce
## sensibilmente le differenze standardizzate (SMD < 0.1 come soglia).

## Love plot (standardized mean differences)
p_love <- love.plot(
    cem_out,
    stats = "mean.diffs",
    threshold = 0.1,
    var.order = "unadjusted",
    abs = TRUE,
    title = "Covariate Balance: Pre vs Post CEM Matching",
    sample.names = c("Unmatched", "Matched (CEM)"),
    stars = "raw",
    line = TRUE
)

ggsave(file.path(out_dir, "CEM_LovePlot.pdf"), plot = p_love, width = 7, height = 5)
ggsave(file.path(out_dir, "CEM_LovePlot.png"),
    plot = p_love, width = 7, height = 5,
    dpi = 300
)

## Balance table manuale in LaTeX
bal_stats <- bal.tab(
    cem_out,
    stats = c("mean.diffs", "variance.ratios"),
    un = TRUE,
    thresholds = c(m = 0.1)
)

bal_df <- as.data.frame(bal_stats$Balance)
bal_df$Variable <- rownames(bal_df)

## Scrivi tabella LaTeX
write_balance_latex <- function(bal_df, filepath) {
    lines <- c(
        "\\begin{table}[htbp]",
        "\\centering",
        "\\caption{Covariate Balance: Pre- and Post-CEM Matching}",
        "\\label{tab:cem_balance}",
        "\\small",
        "\\begin{tabular}{lcccc}",
        "\\hline\\hline",
        "& \\multicolumn{2}{c}{Unmatched} & \\multicolumn{2}{c}{Matched (CEM)} \\\\",
        "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
        "Variable & SMD & Var. Ratio & SMD & Var. Ratio \\\\",
        "\\hline"
    )

    for (i in seq_len(nrow(bal_df))) {
        row <- bal_df[i, ]
        var_name <- gsub("_", "\\\\_", row$Variable)
        smd_un <- ifelse(!is.na(row[["Diff.Un"]]),
            formatC(row[["Diff.Un"]], digits = 3, format = "f"), "."
        )
        vr_un <- ifelse(!is.na(row[["V.Ratio.Un"]]),
            formatC(row[["V.Ratio.Un"]], digits = 3, format = "f"), "."
        )
        smd_adj <- ifelse(!is.na(row[["Diff.Adj"]]),
            formatC(row[["Diff.Adj"]], digits = 3, format = "f"), "."
        )
        vr_adj <- ifelse(!is.na(row[["V.Ratio.Adj"]]),
            formatC(row[["V.Ratio.Adj"]], digits = 3, format = "f"), "."
        )
        lines <- c(
            lines,
            sprintf(
                "%s & %s & %s & %s & %s \\\\",
                var_name, smd_un, vr_un, smd_adj, vr_adj
            )
        )
    }

    lines <- c(
        lines,
        "\\hline\\hline",
        paste0(
            "\\multicolumn{5}{l}{\\footnotesize \\textit{Note:} ",
            "SMD = Standardised Mean Difference. Threshold: SMD $<$ 0.10.} \\\\"
        ),
        "\\end{tabular}",
        "\\end{table}"
    )

    writeLines(lines, filepath)
    cat("Balance table salvata in:", filepath, "\n")
}

write_balance_latex(bal_df, file.path(out_dir, "CEM_Balance_Table.tex"))

# ─────────────────────────────────────────────────────────────────────
# PARTE 3 — FILTRAGGIO DEL DATASET PRINCIPALE
# ─────────────────────────────────────────────────────────────────────
## Carica il dataset completo, filtra sui country_code del campione matched,
## e salva come .fst per uso nei blocchi di regressione successivi.
##
## NOTA: i pesi CEM vengono aggiunti al dataset filtrato. Per le regressioni
## con pesi MatchIt, usa l'argomento `weights` in feols/fepois.
## Per semplicità, il robustness check principale è la stima NON pesata
## sul campione ristretto (equivale a dare peso 0 ai non-matched).
## La versione pesata è una alternativa più efficiente ma meno intuitiva.

cat("\nCaricamento dataset principale e filtraggio sui matched...\n")

vars_needed <- c(
    "ln_export", "ln_export_qua", "ln_export_value",
    "export", "exp_qua", "uv_exp",
    "WB_EP_Depth", "WB_EP_Depth_Binary",
    "TREND_EP_Count", "TREND_EP_Count_Binary",
    "env_good", "tariffs", "ln_hhi_baci",
    "fpd", "year", "pdt", "hs6", "country_code"
)

dt_full <- as.data.table(read_fst(data_file, columns = vars_needed))

## Filtra sui paesi matched
dt_cem <- dt_full[country_code %in% matched_country_codes]

cat(sprintf(
    "Osservazioni nel campione originale: %s\n",
    format(nrow(dt_full), big.mark = ",")
))
cat(sprintf(
    "Osservazioni nel campione matched: %s (%.1f%%)\n",
    format(nrow(dt_cem), big.mark = ","),
    100 * nrow(dt_cem) / nrow(dt_full)
))
cat(sprintf(
    "Destinazioni nel campione matched: %d\n",
    dt_cem[, uniqueN(country_code)]
))

## Aggiungi pesi CEM al dataset filtrato
cem_weights_dt <- dt_matched[
    !is.na(country_code),
    .(country_code, cem_weight = weights)
]
## Poiché i pesi variano per paese (non per osservazione), facciamo merge m:1
dt_cem <- merge(dt_cem, unique(cem_weights_dt), by = "country_code", all.x = TRUE)
dt_cem[is.na(cem_weight), cem_weight := 1] # trattati hanno peso 1

## Salva il dataset matched
cem_file <- file.path(out_dir, "data_cem_matched.fst")
write_fst(dt_cem, cem_file, compress = 50)
cat("Dataset matched salvato in:", cem_file, "\n")

rm(dt_full, dt_cem)
gc()

# ─────────────────────────────────────────────────────────────────────
# PARTE 4 — OLS HDFE SUL CAMPIONE MATCHED
# ─────────────────────────────────────────────────────────────────────
## Stesse specifiche dell'analisi principale; il campione ristretto
## è l'unica differenza rispetto ai risultati originali.
## Il confronto diretto OLS_main vs CEM_OLS è il robustness check.

cat("\n=== PARTE 4: OLS sul campione matched ===\n")
start <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

cm_wb <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend_int <- c(
    "TREND_EP_Count"             = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good"    = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                    = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                = "\\textit{ln HHI\\textsubscript{pdt}}"
)

## BLOCCO OLS-1: WB No Interaction
f_ols1 <- c(
    "ln_export ~ WB_EP_Depth | fpd + year",
    "ln_export_qua ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats_ols1 <- run_block(f_ols1, "CEM_OLS_WB_NoInt", "ols",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ols
)
make_table(stats_ols1, cm_wb, "CEM_OLS_WB_No_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ols
)

## BLOCCO OLS-2: WB Interaction
f_ols2 <- c(
    "ln_export ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_ols2 <- run_block(f_ols2, "CEM_OLS_WB_Int", "ols",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ols
)
make_table(stats_ols2, cm_wb_int, "CEM_OLS_WB_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ols
)

## BLOCCO OLS-3: TREND No Interaction
f_ols3 <- c(
    "ln_export ~ TREND_EP_Count | fpd + year",
    "ln_export_qua ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats_ols3 <- run_block(f_ols3, "CEM_OLS_TREND_NoInt", "ols",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ols
)
make_table(stats_ols3, cm_trend, "CEM_OLS_TREND_No_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ols
)

## BLOCCO OLS-4: TREND Interaction
f_ols4 <- c(
    "ln_export ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_ols4 <- run_block(f_ols4, "CEM_OLS_TREND_Int", "ols",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ols
)
make_table(stats_ols4, cm_trend_int, "CEM_OLS_TREND_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ols
)

cat("OLS completato in:", round(as.numeric(now() - start, units = "mins"), 1), "minuti\n")

# ─────────────────────────────────────────────────────────────────────
# PARTE 5 — PPML SUL CAMPIONE MATCHED
# ─────────────────────────────────────────────────────────────────────
cat("\n=== PARTE 5: PPML sul campione matched ===\n")
start <- now()
show_stats_ppml <- c("nobs", "n_clust")

## BLOCCO PPML-1: WB No Interaction
f_ppml1 <- c(
    "export ~ WB_EP_Depth | fpd + year",
    "exp_qua ~ WB_EP_Depth | fpd + year",
    "uv_exp ~ WB_EP_Depth | fpd + year",
    "export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats_ppml1 <- run_block(f_ppml1, "CEM_PPML_WB_NoInt", "ppml",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ppml
)
make_table(stats_ppml1, cm_wb, "CEM_PPML_WB_No_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ppml
)

## BLOCCO PPML-2: WB Interaction
f_ppml2 <- c(
    "export ~ WB_EP_Depth * env_good | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good | fpd + year",
    "uv_exp ~ WB_EP_Depth * env_good | fpd + year",
    "export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_ppml2 <- run_block(f_ppml2, "CEM_PPML_WB_Int", "ppml",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ppml
)
make_table(stats_ppml2, cm_wb_int, "CEM_PPML_WB_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ppml
)

## BLOCCO PPML-3: TREND No Interaction
f_ppml3 <- c(
    "export ~ TREND_EP_Count | fpd + year",
    "exp_qua ~ TREND_EP_Count | fpd + year",
    "uv_exp ~ TREND_EP_Count | fpd + year",
    "export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats_ppml3 <- run_block(f_ppml3, "CEM_PPML_TREND_NoInt", "ppml",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ppml
)
make_table(stats_ppml3, cm_trend, "CEM_PPML_TREND_No_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ppml
)

## BLOCCO PPML-4: TREND Interaction
f_ppml4 <- c(
    "export ~ TREND_EP_Count * env_good | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good | fpd + year",
    "uv_exp ~ TREND_EP_Count * env_good | fpd + year",
    "export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_ppml4 <- run_block(f_ppml4, "CEM_PPML_TREND_Int", "ppml",
    cem_file, dirs$models,
    vcov = ~pdt,
    requested_stats = show_stats_ppml
)
make_table(stats_ppml4, cm_trend_int, "CEM_PPML_TREND_Interaction.tex", dirs$tables,
    digits = 5, show_stats = show_stats_ppml
)

cat("PPML completato in:", round(as.numeric(now() - start, units = "mins"), 1), "minuti\n")

# ─────────────────────────────────────────────────────────────────────
# PARTE 6 — TWFE DiD SUL CAMPIONE MATCHED
# ─────────────────────────────────────────────────────────────────────
## Il DiD TWFE è particolarmente sensibile alla composizione del gruppo
## di controllo: parallel trends è più credibile quando i controlli
## sono simili ai trattati lungo le covariate pre-trattamento.
## Questo blocco ripete il TWFE del Replication_Neri_Laine sul campione CEM.

cat("\n=== PARTE 6: TWFE DiD sul campione matched ===\n")
start <- now()

## Carica e prepara il dataset matched (aggiungendo variabili per DiD)
dt_did <- as.data.table(read_fst(cem_file, columns = c(
    "ln_export", "WB_EP_Depth", "WB_EP_Depth_Binary",
    "TREND_EP_Count", "TREND_EP_Count_Binary",
    "env_good", "tariffs", "ln_hhi_baci",
    "fpd", "year", "pdt", "country_code"
)))

## Costruisci dummy PTA binaria (trattamento attivo)
dt_did[, PTA_WB := as.integer(WB_EP_Depth > 0)]
dt_did[, PTA_TREND := as.integer(TREND_EP_Count > 0)]

## Salva per uso nelle regressioni
did_file <- file.path(out_dir, "data_cem_did.fst")
write_fst(dt_did, did_file, compress = 50)
rm(dt_did)
gc()

cm_twfe <- c(
    "PTA_WB"              = "\\textit{PTA}_{dt} \\textit{(WB)}",
    "PTA_TREND"           = "\\textit{PTA}_{dt} \\textit{(TREND)}",
    "PTA_WB:env_good"     = "\\textit{PTA}_{dt} \\textit{(WB)} $\\times$ \\textit{EnvGood}_{p}",
    "PTA_TREND:env_good"  = "\\textit{PTA}_{dt} \\textit{(TREND)} $\\times$ \\textit{EnvGood}_{p}",
    "tariffs"             = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"         = "\\textit{ln HHI\\textsubscript{pdt}}"
)

f_twfe <- c(
    "ln_export ~ PTA_WB + tariffs + ln_hhi_baci | fpd + year",
    "ln_export ~ PTA_TREND + tariffs + ln_hhi_baci | fpd + year",
    "ln_export ~ PTA_WB * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export ~ PTA_TREND * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_twfe <- run_block(f_twfe, "CEM_TWFE_DiD", "ols",
    did_file, dirs$models,
    vcov = ~pdt,
    requested_stats = c("nobs", "r2", "n_clust")
)
make_table(stats_twfe, cm_twfe, "CEM_TWFE_DiD.tex", dirs$tables,
    digits = 5,
    dep_vars = c("WB", "TREND", "WB \\times EnvGood", "TREND \\times EnvGood"),
    dep_subscript = "fpdt",
    group_headers = c("ln Exports — TWFE DiD (CEM Matched Sample)"),
    group_cols = c(4),
    show_stats = c("nobs", "r2", "n_clust")
)

cat("TWFE DiD completato in:", round(as.numeric(now() - start, units = "mins"), 1), "minuti\n")

# ─────────────────────────────────────────────────────────────────────
# RIEPILOGO FINALE
# ─────────────────────────────────────────────────────────────────────
cat("\n=== CEM ROBUSTNESS — COMPLETATO! ===\n")
cat("Output directory:", out_dir, "\n")
cat("\nTabelle LaTeX prodotte:\n")
cat("  CEM_Balance_Table.tex\n")
cat("  CEM_OLS_WB_No_Interaction.tex\n")
cat("  CEM_OLS_WB_Interaction.tex\n")
cat("  CEM_OLS_TREND_No_Interaction.tex\n")
cat("  CEM_OLS_TREND_Interaction.tex\n")
cat("  CEM_PPML_WB_No_Interaction.tex\n")
cat("  CEM_PPML_WB_Interaction.tex\n")
cat("  CEM_PPML_TREND_No_Interaction.tex\n")
cat("  CEM_PPML_TREND_Interaction.tex\n")
cat("  CEM_TWFE_DiD.tex\n")
cat("\nFigure:\n")
cat("  CEM_LovePlot.pdf / .png\n")
