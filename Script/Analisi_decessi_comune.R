library(data.table)
library(tidyverse)
library(janitor)

if (basename(getwd()) == "Script") setwd("..")

# ── Helper ─────────────────────────────────────────────────────────────────────

age_to_fascia <- function(anni) {
  case_when(
    anni == 0                 ~ "0",
    anni >= 1  & anni <= 4   ~ "1-4",
    anni >= 5  & anni <= 9   ~ "5-9",
    anni >= 10 & anni <= 14  ~ "10-14",
    anni >= 15 & anni <= 19  ~ "15-19",
    anni >= 20 & anni <= 24  ~ "20-24",
    anni >= 25 & anni <= 29  ~ "25-29",
    anni >= 30 & anni <= 34  ~ "30-34",
    anni >= 35 & anni <= 39  ~ "35-39",
    anni >= 40 & anni <= 44  ~ "40-44",
    anni >= 45 & anni <= 49  ~ "45-49",
    anni >= 50 & anni <= 54  ~ "50-54",
    anni >= 55 & anni <= 59  ~ "55-59",
    anni >= 60 & anni <= 64  ~ "60-64",
    anni >= 65 & anni <= 69  ~ "65-69",
    anni >= 70 & anni <= 74  ~ "70-74",
    anni >= 75 & anni <= 79  ~ "75-79",
    anni >= 80 & anni <= 84  ~ "80-84",
    anni >= 85 & anni <= 89  ~ "85-89",
    anni >= 90 & anni <= 94  ~ "90-94",
    anni >= 95 & anni <= 99  ~ "95-99",
    anni >= 100               ~ "100+"
  )
}

read_posas_comuni <- function(path, anno) {
  as.data.frame(fread(path, sep = ";", skip = 1)) %>%
    clean_names() %>%
    select(codice_comune, eta, totale_maschi, totale_femmine) %>%
    mutate(
      anno              = anno,
      totale            = totale_maschi + totale_femmine,
      fascia_anagrafica = age_to_fascia(eta)
    ) %>%
    group_by(codice_comune, anno, fascia_anagrafica) %>%
    summarise(pop = sum(totale), .groups = "drop")
}

# ── Popolazione ────────────────────────────────────────────────────────────────

# 2015-2019: dati provinciali ISTAT già convertiti in long
pop_2015_2019 <- fread("Input_Comuni/popolazione_comuni_2015_2019.csv") %>%
  as_tibble() %>%
  select(codice_comune, anno, fascia_anagrafica, pop)

# 2020-2025: POSAS
pop_2020_2025 <- bind_rows(
  read_posas_comuni("Input_Comuni/POSAS_2020_it_Comuni.csv", 2020),
  read_posas_comuni("Input_Comuni/POSAS_2021_it_Comuni.csv", 2021),
  read_posas_comuni("Input_Comuni/POSAS_2022_it_Comuni.csv", 2022),
  read_posas_comuni("Input_Comuni/POSAS_2023_it_Comuni.csv", 2023),
  read_posas_comuni("Input_Comuni/POSAS_2024_it_Comuni.csv", 2024),
  read_posas_comuni("Input_Comuni/POSAS_2025_it_Comuni.csv", 2025)
)

pop_comuni <- bind_rows(pop_2015_2019, pop_2020_2025)

# ── Mortalità ──────────────────────────────────────────────────────────────────

mortalita <- as.data.frame(fread("Input/comuni_giornaliero_31dicembre25.csv", encoding = "Latin-1")) %>%
  group_by(REG, PROV, NOME_REGIONE, NOME_PROVINCIA, NOME_COMUNE, COD_PROVCOM, CL_ETA) %>%
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  gather(anno_raw, decessi, T_11:T_25) %>%
  mutate(
    anno = as.numeric(paste0("20", substr(anno_raw, 3, 4))),
    fascia_anagrafica = case_when(
      CL_ETA == 0  ~ "0",
      CL_ETA == 1  ~ "1-4",
      CL_ETA == 2  ~ "5-9",
      CL_ETA == 3  ~ "10-14",
      CL_ETA == 4  ~ "15-19",
      CL_ETA == 5  ~ "20-24",
      CL_ETA == 6  ~ "25-29",
      CL_ETA == 7  ~ "30-34",
      CL_ETA == 8  ~ "35-39",
      CL_ETA == 9  ~ "40-44",
      CL_ETA == 10 ~ "45-49",
      CL_ETA == 11 ~ "50-54",
      CL_ETA == 12 ~ "55-59",
      CL_ETA == 13 ~ "60-64",
      CL_ETA == 14 ~ "65-69",
      CL_ETA == 15 ~ "70-74",
      CL_ETA == 16 ~ "75-79",
      CL_ETA == 17 ~ "80-84",
      CL_ETA == 18 ~ "85-89",
      CL_ETA == 19 ~ "90-94",
      CL_ETA == 20 ~ "95-99",
      CL_ETA == 21 ~ "100+"
    )
  ) %>%
  rename(
    codice_comune  = COD_PROVCOM,
    nome_comune    = NOME_COMUNE,
    nome_provincia = NOME_PROVINCIA,
    nome_regione   = NOME_REGIONE
  ) %>%
  select(codice_comune, nome_comune, nome_provincia, nome_regione,
         fascia_anagrafica, anno, decessi)

# ── Join mortalità + popolazione ───────────────────────────────────────────────

comuni <- inner_join(
  mortalita,
  pop_comuni,
  by = c("codice_comune", "fascia_anagrafica", "anno")
)

# ── Popolazione standard (Italia 2019) ────────────────────────────────────────
# Anno finale della baseline: stabile, pre-pandemia

std <- pop_comuni %>%
  filter(anno == 2019) %>%
  group_by(fascia_anagrafica) %>%
  summarise(pop_std = sum(pop), .groups = "drop") %>%
  mutate(peso = pop_std / sum(pop_std))

# ── Funzione: tasso standardizzato per periodo ────────────────────────────────

tasso_periodo <- function(df, anni, label) {
  df %>%
    filter(anno %in% anni) %>%
    group_by(codice_comune, nome_comune, nome_provincia, nome_regione, fascia_anagrafica) %>%
    summarise(
      decessi = sum(decessi),
      pop     = sum(pop),
      .groups = "drop"
    ) %>%
    filter(pop > 0) %>%
    left_join(std, by = "fascia_anagrafica") %>%
    group_by(codice_comune, nome_comune, nome_provincia, nome_regione) %>%
    summarise(
      !!paste0("tasso_std_", label)   := sum((decessi / pop) * peso) * 100000,
      !!paste0("decessi_", label)     := sum(decessi),
      !!paste0("pop_", label)         := sum(pop),
      .groups = "drop"
    )
}

t_baseline <- tasso_periodo(comuni, 2015:2019, "baseline")
t_covid    <- tasso_periodo(comuni, 2020:2022, "covid")
t_post     <- tasso_periodo(comuni, 2023:2025, "post")

# ── Output principale ──────────────────────────────────────────────────────────

out <- t_baseline %>%
  inner_join(t_covid, by = c("codice_comune", "nome_comune", "nome_provincia", "nome_regione")) %>%
  inner_join(t_post,  by = c("codice_comune", "nome_comune", "nome_provincia", "nome_regione")) %>%
  mutate(
    # Eccesso covid rispetto alla baseline
    eccesso_covid      = tasso_std_covid    - tasso_std_baseline,
    eccesso_covid_perc = eccesso_covid / tasso_std_baseline * 100,
    # Variazione post-covid rispetto alla baseline
    variazione_post      = tasso_std_post   - tasso_std_baseline,
    variazione_post_perc = variazione_post  / tasso_std_baseline * 100,
    # Differenza diretta tra i due periodi di confronto
    diff_tasso      = tasso_std_post - tasso_std_covid,
    diff_tasso_perc = diff_tasso / tasso_std_covid * 100
  )

write_csv(out, file = "Output/decessi_comuni_2020_22_vs_2023_25.csv")

# ── Analisi harvesting ─────────────────────────────────────────────────────────
# Ipotesi harvesting: i comuni con maggiore eccesso di mortalità durante il
# Covid (eccesso_covid alto) dovrebbero mostrare un calo maggiore nel post-Covid
# (variazione_post bassa / negativa).
# Test: cor(eccesso_covid, variazione_post) negativa = evidenza di harvesting.

# Filtro comuni con popolazione sufficiente (almeno 500 abitanti medi nel baseline)
analisi <- out %>%
  filter(
    is.finite(tasso_std_baseline),
    is.finite(tasso_std_covid),
    is.finite(tasso_std_post),
    pop_baseline / 5 >= 500   # ≥500 ab. medi annui nel periodo baseline
  )

message("Comuni nell'analisi: ", nrow(analisi))

# Correlazioni
cor_harvesting_pearson  <- cor(analisi$eccesso_covid, analisi$variazione_post,
                               use = "complete.obs")
cor_harvesting_spearman <- cor(analisi$eccesso_covid, analisi$variazione_post,
                               use = "complete.obs", method = "spearman")

# Regressione principale: variazione_post ~ eccesso_covid
# Beta < 0 = harvesting; beta = -1 = compensazione completa
mod_harv <- lm(variazione_post ~ eccesso_covid, data = analisi)
mod_harv_sum <- summary(mod_harv)

# Regressione di controllo: periodo post ~ periodo covid (confronto diretto)
mod_ctrl <- lm(tasso_std_post ~ tasso_std_covid, data = analisi)
mod_ctrl_sum <- summary(mod_ctrl)

analisi_out <- tibble(
  metrica = c(
    "n_comuni",
    # Test harvesting diretto
    "harvesting_cor_pearson",
    "harvesting_cor_spearman",
    "harvesting_beta",       # < 0 = harvesting; -1 = compensazione completa
    "harvesting_r2",
    "harvesting_p_value",
    # Confronto diretto covid vs post (per riferimento)
    "ctrl_beta_covid_vs_post",
    "ctrl_r2_covid_vs_post",
    "ctrl_p_covid_vs_post"
  ),
  valore = c(
    nrow(analisi),
    cor_harvesting_pearson,
    cor_harvesting_spearman,
    coef(mod_harv)[2],
    mod_harv_sum$r.squared,
    coef(mod_harv_sum)[2, 4],
    coef(mod_ctrl)[2],
    mod_ctrl_sum$r.squared,
    coef(mod_ctrl_sum)[2, 4]
  )
)

write_csv(analisi_out, file = "Output/analisi_orvesting_comuni_2020_22_vs_2023_25.csv")

message("harvesting_cor_pearson  = ", round(cor_harvesting_pearson,  3))
message("harvesting_cor_spearman = ", round(cor_harvesting_spearman, 3))
message("harvesting_beta         = ", round(coef(mod_harv)[2], 3),
        "  (p=", round(coef(mod_harv_sum)[2, 4], 4), ")")
