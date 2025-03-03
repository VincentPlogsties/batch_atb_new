
# Pfade -------------------------------------------------------------------

path_output <- str_c("output/", str_extract(basename(dirname((getwd()))), "^GT_\\d{4}_\\d{2}"))

# Analysen ----------------------------------------------------------------

write_rds(
  df_analytik,
  file = str_c(path_output, "_analytik.rds")
)

# Probenbeschreibung ------------------------------------------------------

write_rds(
  df_proben,
  file = str_c(path_output, "_proben.rds")
)
# vommfdkg

# Batchtest  ------------------------------------------------------------------

## Einzelwerte -------------------------------------------------------------

#### Zeitverlauf / daily------------------------------------------------------------------

write_rds(
  df_results_individuals_daily,
  file = str_c(path_output, "_results_individuals_daily.rds")
)

#### Ergebnisse / summary ------------------------------------------------------------------
# die letzte Gasanalyse wird als Endwert genommen!
df_results_individuals_summary <- df_results_individuals_daily %>%
  group_by(projekt, analytik_nr, probe, wanne, platz) %>%
  summarise(
    biogas_yield_odm  = last(biogas_yield_odm),
    methane_yield_odm = last(methane_yield_odm),
    versuchsdauer     = last(day)
  ) %>%
  mutate(
    methane_perc = methane_yield_odm / biogas_yield_odm * 100,
    .after = methane_yield_odm
  ) %>%
  ungroup() %>%
  view()

write_rds(
  df_results_individuals_summary,
  file = str_c(path_output, "_results_individuals_summary.rds")
)

## Mittelwerte -----------------------------------------------------------

#### Zeitverlauf / daily -------------------------------------------------------------

df_results_means_daily <- df_results_individuals_daily %>%
  group_by(projekt, analytik_nr, probe, day) %>%
  summarise(
    biogas_yield_odm_mean = mean(biogas_yield_odm),
    biogas_yield_odm_min  = min(biogas_yield_odm),
    biogas_yield_odm_max  = max(biogas_yield_odm),
    methane_yield_odm_mean= mean(methane_yield_odm),
    methane_yield_odm_min = min(methane_yield_odm),
    methane_yield_odm_max = max(methane_yield_odm),
    methane_perc_mean     = mean(methane_perc),
    methane_perc_min      = min(methane_perc),
    methane_perc_max      = max(methane_perc)
  ) %>%
  ungroup() %>%
  view()

write_rds(
  df_results_means_daily,
  file = str_c(path_output, "_results_means_daily.rds")
)

#### Ergebnisse / summary --------------------------------------------------------------

df_results_means_summary <- df_results_individuals_summary %>%
  group_by(projekt, analytik_nr, probe) %>%
  summarise(
    n = n(),
    biogas_yield_odm_mean  = mean(biogas_yield_odm),
    biogas_yield_odm_min   = min(biogas_yield_odm),
    biogas_yield_odm_max   = max(biogas_yield_odm),
    methane_yield_odm_mean = mean(methane_yield_odm),
    methane_yield_odm_min  = min(methane_yield_odm),
    methane_yield_odm_max  = max(methane_yield_odm),
    methane_perc_mean   = mean(methane_perc),
    methane_perc_min    = min(methane_perc),
    methane_perc_max    = max(methane_perc),
    versuchsdauer       = mean(versuchsdauer)   
  ) %>%
  ungroup() %>%
  view()

write_rds(
  df_results_means_summary,
  file = str_c(path_output, "_results_means_summary.rds")
)


# Validität ---------------------------------------------------------------

write_rds(
  test_validity,
  file = str_c(path_output, "_validity.rds")
)




