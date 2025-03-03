
# Exportpfade -------------------------------------------------------------

path_results_excel <- paste0("output/",basename(dirname(getwd())))

# Excel export Ergebnisse -------------------------------------------------
df_wb_individuals <- df_results_individuals_summary
dput(names(df_wb_means))


df_wb_means <- df_results_means_summary %>%
  select(
    c(
      "gaertest", "projekt", "analytik_nr", "probe", "variante", "beschreibung", "probenform", 
    "datum_probenname", "var2", "var3", "n", "einwaage_in_g_fm", 
    "otm_k_fm", "p_i", "biogas_yield_odm_mean", 
    "biogas_yield_odm_min", "biogas_yield_odm_max", "methane_yield_odm_mean", 
    "methane_yield_odm_min", "methane_yield_odm_max", "methane_perc_mean", 
    "methane_perc_min", "methane_perc_max", "versuchsdauer"
    )
    ) %>%
  arrange(projekt, probe) %>%
  rename( 
    'Einwaage [gFM]'       = einwaage_in_g_fm,
    'oTM [%FM]'            = otm_k_fm,
    'Biogasausbeute oTM mean' = biogas_yield_odm_mean,
    'Biogasausbeute oTM min' = biogas_yield_odm_min,
    'Biogasausbeute oTM max' = biogas_yield_odm_max,
    'Methanausbeute oTM mean' = methane_yield_odm_mean,
    'Methanausbeute oTM min' = methane_yield_odm_min,
    'Methanausbeute oTM max' = methane_yield_odm_max,
    'CH4 mean [Vol%] '         = methane_perc_mean,
    'CH4 min [Vol%]'         = methane_perc_min,
    'CH4 max [Vol%]'         = methane_perc_max,
  ) %>%
  view()


df_wb_individuals <- df_results_individuals_summary %>%
  select(
    c("gaertest", "projekt", "analytik_nr", "probe", "variante", "beschreibung", "probenform", 
      "datum_probenname", "var2", "var3", "messplatz", "einwaage_in_g_fm", 
      "otm_k_fm", "p_i",  
      "biogas_yield_odm", "methane_yield_odm", "methane_perc", 
      "versuchsdauer")
  ) %>%

  arrange(projekt, probe)%>%
  rename( 
    'Einwaage [gFM]'       = einwaage_in_g_fm,
    'oTM [%FM]'            = otm_k_fm,
    'Biogasaus\nbeute oTM' = biogas_yield_odm,
    'Methan\nausbeute oTM' = methane_yield_odm,
    'CH4\n [Vol%]'         = methane_perc,
         ) %>%
  view()

wb <- wb_workbook(title = "results") %>%
  wb_add_worksheet(sheet = "individuals") %>%
  wb_add_data(
    sheet = "individuals",
    x = df_wb_individuals
  ) %>%
  wb_add_worksheet(sheet = "means") %>%
  wb_add_data(
    sheet = "means",
    x = df_wb_means
  ) %>%
  wb_add_numfmt("individuals", "L2:R60", numfmt = "0.0") %>%
  wb_add_numfmt("means", "L2:X60", numfmt = "0.0") %>%
  wb_open()




basename(dirname(getwd()))


 


