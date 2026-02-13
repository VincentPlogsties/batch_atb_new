
# pfade -------------------------------------------------------------

analytik_files <- list.files(
  "data", 
  pattern = "Analysen.*\\.xlsx$",
  ignore.case = TRUE,
  full.names = TRUE
  )
analytik_files <- 
  analytik_files[!str_detect(basename(analytik_files), "^~\\$")]

protokoll_files <- list.files(
  "data", 
  pattern = "Protokoll.*\\.xlsx$", 
  full.names = TRUE,
  ignore.case = TRUE
  )
protokoll_files <- 
  protokoll_files[!str_detect(basename(protokoll_files), "^~\\$")]

# Analytik ----------------------------------------------------------------

lookup <- c(
  analytik_nr            = "probennummer"                         ,
  probe                  = "name"                                 ,
  pk                     = "pufferkapazitat_in_g_ms_100g_ts"      ,
  ts_perc_fm             = "ts105_in_percent_fm"                  ,
  ots_perc_ts            = "o_ts_in_percent_ts"                   ,
  ots_perc_fm            = "o_ts_in_percent_fm"                   ,
  ts60_perc_fm           = "ts60_in_percent"                      ,
  ts105_ts60_perc        = "ts_60_105_c"                          ,
  ph                     = "p_h_wert"                             ,
  lf_ms_cm               = "leitfahigkeit_in_m_s_cm"              ,
  nh4_n_mg_kg_fm         = "nh4_n_in_mg_kg_fm"                    ,
  nitrogen_mg_kg_fm      = "n_kjeld_in_mg_kg_fm"                  ,
  rprot_perc_ts          = "rohprotein_in_percent_ts"             ,
  methanol_g_l           = "gc_methanol_in_g_l"                   ,
  ethanol_g_l            = "gc_ethanol_in_g_l"                    ,
  propanol_g_l           = "gc_propanol_in_g_l"                   ,
  butanol_g_l            = "gc_butanol_in_g_l"                    ,
  es_g_l                 = "gc_essigsaure_in_g_l"                 ,
  ps_g_l                 = "gc_propionsaure_in_g_l"               ,
  bs_g_l                 = "gc_buttersaure_in_g_l"                ,
  iso_bs_g_l             = "gc_i_buttersaure_in_g_l"              ,
  iso_vs_g_l             = "gc_i_valeriansaure_in_g_l"            ,
  vs_g_l                 = "gc_valeriansaure_in_g_l"              ,
  iso_cs_g_l             = "gc_i_capronsaure_in_g_l"              ,
  cs_g_l                 = "gc_capronsaure_in_g_l"                ,
  gc_ges_g_l             = "gc_gesamt_als_essigsaure_in_g_l"      ,
  ms_g_l                 = "hplc_milchsaure_in_g_l"               ,
  gluc_g_l               = "hplc_glucose_in_g_l"                  ,
  sacc_g_l               = "hplc_saccharose_in_g_l"               ,
  fruc_g_l               = "hplc_fructose_in_g_l"                 ,
  rfett_perc_ts          = "rohfett_in_percent_ts"                ,
  rfaser_perc_ts         = "rohfaser_in_percent_ts"               ,
  ndf_perc_ts            = "ndf_in_percent_ts"                    ,
  adf_perc_ts            = "adf_in_percent_ts"                    ,
  adl_perc_ts            = "adl_in_percent_ts"                    ,
  n_perc_ts              = "n_in_percent_ts"                      ,
  c_perc_ts              = "c_in_percent_ts"                      ,
  s_perc_ts              = "s_in_percent_ts"                      ,
  h_perc_ts              = "h_in_percent_ts"                      ,
  gluc_wlk_g_l           = "wlk_glucose"                          ,
  fruc_wlk_g_l           = "wlk_fructose"                         ,
  sac_wlk_g_l            = "wlk_saccharose"                       ,
  inu_wlk_g_l            = "wlk_inulin"                           ,
  pges_mg_kg_fm          = "pges_in_mg_kg_fm"                     ,
  zuc_ges_wlk_g_l        = "wlk_gesamtzucker"                     ,
  zuc_ges_wlk_perc_ts    = "wlk_in_percent_ts"                    ,
  no3_mg_kg_ts60         = "ic_no3_in_mg_kg_ts60"                 ,
  staerke_perc_ts        = "staerke_nach_ewers_in_percent_ts"     ,
  zucker_perc_ts         = "zuckergehalt_in_percent_ts"
)


df_import_analytik <- analytik_files %>%
# alle dateien mit "_analysen.xlsx" importieren und Dateinamen als Spalten einfügen  
  map(
    ~ readxl::read_xlsx(
      .x,
      col_types = "text"
    ) %>% 
      mutate(
        filename = basename(.x),
        .before = 1
        ) %>% 
      janitor::clean_names(.)
  ) %>% 
  bind_rows() %>%
# Variablennamen vereinheitlichen
  rename(any_of(lookup)) %>%

# # Variablen ergänzen (-> Projekt notwendig?)
#   mutate(
#     filename = sub( "_Analysen.xlsx","", filename)
#   ) %>% 
#   extract(
#     filename,
#     into = c("gaertest", "projekt"),
#     regex = "(GT_\\d{4}_\\d{2})(?:_(.*))?"
#   ) %>%                              # mit tidyr::separate_wider_regex ersetzen?
#   mutate(
#     projekt = na_if(projekt, ""),
#     ) %>%
# Werte unter Nachweisgrenze "0" setzen --> Vorgehen noch abklären!
  select(-contains("percent")) %>%
  mutate(
    probenart = case_match(
      probenart,
      c("Feststoffprobe", "Frischmassen") ~ "FM",
      "Silage" ~ "S",
      .default = probenart
    )
  ) %>% 
  # Werte unter Nachweisgrenze auf 0 setzen -- Absprache nötig
  mutate(
    across(
      everything(), 
      ~ if_else(
        str_detect(., "<|n.a"),
        "0",
        .
      )
    )
  ) %>%
  # Formate festlegen
  mutate(
    across(
      -any_of(c("filename", "analytik_nr", "probe", "probenart")),
      as.numeric),
    datum = lubridate::as_date(datum, origin = "1899-12-30") # startzeit hängt bei excel von der Version ab - anpassen!
  ) %>% 
# Zeichenketten säubern
  mutate(
    across(
      where(is.character),
      ~ .x %>%
        # str_to_lower() %>%
        # str_replace_all(c("ö" = "oe","ä" = "ae","ü" = "ue")) %>% 
        str_squish() %>% 
        str_replace_all(pattern = "\\s+-\\s+", replacement = "-") # whitespace around hyphens
    )
  ) 



  
# janitor::remove_empty(
#   .,
#   which = "cols"
# ) %>% view()

# Proben ------------------------------------------------------------------

df_import_samples <-
  openxlsx::read.xlsx(
    protokoll_files[1], 
    sheet = "Proben", 
    rows = 4:121, 
    cols = 1:20) %>%
  rename_with(
    ~ str_replace_all(
      .x, 
      c("ö" = "oe","ä" = "ae","ü" = "ue","oTM" = "otm", "%" = "")
      )
    ) %>% 
  janitor::clean_names() %>%
  as_tibble() %>%
  drop_na(einwaage_in_g_fm) %>%
  mutate(
    across(
        -c(platz, gasmaus, d_gasmaus_cm, einwaage_in_g_fm, otm_k_fm),
      as.character
      ),
    across(
      c(platz, gasmaus, d_gasmaus_cm, nullpunkt_cm, einwaage_in_g_fm, otm_k_fm),
      as.numeric
      )
    ) %>%
  mutate(
    projekt = str_to_lower(projekt),
    projekt = str_squish(projekt)
  ) %>%
    janitor::remove_empty(
      .,
      which = "cols"
    )

# Gaswerte --------------------------------------------------------------------

sheets_daten <- c("w1", "w2", "w3", "w4", "w5", "w6")

df_import_gaswerte <- sheets_daten %>%
  map(
    ~ readxl::read_xlsx(
      path = protokoll_files[1],
      sheet = .x,
      range = "A6:BQ71",
      col_names = FALSE,
      col_types = "numeric"
    ) %>% 
      mutate(
        sheet_name = .x
      )
  ) %>% 
    # ~ openxlsx::read.xlsx(
    #   protokoll_files[1],
    #   colNames = FALSE, # Ohne Spaltenname damit rowbind die unterschiedlichen Wannen den gleichen Spalten zuordnet (A,B...)
    #   sheet = .x,
    #   rows = c(6:71),
    #   cols = c(1:69),
    #   skipEmptyCols = FALSE
    #   #types = c(A = 1)
    #   ) %>%
    #     mutate(
    #       sheet_name = .x
    #       )
    # ) %>%
  bind_rows() %>%
  relocate(sheet_name,.after = 4) %>%
  rename_with(
    .,
    ~ c(
      "datum", "zeit", "druck", "temp", "wanne",
      str_c(
        rep(1:13, 
            each = 5),
        rep(
          c("hoehe","ch4","co2","o2","h2s"), 
          times = 13),
        sep = "_"
        )
      ), 
    all_of(colnames(.))
    ) %>% 
  pivot_longer(
    cols      = !(datum:wanne),
    names_to  = c("num", ".value"),  
    names_sep = "_"
    ) %>%
  mutate(
    platz = as.numeric(num),
    .before = hoehe,
    .keep = "unused"
    ) %>%
  drop_na(zeit, hoehe) %>%
  rename(
    "hoehe_mm" = "hoehe",
    "ch4_perc" = "ch4",
    "co2_perc" = "co2",
    "o2_perc"  = "o2",
    "h2s_ppm"  = "h2s"
    ) %>%
  mutate(
    datum = as_date(datum, origin = "1899-12-30"),
    zeit = as_datetime(datum + seconds_to_period(zeit * 86400))
  ) 
  




