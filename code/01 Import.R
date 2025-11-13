
# pfade -------------------------------------------------------------

analytik_files <- list.files("data", pattern = "Analysen\\.xlsx$", full.names = TRUE)
protokoll_files <- list.files("data", pattern = "Protokoll\\.xlsx$", full.names = TRUE)

# Analytik ----------------------------------------------------------------

lookup <- c(
  analytik_nr            = "probennummer",
  probe                  = "name",
  ts_perc_fm             = "ts105_in_percent_fm",
  ots_perc_ts            = "o_ts_in_percent_ts" ,
  ots_perc_fm            = "o_ts_in_percent_fm" ,
  ph                     = "p_h_wert" ,
  nh4_n_mg_per_kgfm      = "nh4_n_in_mg_kg_fm",
  nitrogen_mg_per_kgfm   = "n_kjeld_in_mg_kg_fm",
  methanol_g_l           = "gc_methanol_in_g_l" ,
  ethanol_g_l            = "gc_ethanol_in_g_l" ,
  propanol_g_l           = "gc_propanol_in_g_l" ,
  butanol_g_l            = "gc_butanol_in_g_l" ,
  es_g_l                 = "gc_essigsaure_in_g_l",
  ps_g_l                 = "gc_propionsaure_in_g_l",
  bs_g_l                 = "gc_buttersaure_in_g_l",
  ibs_g_l                = "gc_i_buttersaure_in_g_l",
  ivs_g_l                = "gc_i_valeriansaure_in_g_l",
  vs_g_l                 = "gc_valeriansaure_in_g_l",
  ics_g_l                = "gc_i_capronsaure_in_g_l" ,
  cs_g_l                 = "gc_capronsaure_in_g_l",
  gc_ges_g_l             = "gc_gesamt_als_essigsaure_in_g_l"
)
# alle dateien mit "_analysen.xlsx" importieren und Dateinamen als Spalten einfügen
df_import_analytik <- analytik_files %>%
  map(
    ~read_xlsx(
      .x,
      skip_empty_rows = TRUE,
      skip_empty_cols = TRUE) %>% 
      mutate(
        filename = basename(.x),
        .before = 1) )%>% 
  bind_rows() %>% 
  mutate(
    across(everything(), ~ ifelse(grepl("<", .), "0", .)),
    filename = sub( "_Analysen.xlsx","", filename)
  ) %>%
  extract(
    filename,
    into = c("gaertest", "projekt"),
    regex = "(GT_\\d{4}_\\d{2})(?:_(.*))?"
  ) %>%
  janitor::clean_names(.) %>%
  rename(any_of(lookup)) %>%
  mutate(
    across(
      -any_of(c("gaertest","projekt", "analytik_nr", "datum", "probe", "probenart")),
      as.numeric),
    across(
      where(is.character), 
      ~ make_clean_names(
        .,
        allow_dupes = TRUE,
        replace = c("ö" = "oe","ä" = "ae","ü" = "ue")) %>%
        str_replace("^x","")  
    ),
    datum = as.Date(datum, origin = "1969-12-30") # startzeit hängt bei excel von der Version ab - anpassen!
  ) %>% 
  mutate(
    across(
      any_of(c("nh4_n_mg_per_kgfm", "nitrogen_mg_per_kgfm")),
      ~ .x/1000,
      .names = "{.col}_converted"
    )
  ) %>% 
  rename(
    nh4_n_g_l  = any_of("nh4_n_mg_per_kgfm_converted"),
    nitrogen_g_l = any_of("nitrogen_mg_per_kgfm_converted")
  ) %>% 
  select(any_of(c("nh4_n_g_l", "nitrogen_g_l")), everything()) %>% 
  rowwise() %>%
  mutate(
    alc_g_l = case_when(
      # If the number of elements selected by c_across is 0 (i.e., no columns exist)
      length(c_across(any_of(c("methanol_g_l", "ethanol_g_l", "propanol_g_l", "butanol_g_l")))) == 0 ~ NA_real_, 
      
      # Otherwise, safely calculate the sum
      TRUE ~ sum(c_across(any_of(c("methanol_g_l", "ethanol_g_l", "propanol_g_l", "butanol_g_l"))), na.rm = TRUE)
    )
  ) %>%
  ungroup()   # Always un-group after rowwise()
  
# janitor::remove_empty(
#   .,
#   which = "cols"
# ) %>% view()

# Proben ------------------------------------------------------------------

df_import_samples <-
  read_xlsx(
    protokoll_files[1], 
    sheet = "Proben", 
    rows = 4:121, 
    cols = 1:20) %>%
  janitor::clean_names(
    .,
    replace = c(
      "ö" = "oe",
      "ä" = "ae",
      "ü" = "ue",
      "oTM"="otm"
      )
    ) %>%
  mutate(
    across(
      where(is.character),
      ~ make_clean_names(
        .,
        allow_dupes = TRUE,
        replace = c(
          "ö" = "oe",
          "ä" = "ae",
          "ü" = "ue",
          "oTM"="otm"
          )
        ) %>%
      str_replace(
        "^x",
        ""
        )
      ),
    across(
      everything(),
      ~ ifelse(
        . 
        == "na",
        NA,
        .
        )
      )
    ) %>%
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
    janitor::remove_empty(
      .,
      which = "cols"
    ) 

# Gaswerte --------------------------------------------------------------------

sheets_daten <- c("w1", "w2", "w3", "w4", "w5", "w6")

df_import_gaswerte <- sheets_daten %>%
  map(
    ~ read_xlsx(
      protokoll_files[1], 
      colNames = FALSE, # Ohne Spaltenname damit rowbind die unterschiedlichen Wannen den gleichen Spalten zuordnet (A,B...)
      sheet = .x,
      rows = c(6:71), 
      cols = c(1:69),
      skip_empty_cols = FALSE,
      types = c(A = 1)
      ) %>%
        mutate(
          sheet_name = .x
          )
    ) %>%
  bind_rows() %>%
  relocate(sheet_name, 
           .after = D
           ) %>%

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
  

#tsr


