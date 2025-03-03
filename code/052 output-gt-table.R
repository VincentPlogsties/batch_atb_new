

# view(df_results_samples)
# view(df_results_variants)

path_results <- paste0("output/", basename(getwd()))

# Results Proben ----------------------------------------------------------

gt_samples <- df_results_samples %>%
  mutate(projekt = as.factor(projekt))
for (i in levels(gt_samples$projekt))
{
  current_data <- gt_samples %>%
    filter(projekt %in% i) %>%
    select(
      probe,
      variante,
      einwaage_in_g_fm,
      otm_k_fm,
      biogas_yield_odm,
      methane_yield_odm,
      methane_perc
    ) %>%
    mutate(across(
      where(is.character),
      ~ str_to_title(.) %>%
        str_replace_all(c(
          "oe" = "ö",
          "ue" = "ü",
          "ae" = "ä"
        ))
    )) %>%
    arrange(variante, probe)
  
  na_columns <- names(current_data)[sapply(current_data, function(col)
    all(is.na(col)))]
  
  table_samples <- gt(current_data, locale = "de") %>%
    
    cols_label(
      variante          = md("**Variante**"),# hier passende Bezeichnung einsetzen
      probe             = md("**Probe**"),
      einwaage_in_g_fm  = md("**Einwaage<br>[gFM]**"),
      otm_k_fm          = md("**oTM<br>[%FM]**") ,
      biogas_yield_odm  = md("**Biogas-<br>ausbeute<br>[l<sub>N</sub><sup>.</sup>kgoTM<sup>-1</sup>]**" ),
      methane_yield_odm = md( "**Methan-<br>ausbeute<br>[l<sub>N</sub><sup>.</sup>kgoTM<sup>-1</sup>]**"),
      methane_perc      = md("**Methan<br>[Vol%]**"),
      fn = md
    ) %>% 
    fmt_markdown(columns = everything()) %>%
    
    fmt_number(columns = 3:7, decimals = 1) %>%
   
    cols_align(align = "center", columns = everything()) %>%
    opt_table_lines("none") %>%
      
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"), 
        color = "black", weight = px(2)
        ),
      locations = cells_column_labels(everything())
    ) %>% 

     tab_style(
       style = cell_borders(
         sides = "bottom", 
         color = "black", 
         weight = px(1)),
       locations = cells_body(
         rows = probe != dplyr::lead(probe) | is.na(dplyr::lead(probe))
       )
     ) %>%
    
    tab_style(
      style = cell_borders(
        sides = "bottom", 
        color = "black", 
        weight = px(2)
      ),
      locations = cells_body(
        rows = nrow(current_data))
    ) %>%
    
    tab_style(
      style = cell_text(
        align = "center", 
        v_align = "middle"),
      locations = cells_body(everything())
    ) %>%
    tab_style(
      style = cell_text(
        align = "center", 
        v_align = "middle"),
      locations = cells_column_labels()
    ) %>%
    cols_hide(columns = na_columns)
  
  gtsave(data = table_samples,filename = paste0(path_results,"_samples_",i,".html"))
  
  table_samples
}

# Results Varianten -------------------------------------------------------

gt_variants <- df_results_variants %>%
  mutate(
    biogas_yield = glue(
      "{gsub('\\\\.', ',', sprintf('%.1f',biogas_yield_odm_mean))}<br>
     ({gsub('\\\\.', ',', sprintf('%.1f', biogas_yield_odm_min))}-{gsub('\\\\.', ',', sprintf('%.1f', biogas_yield_odm_max))})"
    )
  ) %>%
  mutate(
    methane_yield = glue(
      "{gsub('\\\\.', ',', sprintf('%.1f',methane_yield_odm_mean))}<br>
     ({gsub('\\\\.', ',', sprintf('%.1f', methane_yield_odm_min))}-{gsub('\\\\.', ',', sprintf('%.1f', methane_yield_odm_max))})"
    )
  ) %>%
  
  mutate(
    methane_perc = glue(
      "{gsub('\\\\.', ',', sprintf('%.1f',methane_perc_mean))}<br>
     ({gsub('\\\\.', ',', sprintf('%.1f', methane_perc_min))}-{gsub('\\\\.', ',', sprintf('%.1f', methane_perc_max))})"
    )
  ) %>%
  select(
    projekt,
    variante,
    probe,
    n,
    einwaage_in_g_fm,
    otm_k_fm,
    versuchsdauer,
    biogas_yield,
    methane_yield,
    methane_perc
  ) %>%
  mutate(projekt = as.factor(projekt))


for(i in levels(gt_variants$projekt))
{
  current_data <- gt_variants %>%
    filter(projekt %in% i) %>%
    select(
      probe,
      variante,
      n,
      einwaage_in_g_fm,
      otm_k_fm,
      versuchsdauer,
      biogas_yield,
      methane_yield,
      methane_perc
    ) %>%
    mutate(
      across(
        where(is.character),
        ~ str_to_title(.) %>%
          str_replace_all(
            c("oe" = "ö", "ue" = "ü", "ae" = "ä")
            )
        )
      )
                      
  #num_rows <- nrow(current_data) 
  na_columns <- names(current_data)[sapply(current_data, function(col)
    all(is.na(col)))]
  
  table_variants <- gt(current_data, locale = "de") %>% 
  
  cols_label(
    variante         = md("**Ernte-<br>jahr**"), # hier passende Bezeichnung einsetzen
    probe            = md("**Probe**"),
    n                = md("**n**"),
    versuchsdauer    = md("**Versuchs-<br>tage<br>[d]**"),
    einwaage_in_g_fm = md("**Einwaage<br>[gFM]**"),
    otm_k_fm         = md("**oTM<br>[%FM]**") ,
    biogas_yield     = md("**Biogas-<br>ausbeute<br>[l<sub>N</sub><sup>.</sup>kgoTM<sup>-1</sup>]**"),
    methane_yield    = md("**Methan-<br>ausbeute<br>[l<sub>N</sub><sup>.</sup>kgoTM<sup>-1</sup>]**"),
    methane_perc     = md("**Methan<br>[Vol%]**"),
    fn = md
    ) %>%
 
  fmt_markdown(
    columns = everything()
    ) %>%
  
  fmt_number(
    columns = 4:8, 
    decimals = 1
    ) %>%
  
  cols_align(
    align = "center", 
    columns = everything()
    ) %>%

  opt_table_lines("none") %>%
  
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"), 
      color = "black", weight = px(2)
      ),
    locations = cells_column_labels(everything())
  ) %>% 
    tab_style(
      style = cell_borders(
        sides = "bottom", 
        color = "black", 
        weight = px(1)
        ),
      locations = cells_body(
          rows = probe != dplyr::lead(probe) | is.na(dplyr::lead(probe))
      )
    )%>%
  tab_style(
    style = cell_borders(
      sides = "bottom", 
      color = "black", 
      weight = px(2)
      ),
    locations = cells_body(
        rows = nrow(current_data))
  ) %>%
    tab_style(
      style = cell_text(
        align = "center", 
        v_align = "middle"
        ),  
      locations = cells_body(everything()) 
    ) %>%
    tab_style(
      style = cell_text(
        align = "center", 
        v_align = "middle"),  
      locations = cells_column_labels()
    ) %>%
    
    cols_hide(columns = na_columns)
  
 gtsave(data = table_variants,filename = paste0(path_results,"_variants_",i,".html"))
 
 table_variants
}

# Analysen ----------------------------------------------------------------

# colnames(df_analytik)

gt_analytik <- df_proben %>%
  mutate(
    projekt = as.factor(projekt)
    )

for(
  i in levels(gt_analytik$projekt)
  )
{
  current_data <- gt_analytik %>%
    filter(projekt %in% i) %>%
    select(
      probe,
      variante,
      ts_perc_fm,
      ots_perc_ts,
      ph,
      nh4_n_g_l,
      nitrogen_g_l,
      alc_g_l,
      gc_ges_g_l
    ) %>%
    mutate(
      across(
        where(is.character),
        ~ str_to_title(.) %>%
        str_replace_all(
          c("oe" = "ö", "ue" = "ü", "ae" = "ä")
          )
        )
      )

  #num_rows <- nrow(current_data)
  na_columns <- names(current_data)[sapply(current_data, function(col) all(is.na(col)))]

table_analytik <- gt(current_data, locale = "de")%>%

  cols_label(
    probe        = md("**Probe**"),
    variante     = md("**Variante**"), # hier passende Bezeichnung eintragen
    ts_perc_fm   = md("**TS<br>[%FM]**"),
    ots_perc_ts  = md("**oTS<br>[%TS]**"),
    ph           = md("**pH<br>[-]**") ,
    nh4_n_g_l    = md("**NH<sub>4</sub>-N<br>[g<sup>.</sup>l<sup>-1</sup>]**"),
    nitrogen_g_l = md("**N<br>[g<sup>.</sup>l<sup>-1</sup>]**"),
    alc_g_l      = md("**Alc<br>[g<sup>.</sup>l<sup>-1</sup>]**"),
    gc_ges_g_l   = md("**Ges-Sr.<br>[g<sup>.</sup>l<sup>-1</sup>]**"),
    fn = md
  ) %>%
  fmt_markdown(columns = everything()) %>%

  fmt_number(columns = 2:8, decimals = 1) %>%

  cols_align(
    align = "center",
    columns = everything()
    ) %>%
  opt_table_lines("none") %>%

  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black", weight = px(2)
      ),
    locations = cells_column_labels(everything())
  ) %>%
   tab_style(
     style = cell_borders(
       sides = "bottom",
       color = "black",
       weight = px(2)
       ),
     locations = cells_body(rows = nrow(current_data))
   ) %>%
  tab_style(
    style = cell_text(
      align = "center",
      v_align = "middle"
      ),
    locations = cells_body(everything())
  ) %>%
  tab_style(
    style = cell_text(
      align = "center",
      v_align = "middle"
      ),
    locations = cells_column_labels()
    ) %>%
  cols_hide(columns = na_columns)

  gtsave(data = table_analytik,filename = paste0(path_results,"_results_analytik_",i,".html"))

}
