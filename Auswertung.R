# skript zur Auswertung von Gärtestprotokollen
# Verzeichnisstruktur: GT_JJJJ_lfd-Nr/R/
 # Protokoll in Ordner "Data" ablegen, Namensschema: GT_JJJJ_lfd-Nr_protokoll.xlsx

# alles markieren und "run" ausführen
  # 1. Lauf: Ermittlung der ungültigen Messplätze 
      #-> optische Bewertung unter "Plots", numerisch unter "test_validity" 
          # ->in bad_channels eintragen
  # 2. Lauf: Auswertung; Ergebnisse sind in Ordner "output"

# Berechnungen-----------------------------------------------------

bad_channels = (c("wx_x", "wx_x", "komma"))

source("code/00 setup.R")
source("code/01 import.R", encoding = "UTF-8")
# view(df_import_gaswerte)
# view(df_import_analytik)
# view(df_import_samples)
# view(df_import_w1_6)
source("code/02 Berechnungen.R", encoding = "UTF-8")
# view(df_w1_6)
# view(df_w1_6_daily_interim)
# view(df_w1_6_daily)
# view(df_results)
# view(df_proben)
# view(df_analytik)
source("code/03 Ergebnisse.R", encoding = "UTF-8")
# view(df_results_individuals_daily)
# view(df_results_means_daily)
# view(df_results_individuals_summary)
# view(df_results_means_summary)
view(test_validity)
view(df_abbruch)
view(df_abbruch_wide)
# view(df_condition)
source("code/04 bad data_outliers.R")

#dput(df$variable)

# Ergebnisse--------------------------------------------------------------

param_projekt <- "hanfmix"
source("code/050 output-Dias.R")
source("code/052 output-gt-table.R")










