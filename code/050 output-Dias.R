
# Diagramme ---------------------------------------------------------------

# setup -------------------------------------------------------------------

## theme -------------------------------------------------------------------
my_theme<-theme(panel.grid = element_blank(),
                panel.grid.major.y =  element_line(linewidth = 0.15, color = "grey"),
                panel.grid.major.x = element_line(linewidth = 0.15, color="grey"),
                # panel.background = element_rect(colour = "grey", fill=NA, size=0.15),
                # panel.border = element_line(colour = "grey", size=0.15),
                
                axis.ticks.length = unit(1, "mm"),
                axis.minor.ticks.length = unit(0.5, "mm"),
                text = element_text(size = 11),
                axis.text.x = element_text(size = 10, color = "black", angle = 0, vjust = 0.5, hjust = 0.5),
                axis.text.y = element_text(size = 10, color = "black"),
                axis.title.y = element_textbox_simple(orientation = "left-rotated", halign = 0.5),
                
                legend.position = "inside",
                legend.position.inside = c(0.5,0.25),
                legend.background = element_blank(),
                legend.box.background = element_rect(colour = "grey", size = 0.15),
                # legend.position = "top",
                # legend.justification = "left",
                legend.box.spacing = unit(0, "pt"),
                legend.title = element_blank(),
                legend.margin=margin(0,0,0,0),
                legend.key.spacing.y = unit(0, "pt"),
                legend.key.spacing.x = unit(0, "pt"),
                legend.key = element_blank(),
                legend.text = element_text(margin = margin(l = 1, r = 4,unit = "pt")),
                  
                #legend.background = element_rect(fill = "white", colour= "black")
                #legend.byrow = 
                #legend.direction = "horizontal",
                plot.caption = element_textbox_simple(margin = margin(t = 5)),# Abstand t(op) von oben
                plot.caption.position = "plot",
                
                plot.title.position = "panel"
                )

## farbpalette -------------------------------------------------------------


### ATB ---------------------------------------------------------------------
#farben <- c("#005ca9", "#94b53d","#FF8976", "#f39200", "#00843d", "#00a5e2", "#666699")


### Individuell -------------------------------------------------------------

farben <- c("#1B9E77","#EFC000FF" ,"#D95F02","#7570B3", "#E7298A","#66A61E" ,
             "#A6761D","#666666","#005ca9","#E6AB02")

### JCO --------------------------------------------------------------
# install.packages("ggsci")
# library("ggsci")
# library("scales")
# show_col(pal_jco("default")(10))


# Verlauf - Varianten --------------------------------------------------------

## label -------------------------------------------------------------------

my_caption <- paste(basename(getwd()),versuch_caption)
                    
x_lab<- "Versuchstage"
y_lab<- "Methanausbeute<br>[l<sub>N</sub><sup>.</sup>kgoTM<sup>-1</sup>]"
y_lab_BG <-"Biogasausbeute<br>[l<sub>N</sub><sup>.</sup>kgoTM<sup>-1</sup>]"

## daten aufbereitung ------------------------------------------------------

df_plots_variants <- df_results_variants_daily %>%
  drop_na(methane_yield_fm_mean) %>%
  filter(
    projekt == param_projekt & !str_detect(probe, "blind_w")
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
  

## plot --------------------------------------------------------------------

    plot_variants <- df_plots_variants %>%
      ggplot() +
      aes(y = methane_yield_odm_mean, 
          x = day, 
          color = probe
          ) +
      geom_line(
        linewidth = 1
        ) +            
      scale_color_manual(
        values = farben
        )+
      #scale_color_colorblind()+  
      geom_errorbar(
        aes(
          x = day, 
          ymin = methane_yield_odm_min, 
          ymax = methane_yield_odm_max),
          color = "black",
          linewidth = 0.2,
          width = 0.5
      ) +
      scale_x_continuous(
        name = x_lab,
        limits = c(0, 60),
        breaks = seq(0, 60, by=5),
        #minor_breaks = seq(0, 30, by=1),
        # guide = guide_axis(
        #   minor.ticks = TRUE
        #   ),
         expand = expansion(mult = c(0, 0.01))
        ) +
      scale_y_continuous(
        name = y_lab,
        limits = c(0, 300),
        breaks = seq(0, 300, by=50),
        #minor_breaks = seq(0, 400, by=50),
        #guide = guide_axis(minor.ticks = TRUE),
        expand = expansion(mult = c(0, 0.01))
        ) +
      #labs(caption = param_projekt) +
      theme_bw() + 
      my_theme +
      guides(
        color = guide_legend(
          nrow = 1 
          )
        ) +
  ggtitle("Erntejahr 2024")

    
    plot_filename_variants <- paste0(basename(getwd()),"_",param_projekt,"_yield_variants.png")
    
    ggsave(filename = plot_filename_variants,
           width = 15,
           height = 10,
           units = "cm",
           path = "output",
           plot = plot_variants)

#einzelne Diagramme, händisch filtern
# 
# plot_variants<-df_plots_variants%>%
#filter(projekt = "xxx")%>%
# ggplot()+
#   aes(y = methane_yield_mean, x = days, color = variant)+
#   geom_line(linewidth = 1)+            
#   scale_color_manual(values = farben)+
#   #scale_color_colorblind()+  
#   
#   geom_errorbar(
#     aes(x = days, ymin = methane_yield_min, ymax = methane_yield_max),
#     color = "black",
#     size = 0.2,
#     width = 0.5
#   )+
#   
#    scale_x_continuous(
#      name = x_lab,
#      limits = c(0, NA),
#      #breaks = seq(0, 30, by=5),
#      #minor_breaks = seq(0, 30, by=1),
#      guide=guide_axis(minor.ticks = TRUE),
#      expand = expansion(mult = c(0, 0.1)))+
#    
# 
#      scale_y_continuous(
#      name = y_lab,
#      limits = c(0, NA),
#      breaks = seq(0, 400, by=100),
#      minor_breaks = seq(0, 400, by=50),
#      guide=guide_axis(minor.ticks = TRUE),
#      expand = expansion(mult = c(0, 0.1)))+
#   
#   labs(caption = my_caption)+
# 
#    theme_bw() + 
#    my_theme+
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE))
#   
# 
# 
# 
# ggsave(filename = plot_filename_samples,
#        width = 15,
#        height = 10,
#        units = "cm",
#        path = "output",
#        plot = plot_samples)
# 
# 
#       
# # Verlauf-Blind------------------------------------------------------
# 
# df_plot_blinds<- df_results_proben_w123_blind_daily#%>%
#   #filter(probe == "Blind")
# 
# plot_blinds<-df_plot_blinds%>%
#   ggplot()+
#   aes(y = biogas_yield_mean, x = days, color = variant)+
#   
#   geom_line(linewidth = 0.5)+
#   scale_color_manual(values = farben)+
#   #scale_color_colorblind()+  
#   
#   geom_errorbar(
#     aes(x = days, ymin = biogas_yield_min, ymax = biogas_yield_max),
#     color = "black",
#     size = 0.2,
#     width = 0.5
#   )+
#   scale_x_continuous(
#     name = x_lab,
#     limits = c(0, NA),
#     #breaks = seq(0, 30, by=5),
#     #minor_breaks = seq(0, 30, by=1),
#     guide=guide_axis(minor.ticks = TRUE),
#     expand = expansion(mult = c(0, 0.1)))+
#   
#   scale_y_continuous(
#     name = y_lab_BG,
#     limits = c(0, NA),
#     #breaks = seq(0, 600, by=200),
#     #minor_breaks = seq(0, 600, by=100),
#     guide=guide_axis(minor.ticks = TRUE),
#     expand = expansion(mult = c(0, 0.1)))+
#   
#   labs(caption = my_caption)+
#   theme_bw()+
#   my_theme
# 
# plot_filename_blinds <- paste0(basename(dirname(getwd())),"_",basename(getwd()),"_yield_blinds.png")
# 
# ggsave(filename = plot_filename_blinds,
#        width = 15,
#        units = "cm",
#        path = "output",
#        plot = plot_blinds)
# 
# # Barchart-Proben ---------------------------------------------------------
# 
# df_plot_bar<- df_results_proben_w123_variant#%>%
#  # filter(probe != "Blind")
# 
# x_lab_bar<- "Variante"
# y_lab_bar<- "Methanausbeute<br>[l<sub>N</sub><sup>.</sup>kgoTM<sup>-1</sup>]"
# 
# 
# plot_bar_samples<-df_plot_bar%>%
#   ggplot()+
#   aes(x = variant, y=methane_yield_mean,)+
#   geom_col(fill = "#005ca9")+
#   geom_errorbar(
#     aes(ymin = methane_yield_min, ymax = methane_yield_max ),
#     color = "black",
#     width = 0.25,
#     size = 0.2
#     )+
#   scale_y_continuous(
#     name = y_lab_bar,
#     limits = c(0, NA),
#     #breaks = seq(0, 600, by=200),
#     #minor_breaks = seq(0, 600, by=100),
#     guide=guide_axis(minor.ticks = TRUE),
#     expand = expansion(mult = c(0, 0.1)))+
#   scale_x_discrete(
#     name = x_lab_bar
#   )+
#   
#   labs(caption = my_caption)+
#   theme_bw()+
#   my_theme+
#   theme(axis.text.x = element_text(angle = 30, 
#                                    hjust = 0.5, 
#                                    vjust = 0.5, 
#                                    size = 11, 
#                                    color = "black"))
# 
# plot_filename_samples_bar <- paste0(basename(dirname(getwd())),"_",basename(getwd()),"_methane_yield.png")
# plot_filename_samples
# 
# ggsave(filename = plot_filename_samples_bar,
#        width = 15,
#        units = "cm",
#        path = "output",
#        plot = plot_bar_samples)
# 
# # Barchart-Blind ----------------------------------------------------------




