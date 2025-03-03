 #source("code/02 datenaufbereitung.R")
#df_w1_6_daily

# Bad data + outliers -----------------------------------------------------

#-----------------------------
df_test <- df_w1_6_daily %>%
  mutate(
    messplatz = factor(messplatz),
    probe = factor(probe))

for(i in levels(df_test$probe))
{
  plots<- ggplot(
    df_test %>%
      #mutate(variante = factor(probe))%>%
      group_by(messplatz,day) %>%
      filter(probe %in% i)
    ) +
    aes(
      y = vol_norm_net_cum, 
      x = day, 
      color = messplatz
      )+
    theme(
      plot.title = element_text(hjust = 0.5,vjust =0.5),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.8,0.2)
      ) +
    geom_line() +
    ggtitle(i)
  
  print(plots)
 
}
view(df_abbruch)
#------------------------------------------------------------------------------
df_test2 <- df_abbruch %>%
  mutate(
    messplatz = factor(messplatz),
    probe = factor(probe))

for(i in levels(df_test2$probe))
{
  plots2<- ggplot(
    df_test2 %>%
      #mutate(variante = factor(probe))%>%
      group_by(messplatz,day) %>%
      filter(probe %in% i)
  ) +
    aes(
      y = vol_perc, 
      x = day, 
      color = messplatz
    ) +
    scale_x_continuous(
      # limits = c(0, 60),
      # breaks = seq(0, 60, by=5),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(
      name = "tgl vol / cum vol [%]",
      # limits = c(0, 100),
      # breaks = seq(0, 100, by=25),

      expand = expansion(mult = c(0, 0.01))
    ) +
    theme(
      plot.title = element_text(hjust = 0.5,vjust =0.5),
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.8,0.2)
    ) +
    geom_line() +
    ggtitle(i)
  
  print(plots2)
  
}

#-------------------------------
# ggplot(
#   df_gas_auto%>%
#     mutate(tub = factor(tub),variante = factor(variante))%>%
#     
#     group_by(tub,channel_number,variante, days)%>%
#     summarise(biogas = max(total_vol_stp)),
#   aes(y = biogas, x = days, color = paste(channel_number,tub))
# )+
#   facet_wrap(vars(tub, variante))+
#   theme(plot.title = element_text(hjust = 0.5,vjust =0.5),
#         legend.title=element_blank())+
#   geom_line()

# Bad data + outliers -----------------------------------------------------

## Blindproben -------------------------------------------------------------

 # df_w1_6_blind%>%
 #   filter(channel_nr %in% c("1_K1", "1_K2", "1_K3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_ml_cum, color = channel_nr)+
 #   geom_line()
 # 
 # df_w1_6_blind%>%
 #   filter(channel_nr %in% c("2_K1", "2_K2", "2_K3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_ml_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6_blind%>%
 #   filter(channel_nr %in% c("3_K1", "3_K2", "3_K3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_ml_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6_blind%>%
 #   filter(channel_nr %in% c("4_K1", "4_K2", "4_K3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_ml_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6_blind%>%
 #   filter(channel_nr %in% c("5_K1", "5_K2", "5_K3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_ml_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6_blind%>%
 #   filter(channel_nr %in% c("6_K1", "6_K2", "6_K3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_ml_cum, color = channel_nr)+
 #   geom_line()
 # 
 # ## Proben -------------------------------------------------------------
 # 
 # df_w1_6%>%
 #   filter(channel_nr %in% c("1_1", "1_2", "1_3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("1_4", "1_5", "1_6"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("1_7", "1_8", "1_9"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # 
 # df_w1_6%>%
 #   filter(channel_nr %in% c("2_1", "2_2", "2_3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("2_4", "2_5", "2_6"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("2_7", "2_8", "2_9"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # 
 # df_w1_6%>%
 #   filter(channel_nr %in% c("3_1", "3_2", "3_3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("3_4", "3_5", "3_6"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("3_7", "3_8", "3_9"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # 
 # df_w1_6%>%
 #   filter(channel_nr %in% c("4_1", "4_2", "4_3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("4_4", "4_5", "4_6"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("4_7", "4_8", "4_9"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # 
 # df_w1_6%>%
 #   filter(channel_nr %in% c("5_1", "5_2", "5_3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("5_4", "5_5", "5_6"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("5_7", "5_8", "5_9"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # 
 # df_w1_6%>%
 #   filter(channel_nr %in% c("6_1", "6_2", "6_3"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("6_4", "6_5", "6_6"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()
 # df_w1_6%>%
 #   filter(channel_nr %in% c("6_7", "6_8", "6_9"))%>%
 #   ggplot()+
 #   aes(x = days, y = biogas_yield_cum, color = channel_nr)+
 #   geom_line()