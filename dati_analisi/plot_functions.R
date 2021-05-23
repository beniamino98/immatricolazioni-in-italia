

# mappe 
immigration_map <- function(Regione = "Puglia", articolo = "in", df = df_fuorisede, mappa = mapIta){
  
  mytheme_solarized <- function (base_size = 15, base_family = "", light = TRUE) {
    rebase <- ggthemes:::solarized_rebase(light)
    ret <- (theme_bw(base_size = base_size, base_family = base_family) + 
              theme(text = element_text(colour = rebase["rebase01"]), 
                    title = element_text(color = rebase["rebase0"]), 
                    line = element_line(color = rebase["rebase01"]), 
                    rect = element_rect(fill = rebase["rebase03"], color = rebase["rebase01"]), 
                    
                    panel.grid = element_line(color = rebase["rebase02"]), 
                    panel.grid.major = element_line(color = rebase["rebase02"]), 
                    panel.grid.minor = element_line(color = rebase["rebase02"]), 
                    plot.background = element_rect(fill = NULL, colour = NA, 
                                                   linetype = 0)))
    ret
  }
  
  
  df2021 <- df %>% 
    filter(regione_dest == toupper(Regione) & regione_res != toupper(Regione) ) %>% 
    filter(anno_acc %in% c("2020/2021")) %>%
    group_by(anno_acc, regione_res) %>% 
    summarise(imm_fuorisede2021 = sum(imm_totali, na.rm = TRUE)) %>%
    mutate(imm_totali2021 = sum(imm_fuorisede2021))%>%
    mutate(perc2021 = imm_fuorisede2021/imm_totali2021*100)
  
  
  mappa_regioni <- tibble(NOME_REG = mappa$NOME_REG) %>%
    mutate(NOME_REG = toupper(NOME_REG), 
           NOME_REG = str_replace_all(NOME_REG, "-", " "), 
           regione_res = NOME_REG) %>%
    left_join(df2021, by = "regione_res") %>%
    mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x)) %>%
    mutate(perc2021 = ifelse(regione_res == toupper(Regione), NA, perc2021))
  
  mappa$Percentuale = mappa_regioni$perc2021
  
  ggplot(mappa)+
    geom_sf(aes( fill = Percentuale), col = "#DBDBDB") +
    geom_sf_text(aes(label = round(Percentuale,2)), check_overlap = TRUE, size = 2, fontface = "bold", color = "black" )+
    mytheme_solarized() +
    scale_fill_gradient(low = "#FFFFFF", high = "#00FF15", na.value = "#FF1802")+
    ggtitle(label = paste0("Da Dove viene chi si iscrive ", articolo, " ", Regione, "?"), subtitle = "Percentuale calcolata con riferimento alla totalità \n di immatricolati fuorisede nel 2021.")+
    xlab("")+
    ylab("")+
    theme(plot.subtitle = element_text(face = "italic"),
          plot.caption = element_text(face = "italic"),
          legend.position = "none", 
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "aliceblue")) +
    labs(caption = "Fonte: Miur Open Data")  +
    coord_sf(xlim = c(0,25), ylim = c(37,46.8), expand = TRUE) 
  
} 

emigration_map <- function(Regione = "Puglia", articolo = "in", df = df_fuorisede, mappa = mapIta){
  
  mytheme_solarized <- function (base_size = 15, base_family = "", light = TRUE) {
    rebase <- ggthemes:::solarized_rebase(light)
    ret <- (theme_bw(base_size = base_size, base_family = base_family) + 
              theme(text = element_text(colour = rebase["rebase01"]), 
                    title = element_text(color = rebase["rebase0"]), 
                    line = element_line(color = rebase["rebase01"]), 
                    rect = element_rect(fill = rebase["rebase03"], color = rebase["rebase01"]), 
                    
                    panel.grid = element_line(color = rebase["rebase02"]), 
                    panel.grid.major = element_line(color = rebase["rebase02"]), 
                    panel.grid.minor = element_line(color = rebase["rebase02"]), 
                    plot.background = element_rect(fill = NULL, colour = NA, 
                                                   linetype = 0)))
    ret
  }
  
  
  df2021 <- df %>% 
    filter(regione_dest != toupper(Regione) & regione_res == toupper(Regione) ) %>% 
    filter(anno_acc %in% c("2020/2021")) %>%
    group_by(anno_acc, regione_dest) %>% 
    summarise(imm_fuorisede2021 = sum(imm_totali, na.rm = TRUE)) %>%
    mutate(imm_totali2021 = sum(imm_fuorisede2021))%>%
    mutate(perc2021 = imm_fuorisede2021/imm_totali2021*100)
  
  
  mappa_regioni <- tibble(NOME_REG = mappa$NOME_REG) %>%
    mutate(NOME_REG = toupper(NOME_REG), 
           NOME_REG = str_replace_all(NOME_REG, "-", " "), 
           regione_dest = NOME_REG) %>%
    left_join(df2021, by = "regione_dest") %>%
    mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x)) %>%
    mutate(perc2021 = ifelse(regione_dest == toupper(Regione), NA, perc2021))
  
  mappa$Percentuale = mappa_regioni$perc2021
  
  ggplot(mappa)+
    geom_sf(aes( fill = Percentuale), col = "#DBDBDB") +
    geom_sf_text(aes(label = round(Percentuale,2)), check_overlap = TRUE, size = 2, fontface = "bold", color = "black" )+
    mytheme_solarized() +
    scale_fill_gradient(low = "#FFFFFF", high = "#00FF15", na.value = "#FF1802")+
    ggtitle(label = paste0("Dove va chi vive ", articolo, " ", Regione, "?"), subtitle = "Percentuale calcolata con riferimento alla totalità \n di immatricolati fuorisede dalla regione nel 2021.")+
    xlab("")+
    ylab("")+
    theme(plot.subtitle = element_text(face = "italic"),
          plot.caption = element_text(face = "italic"),
          legend.position = "none", 
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "aliceblue")) +
    labs(caption = "Fonte: Miur Open Data")  +
    coord_sf(xlim = c(0,25), ylim = c(37,46.8), expand = TRUE) 
  
} 




# funzioni per grafici a barre
migration_plot <- function(Regione = "Puglia", articolo = "in", df = df_fuorisede, df2 = df_emigrazioni){
  
  
  df_regione <- filter(df2, regione == toupper(Regione) )
  
  immatricolati_stessa_regione20 <- df_regione$residenti_1920/df_regione$totale_1920
  immatricolati_stessa_regione21 <- df_regione$residenti_2021/df_regione$totale_2021
  
  var_residenti = round((immatricolati_stessa_regione21-immatricolati_stessa_regione20)*100,2)
  var_fuorisede = round(df_regione$var_fuorisede,2)
  
  
  df_rank = left_join(
    df %>%
      filter(anno_acc %in% c("2020/2021") & regione_dest == toupper(Regione) & regione_res != toupper(Regione)) %>%
      group_by(anno_acc, regione_res) %>%
      summarise(imm_totali = sum(imm_totali, na.rm = TRUE))%>%
      ungroup()%>%
      select(anno_acc, regione_res, imm_totali),
    df %>%
      filter(anno_acc %in% c("2019/2020") & regione_dest == toupper(Regione) & regione_res != toupper(Regione)) %>%
      group_by(anno_acc, regione_res) %>%
      summarise(imm_totali = sum(imm_totali, na.rm = TRUE))%>%
      ungroup()%>%
      select(regione_res, imm_totali), by = c("regione_res")) %>%
    select(regione_res, `Immatricolati 2020` = "imm_totali.x", `Immatricolati 2019` = "imm_totali.y" ) %>%
    mutate(`Immatricolati 2019` = ifelse(is.na(`Immatricolati 2019`), 0, `Immatricolati 2019`)) %>%
    gather("Anno Accademico", "value", `Immatricolati 2020`, `Immatricolati 2019`)
  
  
  
  df_rank %>% 
    mutate(
      regione_res = case_when(
        regione_res == "TRENTINO ALTO ADIGE" ~ "TRENTINO",
        regione_res == "FRIULI VENEZIA GIULIA" ~ "FRIULI",
        TRUE ~ regione_res
      )
    ) %>%
    ggplot(aes(value, reorder(regione_res, value), fill = `Anno Accademico`)) +
    geom_bar(stat = "identity", colour = "black", width = 0.7, position =  position_dodge(0.7)) +
    scale_fill_manual(values = c("#F58772", "#B7F28D"))+
    ggthemes::theme_solarized()+
    xlab("Immatricolati (valore assoluto)")+
    ylab("")+
    ggtitle(paste0("Da dove viene chi si iscrive ", articolo, " ", Regione, "?" ), 
            subtitle = paste0("La variazione della differenza del rapporto di immatricolati residenti sul \n  totale tra il 2019/20 e il 2020/21, è stata di: " , var_residenti, " %, \n", 
                              "mentre la variazione di fuorisede per il 2020/21 è stata: ", var_fuorisede, " %." ))+
    theme(axis.text.x = element_text(angle = 0, face = "bold"), 
          axis.text.y = element_text(face = "bold"), 
          axis.title  = element_text(face = "bold"),
          plot.title  = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          plot.caption = element_text(face = "italic"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour="grey60", linetype="dotted"),
          legend.text = element_text(face = "italic"),
          legend.title = element_text(face = "bold"),
          legend.position = c(0.8,0.2) ) +
    labs(caption = "Fonte: Miur Open Data")  
  
}

emigration_plot <- function(Regione = "Puglia", articolo = "in", df = df_fuorisede, df2 = df_emigrazioni){
  
  df_regione <- filter(df2, regione == toupper(Regione) )
  
  var_emigrati = round(df_regione$var_emigrati,2)
  
  df_rank = left_join(
    df %>%
      filter(anno_acc %in% c("2020/2021") & regione_res == toupper(Regione) & regione_dest != toupper(Regione)) %>%
      group_by(anno_acc, regione_dest) %>%
      summarise(imm_totali = sum(imm_totali, na.rm = TRUE))%>%
      ungroup()%>%
      select(anno_acc, regione_dest, imm_totali),
    df %>%
      filter(anno_acc %in% c("2019/2020") & regione_res == toupper(Regione) & regione_dest != toupper(Regione)) %>%
      group_by(anno_acc, regione_dest) %>%
      summarise(imm_totali = sum(imm_totali, na.rm = TRUE))%>%
      ungroup()%>%
      select(regione_dest, imm_totali), by = c("regione_dest")) %>%
    select(regione_dest, `Immatricolati 2020` = "imm_totali.x", `Immatricolati 2019` = "imm_totali.y" ) %>%
    mutate_if(is.numeric, ~ifelse(is.na(.x),0, .x)) %>%
    gather("Anno Accademico", "value", `Immatricolati 2020`, `Immatricolati 2019`)
  
  df_rank %>% 
    mutate(
      regione_dest = case_when(
        regione_dest == "TRENTINO ALTO ADIGE" ~ "TRENTINO",
        regione_dest == "FRIULI VENEZIA GIULIA" ~ "FRIULI",
        TRUE ~ regione_dest
      )
    ) %>%
    ggplot(aes(value, reorder(regione_dest, value), fill = `Anno Accademico`)) +
    geom_bar(stat = "identity", colour = "black", width = 0.7, position = position_dodge(0.7)) +
    scale_fill_manual(values = c("#F58772", "#B7F28D"))+
    ggthemes::theme_solarized()+
    xlab("Immatricolati (valore assoluto)")+
    ylab("")+
    ggtitle(paste0("Dove va chi è residente ", articolo, " ", Regione, "?" ), 
            subtitle =  paste0("La variazione di studenti residenti emigrati dalla regione tra il\n ", "2019/20 e il 2020/21 è: ",  var_emigrati, "%", "\n"))+
    theme(axis.text.x = element_text(angle = 0, face = "bold"), 
          axis.text.y = element_text(face = "bold"), 
          axis.title  = element_text(face = "bold"),
          plot.title  = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          plot.caption = element_text(face = "italic"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour="grey60", linetype="dotted"),
          legend.text = element_text(face = "italic"),
          legend.title = element_text(face = "bold"),
          legend.position = c(0.8,0.2) ) +
    labs(caption = "Fonte: Miur Open Data")  
  
  
  
}
