library(tidyverse)
library(tm)


# inizializzare l'environment 
miur_env$webdata <- read_excel("miur_weblink.xlsx", sheet = "OpenData")


# funzione di ricerca 

search_miur <- function(..., env = miur_env){
  
  dplyr::filter(env$webdata, ...)
  
}


# funzione di download e pulizia dei dati uniformando i nomi delle colonne e riparando gli errori

get_miur <- function(id, .repair_names = TRUE, .repair = TRUE, bind_col = TRUE, delim = ";", verbose = TRUE){
  
    filter_data <- miur_env$webdata[which(miur_env$webdata$id %in% id),]
    
    url <- filter_data$url_download
    
    quiet_import <- purrr::quietly(readr::read_delim)
    
    df = list()
    
    for(i in 1:length(id)){
      
      if(verbose) message("ID: ", i, " ", filter_data$descrizione[i] )
      
      df[[i]] = quiet_import(url[i], delim = delim)$result
      
    }
    
    # pulizia e ricodifica dei nomi delle colonne
    if(.repair_names){
    
      adjust_col_names <- function(x) {
        
        col_names <- colnames(x)
        col_names <- tolower(col_names)
        col_names <- tm::removePunctuation(col_names)
        col_names <- stringr::str_trim(col_names)
        col_names <- stringr::str_replace_all(col_names, " ", "_")
        
        col_names <- dplyr::case_when(
          
          # anno 
          col_names == "annoa" ~ "anno_acc",
          col_names == "annos" ~ "anno_sol",
          col_names == "annosolare" ~ "anno_sol",
          col_names == "anno_solare" ~ "anno_sol",
          col_names == "annocorso" ~ "anno_corso",
          col_names == "anno" ~ "anno_sol",
          
          # immatricolati 
          col_names == "immm" ~ "imm_maschi",
          col_names == "immf" ~ "imm_femmine",
          col_names == "imm" ~ "imm_totali",
          col_names == "immms" ~ "imm_maschi_s",
          col_names == "immfs" ~ "imm_femmine_s",
          col_names == "imms" ~ "imm_totali_s",
          col_names == "immmrs" ~ "imm_maschi_res_e",
          col_names == "immfrs" ~ "imm_femmine_res_e",
          col_names == "immrs" ~ "imm_totali_res_s",
          
          # iscritti 
          col_names == "iscm" ~ "isc_maschi",
          col_names == "iscf" ~ "isc_femmine",
          col_names == "isc" ~ "isc_totali",
          col_names == "iscms" ~ "isc_maschi_s",
          col_names == "iscfs" ~ "isc_femmine_s",
          col_names == "iscs" ~ "isc_totali_s",
          
          # ateneo e geo
          col_names == "ateneocod" ~ "id_ateneo",
          col_names == "codateneo" ~ "id_ateneo",
          col_names == "codiceateneo" ~ "id_ateneo",
          col_names == "ateneonome" ~ "ateneo",
          col_names == "nomeateneo" ~ "ateneo",
          col_names == "ateneoregione" ~ "regione_ateneo",
          col_names == "regateneo" ~ "regione_ateneo",
          col_names == "ateneoareageo" ~ "geo_ateneo",
          col_names == "areageo" ~ "geo_ateneo",
          
          col_names == "residenzar" ~ "regione_res",
          col_names == "residenzap" ~ "provincia_res",
          col_names == "istatp" ~ "istat_provincia",
          col_names == "sedep" ~ "provincia_sede",
          col_names == "sedec" ~ "sede",
          
          # laureati 
          col_names == "laum" ~ "lau_maschi",
          col_names == "lauf" ~ "lau_femmine",
          col_names == "lau" ~ "lau_totali",
          col_names == "laums" ~ "lau_maschi_s",
          col_names == "laufs" ~ "lau_femmine_s",
          col_names == "laus" ~ "lau_totali_s",
          col_names == "laureatimaschi" ~ "lau_maschi",
          col_names == "laureatifemmine" ~ "lau_femmine",
          col_names == "laureatitotale" ~ "lau_totali",
          
          
          # variabili studenti 
          col_names == "annonascita" ~ "anno_nascita",
          col_names == "sesso" ~ "genere",
          col_names == "provenienza" ~ "fuoriregione",
          col_names == "cittadinanzanome" ~ "paese",
          
          # variabili corsi (gruppo, tipo, classe, corso)
          col_names == "grupponome" ~ "nome_gruppo",
          col_names == "gruppocod"  ~ "codice_gruppo",
          col_names == "corsotipo"  ~ "tipo_laurea",
          col_names == "tipo_del_corso_di_studio" ~ "tipo_laurea",
      
          col_names == "classenome"    ~ "nome_classe",
          col_names == "classenumero"  ~ "codice_classe",
          col_names == "classe_numero" ~ "codice_classe",
          col_names == "cod"           ~ "codice_classe",
          col_names == "corsonome" ~ "laurea",
          col_names == "nomecorso" ~ "laurea",
          col_names == "classevotolaurea" ~ "voto_laurea",

          
          # variabili docenti 
          col_names == "codqualifica" ~ "codice_qualifica",
          col_names == "descqualifica" ~ "qualifica",
          col_names == "npers" ~ "n_persone",
          
          # no change names 
          col_names == "note" ~ "note",
          col_names == "codfoet2013" ~ "codfoet2013",
          col_names == "descfoet2013" ~ "descfoet2013",

          TRUE ~ col_names
        )
        
        colnames(x) <- col_names
        return(x)
                               
      }
      
      # correzione colonne 
      df <- purrr::map(df, ~.x[,!stringr::str_detect(colnames(.x), "X|fonte")])
      df <- purrr::map(df, adjust_col_names)
      df <- purrr::map(df, ~.x[,!stringr::str_detect(colnames(.x), "fonte")])
      df <- purrr::map(df, ~.x[,!stringr::str_detect(colnames(.x), "note")])
      
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, stringr::str_conv, encoding = "iso-8859-1"))
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, str_replace_all, "\u0092", "'"))
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, str_replace_all, "\u0096", "-"))
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, str_replace_all, "\\?", "'"))
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, str_replace_all, "à", "a"))
      
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, tm::stripWhitespace))
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, str_trim))
      df <- purrr::map(df, ~dplyr::mutate_if(.x, is_character, tolower))
      df <- purrr::map(df, ~purrr::map_df(.x, ~ifelse(.x == "", NA_character_, .x)))
    
    }
    
    # adjust miur errors 
    if(.repair){
      df <- purrr::map2(df, id, ~repair_miur_data(.x, .y, verbose = verbose))
    }
    
    if(length(id) > 1 & bind_col){
      warning("Binding Cols May not give a correct result.")
    }
    
    if(bind_col) df <- dplyr::bind_rows(df)
   
    return(df)

}



# sistema i dati in modo da uniformarli il più possibile (sono stati ripuliti ID da 1 a 44)

repair_miur_data <- function(df, id = "", verbose = TRUE ){
  
  if(verbose) message("Repai ID: ", id)
  
  # provincia residenza in maiuscolo 
  if(id %in% c(7,9, 22, 28, 36) ){
    
    df <- dplyr::mutate(df, provincia_res = toupper(provincia_res))
  }
  
  # regione residenza in maiscolo
  if(id %in% c(8, 9, 28) ){
    df <- dplyr::mutate(df, regione_res = toupper(regione_res))
  }
  
  # sede in maiuscolo 
  if(id %in% c(23, 37, 38)){
    df =  dplyr::mutate(df, sede =  toupper(sede))
  }
  
  # continente e paese in maiuscolo
  if(id %in% c(11,27)){
    df =  dplyr::mutate(df, continente =  toupper(continente), paese = toupper(paese))
  }
  
  # codice_classe in maiuscolo 
  if(id %in% c(15, 23, 25, 37, 38)){
    df =  dplyr::mutate(df, codice_classe =  toupper(codice_classe))
  }
  
  # regione ateneo e geo in maiuscolo
  if(id %in% c(29,41, 42, 45, 50, 56, 63)){
    
    df = dplyr::mutate(df, 
                regione_ateneo = toupper(regione_ateneo),
                geo_ateneo = toupper(geo_ateneo)
    )
  }
  
  # aggiusto id_ateneo 
  if(id %in% c(15, 25, 28, 43)){
    
    df = df %>% 
      mutate(len_codice = str_length(id_ateneo), 
             len_codice = case_when(
               len_codice == 3 ~ paste0("00", id_ateneo),
               len_codice == 4 ~ paste0("0", id_ateneo),
               TRUE ~ NA_character_
             )) %>% 
      select(-id_ateneo) 
      
      if(id==43){
        df = select(df, anno_sol, id_ateneo = "len_codice", everything()) 
      } else {
        df = select(df, anno_acc, id_ateneo = "len_codice", everything()) 
      }
      
  }
  
  # aggiusto nomi colonne 
  if(id == 17){
    colnames(df) <- c("anno_acc", "ateneo", "id_ateneo", "isc_maschi", "isc_femmine")
  }
  
  # sistemo nomi lauree
  if(id %in% c(23, 37, 38)) {
    
    df = mutate(df, 
                laurea = str_remove(laurea, "\\((.*?)\\)"), 
                laurea = str_trim(laurea),
                laurea = str_replace_all(laurea, " ' ", " - ")
                ) 
    
  }
  
  # aggiusto errore in codifica stringa 
  if(id == 24){
    
    df =  dplyr::mutate(df, anno_corso =  stringr::str_replace_all(anno_corso, "piaaaaaa", "più"))
    
  }
  
  # aggiusto errore in codifica stringa 
  if(id %in% c(29, 30, 41, 42)){
    
    df =  dplyr::mutate(df, ateneo = stringr::str_replace_all(ateneo, "universitaaaaaa", "universita"))
    
  }
  
  # pulisco errori e disaggrego maschi e femmine
  if(id == 25){
    
    df = df %>%
      filter(genere != "dato aggregato") %>%
      mutate(genere = ifelse(genere == "m", "isc_maschi", "isc_femmine")) %>%
      group_by(anno_acc, id_ateneo, codice_classe, laurea, genere) %>%
      summarise(isc_totali = sum(isc_totali, na.rm = TRUE)) %>%
      spread(genere, isc_totali) %>%
      ungroup()
    
  }
  
  # disaggrego maschi e femmine
  if(id == 29){
  
  df = df %>%
    filter(ateneo != "totale atenei") %>%
    spread(genere, isc_totali) %>%
    select(anno_acc = "anno_sol", geo_ateneo, regione_ateneo, id_ateneo, tipo_laurea,codfoet2013, descfoet2013, isc_maschi = "m", isc_femmine = "f") %>%
    mutate(isc_totali = isc_maschi + isc_femmine)
  
  }
  
  # pulisco df docenti ricodificando qualifiche 
  if( id %in% c(45,50, 56, 63)){
       
     df = df %>%
      mutate(
        tag_qualifica = case_when(
          
        qualifica == "professore i/ii fascia (ordinario/associato)" ~ "Ruolo", 
        qualifica == "ricercatore a tempo indeterminato" ~ "Ricercatore", 
        qualifica == "personale docente a contratto"    ~ "Contratto", 
        qualifica == "collaboratori linguistici"  ~ "Collaboratore", 
        qualifica == "personale tecnico-amministrativo" ~ "Personale", 
        qualifica == "collaboratori in attivita di ricerca" ~ "Collaboratore", 
        qualifica == "titolare di assegno di ricerca" ~ "Titolare Assegni di Ricerca",
        qualifica == "ricercatore a tempo indeterminato/determinato" ~ "Ricercatore",
        TRUE ~ qualifica
      ))

      
  }
  
  
  
  # pulisco errori stringhe e disaggrego maschi e femmine
  if(id %in% c(41, 42, 43)) {
    
    df = df  %>%
      filter(ateneo != "totale atenei") %>%
      spread(genere, lau_totali) %>%
      select(everything(),  lau_maschi = "m", lau_femmine = "f")
    
  }
  
  # ordino laureati per anno solare 
  if(id %in% c(31, 32, 33, 34, 35,36, 37,38, 39, 40, 41, 42,  43, 44)){
    df =  dplyr::arrange(df, desc(anno_sol))
  }
  
  # somma immatricolati totali dove non presente 
  if(id %in% c(2,3,4,5,6,7)){
    df = mutate(df, imm_totali = imm_maschi + imm_femmine)
  }
  
  # somma iscritti totali dove non presente 
  if(id %in% c(17,18,19,20,21,22,25)){
    df = mutate(df, isc_totali = isc_maschi + isc_femmine)
  }
  
  # somma laureati totali dove non presente 
  if(id %in% c(32,33,34,35,36,39,41,42,43,44)){
    df = mutate(df, lau_totali = lau_maschi + lau_femmine)
  }

  return(df)
  
}




 
