library(readxl)
library(tidyverse)

load("data/miur_env.RData")

miur_env <- list()

message("Updating Miur Data....")

miur_env$webdata   <- read_excel("data/miur_weblink.xlsx", sheet = "OpenData")
miur_env$collegi   <- read_excel("data/miur_weblink.xlsx", sheet = "Collegi")
miur_env$atenei    <- read_excel("data/miur_weblink.xlsx", sheet = "Atenei")
miur_env$classi    <- read_excel("data/miur_weblink.xlsx", sheet = "Classi")
miur_env$ssd       <- read_excel("data/miur_weblink.xlsx", sheet = "SSD")

miur_env$istat <- list()

miur_env$istat$italia =  readxl::read_excel("data/tavole_istat/tavole_istat.xlsx", sheet =  "7.3 Iscritti Serie Storica", na = "NA")
miur_env$istat$iscritti =  readxl::read_excel("data/tavole_istat/tavole_istat.xlsx", sheet =  "7.3 Iscritti Serie Storica", na = "NA")
miur_env$istat$diplomati_istituto = readxl::read_excel("data/tavole_istat/tavole_istat.xlsx", sheet =  "7.11 Diplomati per Istituto", na = "NA")
miur_env$istat$iscritti_gruppo = readxl::read_excel("data/tavole_istat/tavole_istat.xlsx", sheet =  "7.15 Iscritti per Gruppo", na = "NA")
miur_env$istat$laureati_gruppo = readxl::read_excel("data/tavole_istat/tavole_istat.xlsx", sheet =  "7.16 Laureati per Gruppo", na = "NA")
miur_env$istat$diplomati_zona = readxl::read_excel("data/tavole_istat/tavole_istat.xlsx", sheet =  "7.17 Diplomati per Zona", na = "NA")
miur_env$istat$diplomati_regione = readxl::read_excel("data/tavole_istat/tavole_istat.xlsx", sheet =  "Diplomati per Regione", na = "NA")


miur_env$atenei = miur_env$atenei %>%
  mutate(
    lat_comune = as.numeric(lat_comune),
    lon_comune = as.numeric(lon_comune), 
    lat_ateneo = as.numeric(lat_ateneo), 
    lon_ateneo = as.numeric(lon_ateneo) ) 

df_atenei = miur_env$atenei 


miur_env$data = list()


########   DF ATENEI MIUR (DATI A LIVELLO DEL SINGOLO ATENEO) ----

search_miur(ateneo == TRUE & anno_inizio >= 2010) 

# 1) Unione Immatriolati per Ateneo e Immatricolati stranieri 
# id = 2 Immatricolati per Ateneo
# id = 13 Immatricolati Stranieri 

df_atenei_miur = left_join(get_miur(2), select(get_miur(13),-ateneo), by = c("anno_acc", "id_ateneo"))

# 2) Estrapolazione immatricolati fuorisede 
# id = 9 Immatricoati per Provenienza --> Immatricolati Fuorisede

df_fuorisede_miur =  get_miur(9) %>%
  mutate(regione_res = 
           case_when(
             regione_res == "PROVINCIA AUTONOMA DI BOLZANO" ~ "BOLZANO", 
             regione_res == "PROVINCIA AUTONOMA DI TRENTO" ~ "TRENTO", 
             regione_res == "REGIONE ESTERA" ~ "INTERNAZIONALE", 
             regione_res == "REGIONE NON FORNITA" ~ "ALTRO", 
             TRUE ~ regione_res
           )) %>%
  filter(regione_res != "ALTRO") %>% 
  group_by(anno_acc, id_ateneo, regione_res) %>%
  summarise(imm_totali = sum(imm_totali, na.rm = TRUE)) %>%
  left_join(df_atenei %>% 
              select(id_ateneo, regione_dest = "regione_ateneo"), by = "id_ateneo")%>%
  mutate(
    imm_fuorisede = case_when(
      regione_dest != regione_res ~ "imm_fs", 
      regione_dest == regione_res ~ "imm_nfs", 
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() 

df_atenei_miur <-  df_fuorisede_miur %>% 
  select(anno_acc, id_ateneo, regione_dest, regione_res, imm_fuorisede,  imm_totali)  %>% 
  group_by(anno_acc, id_ateneo, imm_fuorisede) %>%
  summarise(n = sum(imm_totali, na.rm = TRUE)) %>%
  spread(imm_fuorisede, n) %>%
  select(anno_acc:imm_nfs) %>%
  ungroup() %>% 
  right_join(df_atenei_miur, by = c("anno_acc",  "id_ateneo"))


# 3) Unione con i dati sugli iscritti 
# id = 17 Iscritti per ateneo

df_atenei_miur = left_join(df_atenei_miur, select(get_miur(17), -ateneo), by = c("anno_acc", "id_ateneo"))

# 4) Unione con i dati sugli iscritti al 1 anno 
# id = 23 Iscritti al 1 anno

df_atenei_miur = get_miur(23) %>%
  group_by(anno_acc, id_ateneo) %>%
  summarise(isc_1anno = sum(isc_totali, na.rm = TRUE)) %>%
  right_join(df_atenei_miur, by = c("anno_acc", "id_ateneo"))

# 5) Estrapolazione iscritti fuorisede 
# id = 28 Iscritti per Proveninza --> Iscritti Fuorisede 

df_fuorisede_miur = get_miur(28) %>%
  mutate(regione_res = 
           case_when(
             regione_res == "PROVINCIA AUTONOMA DI BOLZANO" ~ "BOLZANO", 
             regione_res == "PROVINCIA AUTONOMA DI TRENTO" ~ "TRENTO", 
             regione_res == "REGIONE ESTERA" ~ "INTERNAZIONALE", 
             regione_res == "REGIONE NON FORNITA" ~ "ALTRO", 
             TRUE ~ regione_res
           )) %>%
  filter(regione_res != "ALTRO") %>% 
  group_by(anno_acc, id_ateneo, regione_res) %>%
  summarise(isc_totali = sum(isc_totali, na.rm = TRUE)) %>%
  left_join(df_atenei %>% 
              select(id_ateneo, regione_dest = "regione_ateneo"), by = "id_ateneo")%>%
  mutate(
    isc_fuorisede = case_when(
      regione_dest != regione_res ~ "isc_fs", 
      regione_dest == regione_res ~ "isc_nfs", 
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  right_join(df_fuorisede_miur, by = c("anno_acc", "id_ateneo", "regione_dest", "regione_res"))%>% 
  select(anno_acc, id_ateneo, regione_dest, regione_res, imm_fuorisede, isc_fuorisede, imm_totali,  isc_totali) 


df_atenei_miur = df_fuorisede_miur %>%
  select(anno_acc, id_ateneo, regione_dest, regione_res, isc_fuorisede,  isc_totali)  %>% 
  group_by(anno_acc, id_ateneo, isc_fuorisede) %>%
  summarise(n = sum(isc_totali, na.rm = TRUE)) %>%
  spread(isc_fuorisede, n) %>%
  select(anno_acc:isc_nfs) %>%
  ungroup() %>% 
  right_join(df_atenei_miur, by = c("anno_acc",  "id_ateneo"))


# 6) Laureati per Ateneo 
# id = 32 Laureati per Ateneo 

df_atenei_miur = get_miur(32) %>%
  mutate(anno_acc = paste0(anno_sol-1, "/", anno_sol)) %>%
  select(-ateneo) %>%
  right_join(df_atenei_miur, by = c("anno_acc",  "id_ateneo")) 


# Amelia::missmap(df_atenei_miur)

df_atenei_miur = inner_join(df_atenei_miur, select(df_atenei, id_ateneo, ateneo_breve), by = "id_ateneo")

df_atenei_miur = select(df_atenei_miur, anno_acc,anno_sol, id_ateneo, ateneo_breve, 
                        imm_maschi, imm_femmine, imm_fs, imm_nfs, imm_totali_s, imm_totali,
                        isc_maschi, isc_femmine, isc_1anno, isc_fs, isc_nfs, isc_totali, 
                        lau_maschi, lau_femmine, lau_totali)

# Amelia::missmap(df_fuorisede_miur)
df_fuorisede_miur = inner_join(df_fuorisede_miur, select(df_atenei, id_ateneo, ateneo_breve), by = "id_ateneo")
df_fuorisede_miur = select(df_fuorisede_miur, anno_acc, id_ateneo, ateneo_breve, everything())

# add in miur env 
miur_env$data$atenei = df_atenei_miur
miur_env$data$fuorisede = df_fuorisede_miur


# save file 
write_csv(df_atenei_miur, "data/df_atenei_miur.csv")
write_csv(df_fuorisede_miur, "data/df_fuorisede_miur.csv")




########   DF CLASSI LAUREA MIUR - Dati su iscritti 1 anno, iscritti totali, immatricolati, immatricolati stranieri e fuorisede e laureati per Ateneo e classe di laurea  -----

search_miur(classe == TRUE & ateneo  == TRUE)

# Immatricolati per ateneo e classe  ( id = 15 )
df_atenei_classe = get_miur(15) %>% 
  select(-ateneo)


# Iscritti al primo anno per laurea --> Iscritti primo anno per classe ( id = 23 )
df_atenei_classe = get_miur(23) %>%
  group_by(anno_acc, id_ateneo, codice_classe) %>%
  summarise(isc_1anno = sum(isc_totali, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(df_atenei_classe, by = c("anno_acc", "id_ateneo", "codice_classe"))


# Aggiungo Iscritti totali ( id = 25 )
df_atenei_classe = get_miur(25, .repair  = TRUE) %>%
  select(-laurea) %>%
  group_by(anno_acc, id_ateneo, codice_classe) %>%
  summarise_all(mean, na.rm = TRUE)%>%
  ungroup() %>%
  filter(str_detect(codice_classe, "L")) %>%
  right_join(df_atenei_classe, by = c("anno_acc", "id_ateneo", "codice_classe"))  

# Aggiungo laureati  ( id = c(37,38) )
df_atenei_classe = get_miur(c(37,38), bind_col = TRUE) %>%
  group_by(anno_sol, id_ateneo, codice_classe) %>%
  summarise(lau_totali = sum(lau_totali, na.rm = TRUE)) %>%
  mutate(anno_acc = paste0(anno_sol-1, "/", anno_sol)) %>%
  ungroup() %>%
  select(-anno_sol) %>%
  right_join(df_atenei_classe, by = c("anno_acc", "id_ateneo", "codice_classe"))

# aggiungo nomi universita (nomi brevi)
df_atenei_classe = inner_join(df_atenei_classe, select(df_atenei, id_ateneo, ateneo_breve), by = "id_ateneo")
df_atenei_classe = select(df_atenei_classe, anno_acc, id_ateneo, ateneo_breve,  codice_classe, isc_1anno, isc_femmine:isc_totali, imm_totali, lau_totali)


# Amelia::missmap(df_atenei_classe)

# add in miur env 
miur_env$data$classe = df_atenei_classe

# save the file 
write_csv(df_atenei_classe, "data/df_atenei_classe.csv")


########   DF LAUREE MIUR - Dati su iscritti 1 anno, iscritti totali e laureati ------

search_miur(corso == TRUE)

# Aggiungo Iscritti primo anno ( id = 23 )  
df_atenei_laurea = get_miur(23) %>%  
  select(-ateneo) %>%
  group_by(anno_acc, id_ateneo, codice_classe, laurea) %>%
  summarise(isc_1anno = sum(isc_totali, na.rm = TRUE)) %>%
  ungroup() 

# Aggiungo Iscritti totali ( id = 25 )
df_atenei_laurea = get_miur(25, .repair  = TRUE) %>% 
  filter(str_detect(codice_classe, "L")) %>%
  right_join(df_atenei_laurea, by = c("anno_acc", "id_ateneo","codice_classe", "laurea")) 


# Aggiungo laureati  ( id = c(37,38) )
df_atenei_laurea =  get_miur(c(37,38), bind_col = TRUE) %>% 
  mutate(anno_acc = paste0(anno_sol-1, "/", anno_sol))  %>%
  group_by(anno_acc, id_ateneo, codice_classe, laurea) %>%
  summarise(lau_totali = sum(lau_totali, na.rm = TRUE)) %>%
  ungroup() %>%
  right_join(df_atenei_laurea, by = c("anno_acc", "id_ateneo", "codice_classe", "laurea"))

# Amelia::missmap(df_atenei_laurea)

# aggiungo nomi universita (nomi brevi)
df_atenei_laurea = inner_join(df_atenei_laurea, select(df_atenei, id_ateneo, ateneo_breve), by = "id_ateneo")
df_atenei_laurea = select(df_atenei_laurea, anno_acc, id_ateneo, ateneo_breve, everything())


# add in miur env 
miur_env$data$laurea = df_atenei_laurea

# save the file 
write_csv(df_atenei_laurea, "data/df_atenei_laurea.csv")




########   UNIONE CON I DATI DI UNIVERSITALY     ------

df_lauree_univ = df_iscritti %>%
  left_join(select(df_lauree, id_ateneo, id_laurea, laurea, classe) %>% unnest(classe), by = c("id_ateneo", "id_laurea")) %>%
  select(anno_acc, id_ateneo, classe, laurea, isc_totali,  everything()) %>%
  select(-id_laurea)

df_lauree_univ = full_join(df_lauree_univ, select(df_atenei_laurea, classe = "codice_classe", everything()),  by = c("anno_acc", "id_ateneo", "classe", "laurea"))%>% 
  mutate(
    isc_totali = case_when(
      is.na(isc_totali.y) ~ isc_totali.x,
      TRUE ~ isc_totali.y
    )) 

df_lauree_univ =  select(df_lauree_univ, anno_acc, id_ateneo, classe, laurea, isc_1anno, isc_totali, lau_totali) 


# add in miur env 
miur_env$data$univ = df_lauree_univ

# save the file 
write_csv(df_lauree_univ, "data/df_lauree_univ.csv")




##### OFFERTA FORMATIVA 

#creazione del dataset per le lauree e gli insegnamenti (Universitaly)
#
# df_insegnamenti
# df_occupazione
# df_posti 
# df_professioni 

# dataset lauree dallo storico dell' offerta formativa
df_lauree = filter(offerta_formativa, anno_acc == "2020/2021")

# aggiungo i posti delle lauree 
df_lauree = left_join(df_lauree, df_posti_lauree, by = c("id_ateneo", "id_laurea"))

# aggiungo le professioni previste
df_lauree =  left_join(df_lauree, df_professioni %>%
                         group_by(id_ateneo, id_laurea) %>%
                         nest(professione = c(professione, codice_istat)), by = c("id_ateneo", "id_laurea"))

# estrapolazione dati Alma Laurea 
df_lauree = left_join(df_lauree, 
                      df_occupazione %>%
                        mutate(
                          variabile = case_when(
                            variabile == "Occupazione a 1 anno" ~ "occ1",
                            variabile == "Occupazione a 3 anni" ~ "occ3",
                            variabile == "Occupazione a 5 anni" ~ "occ5",
                            
                            variabile == "Retribuzione mensile netta a 1 anno" ~ "retrib1",
                            variabile == "Retribuzione mensile netta a 3 anni" ~ "retrib3",
                            variabile == "Retribuzione mensile netta a 5 anni" ~ "retrib5",
                            
                            variabile == "Laureati in corso" ~ "inCorso",
                            variabile == "Decisamente soddisfatti" ~ "soddisf",
                            variabile == "Voto di laurea" ~ "voto",
                            variabile == "Frequentanti" ~ "freq",
                            TRUE ~ NA_character_
                          )
                        ) %>%
                        filter(!is.na(variabile)) %>%
                        select(-value_ateneo) %>%
                        spread(variabile, value_laurea) %>%
                        group_by(id_ateneo, id_laurea) %>%
                        nest(almalaurea = c(freq:voto)), by = c("id_ateneo", "id_laurea"))


# inserimento informazioni da df_atenei (miur weblink) (Usiamo Nome Breve)
# 
df_lauree = select(df_lauree, -ateneo) 

df_lauree = left_join(df_lauree,
                      select(df_atenei, -partita_iva, -codice_fiscale, -url_open_data, -dimensione, -ateneo,-tipologia_ateneo) %>%
                        nest(coords = c(indirizzo_ateneo, lat_comune, lon_comune, lat_ateneo, lon_ateneo))  %>%
                        select(ateneo = "ateneo_breve", everything()), by = "id_ateneo") %>%
  select(anno_acc, id_ateneo, ateneo, statale, zona, regione = "regione_ateneo", provincia = "provincia_ateneo", comune = "comune_ateneo", everything())

df_lauree = select(df_lauree, anno_acc, id_ateneo, id_laurea, ateneo, laurea, statale:nome_classe, classe = "codice_classe", everything())
df_lauree = select(df_lauree, -url_completo, -url_sintetico)


# aggiungo iscritti
df_lauree = left_join(df_lauree,
                      
                      miur_env$data$univ %>%
                        filter(anno_acc == "2019/2020") %>%
                        select(-anno_acc, -lau_totali, -classe) %>%
                        nest(isc = c(isc_totali, isc_1anno)), by = c("id_ateneo", "laurea") )




df_lauree
df_insegnamenti



miur_env$lauree = df_lauree

miur_env$insegnamenti = df_insegnamenti

save(miur_env, file = "data/miur_env.RData")

save(miur_env, file = "~//Documents/projects/app_myuniversity/miur_env.RData")










########  DF Emigrazioni/Immigrazioni (2019/20 - 2020/21)

df_fuorisede =  get_miur(9, verbose = FALSE) %>%
  mutate(regione_res = 
           case_when(
             regione_res == "PROVINCIA AUTONOMA DI BOLZANO" ~ "TRENTINO ALTO ADIGE", 
             regione_res == "PROVINCIA AUTONOMA DI TRENTO" ~ "TRENTINO ALTO ADIGE", 
             regione_res == "REGIONE ESTERA" ~ "ALTRO", 
             regione_res == "REGIONE NON FORNITA" ~ "ALTRO", 
             regione_res == "DATI AGGREGATI" ~ "ALTRO", 
             regione_res == "TRENTO" ~ "TRENTINO ALTO ADIGE", 
             regione_res == "BOLZANO" ~ "TRENTINO ALTO ADIGE", 
             TRUE ~ regione_res
           )) %>%
  filter(regione_res != "ALTRO") %>%
  group_by(anno_acc, id_ateneo, regione_res) %>%
  summarise(imm_totali = sum(imm_totali, na.rm = TRUE)) %>%
  ungroup()%>%
  inner_join(df_atenei %>% 
               select(id_ateneo, regione_dest = "regione_ateneo"), by = "id_ateneo") %>%
  mutate(regione_dest = 
           case_when(
             regione_dest == "PROVINCIA AUTONOMA DI BOLZANO" ~ "TRENTINO ALTO ADIGE", 
             regione_dest == "PROVINCIA AUTONOMA DI TRENTO" ~ "TRENTINO ALTO ADIGE", 
             regione_dest == "REGIONE ESTERA" ~ "INTERNAZIONALE", 
             regione_dest == "REGIONE NON FORNITA" ~ "ALTRO", 
             regione_dest == "DATI AGGREGATI" ~ "ALTRO", 
             regione_dest == "TRENTO" ~ "TRENTINO ALTO ADIGE", 
             regione_dest == "BOLZANO" ~ "TRENTINO ALTO ADIGE", 
             TRUE ~ regione_dest
           )) %>% 
  mutate(
    is_fuorisede = case_when(
      regione_dest != regione_res ~ "imm_fs", 
      regione_dest == regione_res ~ "imm_nfs", 
      TRUE ~ NA_character_
    )
  ) 

df_fuorisede = left_join(df_fuorisede, unique(select(df_atenei, regione_dest = "regione_ateneo", zona_dest = "zona")), by = c("regione_dest")) %>%
  mutate(zona_dest = case_when(
    is.na(zona_dest) & regione_dest == "TRENTINO ALTO ADIGE" ~ "NORD-EST",
    TRUE ~ zona_dest
  ))
df_fuorisede = left_join(df_fuorisede, unique(select(df_atenei, regione_res = "regione_ateneo", zona_res = "zona")), by = c("regione_res")) %>%
  mutate(zona_res = case_when(
    is.na(zona_res) & regione_res == "TRENTINO ALTO ADIGE" ~ "NORD-EST",
    TRUE ~ zona_res
  ))

# migrazioni (studenti che vanno nella regione)
df_migrazioni = df_fuorisede %>%
  group_by(anno_acc, regione_dest, is_fuorisede) %>%
  summarise(imm_totali = sum(imm_totali, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(is_fuorisede, imm_totali) 

df_migrazioni = inner_join(
  tibble(regione_dest = toupper(mapIta$NOME_REG)) %>%
    mutate(regione_dest = str_replace_all(regione_dest, "-", " ")) %>%
    left_join(filter(df_migrazioni, anno_acc == "2019/2020"), by = "regione_dest") %>%
    mutate_if(is.numeric, ~ifelse(is.na(.x), 0,.x)) %>%
    select(regione_dest, imm_fs1920 = "imm_fs", imm_nfs1920 = "imm_nfs"), 
  
  tibble(regione_dest = toupper(mapIta$NOME_REG)) %>%
    mutate(regione_dest = str_replace_all(regione_dest, "-", " ")) %>%
    left_join(filter(df_migrazioni, anno_acc == "2020/2021"), by = "regione_dest") %>%
    mutate_if(is.numeric, ~ifelse(is.na(.x), 0,.x)) %>%
    select(-anno_acc)%>%
    select(regione_dest, imm_fs2021 = "imm_fs", imm_nfs2021 = "imm_nfs"), by = c("regione_dest"))  


df_migrazioni = df_migrazioni %>%
  select(regione = "regione_dest", 
         
         immigrati_1920 = "imm_fs1920",
         immigrati_2021 = "imm_fs2021",
         residenti_1920 = "imm_nfs1920",
         residenti_2021 = "imm_nfs2021"
         ) %>% 
  mutate(
    totale_1920   = immigrati_1920 + residenti_1920,
    totale_2021   = immigrati_2021 + residenti_2021, 
    var_totale    = (totale_2021-totale_1920)/totale_1920*100, 
    var_fuorisede = (immigrati_2021-immigrati_1920)/immigrati_1920*100
  ) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x))





# emigrazioni
df_emigrazioni = df_fuorisede %>%
  group_by(anno_acc, regione_res, is_fuorisede) %>%
  summarise(imm_totali = sum(imm_totali, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(is_fuorisede, imm_totali) 

df_emigrazioni = inner_join(
  
  tibble(regione_res = toupper(mapIta$NOME_REG)) %>%
    mutate(regione_res = str_replace_all(regione_res, "-", " ")) %>%
    left_join(filter(df_emigrazioni, anno_acc == "2019/2020"), by = "regione_res") %>%
    mutate_if(is.numeric, ~ifelse(is.na(.x), 0,.x)) %>%
    select(regione_res, imm_fs1920 = "imm_fs", imm_nfs1920 = "imm_nfs"), 
  
  tibble(regione_res = toupper(mapIta$NOME_REG)) %>%
    mutate(regione_res = str_replace_all(regione_res, "-", " ")) %>%
    left_join(filter(df_emigrazioni, anno_acc == "2020/2021"), by = "regione_res") %>%
    mutate_if(is.numeric, ~ifelse(is.na(.x), 0,.x)) %>%
    select(-anno_acc)%>%
    select(regione_res, imm_fs2021 = "imm_fs", imm_nfs2021 = "imm_nfs"), by = c("regione_res")) 


df_regioni = 
inner_join(df_migrazioni,  df_emigrazioni %>%
             select(regione = "regione_res", 
                    emigrati_1920 = "imm_fs1920",
                    emigrati_2021 = "imm_fs2021"
             ), by = "regione") %>%
  select(regione, 
         immigrati_1920, emigrati_1920, residenti_1920, totale_1920,
         immigrati_2021, emigrati_2021, residenti_2021, totale_2021, var_totale, var_fuorisede) %>%
  mutate(
    var_emigrati = (emigrati_2021-emigrati_1920)/emigrati_1920*100,
    bilancio1920 = immigrati_1920 - emigrati_1920,
    bilancio2021 = immigrati_2021 - emigrati_2021
    
  )

df_regioni = left_join(df_regioni, unique(select(df_atenei, regione = "regione_ateneo", zona)), by = c("regione")) %>%
  mutate(zona = case_when(
    is.na(zona) & regione == "TRENTINO ALTO ADIGE" ~ "NORD-EST",
    TRUE ~ zona
  ))



miur_env$data$regioni = df_regioni
miur_env$data$fuorisede2 = df_fuorisede
save(miur_env, file = "data/miur_env.RData") 



