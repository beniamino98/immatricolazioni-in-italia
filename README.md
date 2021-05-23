# Le Immatricolazioni in Italia nel 2020

## Obbiettivi generali del Contest  

Le competenze del data scientist sono oggi tra le più richieste, nei settori più disparati: saper leggere i dati e saperli interpretare e rappresentare al fine di raggiungere un obiettivo, o di sviluppare un progetto di squadra, rappresenta una skill preziosissima che ogni studente, indipendentemente dal percorso di studi intrapreso, può sviluppare.
Essere in grado di creare prodotti derivati dai dati è per natura una competenza interdisciplinare e pertanto anche un ottimo mezzo per fare interagire fra di loro i saperi.
Si tratta quindi di un obiettivo fondamentale per il futuro della società, come testimonia il documento della Commissione Europea [una strategia europea sui dati](https://eur-lex.europa.eu/legal-content/IT/TXT/HTML/?uri=CELEX:52020DC0066&from=IT), in cui si delinea l’importanza di creare una società *data driven* e di formare anche i giovani su queste tematiche.
Questo contest pertanto si pone come un’opportunità preziosa per lo sviluppo di questo percorso


## Il nostro Team 

Il nostro team è composto da due persone:

- Micol Allegro, studententessa al quinto anno di Giurisprudenza all'Università di Bologna. 
- Beniamino Sartini, studente di Quantitative Finance all'Università di Bologna.


## Analisi sulle Immatricolazioni nel 2020 

La nostra analisi è focalizzata sulle immatricolazioni italiane. Come prima cosa ne descriviamo il fenomeno con i dati degli ultimi 50 anni. 
Proveremo poi a creare dei modelli econometrici per *misurare l'effetto covid19* e per *prevedere le immatricolazioni*. L'ultima parte dell'articolo è dedicata ad un'analisi regionale per cercare di capire come siano cambiate le immatricolazioni di fuorisede e non tra il 2019 e 2020 con la pandemia. 


## Importanza dei Dati Open 

Per effettuare le nostra analisi abbiamo dovuto ricercare e soprattutto ripulire e organizzare (con molta difficoltà) tutti i dati di cui potevamo
aver bisogno. Dato che nonostante questi dati siano in teoria *open* nella pratica il loro utilizzo non sembra essere destinato a chi non ha adeguati strumenti per ripulire i dati. Di conseguenza abbiamo caricato nella cartella **data** tutti i dataset creati (anche se non utilizzati), e le fonti dal quale sono stati presi. 


## Descrizione dei dataset 

### Dati Analisi 

- **df_emigrazioni**: dataset con emigrazioni/immigrazioni e variazioni per il 2019/20 e 2020/21
- **df_fuorisede_miur**: dataset del miur, ripulito con le immatricolazioni per ateneo e regione di provenienza dal 2010 al 2021.
- **df_model**: contiene varie serie storiche tra cui quella degli iscritti, immatricolati, diplomati, laureati, popolazione tra i 14-19 anni e popolazione tra 19-25 anni. Il dataset è stato ricavato unendo le serie storiche dell'istat disponibili nella cartella **istat_datasets**, con i dati disponibili sul sito del Miur. 

### Miur Datasets
Contiene tre dataset in cui sono state aggregate tutte le informazioni disponibili sul sito del Miur: 

- df_atenei_miur: contiene tutti i dati riguardanti gli atenei, iscritti, iscritti al 1anno, immatricolati, laureati...
- df_atenei_classe: contiene tutti i dati disponibili aggregati per ateneo e classe di laurea, sono disponibili solo i dati sulle immatricolazioni e sulle iscrizioni e sulle lauree, ma sono stati ricavati e aggiunti, non essendo disponibili in questo formato.
- df_atenei_laurea: pochi e mancanti dati a livello di ateneo, classe e laurea, disponibili solo iscritti , iscritti al 1 anno e laureati, non per tutte le lauree e con molti dati mancanti. 


