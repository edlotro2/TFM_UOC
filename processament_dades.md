# Processament de dades

En aquesta primera fase, es portaran a terme diferents aspectes del
tractament de les dades.

### Unificació dels conjunts de dades

Alguns dels conjunts de dades que s’han anat descarregant del portals de
dades obertes tenen la informació desglossada per anys, fet que
incrementa el nombre d’arxius amb els que treballar. Per tant,
l’objectiu fonamental d’aquesta secció serà reduir el nombre de fitxers
a un per cada una de les dimensions amb les quals es treballarà al llarg
del projecte.

En primer lloc, s’han de llistar els directoris que s’han creat per
descarregar la informació prèviament:

    ## Llistar directoris
    dirs <- c("atles-renda-index-gini", "pad_dom_mdbas_tipus-domicili", "pad_mdbas_edat-1", "pad_mdbas_nacionalitat-regio_sexe", "pad_mdbas_niv-educa-esta_sexe", "renda-tributaria-per-persona-atlas-distribucio")

A continuació, com que el nombre d’arxius per dimensió és elevat i la
diferència d’anys entre el primer arxiu i l’ultim és gran, s’ha de
considerar la possibilitat de què el nombre de camps de cada arxiu no
siga igual que la resta. Per tant, s’ha de comprovar que els camps
coincideixen:

    ## Funció comprovació camps
    comprovar_camps <- function(directori){
      arxius <- list.files(path = directori, pattern = "*.csv", full.names = TRUE)
      camps <- lapply(arxius, function(arxiu) {
        colnames(read.csv(arxiu, n_max = 1))
      })
      camps_iguals <- all(sapply(camps, function(camp) {
        identical(camp, camps[[1]])
      }))
      return(camps_iguals)
    }

Una vegada ja s’ha creat la funció, és el moment de comprovar que els
camps són iguals en tots els arxius d’una mateixa dimensió a analitzar:

    ## Comprovació camps
    comprovacio <- sapply(dirs, comprovar_camps)
    names(comprovacio) <- dirs
    print(comprovacio)

    ##                         atles-renda-index-gini 
    ##                                           TRUE 
    ##                   pad_dom_mdbas_tipus-domicili 
    ##                                           TRUE 
    ##                               pad_mdbas_edat-1 
    ##                                           TRUE 
    ##              pad_mdbas_nacionalitat-regio_sexe 
    ##                                           TRUE 
    ##                  pad_mdbas_niv-educa-esta_sexe 
    ##                                           TRUE 
    ## renda-tributaria-per-persona-atlas-distribucio 
    ##                                           TRUE

Com es pot comprovar els camps coincideixen en tots els arxius de cada
una de les dimensions. Es tracta d’un pas fonamental i el resultat
positiu de la comprovació permet avançar. Seguidament, és moment de
portar a terme la creació de la funció per la unificació dels arxius,
emmagatzemant-los al nou directori de nom *V1:*

    ## Instal·lació llibreries
    if (!require(dplyr)) install.packages("dplyr"); library(dplyr)

    ## Cargando paquete requerido: dplyr

    ## 
    ## Adjuntando el paquete: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    if (!require(readr)) install.packages("readr"); library(readr)

    ## Cargando paquete requerido: readr

    if (!require(here)) install.packages("here"); library(here)

    ## Cargando paquete requerido: here

    ## here() starts at C:/Users/edlopez/OneDrive - FERMAX ELECTRONICA S.A.U/MÁSTER CD/2B/TRABAJO FIN DE MÁSTER/ENTREGUES/TFM M3/DATASET

    ## Funció unificació arxius
    unificar_arxius_per_subdirectori <- function(dir_origen, dir_destinacio) {
      ruta_origen <- file.path(here(), dir_origen)
      ruta_destinacio <- file.path(here(), dir_destinacio)
      
      if (!dir.exists(ruta_destinacio)) {
        dir.create(ruta_destinacio)
      }
      
      for (subdir in dirs) {
        arxius <- list.files(subdir, pattern = "\\.csv$", full.names = TRUE)
        if (length(arxius) > 0) {
          data_unificada <- do.call(rbind, lapply(arxius, read.csv))
          nom_subdir <- basename(subdir)
          arxiu_unificat <- file.path(ruta_destinacio, paste0(nom_subdir, "_unificat.csv"))
          write.csv(data_unificada, arxiu_unificat, row.names = FALSE)
          cat("Arxiu unificat creat", "\n")
        } else {
          cat("No s'han trobat més arxius CSV", "\n")
        }
      }
    }

Una vegada ja s’ha creat la funció, és el moment d’unificar els
directoris:

    ## Unificació directoris
    unificar_arxius_per_subdirectori("V0", "V1")

    ## No s'han trobat més arxius CSV 
    ## No s'han trobat més arxius CSV 
    ## No s'han trobat més arxius CSV 
    ## No s'han trobat més arxius CSV 
    ## No s'han trobat més arxius CSV 
    ## No s'han trobat més arxius CSV

### Tractament de les dades

El següent pas consisteix en portar a terme un primer estudi dels
diferents arxius generats en la unificació de la darrera secció, a més
d’estudiar la resta de conjunts que no s’han utilitzat abans, perque ja
estaven unificats. Així, es poden proposar les mesures necessàries per
tractar les dades de manera que es puguen explotar en els següents
passos.

#### Edat

El primer pas que es va a relitzar per analitzar la dimensió *Edat* és
veure l’estructura de l’arxiu CSV que conté la informació:

    ## Càrrega del conjunt de dades
    path = 'V1/pad_mdbas_edat-1_unificat.csv'
    edat <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(edat)

    ## 'data.frame':    2884833 obs. of  9 variables:
    ##  $ Data_Referencia: chr  "1997-01-01" "1997-01-01" "1997-01-01" "1997-01-01" ...
    ##  $ Codi_Districte : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Districte  : chr  "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" ...
    ##  $ Codi_Barri     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Barri      : chr  "el Raval" "el Raval" "el Raval" "el Raval" ...
    ##  $ AEB            : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Seccio_Censal  : int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...
    ##  $ Valor          : chr  "8" ".." "7" "5" ...
    ##  $ EDAT_1         : int  0 1 2 3 4 5 6 7 8 9 ...

Com es pot observar a l’estructura, el conjunt de dades que representa
la població per edat està format per 2.884.833 registres amb 9 camps.

D’aquesta primera visió es poden veure aspectes que van a tenir que
corregir-se. En primer lloc, és necessari modificar el camps
*Codi\_Districte* i *Codi\_Barri*, ja que aquells valors menors a 10 han
de contenir un 0 davant, per poder creuar-los posteriorment amb el
*dataset* que conté la informació geogràfica. Per tant, també s’haurà de
canviar el tipus de camp.

    ## Canvi als codis de districte i barri
    edat$Codi_Barri <- sprintf("%02d", edat$Codi_Barri)
    edat$Codi_Districte <- sprintf("%02d", edat$Codi_Districte)

Seguidament, també resulta necessari extraure l’any de cada observació,
ja que les xifres són anuals i facilita el posterior anàlisi:

    ## Instal·lació llibreria lubridate
    if (!require(lubridate)) install.packages("lubridate"); library(lubridate)

    ## Cargando paquete requerido: lubridate

    ## 
    ## Adjuntando el paquete: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## Extracció de l'any de l'observació
    edat <- edat %>%
      mutate(Data_Referencia = year(Data_Referencia))

D’altra banda, com ja es va comentar en la descripció dels conjunts de
dades, quan el nombre d’observacions a menor a 5, el camp *Valor* fa ús
de “..”. Per tant, s’ha de substituir aquest cas per el valor *NA:*

    ## Substitució .. per NA
    edat <- edat %>%
      mutate(Valor = as.numeric(na_if(Valor, "..")))

Un últim aspecte que, a l’igual que els que s’acaben de tractar, també
va a sorgir a la resta de *datasets* és el fet de què no existeixen
totes les combinacions entre el codi de barri i l’atribut que defineix
la variable (*EDAT\_1)*. Per tant, resulta necessari la creació d’una
taula que continga totes les possibles combinacions, assignant un valor
*NA* a aquelles combinacions que fins el moment no existien:

    ## Instal·lació llibreria tidyr
    if (!require(tidyr)) install.packages("tidyr"); library(tidyr)

    ## Cargando paquete requerido: tidyr

    ## Generació de totes les combinacions
    combinacions_edat <- expand.grid(
      Data_Referencia = unique(edat$Data_Referencia),
      Codi_Districte = unique(edat$Codi_Districte),
      Codi_Barri = unique(edat$Codi_Barri),
      EDAT_1 = unique(edat$EDAT_1),
      stringsAsFactors = FALSE
    ) %>%
      right_join(
        nesting(Codi_Districte = edat$Codi_Districte,
                Codi_Barri = edat$Codi_Barri),
        by = c("Codi_Districte", "Codi_Barri")
      ) %>%
      arrange(Data_Referencia, Codi_Districte, Codi_Barri, EDAT_1)

    full_edat <- edat %>%
      complete(combinacions_edat, fill = list(Valor = NA))
    rm(combinacions_edat)
    write_csv(full_edat, file = file.path("V2/", "edat_complet.csv"))

Per últim, resulta interessant també la generació d’un nou camp per
categoritzar la variable *EDAT\_1.* L’objectiu fonamental és fer més
senzill el posterior anàlisi de la informació relativa a l’edat, ja que
es van a crear 8 grups, en funció de l’edat, de 10 en 10 anys:

    ## Instal·lació llibreria readr
    if (!require(readr)) install.packages("readr"); library(readr)

    ## Creació nova variable derivada
    codis_edat <- data.frame(
      GRUPS = 1:8,
      Edat = c(
        "0-9 anys", "10-19 anys", "20-29 anys", "30-39 anys",
        "40-49 anys", "50-59 anys", "60-69 anys", ">70 anys"
      )
    )

    df_edat_agrupat <- edat %>%
      mutate(
        GRUPS = floor(EDAT_1 / 10) + 1,
        GRUPS = if_else(GRUPS >= 8, 8L, GRUPS),
        Codi_Ciutat = "01"
      ) %>%
      select(-EDAT_1) %>%
      left_join(codis_edat, by = "GRUPS") %>%
      group_by(Data_Referencia, Codi_Ciutat, Codi_Districte, Codi_Barri, Edat) %>%
      summarise(Valor = sum(as.numeric(Valor), na.rm = TRUE), .groups = "drop")

    ## Guardar arxiu
    write_csv(df_edat_agrupat, file = file.path("V2", "Poblacio_Edat.csv"))

    ## Estructura del conjunt de dades agrupat
    str(df_edat_agrupat)

    ## tibble [16,352 × 6] (S3: tbl_df/tbl/data.frame)
    ##  $ Data_Referencia: num [1:16352] 1997 1997 1997 1997 1997 ...
    ##  $ Codi_Ciutat    : chr [1:16352] "01" "01" "01" "01" ...
    ##  $ Codi_Districte : chr [1:16352] "01" "01" "01" "01" ...
    ##  $ Codi_Barri     : chr [1:16352] "01" "01" "01" "01" ...
    ##  $ Edat           : chr [1:16352] "0-9 anys" "10-19 anys" "20-29 anys" "30-39 anys" ...
    ##  $ Valor          : num [1:16352] 2280 2716 4603 4717 3726 ...

Així, es redueix la dimensió del conjunt de dades, ja que es poden
agrupar més senzillament pel camp Edat, afavorint així el tractament de
la informació. Aquestes transformacions també es portaran a la resta de
variables, quan siga necessari.

#### Nacionalitat

El tractament inicial d’aquest conjunt de dades va a ser molt similar al
que s’ha aplicat per a la població per edat. El primer pas és revisar
l’estructura interna del propi conjunt de dades:

    ## Càrrega del conjunt de dades
    path = 'V1/pad_mdbas_nacionalitat-regio_sexe_unificat.csv'
    origen <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(origen)

    ## 'data.frame':    638349 obs. of  10 variables:
    ##  $ Data_Referencia   : chr  "1997-01-01" "1997-01-01" "1997-01-01" "1997-01-01" ...
    ##  $ Codi_Districte    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Districte     : chr  "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" ...
    ##  $ Codi_Barri        : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Barri         : chr  "el Raval" "el Raval" "el Raval" "el Raval" ...
    ##  $ AEB               : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Seccio_Censal     : int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...
    ##  $ Valor             : chr  ".." "6" ".." "6" ...
    ##  $ NACIONALITAT_REGIO: int  3 3 6 8 8 9 12 12 13 13 ...
    ##  $ SEXE              : int  1 2 1 1 2 2 1 2 1 2 ...

Es pot observar una casuística similar a la del cas anterior, com pot
ser l’existència de valors “..” al camp *Valor* o la necessitat d’afegir
el valor 0 als camps *Codi\_Districte* i *Codi\_Barri*, per tal de poder
identificar les regions geogràfiques correctament. També s’ha d’extraure
l’any de la data de referència:

    ## Canvi als codis de districte i barri
    origen$Codi_Barri <- sprintf("%02d", origen$Codi_Barri)
    origen$Codi_Districte <- sprintf("%02d", origen$Codi_Districte)

    ## Extracció de l'any de l'observació
    origen <- origen %>%
      mutate(Data_Referencia = year(Data_Referencia))

    ## Substitució .. per NA
    origen <- origen %>%
      mutate(Valor = as.numeric(na_if(Valor, "..")))

Ara, resulta necessari portar a terme la combinació d’aquest conjunt de
dades amb el de les dimensions comunes, ja que el camp
*NACIONALITAT\_REGIO* està codificat, fet pel qual no es pot coneixer a
quina regió es correspon cada codi. Així, es necessari carregar la taula
de dimensions comunes i encreuar-la amb aquesta.

    ## Càrrega del conjunt de dades de dimensions comunes
    path = 'pad_dimensions.csv'
    dimensions <- read.csv(path, row.names = NULL)

    str(dimensions)

    ## 'data.frame':    1143 obs. of  6 variables:
    ##  $ Codi_Dimensio: int  1 1 2 2 2 2 2 2 2 2 ...
    ##  $ Desc_Dimensio: chr  "SEXE" "SEXE" "EDAT_1" "EDAT_1" ...
    ##  $ Codi_Valor   : int  1 2 0 1 2 3 4 5 6 7 ...
    ##  $ Desc_Valor_CA: chr  "Dona" "Home" "0 anys" "1 anys" ...
    ##  $ Desc_Valor_ES: chr  "Mujer" "Hombre" "0 años" "1 años" ...
    ##  $ Desc_Valor_EN: chr  "Female" "Male" "0 years" "1 years" ...

Com es pot observar, la taula dimensions té per cada una de les
dimensions, un identificador i la descripció de cadascun dels
identificadors. Així, s’haurà de creuar amb el *dataframe* origen:

    ## Filtre taula dimensions
    dim_nacionalitat <- dimensions %>%
      filter(Desc_Dimensio == "NACIONALITAT_REGIO") %>%
      select(Codi_Valor, Desc_Valor_CA)

    ## Substitució del codi per la descripció
    origen <- origen %>%
      left_join(dim_nacionalitat, by = c("NACIONALITAT_REGIO" = "Codi_Valor")) %>%
      rename(Nacionalitat = Desc_Valor_CA) %>%
      select(-NACIONALITAT_REGIO)

A l’igual que s’ha fet amb el conjunt de dades de la població per edat,
resulta interessant generar una taula amb totes les combinacions
possibles, per si fóra necessària més avant:

    ## Generació de totes les combinacions
    combinacions_origen <- expand.grid(
      Data_Referencia = unique(origen$Data_Referencia),
      Codi_Districte = unique(origen$Codi_Districte),
      Codi_Barri = unique(origen$Codi_Barri),
      SEXE = unique(origen$SEXE),
      Nacionalitat = unique(origen$Nacionalitat),
      stringsAsFactors = FALSE
    ) %>%
      right_join(
        nesting(Codi_Districte = origen$Codi_Districte,
                Codi_Barri = origen$Codi_Barri),
        by = c("Codi_Districte", "Codi_Barri")
      ) %>%
      arrange(Data_Referencia, Codi_Districte, Codi_Barri, Nacionalitat, SEXE)

    full_origen <- origen %>%
      complete(combinacions_origen, fill = list(Valor = NA))
    rm(combinacions_origen)
    write_csv(full_origen, file = file.path("V2/", "origen_complet.csv"))

Ara, hi ha un problema menor consistent en l’elevat nombre de
nacionalitats diferents definides a les dimensions. Per tant, es pot
realitzar una agrupació, per tal de reduir aquest nombre, simplificant
posteriors anàlisis:

    ## Canvi valors Nacionalitat
    origen <- origen %>%
      mutate(
        Nacionalitat = case_when(
          Nacionalitat %in% c(
            "Àfrica oriental", "Àfrica central", "Àfrica septentrional",
            "Àfrica meridional", "Àfrica occidental"
          ) ~ "Àfrica",
          Nacionalitat %in% c("Carib", "Amèrica central", "Amèrica del sud") ~ "Amèrica Llatina",
          Nacionalitat == "Amèrica del nord" ~ "Amèrica del Nord",
          Nacionalitat %in% c(
            "Àsia central", "Àsia oriental", "Àsia meridional",
            "Àsia sud-oriental", "Àsia occidental"
          ) ~ "Àsia",
          Nacionalitat == "Europa occidental" ~ "Europa Occidental",
          Nacionalitat %in% c("Europa oriental", "Europa septentrional", "Europa meridional") ~ "Altres Europa",
          Nacionalitat %in% c("Austràlia i Nova Zelanda", "Melanèsia", "Micronèsia", "Polinèsia") ~ "Oceania",
          Nacionalitat == "No consta" ~ "No consta",
          TRUE ~ "Altres"
        )
      )


    df_origen_agrupat <- origen %>%
      mutate(Codi_Ciutat = "01") %>%  
      group_by(Data_Referencia, Codi_Ciutat, Codi_Districte, Codi_Barri, Nacionalitat) %>%
      summarise(Valor = sum(as.numeric(Valor), na.rm = TRUE), .groups = "drop")

    ## Guardar l'arxiu
    write_csv(df_origen_agrupat, file = file.path("V2", "Poblacio_Nacionalitat.csv"))

#### Nivell d’estudis

Aquesta altra dimensió és molt similar a l’anterior, ja que també
s’haurà de fer la recerca del valor del codi de la dimensió a la taula
de dimensions. El primer pas és revisar l’estructura interna del propi
conjunt de dades:

    ## Càrrega del conjunt de dades
    path = 'V1/pad_mdbas_niv-educa-esta_sexe_unificat.csv'
    educacio <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(educacio)

    ## 'data.frame':    356477 obs. of  10 variables:
    ##  $ Data_Referencia: chr  "1997-01-01" "1997-01-01" "1997-01-01" "1997-01-01" ...
    ##  $ Codi_Districte : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Districte  : chr  "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" ...
    ##  $ Codi_Barri     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Barri      : chr  "el Raval" "el Raval" "el Raval" "el Raval" ...
    ##  $ AEB            : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Seccio_Censal  : int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...
    ##  $ Valor          : chr  "151" "80" "172" "165" ...
    ##  $ NIV_EDUCA_esta : int  1 1 2 2 3 3 4 4 5 5 ...
    ##  $ SEXE           : int  1 2 1 2 1 2 1 2 1 2 ...

Segueix amb les modificacions que s’han anat portant a terme fins ara:

    ## Canvi als codis de districte i barri
    educacio$Codi_Barri <- sprintf("%02d", educacio$Codi_Barri)
    educacio$Codi_Districte <- sprintf("%02d", educacio$Codi_Districte)

    ## Extracció de l'any de l'observació
    educacio <- educacio %>%
      mutate(Data_Referencia = year(Data_Referencia))

    ## Substitució .. per NA
    educacio <- educacio %>%
      mutate(Valor = as.numeric(na_if(Valor, "..")))

Aprofitant que ja està carregada la taula de dimensions, es pot
utilitzar per creuar-la amb la del nivell educatiu, per conèixer els
diferents nivells a que fan referència els respectius codis:

    ## Filtre taula dimensions
    dim_educacio <- dimensions %>%
      filter(Desc_Dimensio == "NIV_EDUCA_esta") %>%
      select(Codi_Valor, Desc_Valor_CA)

    ## Substitució del codi per la descripció
    educacio <- educacio %>%
      left_join(dim_educacio, by = c("NIV_EDUCA_esta" = "Codi_Valor")) %>%
      rename(Educacio = Desc_Valor_CA) %>%
      select(-NIV_EDUCA_esta)

    str(educacio)

    ## 'data.frame':    356477 obs. of  10 variables:
    ##  $ Data_Referencia: num  1997 1997 1997 1997 1997 ...
    ##  $ Codi_Districte : chr  "01" "01" "01" "01" ...
    ##  $ Nom_Districte  : chr  "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" ...
    ##  $ Codi_Barri     : chr  "01" "01" "01" "01" ...
    ##  $ Nom_Barri      : chr  "el Raval" "el Raval" "el Raval" "el Raval" ...
    ##  $ AEB            : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Seccio_Censal  : int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...
    ##  $ Valor          : num  151 80 172 165 67 96 66 85 35 44 ...
    ##  $ SEXE           : int  1 2 1 2 1 2 1 2 1 2 ...
    ##  $ Educacio       : chr  "Sense estudis" "Sense estudis" "Estudis primaris, certificat d'escolaritat, EGB" "Estudis primaris, certificat d'escolaritat, EGB" ...

A l’igual que s’ha fet amb els conjunts de dades anteriors, resulta
interessant generar una taula amb totes les combinacions possibles, per
si fóra necessària més avant:

    ## Generació de totes les combinacions
    combinacions_educacio <- expand.grid(
      Data_Referencia = unique(educacio$Data_Referencia),
      Codi_Districte = unique(educacio$Codi_Districte),
      Codi_Barri = unique(educacio$Codi_Barri),
      SEXE = unique(educacio$SEXE),
      Educacio = unique(educacio$Educacio),
      stringsAsFactors = FALSE
    ) %>%
      right_join(
        nesting(Codi_Districte = educacio$Codi_Districte,
                Codi_Barri = educacio$Codi_Barri),
        by = c("Codi_Districte", "Codi_Barri")
      ) %>%
      arrange(Data_Referencia, Codi_Districte, Codi_Barri, Educacio, SEXE)

    full_educacio <- educacio %>%
      complete(combinacions_educacio, fill = list(Valor = NA))
    rm(combinacions_educacio)
    write_csv(full_educacio, file = file.path("V2/", "educacio_complet.csv"))

També es pot realitzar una agrupació, per tal de reduir aquest nombre,
simplificant posteriors anàlisis:

    df_educacio_agrupat <- educacio %>%
      mutate(Codi_Ciutat = "01") %>%
      group_by(Data_Referencia, Codi_Ciutat, Codi_Districte, Codi_Barri, Educacio) %>%
      summarise(Valor = sum(as.numeric(Valor), na.rm = TRUE), .groups = "drop")

    ## Guardar l'arxiu
    write_csv(df_educacio_agrupat, file = file.path("V2", "Poblacio_Educacio.csv"))

#### Composició de la llar

Aquesta dimensió és molt similar a les anteriors, ja que també s’haurà
de fer la recerca del valor del codi de la dimensió a la taula de
dimensions. Primerament, es revisa l’estructura:

    ## Càrrega del conjunt de dades
    path = 'V1/pad_dom_mdbas_tipus-domicili_unificat.csv'
    domicili <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(domicili)

    ## 'data.frame':    355689 obs. of  9 variables:
    ##  $ Data_Referencia: chr  "1997-01-01" "1997-01-01" "1997-01-01" "1997-01-01" ...
    ##  $ Codi_Districte : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Districte  : chr  "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" ...
    ##  $ Codi_Barri     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Barri      : chr  "el Raval" "el Raval" "el Raval" "el Raval" ...
    ##  $ AEB            : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Seccio_Censal  : int  1001 1001 1001 1001 1001 1001 1001 1001 1001 1001 ...
    ##  $ Valor          : int  28 49 54 14 62 39 34 86 4 3 ...
    ##  $ TIPUS_DOMICILI : int  1 2 3 4 5 6 7 8 9 10 ...

Seguint amb les modificacions següents:

    ## Canvi als codis de districte i barri
    domicili$Codi_Barri <- sprintf("%02d", domicili$Codi_Barri)
    domicili$Codi_Districte <- sprintf("%02d", domicili$Codi_Districte)

    ## Extracció de l'any de l'observació
    domicili <- domicili %>%
      mutate(Data_Referencia = year(Data_Referencia))

Aprofitant que ja està carregada la taula de dimensions, es pot
utilitzar per creuar-la amb la de l’estructura de domicili, per conèixer
els diferents nivells a que fan referència els respectius codis:

    ## Filtre taula dimensions
    dim_domicili <- dimensions %>%
      filter(Desc_Dimensio == "TIPUS_DOMICILI") %>%
      select(Codi_Valor, Desc_Valor_CA)

    ## Substitució del codi per la descripció
    domicili <- domicili %>%
      left_join(dim_domicili, by = c("TIPUS_DOMICILI" = "Codi_Valor")) %>%
      rename(Domicili = Desc_Valor_CA) %>%
      select(-TIPUS_DOMICILI)

A l’igual que s’ha fet amb els conjunts de dades anteriors, resulta
interessant generar una taula amb totes les combinacions possibles, per
si fóra necessària més avant:

    ## Generació de totes les combinacions
    combinacions_domicili <- expand.grid(
      Data_Referencia = unique(domicili$Data_Referencia),
      Codi_Districte = unique(domicili$Codi_Districte),
      Codi_Barri = unique(domicili$Codi_Barri),
      Domicili = unique(domicili$Domicili),
      stringsAsFactors = FALSE
    ) %>%
      right_join(
        nesting(Codi_Districte = domicili$Codi_Districte,
                Codi_Barri = domicili$Codi_Barri),
        by = c("Codi_Districte", "Codi_Barri")
      ) %>%
      arrange(Data_Referencia, Codi_Districte, Codi_Barri, Domicili)

    full_domicili <- domicili %>%
      complete(combinacions_domicili, fill = list(Valor = NA))
    rm(combinacions_domicili)
    write_csv(full_domicili, file = file.path("V2/", "domicili_complet.csv"))

També es pot realitzar una agrupació, per tal de reduir aquest nombre,
simplificant posteriors anàlisis:

    df_domicili_agrupat <- domicili %>%
      mutate(Codi_Ciutat = "01") %>%
      group_by(Data_Referencia, Codi_Ciutat, Codi_Districte, Codi_Barri, Domicili) %>%
      summarise(Valor = sum(as.numeric(Valor), na.rm = TRUE), .groups = "drop")

    ## Guardar l'arxiu
    write_csv(df_domicili_agrupat, file = file.path("V2", "Poblacio_Domicili.csv"))

#### Renda

Primerament, es revisa l’estructura:

    ## Càrrega del conjunt de dades
    path = 'V1/renda-tributaria-per-persona-atlas-distribucio_unificat.csv'
    renda <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(renda)

    ## 'data.frame':    8544 obs. of  7 variables:
    ##  $ Any           : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Districte: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Districte : chr  "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" ...
    ##  $ Codi_Barri    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Barri     : chr  "el Raval" "el Raval" "el Raval" "el Raval" ...
    ##  $ Seccio_Censal : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Import_Euros  : int  9310 7496 7300 9346 7690 8697 7804 6345 8513 7013 ...

Seguint amb les modificacions següents:

    ## Canvi als codis de districte i barri
    renda$Codi_Barri <- sprintf("%02d", renda$Codi_Barri)
    renda$Codi_Districte <- sprintf("%02d", renda$Codi_Districte)

En aquest cas no cal cap modificació adicional. No obstant, només es
disposa de dades fins l’any 2022. Per tant, resulta interessant
recolzar-se en alguna tècnica d’interpolació per obtenir el valors
faltants dels anys 2023 i 2024. Encara que, primerament, s’ha de
calcular la secció censal que en aquest cas no està representada
correctament:

    ## Instal·lació llibreria stringr
    if (!require(stringr)) install.packages("stringr"); library(stringr)

    ## Cargando paquete requerido: stringr

    ## Correcció Seccio Censal
    renda <- renda %>%
      mutate(
        Codi_Districte = str_pad(as.character(Codi_Districte), 2, pad = "0"),
        Seccio_Censal = as.numeric(
          paste0(Codi_Districte, str_pad(as.character(Seccio_Censal), 3, pad = "0"))
        )
      )

    renda <- renda %>%
      rename(Data_Referencia = Any)

A l’igual que s’ha fet amb els conjunts de dades anteriors, resulta
interessant generar una taula amb totes les combinacions possibles, per
si fóra necessària més avant:

    ## Generació de totes les combinacions
    combinacions_renda <- expand.grid(
      Data_Referencia = c(unique(renda$Data_Referencia), 2023, 2024),
      Codi_Ciutat = "01",
      Codi_Districte = unique(renda$Codi_Districte),
      Codi_Barri = unique(renda$Codi_Barri),
      Seccio_Censal = unique(renda$Seccio_Censal),
      stringsAsFactors = FALSE
    ) %>%
      right_join(
        nesting(Codi_Districte = renda$Codi_Districte,
                Codi_Barri = renda$Codi_Barri,
                Seccio_Censal = renda$Seccio_Censal),
        by = c("Codi_Districte", "Codi_Barri", "Seccio_Censal")
      ) %>%
      arrange(Data_Referencia, Codi_Districte, Codi_Barri, Seccio_Censal)


    full_renda <- combinacions_renda %>%
      left_join(renda, by = c("Data_Referencia", "Codi_Districte", "Codi_Barri", "Seccio_Censal"))

    write_csv(full_renda, file = file.path("V2/", "renda_complet.csv"))

Ara, com que es coneix la informació de la renda per secció censal, però
no a nivell de barri i districte, és necessari conèixer el volum de
població a nivell de secció censal. Així, es podrà calcular de manera
aproximada l’import total de la renda per secció censal. Posteriorment,
ja es pot procedir a interpolar les xifres per a la renda per càpita
dels anys 2023 i 2024 de les diferents combinacions dels camps
disponibles:

    ## Càlcul població per secció censal
    pobl_seccio <- edat %>%
      group_by(Data_Referencia, Seccio_Censal) %>%
      summarise(Poblacio = sum(Valor, na.rm = TRUE), .groups = "drop")

    comb <- expand.grid(
      Data_Referencia = unique(pobl_seccio$Data_Referencia),
      Seccio_Censal = unique(pobl_seccio$Seccio_Censal)
    )

    pobl_seccio <- comb %>%
      left_join(pobl_seccio, by = c("Data_Referencia", "Seccio_Censal")) %>%
      arrange(Data_Referencia, Seccio_Censal)

    ## Instal·lació llibreria zoo
    options(timeout = 600)
    if (!require(zoo)) install.packages("zoo"); library(zoo)

    ## Cargando paquete requerido: zoo

    ## 
    ## Adjuntando el paquete: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Interpolació renda anys 2023 i 2024
    df_renda_agrupat <- full_renda %>%
      left_join(pobl_seccio %>% filter(Data_Referencia >= 2015), 
                by = c("Data_Referencia", "Seccio_Censal")) %>%
      mutate(
        Import_Euros = ifelse(Import_Euros == 0 & Data_Referencia %in% c(2023, 2024), NA, Import_Euros)
      ) %>%
      arrange(Seccio_Censal, Data_Referencia) %>% 
      group_by(Seccio_Censal) %>%
      mutate(
        Import_Euros = zoo::na.approx(Import_Euros, x = Data_Referencia, na.rm = FALSE, rule = 2)  
      ) %>%
      ungroup() %>%
      mutate(
        Import_Total = Import_Euros * Poblacio
      ) %>%
      group_by(Data_Referencia, Codi_Districte, Codi_Barri) %>%
      summarise(
        Poblacio = sum(Poblacio, na.rm = TRUE),
        Import_Total = sum(Import_Total, na.rm = TRUE),
        Renda_Mitjana = Import_Total / Poblacio,
        .groups = "drop"
      )

    ## Guardar fitxer resultant
    write_csv(df_renda_agrupat, file = file.path("V2/", "Població_Renda.csv"))

#### Index de Gini

Primerament, es revisa l’estructura:

    ## Càrrega del conjunt de dades
    path = 'V1/atles-renda-index-gini_unificat.csv'
    gini <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(gini)

    ## 'data.frame':    8544 obs. of  7 variables:
    ##  $ Any           : int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Districte: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Districte : chr  "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" "Ciutat Vella" ...
    ##  $ Codi_Barri    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Nom_Barri     : chr  "el Raval" "el Raval" "el Raval" "el Raval" ...
    ##  $ Seccio_Censal : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Index_Gini    : chr  "36.5" "40.4" "38.4" "40.4" ...

El cas de l’Índex de Gini és molt semblant a la variable *Renda,* que
s’acaba d’estudiar. En primer lloc, es porten a terme algunes
modificacions senzilles:

    ## Canvi als codis de districte i barri
    gini$Codi_Barri <- sprintf("%02d", gini$Codi_Barri)
    gini$Codi_Districte <- sprintf("%02d", gini$Codi_Districte)

    ## Substitució .. per NA
    gini <- gini %>%
      mutate(
        Index_Gini = ifelse(Index_Gini %in% c("-", "."), NA, Index_Gini),
        Index_Gini = as.numeric(Index_Gini)
      ) 

A més, s’ha de recalcular la secció censal, que no està amb el format
correcte:

    ## Instal·lació llibreria stringr
    if (!require(stringr)) install.packages("stringr"); library(stringr)

    ## Correcció Seccio Censal
    gini <- gini %>%
      mutate(
        Codi_Districte = str_pad(as.character(Codi_Districte), 2, pad = "0"),
        Seccio_Censal = as.numeric(
          paste0(Codi_Districte, str_pad(as.character(Seccio_Censal), 3, pad = "0"))
        )
      )

    gini <- gini %>%
      rename(Data_Referencia = Any)

    write_csv(gini, file = file.path("V2/", "Poblacio_Gini.csv"))

A l’igual que s’ha fet amb els conjunts de dades anteriors, resulta
interessant generar una taula amb totes les combinacions possibles, per
si fóra necessària més avant:

    ## Generació de totes les combinacions
    combinacions_2023_2024 <- expand.grid(
      Data_Referencia = c(2023, 2024),
      Codi_Ciutat = "01",  # Assumim que Codi_Ciutat és sempre "01"
      Codi_Districte = unique(gini$Codi_Districte),
      Codi_Barri = unique(gini$Codi_Barri),
      Seccio_Censal = unique(gini$Seccio_Censal),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        # Assignem l'Index_Gini a 0 per a 2023 i 2024
        Index_Gini = 0
      )

    # Fem un left_join per agafar els noms de districte i barri per als anys 2023 i 2024
    gini_2023_2024 <- combinacions_2023_2024 %>%
      left_join(gini %>% select(Data_Referencia, Codi_Districte, Codi_Barri, Nom_Districte, Nom_Barri), 
                by = c("Data_Referencia", "Codi_Districte", "Codi_Barri"))

    # Ara unim amb les dades existents de gini (per mantenir les files ja existents) i per no duplicar-les
    gini_complet <- bind_rows(
      gini,  # Dades existents
      gini_2023_2024  # Noves files per als anys 2023 i 2024
    )

    # Guardem el resultat
    write_csv(gini_complet, file = file.path("V2/", "gini_complet.csv"))

Seguidament, com que no es disposa de les dades per als anys 2023 i
2024, es pot portar a terme l’extrapolació:

    ## Afegir anys 2023 i 2024
    anys_a_afegir <- expand.grid(
      Data_Referencia = c(2023, 2024), 
      Codi_Districte = unique(gini$Codi_Districte), 
      Codi_Barri = unique(gini$Codi_Barri), 
      Seccio_Censal = unique(gini$Seccio_Censal), 
      stringsAsFactors = FALSE
    )
    gini_ampliat <- bind_rows(gini, anys_a_afegir)


    ## Extrapolació de l'índex de Gini per a 2023 i 2024
    gini_extrapolat <- gini_ampliat %>%
      mutate(
        Index_Gini_temp = ifelse(Data_Referencia %in% c(2023, 2024), NA, Index_Gini)
      ) %>%
      group_by(Seccio_Censal) %>%
      mutate(
        Index_Gini = zoo::na.approx(Index_Gini_temp, x = Data_Referencia, na.rm = FALSE)
      ) %>%
      ungroup()

    # Comprovació de les files amb valors interpolats
    head(gini_extrapolat)

    ## # A tibble: 6 × 8
    ##   Data_Referencia Codi_Districte Nom_Districte Codi_Barri Nom_Barri
    ##             <dbl> <chr>          <chr>         <chr>      <chr>    
    ## 1            2015 01             Ciutat Vella  01         el Raval 
    ## 2            2015 01             Ciutat Vella  01         el Raval 
    ## 3            2015 01             Ciutat Vella  01         el Raval 
    ## 4            2015 01             Ciutat Vella  01         el Raval 
    ## 5            2015 01             Ciutat Vella  01         el Raval 
    ## 6            2015 01             Ciutat Vella  01         el Raval 
    ## # ℹ 3 more variables: Seccio_Censal <dbl>, Index_Gini <dbl>,
    ## #   Index_Gini_temp <dbl>

    # Comprovació del resum per veure la distribució dels valors extrapolats
    summary(gini_extrapolat$Index_Gini)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   22.50   29.80   32.20   32.67   35.00   46.70 1559290

    # Comprovació dels NA per assegurar-nos que la interpolació es va aplicar correctament
    sum(is.na(gini_extrapolat$Index_Gini))

    ## [1] 1559290

    gini_2023_2024 <- gini_ampliat %>% filter(Data_Referencia %in% c(2023, 2024))

    # Veiem els primers casos
    str(gini_2023_2024)

    ## 'data.frame':    1559280 obs. of  7 variables:
    ##  $ Data_Referencia: num  2023 2024 2023 2024 2023 ...
    ##  $ Codi_Districte : chr  "01" "01" "02" "02" ...
    ##  $ Nom_Districte  : chr  NA NA NA NA ...
    ##  $ Codi_Barri     : chr  "01" "01" "01" "01" ...
    ##  $ Nom_Barri      : chr  NA NA NA NA ...
    ##  $ Seccio_Censal  : num  1001 1001 1001 1001 1001 ...
    ##  $ Index_Gini     : num  NA NA NA NA NA NA NA NA NA NA ...

#### Turisme

Primerament, es revisa l’estructura:

    ## Càrrega del conjunt de dades
    path = 'V1/nombre_habitatges_us_turistic_2.csv'
    turisme <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(turisme)

    ## 'data.frame':    65 obs. of  14 variables:
    ##  $ Codi_Districte: int  1 1 1 1 2 2 2 2 2 2 ...
    ##  $ Codi_Barri    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Nom_Barri     : chr  "el Raval" "el Barri Gòtic" "la Barceloneta" "Sant Pere, Santa Caterina i la Ribera" ...
    ##  $ X2014         : int  180 184 69 171 343 794 1747 727 428 568 ...
    ##  $ X2015         : int  180 184 69 171 343 794 1747 727 428 568 ...
    ##  $ X2016         : int  177 188 68 169 331 772 1653 709 418 590 ...
    ##  $ X2017         : int  177 188 68 169 331 772 1653 709 418 590 ...
    ##  $ X2018         : int  179 194 66 168 331 768 1664 708 413 587 ...
    ##  $ X2019         : int  191 192 65 159 332 768 1660 710 413 584 ...
    ##  $ X2020         : int  191 193 64 159 332 765 1662 709 413 586 ...
    ##  $ X2021         : int  189 193 64 159 330 755 1650 688 404 576 ...
    ##  $ X2022         : int  199 194 64 159 328 750 1631 670 520 554 ...
    ##  $ X2023         : int  212 201 64 171 333 750 1655 678 484 567 ...
    ##  $ X2024         : int  238 217 85 192 360 817 1741 718 506 607 ...

A primera vista, es pot observar que es tracta d’un conjunt de dades
diferent. Així, és moment d’adaptar-lo per treballar de manera més
senzilla:

    ## Millora de les columnes
    turisme <- turisme %>%
      pivot_longer(cols = c(`X2014`:`X2024`), names_to = "Data_Referencia", values_to = "Valor") %>%
      mutate(Codi_Districte = str_pad(as.character(Codi_Districte), 2, pad = "0", side = "left")) %>%
      mutate(Codi_Barri = str_pad(as.character(Codi_Barri), 2, pad = "0", side = "left")) %>%
      mutate(Codi_Ciutat = "01") %>%
      arrange(Data_Referencia, Codi_Districte, Codi_Barri)

    turisme$Data_Referencia <- sub("^X", "", turisme$Data_Referencia)

A l’igual que s’ha fet amb els conjunts de dades anteriors, resulta
interessant generar una taula amb totes les combinacions possibles, per
si fóra necessària més avant:

    ## Generació de totes les combinacions
    combinacions_turisme <- expand.grid(
      Data_Referencia = unique(turisme$Data_Referencia),
      Codi_Ciutat = "01",
      Codi_Districte = unique(turisme$Codi_Districte),
      Codi_Barri = unique(turisme$Codi_Barri)
    ) %>%
      right_join(
        nesting(Codi_Districte = turisme$Codi_Districte,
                Codi_Barri = turisme$Codi_Barri), 
        by = c("Codi_Districte", "Codi_Barri")
      ) %>%
      arrange(Data_Referencia, Codi_Districte, Codi_Barri)

    full_turisme <- complete(turisme, combinacions_turisme, fill = list(Valor = NA))

    write_csv(full_turisme, file = file.path("V2/", "turisme_complet.csv"))

Per últim, s’emmagatzema el dataset general:

    ## Emmagatzematge dataset
    write_csv(turisme, file = file.path("V2/", "Poblacio_Turisme.csv"))
