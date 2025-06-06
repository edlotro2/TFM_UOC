# Preparació *clustering*

En aquesta fase, una vegada ja es disposa dels diversos conjunts de
dades correctament tractats, l’objectiu és preparar les dades per poder
aplicar una tècnica d’aprenentatge automàtic. Així, el resultat d’aquest
pas serà un fitxer amb les variacions ponderades de les diferents
variables analitzades, respecte dels diversos anys i barris. així,
posteriorment, es podrà observar com afecta la gentrificació als barris
de la ciutat de Barcelona.

En primer lloc, el que s’ha de portar a terme, en cas que siga
necessari, és un filtrat i una senzilla adaptació dels conjunts de
dades, per així facilitar el posterior anàlisi.

### Instal·lació de llibreries

El primer que s’ha de fer és anar instal·lant i carregant les llibreries
necessàries per a poder executar el codi a continuació.

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

    if (!require(lubridate)) install.packages("lubridate"); library(lubridate)

    ## Cargando paquete requerido: lubridate

    ## 
    ## Adjuntando el paquete: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    if (!require(stringr)) install.packages("stringr"); library(stringr)

    ## Cargando paquete requerido: stringr

    if (!require(here)) install.packages("here"); library(here)

    ## Cargando paquete requerido: here

    ## here() starts at C:/Users/edlopez/OneDrive - FERMAX ELECTRONICA S.A.U/MÁSTER CD/2B/TRABAJO FIN DE MÁSTER/ENTREGUES/TFM M3/DATASET

    if (!require(tidyr)) install.packages("tidyr"); library(tidyr)

    ## Cargando paquete requerido: tidyr

    if (!require(readr)) install.packages("readr"); library(readr)

    ## Cargando paquete requerido: readr

    if (!require(purrr)) install.packages("purrr"); library(purrr)

    ## Cargando paquete requerido: purrr

### Filtrat definitiu

Ara, per a cada una de les variables, en cas de què siga necessari, es
filtrarà la informació, per centrar el focus en analitzar determinats
grups, ja que poden ser els que més afecten la gentrificació. En
cadascuna de les variables es va a justificar el motiu. A més, com que
es va a reduir el nombre de registres per cada any i barri, va a
resultar interessant calcular també el pes de la població seleccionada
respecte del total de població de cada segment, per veure si té o no
importància i resulta significativa. Per tant,

#### Edat

En primer lloc, es pot carregar el fitxer per veure l’estructura:

    ## Càrrega del conjunt de dades
    path = 'V2/Poblacio_Edat.csv'
    edat <- read.csv(path, row.names = NULL)

    ## Estructura del conjunt de dades
    str(edat)

    ## 'data.frame':    16352 obs. of  6 variables:
    ##  $ Data_Referencia: int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ Codi_Ciutat    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Districte : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Barri     : int  1 1 1 1 1 1 1 1 2 2 ...
    ##  $ Edat           : chr  "0-9 anys" "10-19 anys" "20-29 anys" "30-39 anys" ...
    ##  $ Valor          : int  2280 2716 4603 4717 3726 3763 5092 6786 749 933 ...

Com es pot observar, sembla que el format del codi del barri no és
correcte. El del districte tampo, però com ja no es va a utilitzar, no
es considera:

    ## Canvi al codi de barri
    edat$Codi_Barri <- sprintf("%02d", edat$Codi_Barri)

Com es va comentar a l’estat de l’art del projecte, la gentrificació és
un fenomen que afecta a la gent jove en gran mesura, ja que pressiona el
mercat immobiliari i quasi prohibeix l’accés a l’habitatge per a aquest
col·lectiu. Per tant, en aquest mètode es va a treballar amb la població
entre 20 i 39 anys. A més, abans de prescindir de la resta de grups de
població, es va a calcular el pes per any i barri de la població jove
respecte del total:

    ## Selecció població jove
    edat <- edat %>%
      filter(Data_Referencia >= 2015, Data_Referencia <= 2023) %>%
      group_by(Data_Referencia, Codi_Barri) %>%
      mutate(Pobl_Total = sum(Valor, na.rm = T)) %>%
      filter(Edat %in% c("20-29 anys", "30-39 anys")) %>%
      mutate(Pobl_Jove = sum(Valor, na.rm = T),
             ## Càlcul percentatge població jove
             Perc_Jove = Pobl_Jove/Pobl_Total) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      subset(select = c(Data_Referencia, Codi_Barri, Pobl_Jove, Perc_Jove))

    str(edat)

    ## tibble [657 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ Data_Referencia: int [1:657] 2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr [1:657] "01" "02" "03" "04" ...
    ##  $ Pobl_Jove      : int [1:657] 18563 6633 5602 9061 9327 14880 12554 12639 16905 11303 ...
    ##  $ Perc_Jove      : num [1:657] 0.397 0.435 0.381 0.409 0.3 ...

#### Nacionalitat

De manera molt semblant a la població per edat, en primer lloc es revisa
l’estructura:

    ## Càrrega del conjunt de dades
    path = 'V2/Poblacio_Nacionalitat.csv'
    regio <- read.csv(path, row.names = NULL)

    ## Canvi al codi de barri
    regio$Codi_Barri <- sprintf("%02d", regio$Codi_Barri)

    ## Estructura del conjunt de dades
    str(regio)

    ## 'data.frame':    14680 obs. of  6 variables:
    ##  $ Data_Referencia: int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ Codi_Ciutat    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Districte : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Barri     : chr  "01" "01" "01" "01" ...
    ##  $ Nacionalitat   : chr  "Altres Europa" "Amèrica Llatina" "Amèrica del Nord" "Europa Occidental" ...
    ##  $ Valor          : int  31236 439 5 38 0 0 783 1227 12756 152 ...

En aquest mètode es va a treballar amb la tota la població, encara que
es va a excloure algunes regions com poden ser l’Europa de l’Est o
regions africanes i sud-americanes, amb nivells de renda més baixos que
els d’Espanya, fet pel qual no semblen necessàries a l’anàlisi. Cal
mencionar que aquest filtratge és totalment personal:

    ## Selecció regions amb capital
    regio <- regio %>%
      filter(Data_Referencia >= 2015, Data_Referencia <= 2023) %>%
      group_by(Data_Referencia, Codi_Barri) %>%
      mutate(Pobl_Total = sum(Valor, na.rm = T)) %>%
      filter(Nacionalitat %in% c("Amèrica del Nord", "Europa Occidental", "Oceania", "Àsia")) %>%
      mutate(Pobl_Regio = sum(Valor, na.rm = T),
             ## Càlcul percentatge població regions triades
             Perc_Regio = Pobl_Regio/Pobl_Total) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      subset(select = c(Data_Referencia, Codi_Barri, Pobl_Regio, Perc_Regio))

    str(regio)

    ## tibble [657 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ Data_Referencia: int [1:657] 2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr [1:657] "01" "02" "03" "04" ...
    ##  $ Pobl_Regio     : int [1:657] 14824 2626 1288 3085 2199 2368 2984 2523 2843 3015 ...
    ##  $ Perc_Regio     : num [1:657] 0.3166 0.1708 0.087 0.1386 0.0707 ...

#### Nivell d’estudis

De manera molt semblant a la població per edat, en primer lloc es revisa
l’estructura:

    ## Càrrega del conjunt de dades 
    path = 'V2/Poblacio_Educacio.csv' 
    estudis <- read.csv(path, row.names = NULL)  

    ## Canvi al codi de barri 
    estudis$Codi_Barri <- sprintf("%02d", estudis$Codi_Barri)

    ##Neteja camp estudis
    estudis <- estudis %>%
      mutate(Educacio = str_replace_all(Educacio, "\u00A0", " "),
             Educacio = str_replace_all(Educacio, "Â", ""),  
             Educacio = str_trim(Educacio)) 

    ## Estructura del conjunt de dades 
    str(estudis)

    ## 'data.frame':    12263 obs. of  6 variables:
    ##  $ Data_Referencia: int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ Codi_Ciutat    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Districte : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Barri     : chr  "01" "01" "01" "01" ...
    ##  $ Educacio       : chr  "Batxillerat elemental, graduat escolar, ESO, FPI" "Batxillerat superior, BUP, COU, FPII, CFGM grau mitjà" "Estudis primaris, certificat d'escolaritat, EGB" "Estudis universitaris, CFGS grau superior" ...
    ##  $ Valor          : int  4544 3933 11006 2134 912 7808 1857 2251 3768 1805 ...

Sovint, les rendes més altes es relacionen amb nivells formatius més
alts. Per tant, es considera que només s’ha de tenir en compte per al
present estudi aquella població amb un estudis superiors com poden ser
els universitaris o els graus superiors:

    ## Selecció població amb estudis superiors
    estudis <- estudis %>%
      filter(Data_Referencia >= 2015, Data_Referencia <= 2023) %>%
      group_by(Data_Referencia, Codi_Barri) %>%
      mutate(Pobl_Total = sum(Valor, na.rm = T)) %>%
      filter(Educacio == "Estudis universitaris, CFGS grau superior") %>%
      mutate(Pobl_Superior = sum(Valor, na.rm = T),
             ## Càlcul percentatge població estudis superiors
             Perc_Superior = Pobl_Superior/Pobl_Total) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      subset(select = c(Data_Referencia, Codi_Barri, Pobl_Superior, Perc_Superior))

    str(estudis)

    ## tibble [657 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ Data_Referencia: int [1:657] 2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr [1:657] "01" "02" "03" "04" ...
    ##  $ Pobl_Superior  : int [1:657] 8727 5178 3230 7275 9911 15295 18334 16315 19914 11157 ...
    ##  $ Perc_Superior  : num [1:657] 0.214 0.367 0.237 0.362 0.359 ...

#### Composició de la llar

De manera molt semblant a la població per edat, en primer lloc es revisa
l’estructura:

    ## Càrrega del conjunt de dades 
    path = 'V2/Poblacio_Domicili.csv' 
    domicili <- read.csv(path, row.names = NULL)  

    ## Canvi al codi de barri 
    domicili$Codi_Barri <- sprintf("%02d", domicili$Codi_Barri)

    ## Estructura del conjunt de dades 
    str(domicili)

    ## 'data.frame':    24516 obs. of  6 variables:
    ##  $ Data_Referencia: int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
    ##  $ Codi_Ciutat    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Districte : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Codi_Barri     : chr  "01" "01" "01" "01" ...
    ##  $ Domicili       : chr  "Altres domicilis amb una o més persones menors de 18 anys" "Dues persones de 18 a 64 anys" "Dues persones de 65 anys i més" "Dues persones o més totes majors de 18 anys" ...
    ##  $ Valor          : int  1140 1590 1384 2498 42 257 1167 1321 1397 628 ...

En aquest cas, la població que es va a seleccionar és aquella que viu
soles i té entre 18 i 64 anys, o viu amb altra persona que també es
troba entre aquestes edats:

    ## Selecció estructura de la llar
    domicili <- domicili %>%
      filter(Data_Referencia >= 2015, Data_Referencia <= 2023) %>%
      group_by(Data_Referencia, Codi_Barri) %>%
      mutate(Pobl_Total = sum(Valor, na.rm = T)) %>%
      filter(Domicili %in% c("Una dona de 18 a 64 anys", "Un home de 18 a 64 anys", "Dues persones de 18 a 64 anys")) %>%
      mutate(Pobl_Host = sum(Valor, na.rm = T),
             ## Càlcul percentatge estructures de llar
             Perc_Host = Pobl_Host/Pobl_Total) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      subset(select = c(Data_Referencia, Codi_Barri, Pobl_Host, Perc_Host))

    str(domicili)

    ## tibble [657 × 4] (S3: tbl_df/tbl/data.frame)
    ##  $ Data_Referencia: int [1:657] 2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr [1:657] "01" "02" "03" "04" ...
    ##  $ Pobl_Host      : int [1:657] 6231 2986 2792 4531 3785 6788 6244 6130 7520 5186 ...
    ##  $ Perc_Host      : num [1:657] 0.377 0.465 0.414 0.46 0.299 ...

#### Renda

De manera molt semblant a la població per edat, en primer lloc es revisa
l’estructura. Recordem que només es disposen de dades des de 2015
d’aquesta variable.

    ## Càrrega del conjunt de dades 
    path = 'V2/Poblacio_Renda.csv' 
    renda <- read.csv(path, row.names = NULL)  

    ## Canvi al codi de barri 
    renda$Codi_Barri <- sprintf("%02d", renda$Codi_Barri)

    ## Estructura del conjunt de dades 
    str(renda)

    ## 'data.frame':    730 obs. of  6 variables:
    ##  $ Data_Referencia: int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Districte : int  1 1 1 1 2 2 2 2 2 2 ...
    ##  $ Codi_Barri     : chr  "01" "02" "03" "04" ...
    ##  $ Poblacio       : int  46767 15232 14707 22145 31138 50442 42910 41170 56759 37618 ...
    ##  $ Import_Total   : int  389026832 172166832 160012195 255313096 482917891 774627715 844964233 747526527 964129029 568620349 ...
    ##  $ Renda_Mitjana  : num  8318 11303 10880 11529 15509 ...

En aquest cas nomé serà necessària la informació de la renda mitjana:

    ## Selecció camps
    renda <- renda %>%
      filter(Data_Referencia >= 2015, Data_Referencia <= 2023) %>%
      subset(select = -c(Poblacio, Import_Total, Codi_Districte))
    str(renda)

    ## 'data.frame':    657 obs. of  3 variables:
    ##  $ Data_Referencia: int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr  "01" "02" "03" "04" ...
    ##  $ Renda_Mitjana  : num  8318 11303 10880 11529 15509 ...

#### Turisme

De manera molt semblant a la població per edat, en primer lloc es revisa
l’estructura. Recordem que només es disposen de dades des de 2015
d’aquesta variable.

    ## Càrrega del conjunt de dades 
    path = 'V2/Poblacio_Turisme.csv' 
    turisme <- read.csv(path, row.names = NULL)  

    ## Canvi al codi de barri 
    turisme$Codi_Barri <- sprintf("%02d", turisme$Codi_Barri)

    turisme <- turisme %>%
      filter(Data_Referencia >= 2015, Data_Referencia <= 2023) %>%
      rename(Habitatges = Valor) %>%
      subset(select = c(Data_Referencia, Codi_Barri, Habitatges))

    ## Estructura del conjunt de dades 
    str(turisme)

    ## 'data.frame':    585 obs. of  3 variables:
    ##  $ Data_Referencia: int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr  "01" "02" "03" "04" ...
    ##  $ Habitatges     : int  180 184 69 171 343 794 1747 727 428 568 ...

Així, no serà necessària cap adaptació, ja que es tracta de xifres que
no han de calcular-se com percentatge.

#### Preus del lloguer

De manera molt semblant a la població per edat, en primer lloc es revisa
l’estructura. Recordem que només es disposen de dades des de 2015
d’aquesta variable.

    ## Càrrega del conjunt de dades 
    path = 'V2/Poblacio_Lloguers.csv' 
    lloguer <- read.csv(path, row.names = NULL) 

    ## Estructura del conjunt de dades 
    str(lloguer)

    ## 'data.frame':    1066 obs. of  6 variables:
    ##  $ Codi_Ciutat    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Data_Referencia: int  2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 ...
    ##  $ Variable       : chr  "Preu_Lloguer" "Preu_Lloguer" "Preu_Lloguer" "Preu_Lloguer" ...
    ##  $ Valor          : num  6 6.87 7.62 8 8.68 ...
    ##  $ Codi_Districte : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Codi_Barri     : int  NA NA NA NA NA NA NA NA NA NA ...

Com que el format de les dades no és el més adequat per a l’explotació,
es modifica lleugerament:

    ## Canvi format
    lloguer <- lloguer %>%
      filter(Data_Referencia >= 2015, Data_Referencia <= 2023) %>%
      pivot_wider(names_from =  Variable, values_from = Valor) %>%
      filter(!is.na(Codi_Barri))

    ## Canvi al codi de barri 
    lloguer$Codi_Barri <- sprintf("%02d", lloguer$Codi_Barri)

    lloguer <- lloguer %>%
      subset(select = -c(Codi_Ciutat, Codi_Districte))

    str(lloguer)

    ## tibble [657 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Data_Referencia: int [1:657] 2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr [1:657] "11" "01" "02" "03" ...
    ##  $ Preu_Lloguer   : num [1:657] 11.1 11.1 11.2 16.3 12.7 ...

### Creació arxiu definitiu

Finalment, una vegada ja es disposa de tots els conjunts de dades
adequats tant al format com a l’àmbit temporal, és a dir, amb les dades
entre l’any 2015 i el 2023, ja es pot generar un arxiu definitiu, amb el
que poder portar a terme diferents tècniques de *clustering.*

D’altra banda, cal mencionar que, malgrat haver fet un tractament previ
de l’Índex de Gini, s’ha descartat de l’anàlisi per la complexitat a
l’hora de calcular les dades faltants i s’ha decidit prescindir d’ell en
la explotació posterior.

    ## Creació dataframe unificat 2015-23
    llista_df <- list(edat, regio, estudis, domicili, renda, turisme, lloguer) 
    df_global <- reduce(llista_df, full_join, by = c("Data_Referencia", "Codi_Barri"))
    ## Emmagatzematge dataset
    write_csv(df_global, file = file.path("V2/", "Poblacio_Clustering.csv"))
