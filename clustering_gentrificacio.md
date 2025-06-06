# *Clustering* gentrificació a Barcelona

En aquesta fase, una vegada ja es disposa dels diversos conjunts de
dades correctament tractats, l’objectiu és aplicar mètodes no
supervisats d’aprenentatge automàtica, com pot ser el *clustering* per
*k-means.*

A mesura que s’avance, s’aniran explicat les diferents característiques
de les diferents tècniques aplicades.

### Instal·lació de llibreries

El primer que s’ha de fer és anar instal·lant i carregant les llibreries
necessàries per a poder executar el codi a continuació.

    ## Instal·lació llibreries
    if (!require(ModelMetrics)) install.packages("ModelMetrics"); library(ModelMetrics)

    ## Cargando paquete requerido: ModelMetrics

    ## 
    ## Adjuntando el paquete: 'ModelMetrics'

    ## The following object is masked from 'package:base':
    ## 
    ##     kappa

    if (!require(factoextra)) install.packages("factoextra"); library(factoextra)

    ## Cargando paquete requerido: factoextra

    ## Cargando paquete requerido: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

    if (!require(corrplot)) install.packages("corrplot"); library(corrplot)

    ## Cargando paquete requerido: corrplot

    ## corrplot 0.95 loaded

    if (!require(esquisse)) install.packages("esquisse"); library(esquisse)

    ## Cargando paquete requerido: esquisse

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

    if (!require(sf)) install.packages("sf"); library(sf)

    ## Cargando paquete requerido: sf

    ## Linking to GEOS 3.13.1, GDAL 3.10.2, PROJ 9.5.1; sf_use_s2() is TRUE

    if (!require(knitr)) install.packages("knitr"); library(knitr)

    ## Cargando paquete requerido: knitr

### Anàlisi de Components Principals

L’Anàlisi de Components Principals (PCA) és un tècnica de selecció de
característiques que consisteix en l’eliminació de variables
considerades com poc *importants*, encara que mantenint la part més
rellevant de les mateixes. L’aspecte més rellevant d’aquesta tècnica
resideix en què, una vegada ja s’ha aplicat, tots els nous camps que es
disposen seran independents entre sí. Així, es podrà eliminar el soroll
que s’haja pogut generar durant l’obtenció del conjunt de dades i el seu
preprocessament.

Per tant, fent ús d’aquesta tècnica es pot reduir el nombre de
dimensions a components principals que conserven la major part de la
informació general del conjunt de dades. S’aconsegueix transformant les
variables potencialment correlaciones en un conjunt més reduït de
variables, que es denominen components principals. Així, es possible que
gràcies a l’aplicació d’aquesta tècnica es puguen minimitzar problemes
com la multicolinealitat. Aquesta circumstància sorgeix quan dos o més
variables independents estan altament correlacionades entre sí, fet que
pot ser problemàtic per al model.

A més, malgrat de disposar de dades des de l’any 2015, es va a generar
el conjunt de clusters per als anys 2019, 2020, 2021, 2022 i 2023 de
manera independent.

    ## Càrrega del conjunt de dades
    path = 'V2/Poblacio_Clustering.csv'
    df_cluster <- read.csv(path, row.names = NULL)
    df_cluster$Codi_Barri <- sprintf("%02d", df_cluster$Codi_Barri)
    str(df_cluster)

    ## 'data.frame':    657 obs. of  13 variables:
    ##  $ Data_Referencia: int  2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...
    ##  $ Codi_Barri     : chr  "01" "02" "03" "04" ...
    ##  $ Pobl_Jove      : int  18563 6633 5602 9061 9327 14880 12554 12639 16905 11303 ...
    ##  $ Perc_Jove      : num  0.397 0.435 0.381 0.409 0.3 ...
    ##  $ Pobl_Regio     : int  14824 2626 1288 3085 2199 2368 2984 2523 2843 3015 ...
    ##  $ Perc_Regio     : num  0.3166 0.1708 0.087 0.1386 0.0707 ...
    ##  $ Pobl_Superior  : int  8727 5178 3230 7275 9911 15295 18334 16315 19914 11157 ...
    ##  $ Perc_Superior  : num  0.214 0.367 0.237 0.362 0.359 ...
    ##  $ Pobl_Host      : int  6231 2986 2792 4531 3785 6788 6244 6130 7520 5186 ...
    ##  $ Perc_Host      : num  0.377 0.465 0.414 0.46 0.299 ...
    ##  $ Renda_Mitjana  : num  8318 11303 10880 11529 15509 ...
    ##  $ Habitatges     : int  180 184 69 171 343 794 1747 727 428 568 ...
    ##  $ Preu_Lloguer   : num  11.1 11.2 16.3 12.7 10.9 ...

Com que l’Anàlisi de Components Principals es basa en la variança, les
variables amb valors grans (per exemple, *Renda\_Mitjana)* dominen
l’anàlisi, per escala, no per importància si no es normalitzen. Per
tant, per evitar la falta de detecció de patrons a altres variables,
resulta necessari normalitzar.

    ## Vector anys escollits
    anys <- 2019:2023
    df_anys_norm <- list()

    ## Normalització variables
    vars <- c("Pobl_Jove", "Perc_Jove", "Pobl_Regio", "Perc_Regio", 
              "Pobl_Superior", "Perc_Superior", "Pobl_Host", "Perc_Host", 
              "Renda_Mitjana", "Habitatges", "Preu_Lloguer")

    for (any in anys) {
      df_any <- df_cluster[df_cluster$Data_Referencia == any, ]
      var_norm <- df_any[, vars]
      df_norm <- scale(var_norm)
      df_norm <- df_norm[complete.cases(df_norm), ]
      df_anys_norm[[as.character(any)]] <- df_norm
    }

    str(df_anys_norm)

    ## List of 5
    ##  $ 2019: num [1:65, 1:11] 2.87367 0.64096 -0.00627 0.7294 0.80958 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:65] "293" "294" "295" "296" ...
    ##   .. ..$ : chr [1:11] "Pobl_Jove" "Perc_Jove" "Pobl_Regio" "Perc_Regio" ...
    ##  $ 2020: num [1:65, 1:11] 2.7779 1.0091 -0.0183 0.7214 0.8753 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:65] "366" "367" "368" "369" ...
    ##   .. ..$ : chr [1:11] "Pobl_Jove" "Perc_Jove" "Pobl_Regio" "Perc_Regio" ...
    ##  $ 2021: num [1:65, 1:11] 2.8119 1.167 -0.0115 0.7237 0.9017 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:65] "439" "440" "441" "442" ...
    ##   .. ..$ : chr [1:11] "Pobl_Jove" "Perc_Jove" "Pobl_Regio" "Perc_Regio" ...
    ##  $ 2022: num [1:65, 1:11] 2.5887 1.1636 -0.0882 0.7044 0.968 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:65] "512" "513" "514" "515" ...
    ##   .. ..$ : chr [1:11] "Pobl_Jove" "Perc_Jove" "Pobl_Regio" "Perc_Regio" ...
    ##  $ 2023: num [1:65, 1:11] 2.413 1.296 -0.125 0.628 1.048 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:65] "585" "586" "587" "588" ...
    ##   .. ..$ : chr [1:11] "Pobl_Jove" "Perc_Jove" "Pobl_Regio" "Perc_Regio" ...

Així, una vegada ja es disposa dels *datasets* normalitzats, ja es pot
aplicar l’Anàlisi de Components Principals:

    ## Anàlisi de Components Principals
    llista_pca <- lapply(df_anys_norm, prcomp)

    for (any in names(llista_pca)) {
      cat("### PCA per a l'any", any, "###\n")
      print(summary(llista_pca[[any]]))
      cat("\n---------------------------\n")
    }

    ## ### PCA per a l'any 2019 ###
    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.3088 1.6399 1.1152 0.83784 0.63773 0.39422 0.29159
    ## Proportion of Variance 0.4986 0.2515 0.1163 0.06566 0.03804 0.01454 0.00795
    ## Cumulative Proportion  0.4986 0.7501 0.8664 0.93210 0.97014 0.98468 0.99263
    ##                            PC8    PC9    PC10    PC11
    ## Standard deviation     0.18407 0.1499 0.13689 0.06053
    ## Proportion of Variance 0.00317 0.0021 0.00175 0.00034
    ## Cumulative Proportion  0.99580 0.9979 0.99966 1.00000
    ## 
    ## ---------------------------
    ## ### PCA per a l'any 2020 ###
    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.2963 1.6665 1.0926 0.82069 0.64706 0.42684 0.32245
    ## Proportion of Variance 0.4927 0.2595 0.1115 0.06293 0.03912 0.01702 0.00971
    ## Cumulative Proportion  0.4927 0.7522 0.8637 0.92666 0.96578 0.98280 0.99252
    ##                            PC8     PC9   PC10    PC11
    ## Standard deviation     0.16947 0.15622 0.1499 0.06720
    ## Proportion of Variance 0.00268 0.00228 0.0021 0.00042
    ## Cumulative Proportion  0.99520 0.99748 0.9996 1.00000
    ## 
    ## ---------------------------
    ## ### PCA per a l'any 2021 ###
    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.2923 1.6697 1.1047 0.81555 0.63738 0.44784 0.31811
    ## Proportion of Variance 0.4908 0.2604 0.1140 0.06212 0.03794 0.01873 0.00945
    ## Cumulative Proportion  0.4908 0.7511 0.8651 0.92725 0.96519 0.98392 0.99337
    ##                            PC8     PC9    PC10    PC11
    ## Standard deviation     0.16842 0.14565 0.12878 0.06909
    ## Proportion of Variance 0.00265 0.00198 0.00155 0.00045
    ## Cumulative Proportion  0.99602 0.99801 0.99955 1.00000
    ## 
    ## ---------------------------
    ## ### PCA per a l'any 2022 ###
    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.3281 1.6416 1.1170 0.80959 0.61611 0.40645 0.32477
    ## Proportion of Variance 0.5045 0.2509 0.1161 0.06102 0.03534 0.01538 0.00982
    ## Cumulative Proportion  0.5045 0.7554 0.8715 0.93255 0.96788 0.98326 0.99308
    ##                           PC8     PC9    PC10    PC11
    ## Standard deviation     0.1572 0.15308 0.14499 0.07190
    ## Proportion of Variance 0.0023 0.00218 0.00196 0.00048
    ## Cumulative Proportion  0.9954 0.99756 0.99952 1.00000
    ## 
    ## ---------------------------
    ## ### PCA per a l'any 2023 ###
    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
    ## Standard deviation     2.3118 1.6463 1.0930 0.81386 0.61717 0.41841 0.32534
    ## Proportion of Variance 0.5018 0.2545 0.1122 0.06219 0.03576 0.01644 0.00994
    ## Cumulative Proportion  0.5018 0.7563 0.8685 0.93064 0.96640 0.98284 0.99278
    ##                            PC8     PC9    PC10    PC11
    ## Standard deviation     0.16262 0.15445 0.14135 0.08147
    ## Proportion of Variance 0.00248 0.00224 0.00188 0.00062
    ## Cumulative Proportion  0.99526 0.99750 0.99938 1.00000
    ## 
    ## ---------------------------

La funció *summary* retorna la proporció de variança aplicada al conjunt
total per cada atribut. Així, es pot saber que la primera component
principal explica el 49,86% de la variança total del model de l’any
2019. A més, només amb les quatre primeres components principals ja
s’explica més del 93,2% del model, fet pel qual es pot sospitar que es
podria reduir la dimensió a aquest nombre de components.

També es pot mostrar a través d’un histograma el pes de cadascun dels
components sobre el conjunt total de les dades:

    ## Histograma del pes dels components
    llista_ev <- lapply(llista_pca, get_eig)
    for (any in names(llista_ev)) {
      print(llista_ev[[any]])
    }

    ##         eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1  5.330718973      49.86001986                    49.86002
    ## Dim.2  2.689199626      25.15299483                    75.01301
    ## Dim.3  1.243564583      11.63148069                    86.64450
    ## Dim.4  0.701982644       6.56588140                    93.21038
    ## Dim.5  0.406697484       3.80397930                    97.01436
    ## Dim.6  0.155409607       1.45359868                    98.46795
    ## Dim.7  0.085027311       0.79528923                    99.26324
    ## Dim.8  0.033881416       0.31690435                    99.58015
    ## Dim.9  0.022484345       0.21030369                    99.79045
    ## Dim.10 0.018740062       0.17528215                    99.96573
    ## Dim.11 0.003663484       0.03426581                   100.00000
    ##         eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1  5.273178024      49.26987881                    49.26988
    ## Dim.2  2.777188205      25.94862636                    75.21851
    ## Dim.3  1.193785019      11.15411673                    86.37262
    ## Dim.4  0.673534378       6.29316079                    92.66578
    ## Dim.5  0.418684740       3.91197610                    96.57776
    ## Dim.6  0.182193255       1.70232060                    98.28008
    ## Dim.7  0.103973393       0.97147422                    99.25155
    ## Dim.8  0.028720626       0.26835084                    99.51990
    ## Dim.9  0.024403380       0.22801270                    99.74792
    ## Dim.10 0.022463346       0.20988602                    99.95780
    ## Dim.11 0.004516176       0.04219684                   100.00000
    ##        eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1  5.25449899      49.07683628                    49.07684
    ## Dim.2  2.78781027      26.03805011                    75.11489
    ## Dim.3  1.22029354      11.39749890                    86.51239
    ## Dim.4  0.66512943       6.21228553                    92.72467
    ## Dim.5  0.40625066       3.79436692                    96.51904
    ## Dim.6  0.20056337       1.87325488                    98.39229
    ## Dim.7  0.10119312       0.94514019                    99.33743
    ## Dim.8  0.02836630       0.26494026                    99.60237
    ## Dim.9  0.02121520       0.19814921                    99.80052
    ## Dim.10 0.01658359       0.15489012                    99.95541
    ## Dim.11 0.00477385       0.04458759                   100.00000
    ##         eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1  5.419818528      50.45323341                    50.45323
    ## Dim.2  2.694735923      25.08536768                    75.53860
    ## Dim.3  1.247648852      11.61439602                    87.15300
    ## Dim.4  0.655442534       6.10153181                    93.25453
    ## Dim.5  0.379585963       3.53357573                    96.78810
    ## Dim.6  0.165202044       1.53787017                    98.32597
    ## Dim.7  0.105478075       0.98189818                    99.30787
    ## Dim.8  0.024722774       0.23014496                    99.53802
    ## Dim.9  0.023434169       0.21814930                    99.75617
    ## Dim.10 0.021023382       0.19570721                    99.95187
    ## Dim.11 0.005169771       0.04812554                   100.00000
    ##         eigenvalue variance.percent cumulative.variance.percent
    ## Dim.1  5.344638673      50.18129066                    50.18129
    ## Dim.2  2.710331678      25.44754660                    75.62884
    ## Dim.3  1.194593348      11.21614382                    86.84498
    ## Dim.4  0.662366323       6.21901667                    93.06400
    ## Dim.5  0.380902571       3.57632832                    96.64033
    ## Dim.6  0.175065333       1.64370407                    98.28403
    ## Dim.7  0.105845632       0.99379411                    99.27782
    ## Dim.8  0.026444757       0.24829219                    99.52612
    ## Dim.9  0.023854198       0.22396919                    99.75009
    ## Dim.10 0.019979458       0.18758891                    99.93767
    ## Dim.11 0.006638073       0.06232546                   100.00000

A banda, es pot graficar, a través d’un *scree plot* el percentatge de
variança explicada per cada un dels components principals que s’han
generat:

    ## Representació scree plot
    llista_eig <- lapply(names(llista_pca), function(any) {
      fviz_eig(llista_pca[[any]]) + ggtitle(paste("Scree plot per a l'any", any))
    })
    names(llista_eig) <- names(llista_pca)
    for (any in names(llista_eig)) {
      print(llista_eig[[any]])
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-6-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-6-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-6-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-6-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-6-5.png)

Visualment es pot veure com, per a cadascun dels anys, el primer
component explica quasi la meitat del model.

    ##Càlcul de la variança dels CP a partir de la desviació estàndar
    llista_var <- lapply(llista_pca, function(pca) pca$sdev^2)
    llista_var

    ## $`2019`
    ##  [1] 5.330718973 2.689199626 1.243564583 0.701982644 0.406697484 0.155409607
    ##  [7] 0.085027311 0.033881416 0.022484345 0.018740062 0.003663484
    ## 
    ## $`2020`
    ##  [1] 5.273178024 2.777188205 1.193785019 0.673534378 0.418684740 0.182193255
    ##  [7] 0.103973393 0.028720626 0.024403380 0.022463346 0.004516176
    ## 
    ## $`2021`
    ##  [1] 5.25449899 2.78781027 1.22029354 0.66512943 0.40625066 0.20056337
    ##  [7] 0.10119312 0.02836630 0.02121520 0.01658359 0.00477385
    ## 
    ## $`2022`
    ##  [1] 5.419818528 2.694735923 1.247648852 0.655442534 0.379585963 0.165202044
    ##  [7] 0.105478075 0.024722774 0.023434169 0.021023382 0.005169771
    ## 
    ## $`2023`
    ##  [1] 5.344638673 2.710331678 1.194593348 0.662366323 0.380902571 0.175065333
    ##  [7] 0.105845632 0.026444757 0.023854198 0.019979458 0.006638073

Atenent al *criteri de Káiser*, es seleccionen aquells components que
tenen un valor de la variança superior a 1. En aquest cas, sembla
tractar-se dels tres primers component, que compleixen amb la condició.

Un valor propi superior a 1 indica que els components principals
representen més variança de la que representa una de les variables
originals de les dades normalitzades. S’utilitza habitualment com a punt
de tall per a conservar el component principal.

Continuant amb l’anàlisi dels components princiapls que s’han generat,
es pot aprofitar la informació que es desprén del mateix per
representar-los visualment:

    ## Elements components principals
    cp <- get_pca_var(llista_pca[[any]])
    cp

    ## Principal Component Analysis Results for variables
    ##  ===================================================
    ##   Name       Description                                    
    ## 1 "$coord"   "Coordinates for the variables"                
    ## 2 "$cor"     "Correlations between variables and dimensions"
    ## 3 "$cos2"    "Cos2 for the variables"                       
    ## 4 "$contrib" "contributions of the variables"

Per al nostre anàlisi existeixen dades al voltant de les coordenades de
les variables, amb els que crear un diagrama de dispersió. També es pot
fer ús del quadrat del cosinus, per estudiar la qualitat de
representació de les variables al mapa de factores. A més, es diposa del
pes de les contribucions de cadascuna de les variables als components
principals que s’han obtingut.

    ## Ús dels 3 components principals localitzats
    llista_coord <- lapply(llista_pca, function(pca) {
      get_pca_var(pca)$coord[, 1:3]
    })
    for (any in names(llista_coord)) {
      cat("### Coordenades dels 3 primers components - Any", any, "###\n")
      print(head(llista_coord[[any]], 11))
      cat("\n---------------------------\n")
    }

    ## ### Coordenades dels 3 primers components - Any 2019 ###
    ##                    Dim.1       Dim.2       Dim.3
    ## Pobl_Jove     -0.8499635 -0.05927271  0.39483449
    ## Perc_Jove     -0.7487595  0.53521914 -0.28418194
    ## Pobl_Regio    -0.8316737  0.33294650 -0.01275957
    ## Perc_Regio    -0.7195272  0.50802273 -0.37041843
    ## Pobl_Superior -0.7702442 -0.49965537  0.29902824
    ## Perc_Superior -0.4567075 -0.71680659 -0.37948348
    ## Pobl_Host     -0.8503599 -0.11792436  0.41132094
    ## Perc_Host     -0.7646432  0.40314174 -0.29184497
    ## Renda_Mitjana -0.1022941 -0.89185812 -0.29508697
    ## Habitatges    -0.7208454 -0.25471639  0.28629605
    ## Preu_Lloguer  -0.4455431 -0.47968578 -0.46120772
    ## 
    ## ---------------------------
    ## ### Coordenades dels 3 primers components - Any 2020 ###
    ##                     Dim.1      Dim.2       Dim.3
    ## Pobl_Jove     -0.85723096 -0.0867207 -0.37237153
    ## Perc_Jove     -0.75930804  0.5304836  0.26762712
    ## Pobl_Regio    -0.84511112  0.3226859  0.02949421
    ## Perc_Regio    -0.71864352  0.5216967  0.37299738
    ## Pobl_Superior -0.75534269 -0.5307721 -0.28029099
    ## Perc_Superior -0.42381554 -0.7254846  0.38612667
    ## Pobl_Host     -0.84462845 -0.1520628 -0.40423447
    ## Perc_Host     -0.75992219  0.4296143  0.26023951
    ## Renda_Mitjana -0.06185559 -0.8797490  0.31347298
    ## Habitatges    -0.71130747 -0.2852643 -0.27912225
    ## Preu_Lloguer  -0.42461457 -0.4908041  0.45666370
    ## 
    ## ---------------------------
    ## ### Coordenades dels 3 primers components - Any 2021 ###
    ##                     Dim.1       Dim.2       Dim.3
    ## Pobl_Jove     -0.86145226 -0.08419201 -0.36345956
    ## Perc_Jove     -0.79370921  0.51145517  0.29356308
    ## Pobl_Regio    -0.85368713  0.31427873  0.03822516
    ## Perc_Regio    -0.73197600  0.50443108  0.38315995
    ## Pobl_Superior -0.74783121 -0.53540611 -0.29286724
    ## Perc_Superior -0.40215041 -0.74325383  0.37236027
    ## Pobl_Host     -0.83800361 -0.16657162 -0.40720741
    ## Perc_Host     -0.75949547  0.39537689  0.26327747
    ## Renda_Mitjana -0.05951519 -0.88112929  0.29928326
    ## Habitatges    -0.71522262 -0.29413652 -0.27157843
    ## Preu_Lloguer  -0.32048208 -0.52901199  0.48047591
    ## 
    ## ---------------------------
    ## ### Coordenades dels 3 primers components - Any 2022 ###
    ##                     Dim.1       Dim.2        Dim.3
    ## Pobl_Jove     -0.85401643 -0.08717956 -0.387379621
    ## Perc_Jove     -0.81213796  0.49933259  0.276542310
    ## Pobl_Regio    -0.84929865  0.32715380  0.009745189
    ## Perc_Regio    -0.72860883  0.52290071  0.354979585
    ## Pobl_Superior -0.75236689 -0.52699401 -0.299145962
    ## Perc_Superior -0.42715811 -0.72671535  0.369397090
    ## Pobl_Host     -0.84085785 -0.14105191 -0.412255707
    ## Perc_Host     -0.74089487  0.43432253  0.257824025
    ## Renda_Mitjana -0.08338512 -0.88259731  0.321072775
    ## Habitatges    -0.73541339 -0.28392333 -0.277111828
    ## Preu_Lloguer  -0.47587216 -0.42823801  0.502748001
    ## 
    ## ---------------------------
    ## ### Coordenades dels 3 primers components - Any 2023 ###
    ##                     Dim.1      Dim.2       Dim.3
    ## Pobl_Jove     -0.85153432 -0.1052557 -0.38269600
    ## Perc_Jove     -0.81958976  0.4906999  0.26676567
    ## Pobl_Regio    -0.85865629  0.3123310  0.02104683
    ## Perc_Regio    -0.73009630  0.5142716  0.35888279
    ## Pobl_Superior -0.74589322 -0.5445188 -0.28207817
    ## Perc_Superior -0.39795905 -0.7277651  0.37594363
    ## Pobl_Host     -0.83962771 -0.1655645 -0.40338578
    ## Perc_Host     -0.76079625  0.4222446  0.28020653
    ## Renda_Mitjana -0.06216984 -0.8774916  0.33190194
    ## Habitatges    -0.74036432 -0.3096753 -0.23543144
    ## Preu_Lloguer  -0.35630216 -0.4457638  0.46905119
    ## 
    ## ---------------------------

#### Qualitat de representació

La qualitat de representació de les variables en el mapa de factores es
denomina cos2 (cosinus quadrat, coordenades quadrades). Es pot accedir
al cos2 de la següent manera:

    ## Extracció cosinus quadrat
    llista_coord <- lapply(llista_pca, function(pca) {
      get_pca_var(pca)$cos2[, 1:3]
    })
    for (any in names(llista_coord)) {
      cat("### Cosinus quadrat dels 3 primers components - Any", any, "###\n")
      print(head(llista_coord[[any]], 11))
      cat("\n---------------------------\n")
    }

    ## ### Cosinus quadrat dels 3 primers components - Any 2019 ###
    ##                    Dim.1       Dim.2        Dim.3
    ## Pobl_Jove     0.72243791 0.003513254 0.1558942770
    ## Perc_Jove     0.56064074 0.286459528 0.0807593725
    ## Pobl_Regio    0.69168108 0.110853375 0.0001628066
    ## Perc_Regio    0.51771946 0.258087094 0.1372098148
    ## Pobl_Superior 0.59327616 0.249655485 0.0894178884
    ## Perc_Superior 0.20858171 0.513811689 0.1440077118
    ## Pobl_Host     0.72311188 0.013906154 0.1691849117
    ## Perc_Host     0.58467921 0.162523260 0.0851734839
    ## Renda_Mitjana 0.01046408 0.795410902 0.0870763226
    ## Habitatges    0.51961804 0.064880438 0.0819654294
    ## Preu_Lloguer  0.19850869 0.230098448 0.2127125645
    ## 
    ## ---------------------------
    ## ### Cosinus quadrat dels 3 primers components - Any 2020 ###
    ##                     Dim.1      Dim.2        Dim.3
    ## Pobl_Jove     0.734844923 0.00752048 0.1386605584
    ## Perc_Jove     0.576548699 0.28141287 0.0716242761
    ## Pobl_Regio    0.714212801 0.10412618 0.0008699084
    ## Perc_Regio    0.516448504 0.27216745 0.1391270464
    ## Pobl_Superior 0.570542578 0.28171907 0.0785630383
    ## Perc_Superior 0.179619609 0.52632790 0.1490938018
    ## Pobl_Host     0.713397214 0.02312308 0.1634055099
    ## Perc_Host     0.577481728 0.18456846 0.0677246028
    ## Renda_Mitjana 0.003826114 0.77395833 0.0982653071
    ## Habitatges    0.505958318 0.08137574 0.0779092322
    ## Preu_Lloguer  0.180297535 0.24088864 0.2085417374
    ## 
    ## ---------------------------
    ## ### Cosinus quadrat dels 3 primers components - Any 2021 ###
    ##                     Dim.1       Dim.2       Dim.3
    ## Pobl_Jove     0.742099996 0.007088294 0.132102853
    ## Perc_Jove     0.629974303 0.261586393 0.086179283
    ## Pobl_Regio    0.728781717 0.098771118 0.001461163
    ## Perc_Regio    0.535788862 0.254450713 0.146811548
    ## Pobl_Superior 0.559251522 0.286659705 0.085771218
    ## Perc_Superior 0.161724954 0.552426249 0.138652172
    ## Pobl_Host     0.702250055 0.027746105 0.165817875
    ## Perc_Host     0.576833371 0.156322889 0.069315024
    ## Renda_Mitjana 0.003542058 0.776388822 0.089570469
    ## Habitatges    0.511543394 0.086516295 0.073754841
    ## Preu_Lloguer  0.102708763 0.279853683 0.230857098
    ## 
    ## ---------------------------
    ## ### Cosinus quadrat dels 3 primers components - Any 2022 ###
    ##                     Dim.1       Dim.2        Dim.3
    ## Pobl_Jove     0.729344064 0.007600276 1.500630e-01
    ## Perc_Jove     0.659568065 0.249333040 7.647565e-02
    ## Pobl_Regio    0.721308189 0.107029607 9.496871e-05
    ## Perc_Regio    0.530870831 0.273425150 1.260105e-01
    ## Pobl_Superior 0.566055938 0.277722688 8.948831e-02
    ## Perc_Superior 0.182464055 0.528115203 1.364542e-01
    ## Pobl_Host     0.707041931 0.019895642 1.699548e-01
    ## Perc_Host     0.548925213 0.188636056 6.647323e-02
    ## Renda_Mitjana 0.006953079 0.778978017 1.030877e-01
    ## Habitatges    0.540832850 0.080612456 7.679097e-02
    ## Preu_Lloguer  0.226454312 0.183387789 2.527556e-01
    ## 
    ## ---------------------------
    ## ### Cosinus quadrat dels 3 primers components - Any 2023 ###
    ##                     Dim.1      Dim.2        Dim.3
    ## Pobl_Jove     0.725110705 0.01107875 0.1464562321
    ## Perc_Jove     0.671727370 0.24078644 0.0711639212
    ## Pobl_Regio    0.737290620 0.09755063 0.0004429691
    ## Perc_Regio    0.533040610 0.26447530 0.1287968583
    ## Pobl_Superior 0.556356695 0.29650072 0.0795680953
    ## Perc_Superior 0.158371408 0.52964206 0.1413336136
    ## Pobl_Host     0.704974690 0.02741160 0.1627200837
    ## Perc_Host     0.578810939 0.17829054 0.0785156990
    ## Renda_Mitjana 0.003865089 0.76999153 0.1101588980
    ## Habitatges    0.548139321 0.09589877 0.0554279619
    ## Preu_Lloguer  0.126951227 0.19870534 0.2200090156
    ## 
    ## ---------------------------

Es pot representar la qualitat gràficament:

    ## Representació gràfica qualitat
    for (any in names(llista_pca)) {
      cp_any <- get_pca_var(llista_pca[[any]])
      corrplot(cp_any$cos2[, 1:3], is.corr = FALSE,
               title = paste("Cosinus quadrat variables - Any", any),
               mar = c(0, 0, 2, 0)) 
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-11-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-11-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-11-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-11-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-11-5.png)

Adicionalment, es pot crear un diagrama de barres de variables cos2, amb
la funció següent:

    ## Diagrama de barres cosinus quadrat
    for (any in names(llista_pca)) {
      print(
        fviz_cos2(llista_pca[[any]], choice = "var", axes = 1:2) +
          ggtitle(paste("Cosinus quadrat variables - Any", any))
      )
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-12-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-12-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-12-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-12-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-12-5.png)

-   Un cos2 elevat indica una bona representació de la variable al
    component principal. En aquest cas, la variable es posiciona a prop
    de la circumferència de correlació.

-   Un cos2 baix indica que la variable no està perfectament
    representada pels components principals. En aquest cas, la variable
    està a prop del centre del cercle.

Per a d’algunes variables, poden ser necessaris més de dos components
per representar perfectament les dades, En aquest cas, les variables es
situen dins del cercle de correlacions.

    ## Gràfica de correlació qualitat
    for (any in names(llista_pca)) {
      print(
        fviz_pca_var(
          llista_pca[[any]],
          col.var = "cos2",
          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
          repel = TRUE
        ) + ggtitle(paste("Variables PCA - Any", any))
      )
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-13-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-13-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-13-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-13-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-13-5.png)

#### Contribució

Les contribucions de les variables en la comptabilització de la
variabilitat d’un determinat component principal s’expressen en
percentatge.

Les variables que estan correlacionades amb PC1 i PC2 són les més
importants per explicar la variabilitat al conjunt de dades. D’altra
banda, les variables que no estan correlacionades amb cap component
principal o amb les últimes dimensions, són variables amb una
contribució baixa i es poden eliminar per simplificar l’anàlisi global.

La contribució de les variables es pot extraure com segueix:

    ## Extracció contribució variables
    llista_coord <- lapply(llista_pca, function(pca) {
      get_pca_var(pca)$contrib[, 1:3]
    })
    for (any in names(llista_coord)) {
      cat("### Contribucions variables dels 3 primers components - Any", any, "###\n")
      print(head(llista_coord[[any]], 11))
      cat("\n---------------------------\n")
    }

    ## ### Contribucions variables dels 3 primers components - Any 2019 ###
    ##                    Dim.1      Dim.2       Dim.3
    ## Pobl_Jove     13.5523541  0.1306431 12.53608209
    ## Perc_Jove     10.5171693 10.6522225  6.49418403
    ## Pobl_Regio    12.9753807  4.1221698  0.01309193
    ## Perc_Regio     9.7120006  9.5971713 11.03358978
    ## Pobl_Superior 11.1293835  9.2836353  7.19044990
    ## Perc_Superior  3.9128252 19.1064912 11.58023586
    ## Pobl_Host     13.5649973  0.5171112 13.60483516
    ## Perc_Host     10.9681116  6.0435551  6.84914037
    ## Renda_Mitjana  0.1962978 29.5779791  7.00215524
    ## Habitatges     9.7476165  2.4126300  6.59116788
    ## Preu_Lloguer   3.7238634  8.5563915 17.10506775
    ## 
    ## ---------------------------
    ## ### Contribucions variables dels 3 primers components - Any 2020 ###
    ##                     Dim.1      Dim.2       Dim.3
    ## Pobl_Jove     13.93552275  0.2707947 11.61520343
    ## Perc_Jove     10.93360960 10.1330141  5.99976336
    ## Pobl_Regio    13.54425734  3.7493382  0.07286977
    ## Perc_Regio     9.79387575  9.8001084 11.65427981
    ## Pobl_Superior 10.81971016 10.1440395  6.58100387
    ## Perc_Superior  3.40628760 18.9518267 12.48916676
    ## Pobl_Host     13.52879062  0.8326076 13.68801814
    ## Perc_Host     10.95130348  6.6458752  5.67309874
    ## Renda_Mitjana  0.07255803 27.8684147  8.23140729
    ## Habitatges     9.59494096  2.9301487  6.52623638
    ## Preu_Lloguer   3.41914372  8.6738321 17.46895245
    ## 
    ## ---------------------------
    ## ### Contribucions variables dels 3 primers components - Any 2021 ###
    ##                   Dim.1      Dim.2      Dim.3
    ## Pobl_Jove     14.123135  0.2542603 10.8254979
    ## Perc_Jove     11.989236  9.3832208  7.0621764
    ## Pobl_Regio    13.869671  3.5429641  0.1197386
    ## Perc_Regio    10.196764  9.1272608 12.0308387
    ## Pobl_Superior 10.643289 10.2826117  7.0287365
    ## Perc_Superior  3.077838 19.8157764 11.3621983
    ## Pobl_Host     13.364739  0.9952652 13.5883596
    ## Perc_Host     10.977895  5.6073719  5.6801927
    ## Renda_Mitjana  0.067410 27.8494140  7.3400756
    ## Habitatges     9.735341  3.1033782  6.0440245
    ## Preu_Lloguer   1.954682 10.0384767 18.9181611
    ## 
    ## ---------------------------
    ## ### Contribucions variables dels 3 primers components - Any 2022 ###
    ##                    Dim.1      Dim.2        Dim.3
    ## Pobl_Jove     13.4569831  0.2820416 12.027660733
    ## Perc_Jove     12.1695600  9.2525964  6.129581168
    ## Pobl_Regio    13.3087148  3.9718032  0.007611814
    ## Perc_Regio     9.7949927 10.1466399 10.099837415
    ## Pobl_Superior 10.4441862 10.3061189  7.172555508
    ## Perc_Superior  3.3666082 19.5980318 10.936908270
    ## Pobl_Host     13.0454909  0.7383151 13.622003313
    ## Perc_Host     10.1281106  7.0001685  5.327879534
    ## Renda_Mitjana  0.1282899 28.9073972  8.262559349
    ## Habitatges     9.9788000  2.9914789  6.154853995
    ## Preu_Lloguer   4.1782637  6.8054086 20.258548901
    ## 
    ## ---------------------------
    ## ### Contribucions variables dels 3 primers components - Any 2023 ###
    ##                     Dim.1     Dim.2       Dim.3
    ## Pobl_Jove     13.56706690  0.408760 12.25992363
    ## Perc_Jove     12.56824664  8.884021  5.95716704
    ## Pobl_Regio    13.79495725  3.599214  0.03708116
    ## Perc_Regio     9.97337037  9.758042 10.78164871
    ## Pobl_Superior 10.40962222 10.939647  6.66068462
    ## Perc_Superior  2.96318269 19.541596 11.83110669
    ## Pobl_Host     13.19031525  1.011374 13.62137869
    ## Perc_Host     10.82974873  6.578182  6.57258800
    ## Renda_Mitjana  0.07231712 28.409495  9.22145584
    ## Habitatges    10.25587236  3.538267  4.63990211
    ## Preu_Lloguer   2.37530047  7.331403 18.41706351
    ## 
    ## ---------------------------

A major valor de la contribució, més contribució hi haurà al component:

    ## Representació gràfica contribució
    for (any in names(llista_pca)) {
      cp_any <- get_pca_var(llista_pca[[any]])
      corrplot(cp_any$contrib[, 1:3], is.corr = FALSE,
               title = paste("Contribucions variables - Any", any),
               mar = c(0, 0, 2, 0)) 
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-15-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-15-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-15-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-15-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-15-5.png)

Les variables més importants, és a dir, que més contribueixen es poden
destacar a la gràfica de correlació de la següent manera:

    ## Gràfica de correlació contribució
    for (any in names(llista_pca)) {
      print(
        fviz_pca_var(
          llista_pca[[any]],
          col.var = "contrib",
          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
          repel = TRUE
        ) + ggtitle(paste("Variables PCA - Any", any))
      )
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-16-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-16-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-16-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-16-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-16-5.png)

Les variables correlacionades positives apunten al mateix costat de la
trama. Per la seua part, les variables correlacionades negatives apunten
en direccions contràries.

### *Clustering*

Seguidament, després d’haver portat a terme l’Anàlisi de Components
Principals segmentat entre els anys 2019 i 2023, es pot generar un model
no supervisat. En aquest cas, es va a generar un model a través de
l’algorisme *k-means*. Es tracta d’un algorisme de classificació no
supervisada (*clustering)* que agrupa les observacions del conjunt de
dades basant-se en les seues característiques.

#### Càlcul del nombre de clùsters òptims

El pas més crític d’aquest mètode és determinar el valor del paràmetre
*k,* és a dir, el nombre de clústers a obtenir. Per poder resoldre
aquesta situació, es van a utilitzar algunes tècniques amb diferents
criteris, que permetran escollir el nombre òptim de grups per a cada
any.

El primer dels mètodes que es va a utilitzar i, possiblement, el més
conegut, és l’anomenat com a mètode *elbow*. Aquesta tècnica fa ús de la
distància de cada observació respecte del seu centroide corresponent, el
que es coneix com *distorsió*. La seua traducció literal és el mètode
del *colze*, per la seua particular forma de escollir el nombre òptim de
clústers.

    ## Aplicació tècnica del colze
    for (any in names(df_anys_norm)) {
      print(
        fviz_nbclust(
          df_anys_norm[[any]],
          kmeans,
          method = "wss"
        ) + ggtitle(paste("Mètode del colze - Any", any))
      )
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-17-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-17-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-17-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-17-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-17-5.png)

Fent ús d’aquesta primera tècnica, per als anys 2019, 2020 i 2021, el
valor de *k* sembla 4, mentre que per a la resta d’anys sembla de 3.

Es pot fer ús també del mètode de la silueta, que consisteix a
representar gràficament com de bé s’ha classificat cadascuna de les
observacions, tenint en compte la distància entre veïns.

    ## Aplicació tècnica de la silueta
    for (any in names(df_anys_norm)) {
      print(
        fviz_nbclust(
          df_anys_norm[[any]],
          kmeans,
          method = "silhouette"
        ) + ggtitle(paste("Mètode de la silueta - Any", any))
      )
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-18-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-18-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-18-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-18-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-18-5.png)

El resultat no coincideix amb el de la tècnica anterior, ja que el
mètode de la silueta sugereix utilitzar el paràmetre *k = 2* per a tots
els anys.

Per tant, com que aquest mètode considera el valor *k = 2*, però
l’anterior no el considerava òptim per a la majoria d’anys, es prendrà
el primer com a referència.

#### Determinació dels clùsters

Com ja s’ha comentat, amb el valor *k = 4*, ja es pot observar el
comportament del algorisme *k\_means* amb aquest paràmetre.

    ## Establiment reproduïbilitat
    set.seed(123)

    ## Càlcul k-means amb k = 4 
    llista_clusters_k4 <- lapply(df_anys_norm, function(df) {
      kmeans(df, centers = 4, nstart = 25)
    })
    llista_clusters_k4

    ## $`2019`
    ## K-means clustering with 4 clusters of sizes 14, 5, 11, 35
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1  1.4149063  0.2900478  0.5667519  0.1063213    1.64158612     0.8526074
    ## 2  1.1733085  2.7092896  2.3456578  2.8270509    0.29095830     0.2499138
    ## 3 -0.4712042 -0.6952543 -0.3380978 -0.1842882    0.08544416     1.3177249
    ## 4 -0.3245383 -0.2795631 -0.3140966 -0.2794996   -0.48761096    -0.5296363
    ##    Pobl_Host  Perc_Host Renda_Mitjana Habitatges Preu_Lloguer
    ## 1  1.5102308  0.4316557     0.6098000  1.2726248    0.5826302
    ## 2  1.0253032  2.4918086    -0.7245473  0.2928738    0.9176224
    ## 3 -0.4903399 -0.5849846     1.5695098 -0.3415161    1.1086187
    ## 4 -0.3468216 -0.3523966    -0.3908069 -0.4435554   -0.3975384
    ## 
    ## Clustering vector:
    ## 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 
    ##   2   2   2   2   1   1   1   1   1   1   2   4   4   4   4   4   4   1   1   3 
    ## 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 333 
    ##   3   3   3   3   3   1   3   3   4   3   1   1   4   4   4   4   4   4   4   4 
    ## 335 336 337 338 340 341 342 343 344 349 351 352 353 354 355 356 357 358 359 360 
    ##   4   4   4   4   4   4   4   4   4   4   4   1   4   4   4   1   4   4   3   1 
    ## 361 362 363 364 365 
    ##   3   4   4   4   4 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 74.25586 58.96497 31.50619 89.66974
    ##  (between_SS / total_SS =  62.8 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2020`
    ## K-means clustering with 4 clusters of sizes 4, 15, 35, 11
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1  1.1225181  3.0456352  2.4975073  3.1019110    0.18864434     0.2444531
    ## 2  1.4219498  0.3495108  0.6674663  0.1890112    1.58006830     0.8089113
    ## 3 -0.3230342 -0.2644846 -0.3149722 -0.2648739   -0.48200134    -0.5185843
    ## 4 -0.4865397 -0.6971210 -0.3498634 -0.1969842    0.07032196     1.3046166
    ##    Pobl_Host  Perc_Host Renda_Mitjana  Habitatges Preu_Lloguer
    ## 1  0.9443327  2.8515024    -0.8171270  0.01919628    0.9045648
    ## 2  1.4957882  0.4548247     0.5461033  1.27954081    0.5881589
    ## 3 -0.3405549 -0.3203530    -0.3720966 -0.44313508   -0.3833149
    ## 4 -0.4986953 -0.6237506     1.5199582 -0.34183358    1.0895648
    ## 
    ## Clustering vector:
    ## 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380 381 382 383 384 385 
    ##   1   1   1   1   2   2   2   2   2   2   2   3   3   3   3   3   3   2   2   4 
    ## 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400 401 402 403 404 406 
    ##   4   4   4   4   4   2   4   4   3   4   2   2   3   3   3   3   3   3   3   3 
    ## 408 409 410 411 413 414 415 416 417 422 424 425 426 427 428 429 430 431 432 433 
    ##   3   3   3   3   3   3   3   3   3   3   3   2   3   3   3   2   3   3   4   2 
    ## 434 435 436 437 438 
    ##   4   3   3   3   3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 51.49622 82.42397 92.94024 31.75721
    ##  (between_SS / total_SS =  62.2 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2021`
    ## K-means clustering with 4 clusters of sizes 4, 11, 35, 15
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1  1.1727750  3.1384928  2.5230586  3.1049440     0.1921587     0.2129524
    ## 2 -0.4897085 -0.6720207 -0.3550351 -0.2020756     0.0621783     1.3019659
    ## 3 -0.3246627 -0.2667490 -0.3162259 -0.2732473    -0.4813376    -0.5125687
    ## 4  1.4145484  0.3840172  0.6676689  0.1895431     1.5870469     0.8137265
    ##    Pobl_Host  Perc_Host Renda_Mitjana  Habitatges Preu_Lloguer
    ## 1  0.9162016  2.7464368    -0.7865449  0.02563812    0.6536223
    ## 2 -0.4994724 -0.6386147     1.5299316 -0.33594396    1.1745638
    ## 3 -0.3388224 -0.3277900    -0.3836286 -0.44436082   -0.3667963
    ## 4  1.5002624  0.4476432     0.5589534  1.27636397    0.5097552
    ## 
    ## Clustering vector:
    ## 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 
    ##   1   1   1   1   4   4   4   4   4   4   4   3   3   3   3   3   3   4   4   2 
    ## 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 479 
    ##   2   2   2   2   2   4   2   2   3   2   4   4   3   3   3   3   3   3   3   3 
    ## 481 482 483 484 486 487 488 489 490 495 497 498 499 500 501 502 503 504 505 506 
    ##   3   3   3   3   3   3   3   3   3   3   3   4   3   3   3   4   3   3   2   4 
    ## 507 508 509 510 511 
    ##   2   3   3   3   3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 51.24838 28.66174 95.27763 82.82169
    ##  (between_SS / total_SS =  62.3 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2022`
    ## K-means clustering with 4 clusters of sizes 13, 15, 33, 4
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1 -0.5115450 -0.6181351 -0.3688017 -0.2076772   -0.01539362     1.1625486
    ## 2  1.4397089  0.4472696  0.6886690  0.1982548    1.59041524     0.8219431
    ## 3 -0.3063163 -0.2675279 -0.3137103 -0.2745899   -0.48024327    -0.5677248
    ## 4  1.0921339  3.1025756  2.5091039  3.1073885    0.15165956     0.1963705
    ##    Pobl_Host  Perc_Host Renda_Mitjana  Habitatges Preu_Lloguer
    ## 1 -0.5162962 -0.5743262     1.3774779 -0.34128830    0.9297291
    ## 2  1.5002271  0.4267520     0.5493371  1.28778983    0.5795049
    ## 3 -0.3281805 -0.3363122    -0.4417809 -0.45522485   -0.4711619
    ## 4  0.9529138  2.7680129    -0.7847786  0.03558012    1.1889653
    ## 
    ## Clustering vector:
    ## 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 531 
    ##   4   4   4   4   2   2   2   2   2   2   2   3   3   3   3   3   3   2   2   1 
    ## 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549 550 552 
    ##   1   1   1   1   1   2   1   1   3   1   2   2   3   3   3   1   3   3   3   3 
    ## 554 555 556 557 559 560 561 562 563 568 570 571 572 573 574 575 576 577 578 579 
    ##   3   3   3   3   3   3   3   3   3   3   3   2   3   3   3   2   3   1   1   2 
    ## 580 581 582 583 584 
    ##   1   3   3   3   3 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 39.61527 83.11176 82.88277 48.89791
    ##  (between_SS / total_SS =  63.0 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2023`
    ## K-means clustering with 4 clusters of sizes 14, 14, 4, 33
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1  1.4459830  0.5887128  0.7807741  0.2687742    1.62314371     0.9004075
    ## 2 -0.4966686 -0.6004133 -0.3709729 -0.2142780   -0.03993433     1.0652284
    ## 3  1.0529590  3.0832736  2.5156379  3.1121623    0.11963416     0.1553544
    ## 4 -0.2482689 -0.2656670 -0.3148575 -0.2966461   -0.42861335    -0.5583118
    ##    Pobl_Host  Perc_Host Renda_Mitjana Habitatges Preu_Lloguer
    ## 1  1.4826902  0.5180280     0.5847473  1.4172284    0.6190960
    ## 2 -0.4919748 -0.5454757     1.2710348 -0.3435017    0.9710234
    ## 3  0.9296901  2.8215647    -0.7890526  0.0409734    0.8066876
    ## 4 -0.2656373 -0.3214860    -0.4365135 -0.4604869   -0.4179087
    ## 
    ## Clustering vector:
    ## 585 586 587 588 589 590 591 592 593 594 595 596 597 598 599 600 601 602 603 604 
    ##   3   3   3   3   1   1   1   1   1   1   1   4   4   4   4   2   4   1   1   2 
    ## 605 606 607 608 609 610 611 612 613 614 615 616 617 618 619 620 621 622 623 625 
    ##   2   2   2   2   2   1   2   2   4   2   1   1   4   4   4   2   4   4   4   4 
    ## 627 628 629 630 632 633 634 635 636 641 643 644 645 646 647 648 649 650 651 652 
    ##   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   1   4   2   2   1 
    ## 653 654 655 656 657 
    ##   2   4   4   4   4 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 72.04149 45.48879 46.47358 91.27604
    ##  (between_SS / total_SS =  62.5 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

Per a evaluar la qualitat dels diferents models (1 per cada any) es pot
analitzar el següent:

    ## Extracció qualitat models
    resultats_ss <- lapply(llista_clusters_k4, function(cl) {
      list(
        ssw = cl$withinss,
        tssw = cl$tot.withinss,
        ssb = cl$betweenss
      )
    })

    for (any in names(resultats_ss)) {
      cat("### Any:", any, "###\n")
      print(resultats_ss[[any]])
    }

    ## ### Any: 2019 ###
    ## $ssw
    ## [1] 74.25586 58.96497 31.50619 89.66974
    ## 
    ## $tssw
    ## [1] 254.3968
    ## 
    ## $ssb
    ## [1] 429.8509
    ## 
    ## ### Any: 2020 ###
    ## $ssw
    ## [1] 51.49622 82.42397 92.94024 31.75721
    ## 
    ## $tssw
    ## [1] 258.6176
    ## 
    ## $ssb
    ## [1] 426.3514
    ## 
    ## ### Any: 2021 ###
    ## $ssw
    ## [1] 51.24838 28.66174 95.27763 82.82169
    ## 
    ## $tssw
    ## [1] 258.0094
    ## 
    ## $ssb
    ## [1] 427.218
    ## 
    ## ### Any: 2022 ###
    ## $ssw
    ## [1] 39.61527 83.11176 82.88277 48.89791
    ## 
    ## $tssw
    ## [1] 254.5077
    ## 
    ## $ssb
    ## [1] 432.9971
    ## 
    ## ### Any: 2023 ###
    ## $ssw
    ## [1] 72.04149 45.48879 46.47358 91.27604
    ## 
    ## $tssw
    ## [1] 255.2799
    ## 
    ## $ssb
    ## [1] 426.3623

Els resultats d’aquestes proves de qualitat s’analitzen a l’apartat
corresponent de la memòria del projecte.

Prenent com a referència els valors que s’acaben d’obtenir, es poden
calcular diversos índexs amb els que seguir avaluant la qualitat del
model. Es tracta de tècniques de validació interna, és a dir, es
fonamenten en el càlcul d’índexs basats en informació del model. En
primer lloc, el quocient entre el total de la suma dels quadrats dintre
del grup entre la suma dels quadrats entre grups, que posteriorment a la
memòria es compara amb altres valors de *k.*

    ## Càlcul d'índexs

    resultats_index <- list()

    for (any in names(llista_clusters_k4)) {
     cl <- llista_clusters_k4[[any]]
     ssw <- cl$tot.withinss
     ssb <- cl$betweenss

     BH <- ssw / 4
     CH <- ssb / (4 - 1)
     H <- log(ssb / ssw)
     WB <- 4 * (ssw / ssb)

     resultats_index[[any]] <- list(
      Ball_Hall = BH,
      Calinski_Harabasz = CH,
      Hartigan = H,
      WB_index = WB
     )
    }

    df_resultats <- do.call(rbind, lapply(names(resultats_index), function(any) {
     c(Any = any, resultats_index[[any]])
    }))
    kable(df_resultats, format = "markdown", col.names = c("Any", "Ball-Hall", "Calinski-Harabasz", "Hartigan", "WB Index"))

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 22%" />
<col style="width: 24%" />
<col style="width: 24%" />
<col style="width: 22%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Any</th>
<th style="text-align: left;">Ball-Hall</th>
<th style="text-align: left;">Calinski-Harabasz</th>
<th style="text-align: left;">Hartigan</th>
<th style="text-align: left;">WB Index</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">2019</td>
<td style="text-align: left;">63.5991900218829</td>
<td style="text-align: left;">143.283630046885</td>
<td style="text-align: left;">0.524543285555349</td>
<td style="text-align: left;">2.3673023918065</td>
</tr>
<tr class="even">
<td style="text-align: left;">2020</td>
<td style="text-align: left;">64.6544094306552</td>
<td style="text-align: left;">142.117118993916</td>
<td style="text-align: left;">0.499913119308761</td>
<td style="text-align: left;">2.42633343121919</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2021</td>
<td style="text-align: left;">64.5023599871213</td>
<td style="text-align: left;">142.405990974983</td>
<td style="text-align: left;">0.504298185014667</td>
<td style="text-align: left;">2.41571709337996</td>
</tr>
<tr class="even">
<td style="text-align: left;">2022</td>
<td style="text-align: left;">63.6269247861192</td>
<td style="text-align: left;">144.332356619398</td>
<td style="text-align: left;">0.531399873334286</td>
<td style="text-align: left;">2.3511262949454</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2023</td>
<td style="text-align: left;">63.8199761973001</td>
<td style="text-align: left;">142.120779299974</td>
<td style="text-align: left;">0.512928934196271</td>
<td style="text-align: left;">2.39495735994506</td>
</tr>
</tbody>
</table>

Per a constrastar aquests índexs, resulta necessari modificar el valor
de *k* i veure l’efecte que sorgeix, com es va a fer seguidament.

Abans, resulta interessant visualitzar els clústers generats per cada
any:

    ## Representació visual clústers
    plots_clusters <- mapply(function(model, data, any) {
      fviz_cluster(model, data = data,
                   geom = "point",
                   ellipse.type = "norm",
                   main = paste("Clústers per a l'any", any))
    }, llista_clusters_k4, df_anys_norm, names(df_anys_norm), SIMPLIFY = FALSE)

    for (any in names(plots_clusters)) {
      print(plots_clusters[[any]])
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-22-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-22-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-22-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-22-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-22-5.png)

A continuació, es va a fer la mateixa anàlisi, però canviant el valor
del paràmetre *k.* En quest cas, es va a reduir el nombre de clústers
fins a 2, com havia estat sugerit quan s’havia protat a terme la prova
de la silueta. Per observar els canvis gràficament, es pot fer ús dels
visuals ja utilitzats anteriorment. D’altra banda, per veure com ha
evolucionat la qualitat del model i contrastar-la amb el model original,
s’utilitzaran els mateixos índexs.

    ## Establiment reproduïbilitat
    set.seed(123)

    ## Càlcul k-means amb k = 2
    llista_clusters_k2 <- lapply(df_anys_norm, function(df) {
      kmeans(df, centers = 2, nstart = 25)
    })
    llista_clusters_k2

    ## $`2019`
    ## K-means clustering with 2 clusters of sizes 18, 47
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1  1.3396732  1.0223497  1.1091954  0.9133777     1.2955025    0.74566723
    ## 2 -0.3187441 -0.3878231 -0.3194715 -0.2686533    -0.3193308   -0.09102626
    ##    Pobl_Host  Perc_Host Renda_Mitjana Habitatges Preu_Lloguer
    ## 1  1.3579753  1.0413964    0.27013297  1.0958685   0.72134332
    ## 2 -0.3341766 -0.4045026    0.07741355 -0.4196943  -0.04166556
    ## 
    ## Clustering vector:
    ## 293 294 295 296 297 298 299 300 301 302 303 304 305 306 307 308 309 310 311 312 
    ##   1   1   1   1   1   1   1   1   1   1   1   2   2   2   2   2   2   1   1   2 
    ## 313 314 315 316 317 318 319 320 321 322 323 324 325 326 327 328 329 330 331 333 
    ##   2   2   2   2   2   1   2   2   2   2   1   1   2   2   2   2   2   2   2   2 
    ## 335 336 337 338 340 341 342 343 344 349 351 352 353 354 355 356 357 358 359 360 
    ##   2   2   2   2   2   2   2   2   2   2   2   2   2   2   2   1   2   2   2   1 
    ## 361 362 363 364 365 
    ##   2   2   2   2   2 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 215.2193 217.1127
    ##  (between_SS / total_SS =  36.8 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2020`
    ## K-means clustering with 2 clusters of sizes 47, 18
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1 -0.3225352 -0.3782051  -0.322582 -0.2604912    -0.3179984   -0.08549919
    ## 2  1.3511301  1.0153083   1.127270  0.8915824     1.2947234    0.74057089
    ##    Pobl_Host  Perc_Host Renda_Mitjana Habitatges Preu_Lloguer
    ## 1 -0.3310503 -0.3898376    0.08066592  -0.419433  -0.03523797
    ## 2  1.3538027  1.0265076    0.26821690   1.095186   0.70366772
    ## 
    ## Clustering vector:
    ## 366 367 368 369 370 371 372 373 374 375 376 377 378 379 380 381 382 383 384 385 
    ##   2   2   2   2   2   2   2   2   2   2   2   1   1   1   1   1   1   2   2   1 
    ## 386 387 388 389 390 391 392 393 394 395 396 397 398 399 400 401 402 403 404 406 
    ##   1   1   1   1   1   2   1   1   1   1   2   2   1   1   1   1   1   1   1   1 
    ## 408 409 410 411 413 414 415 416 417 422 424 425 426 427 428 429 430 431 432 433 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   2   1   1   1   2 
    ## 434 435 436 437 438 
    ##   1   1   1   1   1 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 217.1388 218.2520
    ##  (between_SS / total_SS =  36.4 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2021`
    ## K-means clustering with 2 clusters of sizes 47, 18
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1 -0.3254836 -0.3749484 -0.3248887 -0.2682940    -0.3192117   -0.08171599
    ## 2  1.3587261  1.0671310  1.1335414  0.8936807     1.3008017    0.73778226
    ##    Pobl_Host  Perc_Host Renda_Mitjana Habitatges Preu_Lloguer
    ## 1 -0.3302373 -0.3998693    0.07473504 -0.4189936 -0.004087009
    ## 2  1.3520507  0.9998243    0.28487889  1.0940387  0.585291003
    ## 
    ## Clustering vector:
    ## 439 440 441 442 443 444 445 446 447 448 449 450 451 452 453 454 455 456 457 458 
    ##   2   2   2   2   2   2   2   2   2   2   2   1   1   1   1   1   1   2   2   1 
    ## 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 479 
    ##   1   1   1   1   1   2   1   1   1   1   2   2   1   1   1   1   1   1   1   1 
    ## 481 482 483 484 486 487 488 489 490 495 497 498 499 500 501 502 503 504 505 506 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   2   1   1   1   2 
    ## 507 508 509 510 511 
    ##   1   1   1   1   1 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 218.2202 217.6402
    ##  (between_SS / total_SS =  36.4 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2022`
    ## K-means clustering with 2 clusters of sizes 47, 18
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1 -0.3263166 -0.3788258 -0.3282624 -0.2673669    -0.3169432   -0.08190129
    ## 2  1.3634738  1.1144432  1.1471069  0.9004638     1.2950586    0.74123344
    ##    Pobl_Host  Perc_Host Renda_Mitjana Habitatges Preu_Lloguer
    ## 1 -0.3358497 -0.4030275    0.07191048 -0.4235462  -0.08155168
    ## 2  1.3643438  0.9917268    0.28053290  1.1059261   0.76774985
    ## 
    ## Clustering vector:
    ## 512 513 514 515 516 517 518 519 520 521 522 523 524 525 526 527 528 529 530 531 
    ##   2   2   2   2   2   2   2   2   2   2   2   1   1   1   1   1   1   2   2   1 
    ## 532 533 534 535 536 537 538 539 540 541 542 543 544 545 546 547 548 549 550 552 
    ##   1   1   1   1   1   2   1   1   1   1   2   2   1   1   1   1   1   1   1   1 
    ## 554 555 556 557 559 560 561 562 563 568 570 571 572 573 574 575 576 577 578 579 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   2   1   1   1   2 
    ## 580 581 582 583 584 
    ##   1   1   1   1   1 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 214.7214 214.5320
    ##  (between_SS / total_SS =  37.6 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"      
    ## 
    ## $`2023`
    ## K-means clustering with 2 clusters of sizes 47, 18
    ## 
    ## Cluster means:
    ##    Pobl_Jove  Perc_Jove Pobl_Regio Perc_Regio Pobl_Superior Perc_Superior
    ## 1 -0.3222603 -0.3653787 -0.3315727 -0.2721109    -0.3128366   -0.07470405
    ## 2  1.3586443  1.1430597  1.1662993  0.9006382     1.2890305    0.73484016
    ##   Pobl_Host  Perc_Host Renda_Mitjana Habitatges Preu_Lloguer
    ## 1 -0.333057 -0.3882063    0.07211792 -0.4256403 -0.004184278
    ## 2  1.359801  1.0299250    0.27945843  1.1113940  0.660782989
    ## 
    ## Clustering vector:
    ## 585 586 587 588 589 590 591 592 593 594 595 596 597 598 599 600 601 602 603 604 
    ##   2   2   2   2   2   2   2   2   2   2   2   1   1   1   1   1   1   2   2   1 
    ## 605 606 607 608 609 610 611 612 613 614 615 616 617 618 619 620 621 622 623 625 
    ##   1   1   1   1   1   2   1   1   1   1   2   2   1   1   1   1   1   1   1   1 
    ## 627 628 629 630 632 633 634 635 636 641 643 644 645 646 647 648 649 650 651 652 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   2   1   1   1   2 
    ## 653 654 655 656 657 
    ##   1   1   1   1   1 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 214.7208 210.9656
    ##  (between_SS / total_SS =  37.5 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

    ## Extracció qualitat models
    resultats_ss <- lapply(llista_clusters_k2, function(cl) {
      list(
        ssw = cl$withinss,
        tssw = cl$tot.withinss,
        ssb = cl$betweenss
      )
    })

    for (any in names(resultats_ss)) {
      cat("### Any:", any, "###\n")
      print(resultats_ss[[any]])
    }

    ## ### Any: 2019 ###
    ## $ssw
    ## [1] 215.2193 217.1127
    ## 
    ## $tssw
    ## [1] 432.332
    ## 
    ## $ssb
    ## [1] 251.9156
    ## 
    ## ### Any: 2020 ###
    ## $ssw
    ## [1] 217.1388 218.2520
    ## 
    ## $tssw
    ## [1] 435.3907
    ## 
    ## $ssb
    ## [1] 249.5782
    ## 
    ## ### Any: 2021 ###
    ## $ssw
    ## [1] 218.2202 217.6402
    ## 
    ## $tssw
    ## [1] 435.8603
    ## 
    ## $ssb
    ## [1] 249.3671
    ## 
    ## ### Any: 2022 ###
    ## $ssw
    ## [1] 214.7214 214.5320
    ## 
    ## $tssw
    ## [1] 429.2534
    ## 
    ## $ssb
    ## [1] 258.2514
    ## 
    ## ### Any: 2023 ###
    ## $ssw
    ## [1] 214.7208 210.9656
    ## 
    ## $tssw
    ## [1] 425.6864
    ## 
    ## $ssb
    ## [1] 255.9559

    ## Càlcul d'índexs
    resultats_index <- list()

    for (any in names(llista_clusters_k2)) {
     cl <- llista_clusters_k2[[any]]
     ssw <- cl$tot.withinss
     ssb <- cl$betweenss

     BH <- ssw / 2
     CH <- ssb / (2 - 1)
     H <- log(ssb / ssw)
     WB <- 2 * (ssw / ssb)

     resultats_index[[any]] <- list(
      Ball_Hall = BH,
      Calinski_Harabasz = CH,
      Hartigan = H,
      WB_index = WB
     )
    }

    df_resultats <- do.call(rbind, lapply(names(resultats_index), function(any) {
     c(Any = any, resultats_index[[any]])
    }))
    kable(df_resultats, format = "markdown", col.names = c("Any", "Ball-Hall", "Calinski-Harabasz", "Hartigan", "WB Index"))

<table>
<colgroup>
<col style="width: 6%" />
<col style="width: 22%" />
<col style="width: 23%" />
<col style="width: 25%" />
<col style="width: 22%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Any</th>
<th style="text-align: left;">Ball-Hall</th>
<th style="text-align: left;">Calinski-Harabasz</th>
<th style="text-align: left;">Hartigan</th>
<th style="text-align: left;">WB Index</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">2019</td>
<td style="text-align: left;">216.16600794206</td>
<td style="text-align: left;">251.915634344067</td>
<td style="text-align: left;">-0.540099601234351</td>
<td style="text-align: left;">3.43235557419702</td>
</tr>
<tr class="even">
<td style="text-align: left;">2020</td>
<td style="text-align: left;">217.695373289979</td>
<td style="text-align: left;">249.578248124412</td>
<td style="text-align: left;">-0.556471410127016</td>
<td style="text-align: left;">3.48901196199534</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2021</td>
<td style="text-align: left;">217.930174083867</td>
<td style="text-align: left;">249.367064705702</td>
<td style="text-align: left;">-0.558395923264867</td>
<td style="text-align: left;">3.49573307671666</td>
</tr>
<tr class="even">
<td style="text-align: left;">2022</td>
<td style="text-align: left;">214.626705528839</td>
<td style="text-align: left;">258.251357944992</td>
<td style="text-align: left;">-0.508114080055158</td>
<td style="text-align: left;">3.32430709734436</td>
</tr>
<tr class="odd">
<td style="text-align: left;">2023</td>
<td style="text-align: left;">212.843192693675</td>
<td style="text-align: left;">255.955857301774</td>
<td style="text-align: left;">-0.508697893419288</td>
<td style="text-align: left;">3.3262484388897</td>
</tr>
</tbody>
</table>

    ## Representació visual clústers
    plots_clusters <- mapply(function(model, data, any) {
      fviz_cluster(model, data = data,
                   geom = "point",
                   ellipse.type = "norm",
                   main = paste("Clústers per a l'any", any))
    }, llista_clusters_k2, df_anys_norm, names(df_anys_norm), SIMPLIFY = FALSE)

    for (any in names(plots_clusters)) {
      print(plots_clusters[[any]])
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-26-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-26-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-26-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-26-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-26-5.png)

### Representació geogràfica clústers

A continuació, l’objectiu és representar geogràficament els clústers
resultants. A partir d’un mapa dels diferents barris de la ciutat de
Barcelona i associant a cada barri cada any el clúster que s’ha
considerat, es va a representar gràficament la gentrificació als
diferents barris.

    ## Assignació clústers k = 4
    df_anys_raw <- list()
    for (any in anys) {
      df_any <- df_cluster[df_cluster$Data_Referencia == any, ]
      df_norm <- df_anys_norm[[as.character(any)]]
      df_any_clean <- df_any[complete.cases(df_any[, vars]), ]
      df_any_clean$Cluster <- as.factor(llista_clusters_k4[[as.character(any)]]$cluster)
      df_anys_raw[[as.character(any)]] <- df_any_clean
    }

    ## Càrrega arxiu .shp dels barris
    shp_barris <- st_read("V2/0301040100_Barris_UNITATS_ADM.shp") %>%
      st_transform(crs = 4326) %>%
      rename(Codi_Barri = BARRI)

    ## Reading layer `0301040100_Barris_UNITATS_ADM' from data source 
    ##   `C:\Users\edlopez\OneDrive - FERMAX ELECTRONICA S.A.U\MÁSTER CD\2B\TRABAJO FIN DE MÁSTER\ENTREGUES\TFM M3\DATASET\V2\0301040100_Barris_UNITATS_ADM.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 73 features and 46 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 420812.5 ymin: 4574282 xmax: 435480.4 ymax: 4591066
    ## Projected CRS: ETRS89 / UTM zone 31N

    ## Representació gràfica mapa gentrificació k = 4
    paleta = c("#00AFBB", "#E7B800", "#FC4E07", "#567423")
    for (any in anys) {
      dades_any <- df_anys_raw[[as.character(any)]]
      dades_geo <- left_join(shp_barris, dades_any, by = "Codi_Barri")

      plot <- ggplot(dades_geo) +
        geom_sf(aes(fill = Cluster), color = "white") +
        scale_fill_manual(values = paleta) +
        labs(title = paste("Clústers amb k = 4 per barris - Any", any)) +
        theme_minimal()

      print(plot)
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-29-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-29-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-29-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-29-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-29-5.png)

Els resultats es poden repetir amb el paràmetre *k = 2:*

    ## Assignació clústers k = 2
    df_anys_raw <- list()
    for (any in anys) {
      df_any <- df_cluster[df_cluster$Data_Referencia == any, ]
      df_norm <- df_anys_norm[[as.character(any)]]
      df_any_clean <- df_any[complete.cases(df_any[, vars]), ]
      df_any_clean$Cluster <- as.factor(llista_clusters_k2[[as.character(any)]]$cluster)
      df_anys_raw[[as.character(any)]] <- df_any_clean
    }

    ## Representació gràfica mapa gentrificació k = 2
    paleta = c("#00AFBB", "#FC4E07")
    for (any in anys) {
      dades_any <- df_anys_raw[[as.character(any)]]
      dades_geo <- left_join(shp_barris, dades_any, by = "Codi_Barri")

      plot <- ggplot(dades_geo) +
        geom_sf(aes(fill = Cluster), color = "white") +
        scale_fill_manual(values = paleta) +
        labs(title = paste("Clústers amb k = 2 per barris - Any", any)) +
        theme_minimal()

      print(plot)
    }

![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-31-1.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-31-2.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-31-3.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-31-4.png)![](clustering_gentrificacio_files/figure-markdown_strict/unnamed-chunk-31-5.png)
