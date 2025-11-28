###################################  ESCUELA DE METODOS ELSOC: ESTIMADORES DE EFECTOS FIJOS Y ALEATORIOS  ###################################
#
# Autor:    Benjamín Muñoz
# Fuente:   Datos de Estudio Longitudinal Social de Chile (ELSOC)
# Código 2: Ejercicios Prácticos del Taller
# Objetivo: Introducir a los aspectos básicos de la estimación de modelos de regresión con datos de panel
# Fecha:    31 de Julio de 2025
#
####################################################     PASO 00: ENTORNO DE TRABAJO     ####################################################

## Remover objetos del entorno de trabajo
rm(list=ls())

## Fijar opciones básicas
options(scipen = 1000000, digits = 3, max.print = 10000)

## En caso de ser necesario, instalar los siguientes paquetes (sólo una vez)
#install.packages(c("here","broom","lmtest,"marginaleffects","modelsummary","ggthemes","panelr","panelView",
#                   "plm","sjlabelled","sjmisc","tidyverse"))

## Cargar paquetes
library(here)              # Gestión de directorios
library(broom)             # Tidy output de modelos
library(lmtest)            # Test de Hipótesis
library(marginaleffects)   # Cálculo de Efectos Marginales
library(modelsummary)      # Tablas de Modelos
library(ggthemes)          # Formato de gráficos
library(panelr)            # Herramientas basicas para datos de panel
library(panelView)         # Visualización de datos de panel
library(plm)               # Modelos de regresión panel
library(sjlabelled)        # Manejo de bases de datos con etiquetas
library(sjmisc)            # Funciones útiles para manejo de datos
library(tidyverse)         # Manipulación de datos

## Fijar el directorio de trabajo
setwd(here::here())

## Verificar el directorio de trabajo actual
cat("El directorio de trabajo ha sido fijado en:", getwd(), "\n")


################################################     PASO 00: IMPORTACIÓN DE DATOS PANEL     ################################################

## Importar base de datos Wide
elsoc_wide <- sjlabelled::read_stata(path = "2_Datos/ELSOC_2016_2022__Wide.dta", convert.factors = F, encoding = "UTF-8", verbose = T)

## Descripción de variable de resultado
sjmisc::frq(elsoc_wide$d01_01_w01)
sjmisc::frq(elsoc_wide$d01_02_w01)

## Crear base de datos
elsoc_wide |>
  ### Recodificar Valores Perdidos
  dplyr::mutate(across(where(is.numeric),
                       ~case_when(.x %in% c(-999, -888, -777, -666) ~ NA_real_,
                                  TRUE ~ .x)),
                across(where(is.character), 
                       ~case_when(.x %in% c("-999", "-888", "-777", "-666") ~ NA_character_,
                                  TRUE ~ .x)) ) |>
  ### Crear variable de resultado
  dplyr::mutate(across(starts_with("d01_01"),
                       ~ .x - get(sub("d01_01", "d01_02", cur_column())),
                       .names = "movil_w{str_sub(.col, -2)}")) |>
  ### Seleccionar variables del estudio
  dplyr::select(idencuesta, 
                muestra, tipo_atricion,
                starts_with("movil_"),
                starts_with("d05_03"),starts_with("d05_04"),
                starts_with("c18_08"),starts_with("c18_09"),
                starts_with("c18_10"),
                m0_sexo_w01,
                starts_with("m0_edad"),starts_with("m01"),
                starts_with("m02"),
                m53_w02, starts_with("m38_w")) |>
  ### Filtrar Muestra Original, Sin Atrición, 
  dplyr::filter(muestra == 1,
                tipo_atricion == 1) |>
  dplyr::select(-muestra, -tipo_atricion, -ends_with("_w06")) -> elsoc_selec

## Explorar base de datos
names(elsoc_selec)
summary(elsoc_selec)


#############################################     EJERCICIO 01: MANIPULACIÓN DE DATOS PANEL     #############################################

## Transformar a Formato Long
elsoc_selec |> 
  tidyr::pivot_longer(cols = -idencuesta,
                      names_to = c(".value", "wave"),
                      names_pattern = "(.*)_w(\\d{2})") |>
  dplyr::mutate(wave = as.numeric(wave)) |> 
  ### Recodificar variables
  dplyr::mutate(ambicion = ifelse(test = d05_03 > 3,  yes = 0, no = 1),
                trabajo = ifelse(test = d05_04 > 3,   yes = 0, no = 1),
                cambsoc = ifelse(test = c18_08 > 3,   yes = 0, no = 1),
                hombre  = ifelse(test = m0_sexo == 1, yes = 1, no = 0),
                educ    = car::recode(m01, "1:3=1;4:5=2;6:7=3;8:10=4"),
                ocup    = car::recode(m02, "1:3 = 1; 4:9 = 0"),
                indig   = ifelse(test = m53 == 10,    yes = 0, no = 1),
                irrel   = car::recode(m38, "1:6=0;7:9=1")) |>
  dplyr::rename(esfuerzo = c18_08, habilidades = c18_09, edad = m0_edad) |>
  dplyr::select(idencuesta, wave, movil,ambicion:irrel, esfuerzo, habilidades, edad) -> elsoc_long

## Explorar base de datos
names(elsoc_long)
summary(elsoc_long)

## Convertir los datos a un formato de panel utilizando pdata.frame
elsoc_panel_01 <- plm::pdata.frame(x = elsoc_long, index = c("idencuesta", "wave"))


## Verificar si el panel está balanceado
plm::is.pbalanced(elsoc_panel_01)         # Balance es sobre estructura de datos
summary(is.na(elsoc_panel_01))            # Revisión de valores perdidos

## Verificar el grado de desbalance
plm::punbalancedness(elsoc_panel_01) # Balance implica Gamma = 1 y Nu = 1

### Está balanceado el Panel? SÍ, está balanceado pero hay valores perdidos (listwise deletion lo desbalanceará)


## Verificar si el panel es consecutivo
summary(plm::is.pconsecutive(elsoc_panel_01)) # Hay espacios entre las olas?

### INTERPRETACIÓN: Es un panel consecutivo? Sí, no hay olas faltantes


## Obtener las dimensiones del panel
plm::pdim(elsoc_panel_01)


## Verificar si las variables están cruzadas
sjmisc::is_crossed(elsoc_panel_01$idencuesta, elsoc_panel_01$wave) # Un panel tiene una estructura cruzada y no anidada

## Verificar si las variables están anidadas
sjmisc::is_nested(elsoc_panel_01$idencuesta, elsoc_panel_01$wave)

### INTERPRETACIÓN: Datos Longitudinales en forma


## Verificar si hay valores faltantes
sjmisc::has_na(elsoc_panel_01)


## Verificar qué variables varían a través de los individuos y del tiempo
elsoc_panel_02 <- panelr::as_panel(data = elsoc_long, id = "idencuesta", wave = "wave")

panelr::are_varying(elsoc_panel_02, type = "time")
plm::pvar(elsoc_panel_01)   # Resumen Global de Variabilidad

### INTERPRETACIÓN: Hay atributos fijos como sexo e indígena


## Resumen Descriptivo
#install.packages("skimr")
summary(elsoc_panel_02)

## Tengo un panel completo?
panelr::complete_data(data = elsoc_panel_02, 
                      vars = c("movil","ambicion","trabajo","cambsoc","hombre","edad","educ","ocup","indig","irrel",
                               "esfuerzo","habilidades"), 
                      min.waves = 5) |> dim()

### INTERPRETACIÓN: NO hay observaciones completas longitudinalmente (hay NAs en, a lo menos, una ola)

panelr::complete_data(data = elsoc_panel_02, 
                      vars = c("movil","ambicion","trabajo","cambsoc","edad","educ","ocup","irrel",
                               "esfuerzo","habilidades"), 
                      min.waves = 5) |> dim()

### INTERPRETACIÓN: Los atributos FIJOS (hombre, indig) sólo aparecen en una medición por individuo


## Imputación de Variables Fijas
elsoc_long |>
  dplyr::group_by(idencuesta) |>
  dplyr::mutate(hombre = ifelse(test = wave == 1, yes = hombre, no = dplyr::first(na.omit(hombre))),
                indig  = ifelse(test = wave == 2, yes = indig,  no = dplyr::first(na.omit(indig)))) |>
  dplyr::ungroup() |>
  plm::pdata.frame(index = c("idencuesta", "wave")) -> elsoc_panel_03

## Remover Valores Perdidos
summary(is.na(elsoc_panel_03))

## Puedo usar NA Omit para Balancear el panel?
elsoc_panel_03 |> na.omit() |> plm::is.pbalanced()    # Panel desbalanceado
elsoc_panel_03 |> na.omit() |> plm::punbalancedness() # Los problemas de desbalance son menores

## Transformar en un Panel Balanceado
elsoc_panel_03 |> na.omit() |> plm::make.pbalanced(balance.type = "shared.individuals") -> elsoc_panel_df

plm::is.pbalanced(elsoc_panel_df)
plm::pdim(elsoc_panel_df)           # N Original = 1304, N Filtrado = 1139
summary(elsoc_panel_df)

## Remover objetos
rm(elsoc_long, elsoc_selec, elsoc_panel_01, elsoc_panel_02, elsoc_panel_03)


#############################################     EJERCICIO 02: ESTIMACIÓN DE MODELOS PANEL     #############################################

## Estimar un Modelo con el Estimador Pooled OLS
m_pooled <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel,
                     data = elsoc_panel_df, model = "pooling")
summary(m_pooled)


## Estimar un Modelo con el Estimador de Primeras Diferencias
m_fd_int <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel,
                     data = elsoc_panel_df, model = "fd", effect = "individual")

plm::has.intercept(m_fd_int)
summary(m_fd_int) # Por defecto, incluye un intercepto que captura un cambio promedio en el tiempo en la variable de resultado (no explicado)
# Tendencia sistemática en el tiempo que no se puede capturar con los predictores diferenciados


m_fd <- plm::plm(formula = movil ~ -1 + ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel,
                 data = elsoc_panel_df, model = "fd", effect = "individual")

plm::has.intercept(m_fd)
summary(m_fd)

### INTERPRETACIÓN: Cómo se Interpreta un Efecto? El cambio en la variable X (ambición) entre dos períodos está asociado a una 
###                 disminución de 0.00190 en la percepción de movilidad social, manteniendo constantes las demás variables.


## Estimar un Modelo con el Estimador Between
m_between <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel,
                      data = elsoc_panel_df, model = "between", effect = "individual")
summary(m_between)


## Estimar un Modelo con el Estimador Within (Efectos Fijos)
m_within <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel,
                     data = elsoc_panel_df, model = "within", effect = "individual")
summary(m_within)

### INTERPRETACIÓN: Qué pasó con el Coeficiente Estimado de hombre? Perfectamente Colineal con Efectos Fijos (Time-Invariant)


## Explorar detalles del objeto
plm::has.intercept(m_within) # No hay intercepto

plm::fixef(m_within)         # Extraer Efectos Fijos
plm::fixef(m_within) |> length()
plm::fixef(m_within) |> summary() # NO sobre-interpretar: Estimador Inconsistente

## Qué son en la Práctica los Efectos Fijos?
fe_as_lvl  <- plm::fixef(m_within, type = "level")
fe_as_dev  <- plm::fixef(m_within, type = "dmean")
global_int <- plm::within_intercept(m_within)
all.equal(global_int + fe_as_dev, fe_as_lvl, check.attributes = FALSE) # TRUE


## Estimar un Modelo con el Estimador de Efectos Aleatorios
m_re <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel,
                 data = elsoc_panel_df, model = "random", effect = "individual",
                 random.method = "swar")
summary(m_re)

## Explorar detalles del objeto
plm::ranef(m_re)
summary(plm::ranef(m_re)) 

## Resumen de Modelos
modelsummary::modelsummary(models = list("Pooled" = m_pooled, "FD" = m_fd, 
                                         "Between" = m_between, "Within" = m_within, "RE" = m_re), 
                           fmt = 3,
                           stars = T,
                           coef_rename = c("Intercepto","Importante: Ambición","Importante: Trabajo Duro",
                                           "Cambio Social Posible","Premia Esfuerzo","Premia Talento",
                                           "Hombre","Edad","Educación","Ocupación","Indígena","Irreligioso"))

### INTERPRETACIÓN: Qué podemos concluir de esta Tabla?  Educación e Indígena son interesantes como potenciales efectos


######################################     EJERCICIO 03:  INFERENCIA ESTADÍSTICA CON MODELOS PANEL     ######################################

# Errores Estándares Clusterizados
modelsummary::modelsummary(models = list("Pooled" = m_pooled, "FD" = m_fd, 
                                         "Between" = m_between, "Within" = m_within, "RE" = m_re), 
                           vcov = list(plm::vcovHC(m_pooled,  type = "HC1", cluster = "group"),
                                       plm::vcovHC(m_fd,      type = "HC1", cluster = "group"),
                                       vcov(m_between),
                                       plm::vcovHC(m_within,  type = "HC1", cluster = "group"),
                                       plm::vcovHC(m_re,      type = "HC1", cluster = "group")),
                           fmt = 3,
                           stars = T,
                           coef_rename = c("Intercepto","Importante: Ambición","Importante: Trabajo Duro",
                                           "Cambio Social Posible","Premia Esfuerzo","Premia Talento",
                                           "Hombre","Edad","Educación","Ocupación","Indígena","Irreligioso"))


# Test Basicos

## Hay Heterogeneidad Individual o Temporal?
plm::pFtest(x = m_within, z = m_pooled)

## Test de Multiplicador de Lagrange para Heterogeneidad Individual o de Tiempo
plm::plmtest(x = m_pooled)

## Test de Combinalidad/Poolability de los Datos
plm::pooltest(x = m_pooled, z = m_within)

### INTERPRETACIÓN: Qué podemos concluir de estos tests? Hay heterogeneidad individual a ser modelada


## Test de Hausman: Usar Efectos Fijos o Aleatorios?
plm::phtest(x = m_within, x2 = m_re)

### INTERPRETACIÓN: Qué podemos concluir de este test? RE es preferible si p-valor < 0.05


# B3) Test de Correlación Serial

## Para MCO Pooled
plm::pwtest(x = m_pooled, effect = "individual") 
plm::pwtest(x = m_pooled, effect = "time")       

### Para Efectos Fijos
plm::pwartest(x = m_within) 

### INTERPRETACIÓN: Qué podemos concluir de estos tests? # La correlación serial se basa en las unidades


################################################     EJERCICIO 04: MODELAMIENTO AVANZADO     ################################################

## Crear variable numérica de tiempo
elsoc_panel_df |>
  dplyr::mutate(time = as.numeric(as.factor(wave))) -> elsoc_panel_esp

# A) INCORPORACIÓN DE HETEROGENEIDAD TEMPORAL
m_pooled_t1 <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + 
                          irrel + time,
                        data = elsoc_panel_esp, model = "pooling")

m_pooled_t2 <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + 
                          irrel + time + I(time^2),
                        data = elsoc_panel_esp, model = "pooling")

m_pooled_td <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + 
                          irrel + wave,
                        data = elsoc_panel_esp, model = "pooling")

m_within_td <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + 
                          irrel + wave,
                        data = elsoc_panel_esp, model = "within", effect = "individual")
m_within_tw <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + 
                          irrel,
                        data = elsoc_panel_esp, model = "within", effect = "twoways")

m_re_tw <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + 
                      irrel + wave,
                    data = elsoc_panel_esp, model = "random", effect = "individual")

## Resumen de Modelos
modelsummary::modelsummary(models = list("T. Lineal" = m_pooled_t1, "T. Cuadrático" = m_pooled_t2, 
                                         "T. Heterogéneo" = m_pooled_td, "FE + T. Lineal" = m_within_td, 
                                         "TWFE" = m_within_tw, "RE + T. Heterogéneo" = m_re_tw), 
                           fmt = 3,
                           stars = T,
                           coef_rename = c("Intercepto","Importante: Ambición","Importante: Trabajo Duro",
                                           "Cambio Social Posible","Premia Esfuerzo","Premia Talento",
                                           "Hombre","Edad","Educación","Ocupación","Indígena","Irreligioso",
                                           "Tiempo","Tiempo^2","Ola: 2017","Ola: 2018","Ola: 2019","Ola: 2021"))


# B) INCORPORACIÓN DE EFECTOS DE VARIABLES CONSTANTES EN EL TIEMPO

summary(m_fd)     # No estima un coeficiente para hombre y indig
summary(m_within) # No estima un coeficiente para hombre y indig

## En Primeras Diferencias
m_fd_int1 <- plm::plm(formula = movil ~ -1 + ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel + 
                        time + time*hombre,
                      data = elsoc_panel_esp, model = "fd", effect = "individual")
m_fd_int2 <- plm::plm(formula = movil ~ -1 + ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel + 
                        wave + wave*indig,
                      data = elsoc_panel_esp, model = "fd", effect = "individual")

## En Efectos Fijos
m_fe_int1 <- plm::plm(formula = movil ~ -1 + ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel + 
                        time + time*hombre,
                      data = elsoc_panel_esp, model = "within", effect = "individual")
m_fe_int2 <- plm::plm(formula = movil ~ -1 + ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + educ + ocup + indig + irrel + 
                        wave + wave*indig,
                      data = elsoc_panel_esp, model = "within", effect = "individual")

## Resumen
modelsummary::modelsummary(models = list("FD, Hombre x Lineal" = m_fd_int1, "FD, Indígena x T. Dummies" = m_fd_int2, 
                                         "FE, Hombre x Lineal" = m_fe_int1, "FE, Indígena x T. Dummies" = m_fe_int2), 
                           fmt = 3,
                           stars = T,
                           coef_rename = c("Ambición","Trabajo Duro","Cambio Social Posible","Premia Esfuerzo","Premia Talento",
                                           "Edad","Educación","Ocupación","Irreligioso",
                                           "Tiempo x Hombre", "Ola: 2016","Ola: 2017","Ola: 2018","Ola: 2019",
                                           "2017 x Indígena","2018 x Indígena","2019 x Indígena","2021 x Indígena",
                                           "Tiempo"))


# C) INCORPORACIÓN DE VARIABLES INDEPENDIENTES REZAGADAS
m_within_lag <- plm::plm(formula = movil ~ ambicion + trabajo + cambsoc + esfuerzo + habilidades + hombre + edad + ocup + indig + irrel + 
                           lag(educ, 1),
                         data = elsoc_panel_esp, model = "within", effect = "individual")
summary(m_within_lag)

## Resumen
modelsummary::modelsummary(models = list("FE" = m_within,"FE + Lag" = m_within_lag), 
                           fmt = 3,
                           stars = T,
                           coef_rename = c("Ambición","Trabajo Duro","Cambio Social Posible","Premia Esfuerzo","Premia Talento",
                                           "Edad","Educación","Ocupación","Irreligioso","Educación Rezagada"))


#################################################     TECHNICAL DETAILS FOR REPLICATION     #################################################

#Macbook Air M2
#mac OS Sequoia 15.5
#
#A .Rproj file was used in the development of this Code.
#sessionInfo()
#R version 4.4.2 (2024-10-31)
#Platform: aarch64-apple-darwin20
#Running under: macOS Sequoia 15.5
#
#Matrix products: default
#BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
#LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
#
#locale:
#[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#time zone: America/New_York
#tzcode source: internal
#
#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#[1] ggthemes_5.1.0         modelsummary_2.0.0     marginaleffects_0.28.0 broom_1.0.9            texreg_1.39.3          lubridate_1.9.4       
#[7] forcats_1.0.0          stringr_1.5.1          dplyr_1.1.4            purrr_1.0.4            readr_2.1.5            tidyr_1.3.1           
#[13] tibble_3.2.1           ggplot2_3.5.2          tidyverse_2.0.0        sjmisc_2.8.10          sjlabelled_1.2.0       plm_2.6-6             
#[19] panelView_1.1.18       panelr_0.7.8           lme4_1.1-37            Matrix_1.7-1           lmtest_0.9-40          zoo_1.8-14            
#[25] here_1.0.1            
#
#loaded via a namespace (and not attached):
#[1] rstudioapi_0.16.0   jsonlite_1.8.8      datawizard_0.13.0   magrittr_2.0.3      estimability_1.5    farver_2.1.2        nloptr_2.2.1       
#[8] rmarkdown_2.27      vctrs_0.6.5         minqa_1.2.8         base64enc_0.1-3     effectsize_0.8.9    htmltools_0.5.8.1   haven_2.5.5        
#[15] Formula_1.2-5       sass_0.4.9          parallelly_1.43.0   bslib_0.7.0         sandwich_3.1-1      emmeans_1.10.1      cachem_1.1.0       
#[22] lifecycle_1.0.4     pkgconfig_2.0.3     R6_2.6.1            fastmap_1.2.0       rbibutils_2.3       future_1.34.0       collapse_2.0.14    
#[29] snakecase_0.11.1    digest_0.6.37       numDeriv_2016.8-1.1 colorspace_2.1-1    miscTools_0.6-28    rprojroot_2.0.4     fixest_0.12.1      
#[36] pkgload_1.3.4       lfe_3.0-0           fansi_1.0.6         timechange_0.3.0    tinytable_0.2.1     httr_1.4.7          abind_1.4-8        
#[43] compiler_4.4.2      wooldridge_1.4-3    bit64_4.6.0-1       withr_3.0.2         pander_0.6.5        backports_1.5.0     carData_3.0-5      
#[50] performance_0.12.4  highr_0.11          MASS_7.3-61         tools_4.4.2         future.apply_1.11.3 glue_1.8.0          nlme_3.1-166       
#[57] stringmagic_1.1.2   grid_4.4.2          checkmate_2.3.1     generics_0.1.3      gtable_0.3.6        tzdb_0.5.0          data.table_1.17.0  
#[64] hms_1.1.3           xml2_1.3.6          car_3.1-3           utf8_1.2.4          tables_0.9.25       pillar_1.10.2       vroom_1.6.5        
#[71] splines_4.4.2       lattice_0.22-6      bit_4.6.0           dreamerr_1.4.0      tidyselect_1.2.1    knitr_1.48          reformulas_0.4.0   
#[78] gridExtra_2.3       svglite_2.1.3       xfun_0.45           skimr_2.1.5         stringi_1.8.7       boot_1.3-31         kableExtra_1.4.0   
#[85] evaluate_0.24.0     codetools_0.2-20    cli_3.6.4           xtable_1.8-4        parameters_0.23.0   systemfonts_1.1.0   Rdpack_2.6.3       
#[92] repr_1.1.7          munsell_0.5.1       jquerylib_0.1.4     Rcpp_1.0.14         globals_0.16.3      coda_0.19-4.1       bdsmatrix_1.3-7    
#[99] parallel_4.4.2      bayestestR_0.15.0   listenv_0.9.1       viridisLite_0.4.2   mvtnorm_1.3-3       scales_1.3.0        insight_1.3.1      
#[106] crayon_1.5.3        maxLik_1.5-2.1      rlang_1.1.6         jtools_2.2.2