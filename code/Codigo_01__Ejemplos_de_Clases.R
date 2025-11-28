###################################  ESCUELA DE METODOS ELSOC: ESTIMADORES DE EFECTOS FIJOS Y ALEATORIOS  ###################################
#
# Autor:    Benjamín Muñoz
# Fuente:   Datos de Estudio Longitudinal Social de Chile (ELSOC)
# Código 1: Ejemplos Incluidos en Diapositivas del Taller
# Objetivo: Introducir a los aspectos básicos de la estimación de modelos de regresión con datos de panel
# Fecha:    31 de Julio de 2025
#
####################################################     PASO 00: ENTORNO DE TRABAJO     ####################################################

## Remover objetos del entorno de trabajo
rm(list=ls())

## Fijar opciones básicas
options(scipen = 1000000, digits = 3, max.print = 10000)

## En caso de ser necesario, instalar los siguientes paquetes (sólo una vez)
#install.packages(c("here","broom","lmtest","marginaleffects","ggthemes","panelr","panelView","plm","sjlabelled","sjmisc","texreg,"tidyverse","wooldridge"))

## Cargar paquetes
library(here)              # Gestión de directorios
library(lmtest)            # Test de Hipótesis
library(panelr)            # Herramientas basicas para datos de panel
library(panelView)         # Visualización de datos de panel
library(plm)               # Modelos de regresión panel
library(sjlabelled)        # Manejo de bases de datos con etiquetas
library(sjmisc)            # Funciones útiles para manejo de datos
library(texreg)            # Tablas de regresión
library(tidyverse)         # Manipulación de datos

## Fijar el directorio de trabajo
setwd(here::here())

## Verificar el directorio de trabajo actual
cat("El directorio de trabajo ha sido fijado en:", getwd(), "\n")


############################################     EJEMPLO 00:  INTRODUCCIÓN AL ANÁLISIS PANEL     ############################################

## Cargar datos de ELSOC (Long)
elsoc_long <- sjlabelled::read_stata(path = "2_Datos/ELSOC_2016_2022__Long.dta", convert.factors = F, encoding = "UTF-8", verbose = T)

## Creación de Datos Recodificados de Posición Ideológica
elsoc_long |>
  ### Recodificar Valores Perdidos
  dplyr::mutate(dplyr::across(where(is.numeric),
                       ~dplyr::case_when(.x %in% c(-999, -888, -777, -666) ~ NA_real_,
                                  TRUE ~ .x)),
                dplyr::across(where(is.character), 
                       ~dplyr::case_when(.x %in% c("-999", "-888", "-777", "-666") ~ NA_character_,
                                  TRUE ~ .x)) ) |>
  ### Filtrar Muestra Original, Sin Atrición, 
  dplyr::filter(muestra == 1, tipo_atricion == 1) |>
  ### Filtrar Observaciones con NAs en c15 en una o varias olas
  dplyr::group_by(idencuesta) |>
  dplyr::mutate(f = sum(c15)) |> 
  ungroup() |> filter(is.na(f) == F) |>
  ### Recodificar Valores de Variables
  dplyr::mutate(ideol = factor(car::recode(c15, "c(0,1,2,3,4) = 'Izquierda';5='Centro';c(6,7,8,9,10)='Derecha';c(11,12)='Ninguno'"),
                               levels = c("Izquierda", "Centro", "Derecha", "Ninguno")),
                ola = factor(ola, levels = 1:6, labels = c("2016", "2017", "2018", "2019", "2021", "2022"))
                ) |>  
  ### Seleccionar Variables de Interés
  dplyr::select(idencuesta, ola, ideol) -> elsoc_ideol
  
  
## Cuadro 1: Frecuencia por OLA
elsoc_ideol |>
  ### Calcular Frecuencias por Grupo
  dplyr::group_by(ola, ideol) |>
  dplyr::summarise(freq = n(), .groups = 'drop') |>
  ### Calcular Porcentajes
  dplyr::group_by(ola) |>
  dplyr::mutate(percentage = round((freq / sum(freq)) * 100, 3)) |>
  dplyr::select(-freq) |>
  ### Definir Formato Definitivo
  tidyr::pivot_wider(names_from = ola, values_from = percentage, values_fill = list(percentage = 0), names_prefix = "w_") |>
  kableExtra::kable(booktabs = TRUE, digits = 2, caption = "Posición Ideológica por Ola", 
                    col.names = c("Posición","2016","2017","2018","2019","2021","2022")) |>
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))


## Cuadro 2: Consistencia de Respuestas
elsoc_ideol |>
  ### Calcular Patrones de Respuestas
  dplyr::group_by(idencuesta) |>
  dplyr::summarise(consistency = ifelse(length(unique(ideol)) == 1, 
                                        paste(unique(ideol), "Siempre", sep = "-"), "Cambia de Posicion")) |>
  ### Calcular Frecuencias por Grupo
  dplyr::group_by(consistency) |>
  dplyr::summarise(freq = n(), .groups = 'drop') |>
  ### Calcular Porcentajes
  dplyr::mutate(percentage = round((freq / sum(freq)) * 100, 2)) |>
  dplyr::select(consistency, percentage) |>
  ### Definir Formato Definitivo
  kableExtra::kable(booktabs = TRUE, digits = 2, caption = "Consistencia de Posición Ideológica", 
                    col.names = c("Patrón","Porcentaje")) |>
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))


## Extra 1: Combinaciones de Respuestas (No incluido en las diapositivas)
elsoc_ideol |>
  ### Transformar a Formato Wide
  tidyr::pivot_wider(names_from = ola, values_from = ideol, names_prefix = "w_") |>
  ### Crear combinaciones de respuestas
  tidyr::unite("combinacion", starts_with("w_"), sep = " / ") |> 
  ### Calcular Frecuencias por Grupo
  dplyr::group_by(combinacion) |>
  dplyr::summarise(freq = n(), .groups = 'drop') |>
  ### Calcular Porcentajes
  dplyr::mutate(percentage = round((freq / sum(freq)) * 100, 2)) |>
  dplyr::arrange(-percentage) 


#################################################     EJEMPLO 01: MANIPULACIÓN DE DATOS     #################################################

## Transformación de Datos Long a Wide
elsoc_long |>
  dplyr::select(idencuesta, ola, c15) |> 
  tidyr::pivot_wider(names_from = ola, values_from = c15, names_prefix = "w_") |> head(3)


## Cargar datos de ELSOC (Wide)
elsoc_wide <- sjlabelled::read_stata(path = "2_Datos/ELSOC_2016_2022__Wide.dta", convert.factors = F, encoding = "UTF-8", verbose = T)

## Transformación de Datos Wide a Long
elsoc_wide |>
  dplyr::select(idencuesta, starts_with("c15")) |> 
  tidyr::pivot_longer(cols = starts_with("c15"), names_to = "ola", values_to = "c15") |> 
  dplyr::mutate(ola = stringr::str_remove(string = ola, pattern = "^c15_w0")) |> head(6)


#############################################     EJEMPLO 02:  VISUALIZACIÓN DE DATOS PANEL     #############################################

## Generar un gráfico de Valores Perdidos (Figura no corresponde a diapositivas por muestreo aleatorio)
elsoc_long |>
  ### Filtrar: Muestra Original y 25 Entrevistados
  filter(muestra == 1) |>
  (\(df) {
    ids <- df |>
      dplyr::distinct(idencuesta) |>
      dplyr::slice_sample(n = 25) |>
      dplyr::pull(idencuesta)
    
    df |> dplyr::filter(idencuesta %in% ids)
  })() |>
  ### Seleccionar Variables de Interés
  dplyr::select(idencuesta, ola, c15, m0_sexo) |>
  ### Crear Gráfico de Valores Perdidos
  panelView::panelview(formula = c15 ~ 1,
                       index   = c("idencuesta", "ola"),
                       type    = "miss")


################################################     EJEMPLO 03: DEPENDENCIA ESTADISTICA     ################################################

# EJEMPLO 1: ELSOC

## Cuadro 3: Calcular Estadísticos Descriptivos (incluyendo Correlación Serial)
elsoc_wide |>
  ### Recodificar valores perdidos
  dplyr::mutate(dplyr::across(where(is.numeric),  
                              ~dplyr::case_when(.x %in% c(-999, -888, -777, -666)         ~ NA_real_,     TRUE ~ .x)),
                dplyr::across(where(is.character), 
                              ~dplyr::case_when(.x %in% c("-999", "-888", "-777", "-666") ~ NA_character_,TRUE ~ .x)) ) |>
  ### Filtrar muestra original
  dplyr::filter(muestra == 1) |>
  ### Seleccionar variables de interés
  dplyr::select(idencuesta,contains("d01_01_")) |> 
  ### Eliminar casos con atrición o No respuesta
  dplyr::mutate(filter = rowSums(across(d01_01_w01:d01_01_w06, is.na))) |> 
  dplyr::filter(filter == 0)  |>
  ### Cálculo de estadísticos descriptivos
  dplyr::summarize(dplyr::across(d01_01_w01:d01_01_w06, list(n = ~ sum(!is.na(.)),
                                                      mean = ~ mean(., na.rm = TRUE),
                                                      sd = ~ sd(., na.rm = TRUE)),
                          .names = "{fn}_{col}"),
                   cor_A_w01_w02 = cor(d01_01_w01, d01_01_w02, use = "complete.obs"),
                   cor_A_w02_w03 = cor(d01_01_w02, d01_01_w03, use = "complete.obs"),
                   cor_A_w03_w04 = cor(d01_01_w03, d01_01_w04, use = "complete.obs"),
                   cor_A_w04_w05 = cor(d01_01_w04, d01_01_w05, use = "complete.obs"),
                   cor_A_w05_w06 = cor(d01_01_w05, d01_01_w06, use = "complete.obs"),
                   cor_B_w01_w02 = cor(d01_01_w01, d01_01_w02, use = "complete.obs"),
                   cor_B_w01_w03 = cor(d01_01_w01, d01_01_w03, use = "complete.obs"),
                   cor_B_w01_w04 = cor(d01_01_w01, d01_01_w04, use = "complete.obs"),
                   cor_B_w01_w05 = cor(d01_01_w01, d01_01_w05, use = "complete.obs"),
                   cor_B_w01_w06 = cor(d01_01_w01, d01_01_w06, use = "complete.obs") ) |> 
  dplyr::mutate(cor_A_w01_w01 = NA_real_, cor_B_w01_w01 = NA_real_) |> 
  ### Transformar a formato long (1 fila por ola)
  tidyr::pivot_longer(cols = everything(), names_to = c(".value", "wave"), names_pattern = "(.*)_(.*)") |>
  ### Recodificar y renombrar variables
  dplyr::mutate(cor_A = coalesce(cor_A_w01, cor_A_w02, cor_A_w03, cor_A_w04, cor_A_w05)) |>
  dplyr::mutate(wave = paste0(stringr::str_to_title(wave),": ",c(2016, 2017, 2018, 2019, 2021, 2022)) ) |>
  dplyr::rename(n = n_d01_01, mean = mean_d01_01, sd = sd_d01_01, cor_B = cor_B_w01) |>
  dplyr::select(wave, n, mean, sd, cor_A, cor_B) |> 
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) |>
  as.data.frame()


# EJEMPLO 2: WAGEPAN

## Activar base de datos
#library(wooldridge)

## Cuadro 4: Calcular Correlación Serial antes y después de Demeaning
wooldridge::wagepan |>
  ### Seleccionar variables relevantes
  dplyr::select(nr, year, lwage) |>
  ### Transformar a formato wide
  tidyr::pivot_wider(names_from = year, values_from = lwage, names_prefix = "lw_") |>
  ### Calcular demeaned variables
  dplyr::rowwise() |> dplyr::mutate(Mean = mean(c_across(starts_with("lw")), na.rm = TRUE)) |>
  dplyr::mutate(across(starts_with("lw"), ~ . - Mean, .names = "dm{.col}")) |> ungroup() |>
  dplyr::select(starts_with("lw"), starts_with("dm")) |>
  dplyr::summarise(
    #### Correlación Serial: Valores originales con respecto a t=1
    dplyr::across(starts_with("lw_")[-8], ~ cor(., get(paste0("lw_", as.numeric(sub("lw_", "", cur_column())) + 1)), 
                                         use = "complete.obs"), .names = "cor_lw_{col}"),
    #### Correlación Serial: Valores demeaned con respecto a t=1
    dplyr::across(starts_with("dmlw_")[-8], ~ cor(., get(paste0("dmlw_", as.numeric(sub("dmlw_", "", cur_column())) + 1)), 
                                           use = "complete.obs"), .names = "cor_dmlw_{col}")
  ) |> dplyr::rename_with(~ gsub("cor_(lw_|cor_next_|dmlw_)", "", .)) |> 
  ### Formato Final
  tidyr::pivot_longer(cols = everything(), names_to = c("type", "year"), names_sep = "_") |>
  tidyr::pivot_wider(names_from = year, values_from = value) |>
  dplyr::mutate(var = c("Raw","Demeaned")) |>
  setNames(nm = c("type", paste0("Y", 1981:1987),"Variable")) |>
  dplyr::select(Variable, Y1981:Y1987)

## Remover objetos usados
rm(elsoc_long, elsoc_wide, elsoc_ideol)
  

##################################################     EJEMPLO 04: SIMULACIÓN DE DATOS     ##################################################

## Parámetros de la simulación
N    <- 5000                   # Número de individuos
T    <- 5                      # Número de períodos

## Fijar semilla para reproducibilidad
set.seed(7303)  

## A) Generar datos: DGP MCO Pooled (No hay heterogeneidad individual)
data.frame(id    = rep(1:N, each = T), 
           time  = rep(1:T, N) )  |> 
  dplyr::group_by(id) |> 
  dplyr::ungroup() |>
  dplyr::mutate(sexo     = rep(sample(0:1, N, replace = TRUE), each = T),             # V. constante en el tiempo
                ideol    = round(scales::rescale(rnorm(N * T, 5, 10), to = c(0,10))), # V. cambiante en el tiempo
                ingresos = abs(rnorm(N * T, mean = 50, sd = 25)),                     # V. cambiante en el tiempo
                apr_pres = 15 + 1.3 * sexo + 3 * ideol + 0.4 * ingresos + rnorm(n(), 0, 10) 
  ) -> df_pooled


## Fijar semilla para reproducibilidad
set.seed(7303)  

## B) Generar datos: DGP Efectos Fijos (Hay heterogeneidad individual correlacionada con X)
data.frame(id    = rep(1:N, each = T), # ID de los individuos
           time  = rep(1:T, N)         # Periodos de tiempo 
)  |> 
  dplyr::group_by(id) |> 
  dplyr::mutate(alpha = unique(rnorm(N, 0, 8)[id]) ) |> # Heterogeneidad individual
  dplyr::ungroup() |>
  dplyr::mutate(sexo     = rep(ifelse(alpha > median(alpha), 1, 0)),                          # V. constante en el tiempo
                ideol    = round(scales::rescale(alpha + rnorm(N * T, 5, 10), to = c(0,10))), # V. cambiante en el tiempo
                ingresos = abs(alpha + rnorm(N * T, mean = 50, sd = 25)),                     # V. cambiante en el tiempo
                apr_pres = 15 + 1.3 * sexo + 3 * ideol + 0.4 * ingresos + 1 * alpha  + rnorm(n(), 0, 10)
  ) -> df_fe


## Fijar semilla para reproducibilidad
set.seed(7303)  

## C) Generar datos: DGP Efectos Aleatorios (Hay heterogeneidad individual no correlacionada con X)
data.frame(id    = rep(1:N, each = T), 
           time  = rep(1:T, N) )  |> 
  dplyr::group_by(id) |> 
  dplyr::mutate(alpha = unique(rnorm(N, 0, 8)[id]) ) |> # Heterogeneidad individual
  dplyr::ungroup() |>
  dplyr::mutate(sexo     = rep(sample(0:1, N, replace = TRUE), each = T),             # V. constante en el tiempo
                ideol    = round(scales::rescale(rnorm(N * T, 5, 10), to = c(0,10))), # V. cambiante en el tiempo
                ingresos = abs(rnorm(N * T, mean = 50, sd = 25)),                     # V. cambiante en el tiempo
                apr_pres = 15 + 1.3 * sexo + 3 * ideol + 0.4 * ingresos + 1 * alpha  + rnorm(n(), 0, 10) 
  ) -> df_re


###############################################     EJEMPLO 05:  MCO CON DATOS COMBINADOS     ###############################################

## Estimación de modelos usando Estimador Pooled MCO
m01_a <-       lm(apr_pres ~ sexo + ideol + ingresos,         data = df_pooled)
m01_b <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_pooled, index = c("id", "time"), model = "pooling")
m01_c <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_fe,     index = c("id", "time"), model = "pooling")
m01_d <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_fe,     index = c("id", "time"), model = "pooling")
m01_e <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_re,     index = c("id", "time"), model = "pooling")
m01_f <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_re,     index = c("id", "time"), model = "pooling")

## Valores de los Párametros: Intercepto = 15, Sexo = 1.3, Ideología = 3, Ingresos = 0.4

## Cuadro 7: Estimación con Pooled MCO según Tipo de Datos
### Extraer coeficientes estimados
data.frame(Verdad = c(15, 1.3, 3, 0.4),
           LM_Pool  = coef(m01_a),
           PLM_Pool = coef(m01_b),
           PLM_FE_1 = coef(m01_c)[1:4],
           PLM_FE_2 = coef(m01_d),
           PLM_RE_1 = coef(m01_e)[1:4],
           PLM_RE_2 = coef(m01_f)) |>
  tibble::rownames_to_column() |>
  ### Definir Formato Definitivo
  kableExtra::kable(booktabs = TRUE, digits = 3, caption = "Estimador Pooled MCO", #format = "latex",
                  col.names = c("Parámetro","Valor","LM","PLM","PLM (FE 1)","PLM (FE 2)","PLM (RE 1)","PLM (RE 2)")) |>
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## Visualización de todos los resultados (No incluidos en diapositivas)
texreg::screenreg(list(m01_a, m01_b, m01_c, m01_d, m01_e, m01_f), digits = 3)


#################################################     EJEMPLO 05:  PRIMERAS DIFERENCIAS     #################################################

## Estimación de modelos usando Estimador First Differences
m02_a <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_pooled, index = c("id", "time"), model = "fd")
m02_b <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_fe,     index = c("id", "time"), model = "fd")
m02_c <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_fe,     index = c("id", "time"), model = "fd")
m02_d <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_re,     index = c("id", "time"), model = "fd")
m02_e <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_re,     index = c("id", "time"), model = "fd")

## Valores de los Párametros: Intercepto = 15, Sexo = 1.3, Ideología = 3, Ingresos = 0.4

## Cuadro 8: Estimación con First Differences según Tipo de Datos
### Extraer coeficientes estimados
data.frame(Verdad = c(15, 1.3, 3, 0.4),
           PLM_Pool = c(coef(m02_a)[1],NA,coef(m02_a)[2:3]),
           PLM_FE_1 = c(coef(m02_b)[1],NA,coef(m02_b)[2:3]),
           PLM_FE_2 = c(coef(m02_c)[1],NA,coef(m02_c)[2:3]),
           PLM_RE_1 = c(coef(m02_d)[1],NA,coef(m02_d)[2:3]),
           PLM_RE_2 = c(coef(m02_e)[1],NA,coef(m02_e)[2:3]))  |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = c("Intercepto","Sexo","Ideología","Ingresos")) |>
  ### Definir Formato Definitivo
  kableExtra::kable(booktabs = TRUE, digits = 3, caption = "Estimador de Primeras Diferencias", #format = "latex",
                    col.names = c("Parámetro","Valor","PLM","PLM (FE 1)","PLM (FE 2)","PLM (RE 1)","PLM (RE 2)")) |>
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## Visualización de todos los resultados (No incluidos en diapositivas)
texreg::screenreg(list(m02_a, m02_b, m02_c, m02_d, m02_e), digits = 3)


#####################################################     EJEMPLO 06: EFECTOS FIJOS     #####################################################

## Estimación de modelos usando Estimador Within (Unit Fixed Effects)
m03_a <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_pooled, index = c("id", "time"), model = "within", effect = "individual")
m03_b <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_fe,     index = c("id", "time"), model = "within", effect = "individual")
m03_c <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_fe,     index = c("id", "time"), model = "within", effect = "individual")
m03_d <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_re,     index = c("id", "time"), model = "within", effect = "individual")
m03_e <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_re,     index = c("id", "time"), model = "within", effect = "individual")

## Valores de los Párametros: Intercepto = 15, Sexo = 1.3, Ideología = 3, Ingresos = 0.4

## Cuadro 9: Estimación con Within Estimator según Tipo de Datos
### Extraer coeficientes estimados
data.frame(Verdad = c(15, 1.3, 3, 0.4),
           PLM_Pool = c(NA,NA,coef(m03_a)),
           PLM_FE_1 = c(NA,NA,coef(m03_b)),
           PLM_FE_2 = c(NA,NA,coef(m03_c)),
           PLM_RE_1 = c(NA,NA,coef(m03_d)),
           PLM_RE_2 = c(NA,NA,coef(m03_e)))  |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = c("Intercepto","Sexo","Ideología","Ingresos")) |>
  ### Definir Formato Definitivo
  kableExtra::kable(booktabs = TRUE, digits = 3, caption = "Estimador de Efectos Fijos", #format = "latex",
                    col.names = c("Parámetro","Valor","PLM","PLM (FE 1)","PLM (FE 2)","PLM (RE 1)","PLM (RE 2)")) |>
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## Visualización de todos los resultados (No incluidos en diapositivas)
texreg::screenreg(list(m03_a, m03_b, m03_c, m03_d, m03_e), digits = 3)


###################################################     EJEMPLO 07: EFECTOS ALEATORIOS     ##################################################

## Estimación de modelos usando Estimador FGLS (Unit Random Effects)
m04_a <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_pooled, index = c("id", "time"), model = "random", effect = "individual")
m04_b <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_fe,     index = c("id", "time"), model = "random", effect = "individual")
m04_c <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_fe,     index = c("id", "time"), model = "random", effect = "individual")
m04_d <- plm::plm(apr_pres ~ sexo + ideol + ingresos + alpha, data = df_re,     index = c("id", "time"), model = "random", effect = "individual")
m04_e <- plm::plm(apr_pres ~ sexo + ideol + ingresos,         data = df_re,     index = c("id", "time"), model = "random", effect = "individual")

## Valores de los Párametros: Intercepto = 15, Sexo = 1.3, Ideología = 3, Ingresos = 0.4

## Cuadro 10: Estimación con FGLS Estimator según Tipo de Datos
### Extraer coeficientes estimados
data.frame(Verdad = c(15, 1.3, 3, 0.4),
           PLM_Pool = coef(m04_a)[1:4],
           PLM_FE_1 = coef(m04_b)[1:4],
           PLM_FE_2 = coef(m04_c)[1:4],
           PLM_RE_1 = coef(m04_d)[1:4],
           PLM_RE_2 = coef(m04_e)[1:4] )  |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = c("Intercepto","Sexo","Ideología","Ingresos")) |>
  ### Definir Formato Definitivo
  kableExtra::kable(booktabs = TRUE, digits = 3, caption = "Estimador de Efectos Aleatorios", #format = "latex",
                    col.names = c("Parámetro","Valor","PLM","PLM (FE 1)","PLM (FE 2)","PLM (RE 1)","PLM (RE 2)")) |>
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"))

## Visualización de todos los resultados
texreg::screenreg(list(m04_a, m04_b, m04_c, m04_d, m04_e), digits = 3)


###################################################     EJEMPLO 08: MODELAMIENTO AVANZADO     ###############################################

# A) Test de Comparación de Modelos: Efectos Fijos vs Efectos Aleatorios

## Test de Hausman
plm::phtest(x = m03_a, x2 = m04_a) # Datos simulados SIN heterogeneidad individual
plm::phtest(x = m03_c, x2 = m04_c) # Datos simulados CON heterogeneidad individual Correlacionada con X (***)
plm::phtest(x = m03_e, x2 = m04_e) # Datos simulados CON heterogeneidad individual Independiente de X

### ***Solo en un caso se rechaza la hipótesis nula de que los efectos aleatorios son consistentes (Escenario FE)


# B) Corrección de Errores Estándares
lmtest::coeftest(m01_b)

## Lo más sencillo: Robustez a Heterocedasticidad
lmtest::coeftest(m01_b, vcov = plm::vcovHC(m01_b, type = "HC1", method = "arellano"))

## Sin embargo, no es suficiente. Dependencia estadística es clusterizada
lmtest::coeftest(m01_b, vcov = plm::vcovHC(m01_b, type = "HC1", method = "arellano", cluster = "group"))

## Esto funciona con cualquier tipo de estimador
lmtest::coeftest(m02_c, vcov = plm::vcovHC(m02_c, type = "HC1", method = "arellano", cluster = "group"))


# C) Pruebas de Hipótesis

# C1) Test de Comparación de Modelos: Incluir Efectos Fijos? (versus Pooled MCO)

## Test F para Heterogeneidad Individual o de Tiempo
plm::pFtest(x = m03_a, z = m01_b) # Datos simulados SIN heterogeneidad individual
plm::pFtest(x = m03_c, z = m01_d) # Datos simulados CON heterogeneidad individual (***)

### *** Se rechaza la hipótesis nula de que los efectos fijos son iguales a cero (Ausencia de Heterogeneidad Individual)


## Test de Multiplicador de Lagrange para Heterogeneidad Individual o de Tiempo
plm::plmtest(x = m01_b) # Datos simulados SIN heterogeneidad individual
plm::plmtest(x = m01_d) # Datos simulados CON heterogeneidad individual (***)

### *** Se rechaza la hipótesis nula de Ausencia de Heterogeneidad Individual


## Test de Combinalidad/Poolability de los Datos
plm::pooltest(x = m01_b, z = m03_a) # Datos simulados SIN heterogeneidad individual
plm::pooltest(x = m01_d, z = m03_c) # Datos simulados CON heterogeneidad individual (***)

### *** Se rechaza la hipótesis nula de estabilidad -> NO es una buena idea asumir Poolability. Preferible incorporar Heterogeneidad


# C2) Test de Correlación Serial

## Para MCO Pooled
plm::pwtest(x = m01_b, effect = "individual") # Datos simulados SIN heterogeneidad individual
plm::pwtest(x = m01_b, effect = "time")       # Datos simulados SIN heterogeneidad individual

plm::pwtest(x = m01_d, effect = "individual") # Datos simulados CON heterogeneidad individual ***
plm::pwtest(x = m01_d, effect = "time")       # Datos simulados CON heterogeneidad individual ***

### *** Se rechaza la hipótesis nula de ausencia de efectos no observados en los residuos (Indicativo de Correlación Serial)
### Esto NO implica que RE sea el estimador adecuado.

### Para Efectos Fijos (H. Nula de ausencia de Correlación Serial)
plm::pwartest(x = m03_a) # Datos simulados SIN heterogeneidad individual
plm::pwartest(x = m03_c) # Datos simulados CON heterogeneidad individual
plm::pwartest(x = m03_e) # Datos simulados CON heterogeneidad individual

## Para Primeras Diferencias
plm::pwfdtest(x = m02_a, h0  = "fd") # Datos simulados SIN heterogeneidad individual ***
plm::pwfdtest(x = m02_c, h0  = "fd") # Datos simulados CON heterogeneidad individual ***
plm::pwfdtest(x = m02_e, h0  = "fd") # Datos simulados CON heterogeneidad individual ***

### *** Se rechaza la hipótesis nula de ausencia de Correlación Serial 
## Este Test también se puede usar con Efectos Fijos: h0  = "fe"
## Útil para paneles cortos. El rechzo de "fe" no implica que "fd" sea el estimador adecuado


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
#[1] lubridate_1.9.4  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4      purrr_1.0.4      readr_2.1.5      tidyr_1.3.1      tibble_3.2.1    
#[9] ggplot2_3.5.2    tidyverse_2.0.0  sjmisc_2.8.10    sjlabelled_1.2.0 plm_2.6-6        panelView_1.1.18 panelr_0.7.8     lme4_1.1-37     
#[17] Matrix_1.7-1     lmtest_0.9-40    zoo_1.8-14       here_1.0.1      
#
#loaded via a namespace (and not attached):
#[1] tidyselect_1.2.1    viridisLite_0.4.2   farver_2.1.2        fastmap_1.2.0       jtools_2.2.2        fixest_0.12.1       digest_0.6.37      
#[8] estimability_1.5    timechange_0.3.0    lifecycle_1.0.4     dreamerr_1.4.0      magrittr_2.0.3      compiler_4.4.2      sass_0.4.9         
#[15] rlang_1.1.6         tools_4.4.2         utf8_1.2.4          collapse_2.0.14     knitr_1.48          texreg_1.39.3       xml2_1.3.6         
#[22] pkgload_1.3.4       abind_1.4-8         withr_3.0.2         numDeriv_2016.8-1.1 grid_4.4.2          xtable_1.8-4        lfe_3.0-0          
#[29] colorspace_2.1-1    emmeans_1.10.1      scales_1.3.0        MASS_7.3-61         insight_1.3.1       wooldridge_1.4-3    cli_3.6.4          
#[36] mvtnorm_1.3-3       rmarkdown_2.27      crayon_1.5.3        miscTools_0.6-28    reformulas_0.4.0    generics_0.1.3      rstudioapi_0.16.0  
#[43] httr_1.4.7          tzdb_0.5.0          bdsmatrix_1.3-7     cachem_1.1.0        minqa_1.2.8         pander_0.6.5        splines_4.4.2      
#[50] parallel_4.4.2      stringmagic_1.1.2   vctrs_0.6.5         boot_1.3-31         sandwich_3.1-1      jsonlite_1.8.8      carData_3.0-5      
#[57] car_3.1-3           hms_1.1.3           Formula_1.2-5       systemfonts_1.1.0   jquerylib_0.1.4     glue_1.8.0          nloptr_2.2.1       
#[64] stringi_1.8.7       gtable_0.3.6        munsell_0.5.1       pillar_1.10.2       htmltools_0.5.8.1   R6_2.6.1            maxLik_1.5-2.1     
#[71] Rdpack_2.6.3        rprojroot_2.0.4     evaluate_0.24.0     kableExtra_1.4.0    lattice_0.22-6      highr_0.11          haven_2.5.5        
#[78] rbibutils_2.3       bslib_0.7.0         Rcpp_1.0.14         svglite_2.1.3       coda_0.19-4.1       gridExtra_2.3       nlme_3.1-166       
#[85] xfun_0.45           pkgconfig_2.0.3   