
# ctrl + o para ver un índice

# Tareas:
# 1. Modificar variable "partidos políticos": no estpa bien elegida la variable
# a recodificar. Además, no hay datos para el 2021
# 2. Tidyr en el gráfico 2
# 3. Neutralizar por la cantidad de muestra: calcular pocentajes, porque
# el aparente aumento de la confianza se ve condicionada por la cantidad
# de personas que contestaron la encuesta.
# Error muestral
# Ver literatura según sexo (para elegir un sexo de referencia)

# Setup -------------------------------------------------------------------


# Paquetes usados
library(tidyverse)
library(patchwork)
library(vtable)
library(gtsummary)
library(GGally)


# Opciones
options(scipen = 9999)


# Importación
ELSOC = haven::read_sav("data/ELSOC_Long_2016_2021_v1.00_SPSS.sav")
vtable(ELSOC)


# Paletas -----------------------------------------------------------------


partisianship = c(
       "Izquierda" = "#E3170A",
       "Centro" = "#F9C80E",
       "Derecha" = "#141B41",
       "Independiente" = "#D741A7",
       "Ninguno" = "#7E8287"
)


matplotlib6 = c("#3082BA", "#FF7D09", "#33A333",
                "#D31819", "#C005C0", "#05C0C0")



# 1. Preparación de la Base de Datos --------------------------------------


# Limpieza (debido a que es una base haven-labelled, los métodos matriciales no funcionan)
ELSOC = ELSOC %>% 
  map(function(x){ifelse(x %in% c(-666, -777, -888, -999),NA,x)}) %>% 
  as.data.frame %>% 
  tibble # visualización amena


# Manipulación y reordenamiento
data = ELSOC %>%
  
  ### SELECCIÓN Variables Dependientes ----
  select(
        # Gobierno:
         c05_01,
        # Partidos Polítics 
         c05_02, 
        # Carabineros
         c05_03,
        # Poder Judicial
         c05_05, 
        # Congreso nacional
         c05_07,
        # Presidente
         c05_08,
        
  ### SELECCIÓN Variables Independientes ----
         m0_sexo, m0_edad, m01,
         c16, c40_01, ola) %>% 
  
  # Para mayor comodidad, renombramos de la forma:
  rename(gobierno = c05_01,
         partidos = c05_02,
         carabineros = c05_03,
         poder_judicial = c05_05,
         congreso = c05_07,
         presidente = c05_08,
         sexo = m0_sexo,
         edad = m0_edad,
         educ = m01,
         id_partidaria = c16,
         bienestar = c40_01) %>%
  
  ### TRANSFORMACIÓN manipulación de variables ----
  # Sexo con etiquietas "Hombre" "Mujer" (Evaluando si elegir un sexo de referencia)
  mutate(sexo = factor(sexo, levels = c(1,2),
                       labels = c("Hombre", "Mujer")),
         
  # Categorías de edad, para observar mejor       
         cat_edad = case_when(edad<25~1,
                              edad<40~2,
                              edad<65~3,
                              edad>65~4),
         cat_edad = factor(cat_edad, levels = 1:4,
                           labels = c("18-24 años",
                                      "25-39 años",
                                      "40-64 años",
                                      "65+")),
  
  # Simplificamos las categorías de nivel educacional
         educ = case_when(educ ==1~1,
                          educ %in% 2:3~2,
                          educ %in% 4:5~3,
                          educ %in% 6:9~4,
                          educ %in% 10~5),
         educ = factor(educ, levels = c(1, 2, 3, 4, 5),
                       labels = c("Sin estudios", "Básica",
                                  "Media", "Técnica o Universitaria",
                                  "Posgrado")),
  
  # Binomial gobierno
         gobierno_bool = ifelse(gobierno %in% c(5, 4),1,0),
  
  # Binomial partidos políticos
         partidos_bool = ifelse(partidos %in% c(5, 4),1,0),
  
  # Binomial carabineros
         carabineros_bool = ifelse(carabineros %in% c(5, 4),1,0),
  
  # Binomial congreso
         congreso_bool = ifelse(congreso %in% c(5, 4),1,0),
  
  # Binomial presidente
         presidente_bool = ifelse(presidente %in% c(5, 4),1,0),
  # Binomial presidente
         poder_judicial_bool = ifelse(poder_judicial %in% c(5, 4),1,0),
  
  # Año (encuesta)
         ola = factor(ola, levels = c(1,2,3,4,5),
                      labels = c("2016", "2017", "2018",
                                 "2019", "2021")),
  
  # Conformación de la variable Tendencia Política
         # id_partidaria = ifelse(id_partidaria %in% c(-666,-777,-888,-999),NA,id_partidaria),
         partido = case_when(id_partidaria == 15~1,
                             id_partidaria %in% c(1,2,3,4,6,12,13,16,19)~2,
                             id_partidaria %in% c(7, 8, 9)~3,
                             id_partidaria %in% c(5, 10, 11, 17, 18)~4),
         partido = factor(partido, levels = c(1,2,3,4),
                          labels = c("Ninguno",
                                     "Izquierda",
                                     "Centro",
                                     "Derecha")),
  
  # Bienestar --> Benevolencia (grados de acuerdo)
         bienestar = factor(bienestar, levels = c(1,2,3,4,5),
                            labels = c("Totalmente en desacuerdo",
                                       "En desacuerdo",
                                       "Ni de acuerdo ni en desacuerdo",
                                       "De acuerdo",
                                       "Totalmente de acuerdo"))) %>%
  
  # Reordenamos para una mejor visualización
  select(ola,edad,cat_edad, sexo, educ,partido,bienestar,
         gobierno,
         presidente,
         partidos,
         congreso,
         poder_judicial,
         carabineros,
         gobierno_bool,
         presidente_bool,
         partidos_bool,
         congreso_bool,
         poder_judicial_bool,
         carabineros_bool)



# Primero que nada... Matriz de correlaciones -----------------------------

# 7 x 6

# Todas
data %>%
  lapply(function(x){if(class(x) != "numeric") {as.numeric(x)}else{x}}) %>% 
  data.frame %>% 
  na.omit %>% 
  ggcorr(method = c("everything", "pearson"), label = TRUE, hjust = 0.9,
         color = "grey30",  label_round = 2, label_size = 4)+
  ggtitle("Coorrelograma")



# Sociodemográfcas
data %>%
  select(edad, sexo, educ, partido) %>% 
  lapply(function(x){if(class(x) != "numeric") {as.numeric(x)}else{x}}) %>% 
  data.frame %>% 
  na.omit %>% 
  ggcorr(method = c("everything", "pearson"), label = TRUE, hjust = 0.9,
         color = "grey30",  label_round = 2)+
  ggtitle("Coorrelograma")



# Dependientes
data %>%
  select(gobierno,
         presidente,
         partidos,
         congreso,
         poder_judicial,
         carabineros) %>% 
  lapply(function(x){if(class(x) != "numeric") {as.numeric(x)}else{x}}) %>% 
  data.frame %>% 
  na.omit %>% 
  ggcorr(method = c("everything", "pearson"), label = TRUE, hjust = 0.9,
         color = "grey30",  label_round = 2)+
  ggtitle("Coorrelograma")



# Dependientes x año
data %>%
  select(gobierno,
         presidente,
         partidos,
         congreso,
         poder_judicial,
         carabineros,
         ola) %>% 
  lapply(function(x){if(class(x) != "numeric") {as.numeric(x)}else{x}}) %>% 
  data.frame %>% 
  na.omit %>% 
  ggcorr(method = c("everything", "pearson"), label = TRUE, hjust = 0.9,
         color = "grey30",  label_round = 2)+
  ggtitle("Coorrelograma")



# 1. Análisis Decriptivo --------------------------------------------------


## 1.1. Tabla 1-2. Análisis descriptivo de todas las variables ------------
data %>% 
    tbl_summary(missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              sort = list(everything() ~ "frequency"),
              label = list(gobierno~ "Confianza en el Gobierno",
                           partidos~ "Confianza en los Partidos Políticos",
                           carabineros~ "Confianza en Carabineros",
                           sindicatos~ "Confianza en Sindicatos",
                           poder_judicial~ "Confianza en el Poder Judicial",
                           congreso~ "Confianza en el Congreso",
                           presidente~ "Confianza en el Presidente de la República",
                           fiscalia~ "Confianza en la Fiscalía",
                           fuerzas_armadas~ "Confianza en FFAA",
                           municipalidad~ "Confianza en su Municipalidad",
                           educ~ "Nivel Educacional",
                           sexo~ "Sexo",
                           partido~ "Partido político",
                           bienestar~ "Benevolencia",
                           ola~ "Año (encuesta)")) %>% 
  add_ci() %>% 
  modify_header(label ~ "**Variable**") %>%
  bold_labels()



# Descriptivo extendido ---------------------------------------------------

descrip <- function(x, rounded = 3){
  require(dplyr)
  c = c("Promedio" = mean(x),
           "SD" = sd(x, na.rm = TRUE),
           "Min" = min(x, na.rm = TRUE),
           "25%" = quantile(x, .25, na.rm = TRUE),
           "50%" = median(x, na.rm = TRUE),
           "75%" = quantile(x, .75, na.rm = TRUE),
           "Max" = max(x, na.rm = TRUE)) %>% 
    round(rounded)
  return(c)
}



# Tabla 3. Desriptivos sociodemográficos ----------------------------------


# Tabla 4. Descriptivos independientes ------------------------------------



# Tabla 5. Descriptivos dependientes --------------------------------------




# by = ola ----------------------------------------------------------------


data %>% 
    tbl_summary(by = ola,
                missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              sort = list(everything() ~ "frequency"),
              label = list(gobierno~ "Confianza en el Gobierno",
                           partidos~ "Confianza en los Partidos Políticos",
                           carabineros~ "Confianza en Carabineros",
                           sindicatos~ "Confianza en Sindicatos",
                           poder_judicial~ "Confianza en el Poder Judicial",
                           congreso~ "Confianza en el Congreso",
                           presidente~ "Confianza en el Presidente de la República",
                           fiscalia~ "Confianza en la Fiscalía",
                           fuerzas_armadas~ "Confianza en FFAA",
                           municipalidad~ "Confianza en su Municipalidad",
                           educ~ "Nivel Educacional",
                           sexo~ "Sexo",
                           partido~ "Partido político",
                           bienestar~ "Benevolencia",
                           ola~ "Año (encuesta)")) %>% 
  add_p() %>% 
  modify_header(label ~ "**Variable**") %>%
  bold_labels()



# Primeros cruces: sociodemográficas --------------------------------------

# Sexo
data %>% 
  select(sexo) %>% 
  count(sexo) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  ggplot(aes(x = sexo, y = prop, fill = sexo)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = prop), position = position_stack(vjust = .5), 
            color = "white", fontface = "bold")


## Edad y nivel educacional -----------------------------------------------

# 6 x 15

# Edad
data %>% 
  select(cat_edad) %>%
  na.omit %>% 
  ggplot(aes(x = cat_edad, fill = cat_edad)) +
  geom_bar()+
  # Estilo matplotlib
  scale_fill_manual(values = matplotlib6[1:4])+
  theme_test()+
  theme(legend.position = "bottom")+

# nivel educacional
data %>% 
  select(educ) %>%
  na.omit %>% 
  ggplot(aes(x = educ, fill = educ)) +
  geom_bar()+
  # Estilo matplotlib
  scale_fill_manual(values = matplotlib6[1:5])+
  theme_test()+
  theme(legend.position = "bottom")+
  coord_flip()

# Personas envejecidas tienen menor nivel eeducativo logrado.
data %>% 
  select(edad, educ) %>% 
  na.omit %>% 
  ggplot(aes(x = educ, y = edad, fill = educ)) +
  geom_boxplot()+
  # Estilo matplotlib
  scale_fill_manual(values = matplotlib6[1:5])+
  theme_test()+
  coord_flip()


# 1. Series 1: Tiempo x Confianza -----------------------------------------


# Gráfico sin tidyr::gather
data %>%
  ggplot(aes(x = ola, group = 1)) +
  stat_summary(aes(y =  gobierno), fun.y = sum, na.rm = TRUE, group = 3, color = 'blue', geom ='line')+
  stat_summary(aes(y =  gobierno), fun.y = sum, na.rm = TRUE, group = 3, color = 'blue', geom ='point',
               show.legend=TRUE)+
  
  stat_summary(aes(y =  presidente), fun.y = sum, na.rm = TRUE, group = 3, color = 'red', geom ='line')+
  stat_summary(aes(y =  presidente), fun.y = sum, na.rm = TRUE, group = 3, color = 'red', geom ='point',
               show.legend=TRUE)+
  
  stat_summary(aes(y =  carabineros), fun.y = sum, na.rm = TRUE, group = 3, color = 'green4', geom ='line')+
  stat_summary(aes(y =  carabineros), fun.y = sum, na.rm = TRUE, group = 3, color = 'green4', geom ='point',
               show.legend=TRUE)+
  
  stat_summary(aes(y =  fuerzas_armadas), fun.y = sum, na.rm = TRUE, group = 3, color = 'purple', geom ='line')+
  stat_summary(aes(y =  fuerzas_armadas), fun.y = sum, na.rm = TRUE, group = 3, color = 'purple', geom ='point',
               show.legend=TRUE)+
  
  stat_summary(aes(y =  poder_judicial), fun.y = sum, na.rm = TRUE, group = 3, color = 'orange', geom ='line')+
  stat_summary(aes(y =  poder_judicial), fun.y = sum, na.rm = TRUE, group = 3, color = 'orange', geom ='point',
               show.legend=TRUE)+
  
  stat_summary(aes(y =  municipalidad), fun.y = sum, na.rm = TRUE, group = 3, color = 'pink', geom ='line')+
  stat_summary(aes(y =  municipalidad), fun.y = sum, na.rm = TRUE, group = 3, color = 'pink', geom ='point',
               show.legend=TRUE)+
  
  # Etiquietas
  xlab("Año")+ ylab("Gran confianza (proporción)")



# Serie 1
data %>% 
  select(gobierno, presidente, poder_judicial, carabineros, ola) %>% 
  tidyr::gather(key = "confianza", value = "nivel", -c(ola)) %>% 
  mutate(nivel = ifelse(nivel == 0,NA, nivel)) %>% 
  na.omit %>% 
  select(ola, confianza) %>% 
  group_by(ola) %>% 
  count(confianza) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = ola, y = prop, group = confianza, 
             fill = confianza, color = confianza)) +
  geom_point()+
  geom_line()+
  
  # Apariencia "matplotlib.pyplot"
  scale_color_manual(values = matplotlib6[1:5])+
  theme_test()+
  
  # Etiquietas
  xlab("Año")+ ylab("Gran confianza (proporción)")



# Curvas de regresión "lm"
data %>% 
  select(ola, gobierno, presidente, carabineros, poder_judicial, partidos, congreso) %>% 
  tidyr::gather(key = "confianza", value = "nivel", -c(ola)) %>% 
  mutate(nivel = ifelse(nivel == 0,NA, nivel)) %>% 
  na.omit %>% 
  select(ola, confianza) %>% 
  group_by(ola) %>% 
  count(confianza) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = ola, y = prop, group = confianza, color = confianza)) +
  geom_line(stat = "summary", fun = sum, linetype = "dashed", color = "grey50")+
  geom_point(stat = "summary", fun = sum)+
  geom_smooth(method = "lm", fill = rgb(.8,.8,.8), alpha = .2)+
  
  # Apariencia "matplotlib.pyplot"
  scale_color_manual(values = matplotlib6)+
  theme_test()+
  
  # Etiquietas
  xlab("Año")+ ylab("Gran confianza (proporción)")


# Sin carabineros
data %>% 
  select(ola, gobierno, presidente, poder_judicial, partidos, congreso) %>% 
  tidyr::gather(key = "confianza", value = "nivel", -c(ola)) %>% 
  mutate(nivel = ifelse(nivel == 0,NA, nivel)) %>% 
  na.omit %>% 
  select(ola, confianza) %>% 
  group_by(ola) %>% 
  count(confianza) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = ola, y = prop, group = confianza, color = confianza)) +
  geom_line(stat = "summary", fun = sum, linetype = "dashed", color = "grey50")+
  geom_point(stat = "summary", fun = sum)+
  
  # Apariencia "matplotlib.pyplot"
  scale_color_manual(values = matplotlib6)+
  theme_test()+
  
  # Etiquietas
  xlab("Año")+ ylab("Gran confianza (proporción)")

## carabineros x partido político -----------------------------------------

data %>% 
  filter(!(partido == "Ninguno")) %>% 
  select(ola, partido, carabineros) %>%
  mutate(carabineros = ifelse(carabineros == 0,NA,carabineros)) %>% 
  na.omit %>% 
  select(ola, partido) %>% 
  group_by(ola) %>% 
  count(partido) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = ola, y = prop, group = partido, fill = partido)) +
  geom_bar(stat = "identity")+
  # Apariencia "matplotlib.pyplot"
  scale_fill_manual(values = partisianship[2:4])+
  geom_label(aes(label = round(prop, 2)), 
            position =position_stack(.5),
            fill = "white", color = "grey30")+
  
  # Etiquietas
  xlab("Año")+ ylab("Gran confianza en carabineros (proporción)")

## benevolencia x partido político ----------------------------------------



# 1. Series 2: Confianza x Tiempo x Partido -------------------------------

data %>% 
  select(gobierno, presidente, carabineros, fuerzas_armadas, partido,ola) %>% 
  tidyr::gather(key = "key", value = "value", -c(partido, ola)) %>% 
  mutate(value = ifelse(value == 0,NA,value)) %>% 
  na.omit() %>% 
  group_by(partido, ola) %>% 
  count(key) %>% 
  ggplot(aes(x = ola, y = n, group = partido, color = partido)) +
  geom_line(stat = "summary", fun = sum)+
  geom_point(stat = "summary", fun = sum)+
  scale_color_manual(values = partisianship)


# -------------------------------------------------------------------------
ELSOC$c40_01
10887-nrow(ELSOC)

ELSOC %>% 
  select(c05_01, c05_02, c05_03,
         c05_04, c05_05, 
         c05_07, c05_08, c05_09,
         c05_10, c05_13,
         m0_sexo, m0_edad, m01,
         c16, c40_01, ola) %>% 
  rename(gobierno = c05_01,
         partidos = c05_02,
         carabineros = c05_03,
         sindicatos = c05_04,
         poder_judicial = c05_05,
         congreso = c05_07,
         presidente = c05_08,
         fiscalia = c05_09,
         fuerzas_armadas = c05_10,
         municipalidad = c05_13,
         sexo = m0_sexo,
         edad = m0_edad,
         educ = m01,
         id_partidaria = c16,
         bienestar = c40_01) %>% 
  mutate(sexo = factor(sexo, levels = c(1,2),
                       labels = c("Hombre", "Mujer")),
         educ = case_when(educ ==1~1,
                          educ %in% 2:3~2,
                          educ %in% 4:5~3,
                          educ %in% 6:9~4,
                          educ %in% 10~5),
         educ = factor(educ, levels = c(1, 2, 3, 4, 5),
                       labels = c("Sin estudios", "Básica",
                                  "Media", "Técnica o Universitaria",
                                  "Posgrado")),
         bienestar = ifelse(bienestar %in% c(4,5),1,0),
         gobierno = ifelse(gobierno %in% c(5, 4),1,0),
         partidos = ifelse(partidos %in% c(5, 4),1,0),
         carabineros = ifelse(carabineros %in% c(5, 4),1,0),
         sindicatos = ifelse(sindicatos %in% c(5, 4),1,0),
         poder_judicial = ifelse(poder_judicial %in% c(5, 4),1,0),
         congreso = ifelse(congreso %in% c(5, 4),1,0),
         presidente = ifelse(presidente %in% c(5, 4),1,0),
         fiscalia = ifelse(fiscalia %in% c(5, 4),1,0),
         fuerzas_armadas = ifelse(fuerzas_armadas %in% c(5, 4),1,0),
         municipalidad = ifelse(municipalidad %in% c(5, 4),1,0)) %>%
  mutate(id_partidaria = ifelse(id_partidaria %in% c(-666,-777,-888,-999),NA,id_partidaria)) %>% 
  mutate(partido = case_when(id_partidaria == 15~1,
                             id_partidaria %in% c(1,2,3,4,6,12,13,16,19)~2,
                             id_partidaria %in% c(7, 8, 9)~3,
                             id_partidaria %in% c(5, 10, 11, 17, 18)~4),
         partido = factor(partido, levels = c(1,2,3,4),
                          labels = c("Ninguno",
                                     "Izquierda",
                                     "Centro",
                                     "Derecha")),
         ola = case_when(ola == 1~"2016",
                         ola == 2~"2017",
                         ola == 3~"2018",
                         ola== 4~"2019",
                         ola== 5~"2021")) %>%
  select(partido,
         gobierno,
         presidente,
         municipalidad,
         carabineros,
         fuerzas_armadas, 
         ola) %>%
  na.omit %>% 
  ggplot(aes(x = ola, y = gobierno, group=partido, color=partido))+
  stat_summary(fun = sum, na.rm = TRUE, geom ='line')+
  stat_summary(fun = sum, na.rm = TRUE, geom ='point')+
  xlab("Año (encuesta)") + ylab("Confianza en el Gobierno")+
  scale_color_discrete(name = "Tendencia política")

# c40_01	haven_labelled	Grado de acuerdo: Estado trabaja por bienestar de personas
# c40_03	haven_labelled	Grado de acuerdo: El Estado usa su poder legitimamente
# c21_11	haven_labelled	Grado de acuerdo: Las politicas son injustas


#c37_09	haven_labelled	Grado de acuerdo: actividad economia y salud publica



# m0_sexo	haven_labelled	Sexo del entrevistado	Num: 1 to 2
# m0_edad	numeric	Edad del entrevistado	Num: 18 to 92
# m01	haven_labelled	Nivel educacional	Num: -999 to 10
# m02	haven_labelled	Actividad principal	Num: -999 to 9
# m03	character	Ocupacion	
# ciuo88_m03	numeric	CIUO (1988) del entrevistado	Num: 1229 to 9999
# ciuo08_m03	numeric	CIUO (2008) del entrevistado	Num: 110 to 9999
# m04	character	Giro de la empresa	
# ciiu3_m04	numeric	CIIU (III version) del entrevistado	Num: 111 to 9999
# ciiu4_m04	numeric	CIIU (IV version) del entrevistado	Num: 111 to 72820

# m31_01	haven_labelled	Bienes: Conexion a TV cable	Num: -999 to 2
# m31_02	haven_labelled	Bienes: Computador	Num: -999 to 2
# m31_03	haven_labelled	Bienes: Conexion a internet	Num: -999 to 2
# m31_04	haven_labelled	Bienes: Secadora de ropa	Num: -999 to 2
# m32	haven_labelled	Cantidad de vehiculos motorizados en hogar	Num: -999 to 5
# m33	haven_labelled	Propiedad de vivienda donde reside	Num: -999 to 7

# c01	haven_labelled	Satisfaccion con la democracia en Chile
# c02	haven_labelled	Confianza Social Generalizada
# c04	haven_labelled	Mayoria de la gente trata de ser justa

# c05_01	haven_labelled	Grado de confianza: El Gobierno	Num: -999 to 5
# c05_02	haven_labelled	Grado de confianza: Los Partidos Politicos	Num: -999 to 5
# c05_03	haven_labelled	Grado de confianza: Carabineros	Num: -999 to 5
# c05_04	haven_labelled	Grado de confianza: Los Sindicatos	Num: -999 to 5
# c05_05	haven_labelled	Grado de confianza: El Poder Judicial	Num: -999 to 5
# c05_06	haven_labelled	Grado de confianza: Las Empresas Privadas	Num: -999 to 5
# c05_07	haven_labelled	Grado de confianza: El Congreso Nacional	Num: -999 to 5
# c05_08	haven_labelled	Grado de confianza: El Presidente/a de la Republica	Num: -999 to 5
# c05_09	haven_labelled	Grado de confianza: Fiscalia Nacional	Num: -999 to 5
# c05_10	haven_labelled	Grado de confianza: Fuerzas Armadas	Num: -999 to 5
# c05_11	haven_labelled	Grado de confianza: Bomberos	Num: -999 to 5
# c05_12	haven_labelled	Grado de confianza: Medios de comunicacion tradicionales	Num: -999 to 5
# c05_13	haven_labelled	Grado de confianza: Su municipalidad	Num: -999 to 
# c06_01	haven_labelled	Grado de confianza: Militantes de la UDI	Num: -999 to 5
# c06_02	haven_labelled	Grado de confianza: Militantes de la Democracia Cristiana	Num: -999 to 5
# c06_03	haven_labelled	Grado de confianza: Militantes del Partido Comunista	Num: -999 to 5
# c06_04	haven_labelled	Grado de confianza: Homosexuales (gays y lesbianas)	Num: -999 to 5
# c06_05	haven_labelled	Grado de confianza: Mapuche	Num: -999 to 5
# c06_06	haven_labelled	Grado de confianza: Inmigrantes	Num: -999 to 5

# c18_01	haven_labelled	Grado de acuerdo: Sociedad requiere grupos superiores e inferiores	Num: -999 to 5
# c18_02	haven_labelled	Grado de acuerdo: Se debe dar a todos las mismas oportunidades	Num: -999 to 5
# c18_03	haven_labelled	Grado de acuerdo: Se debe igualar las condiciones diferentes grupos	Num: -999 to 5
# c18_04	haven_labelled	Grado de acuerdo: Mas que derechos necesitamos un gobierno firme	Num: -999 to 5
# c18_05	haven_labelled	Grado de acuerdo: Pais necesita un mandatario fuerte	Num: -999 to 5
# c18_06	haven_labelled	Grado de acuerdo: Obediencia y respeto importantes que aprendan los ninnios	Num: -999 to 5
# c18_07	haven_labelled	Grado de acuerdo: Obediencia y disciplina son claves para buena vida	Num: -999 to 5
# c18_08	haven_labelled	Grado de acuerdo: El cambio social es posible	Num: -999 to 5
# c18_09	haven_labelled	Grado de acuerdo: Las personas son recompensadas por sus esfuerzos	Num: -999 to 5
# c18_10	haven_labelled	Grado de acuerdo: Las personas son recompensada por su inteligencia	Num: -999 to 5
# c18_11	haven_labelled	Grado de acuerdo: Las diferencias de ingreso son demasiado grandes	Num: -999 to 5
# c18_12	haven_labelled	Grado de acuerdo: Hay grupos de personas inferiores	Num: -999 to 5
# c18_13	haven_labelled	Grado de acuerdo: Las personas tienen igualdad de oportunidades	Num: -999 to 5
# c20	haven_labelled	Movimiento social que mas valora	Num: -999 to 12
# c20_otro	character	Movimiento social que mas valora. Otro: especifique	
# c21_01	haven_labelled	Grado de acuerdo: Compromiso con el movimiento	Num: -999 to 5
# c21_02	haven_labelled	Grado de acuerdo: Identificacion con el movimiento	Num: -999 to 5
# c21_03	haven_labelled	Grado de acuerdo: Las acciones del movimiento	Num: -999 to 5
# c21_04	haven_labelled	Grado de acuerdo: Esperanzado por el futuro del movimiento	Num: -999 to 5
# c21_05	haven_labelled	Grado de acuerdo: Las acciones del movimiento generan cambio social	Num: -999 to 5
# c21_06	haven_labelled	Grado de acuerdo: Los participantes me ven como uno mas	Num: -999 to 5
# c21_07	haven_labelled	Grado de acuerdo: El movimiento esta alineado con mis valores	Num: -999 to 5
# c21_08	haven_labelled	Grado de acuerdo: Posicion similar el movimiento y la gente	Num: -999 to 5
# c21_09	haven_labelled	Grado de acuerdo: Posicion opuesta el movimiento y las autoridades	Num: -999 to 5
# c21_10	haven_labelled	Grado de acuerdo: Los participantes me apoyan	Num: -999 to 5
# c21_11	haven_labelled	Grado de acuerdo: Las politicas son injustas	Num: -999 to 5


# c10_01	haven_labelled	Grado de acuerdo: Votar es mi deber como ciudadano	Num: -999 to 5
# c10_02	haven_labelled	Grado de acuerdo: Mi voto influye en el resultado	Num: -999 to 5
# c10_03	haven_labelled	Grado de acuerdo: Votar permite expresar mis ideas	Num: -999 to 5
# c11	haven_labelled	Participacion electoral retrospectiva	Num: -999 to 3
# c36	haven_labelled	Intencion de voto en Elecciones Presidenciales	Num: -999 to 10
# c37_01	haven_labelled	Grado de acuerdo: adopcion homoparental	Num: -999 to 5
# c37_02	haven_labelled	Grado de acuerdo: aborto	Num: -999 to 5
# c37_03	haven_labelled	Grado de acuerdo: rol del Estado en educacion	Num: -999 to 5
# c37_04	haven_labelled	Grado de acuerdo: capitalizacion individual pensiones	Num: -999 to 5
# c37_05	haven_labelled	Grado de acuerdo: restricciones ingreso migrantes	Num: -999 to 5
# c37_06	haven_labelled	Grado de acuerdo: Educacion sexual responsabilidad de padres	Num: -999 to 5
# c37_07	haven_labelled	Grado de acuerdo: Restriccion empresas contaminantes	Num: -999 to 5
# c37_08	haven_labelled	Grado de acuerdo: Gasto social focalizado	Num: -999 to 5
# c37_09	haven_labelled	Grado de acuerdo: actividad economia y salud publica	Num: -888 to 5
# c37_10	haven_labelled	Grado de acuerdo: cuarentenas	Num: -999 to 5
# c38	haven_labelled	Extension de corrupcion politica	Num: -999 to 5
# c39	haven_labelled	Voto (retrospectivo) en elecciones presidenciales [2013/2017]	Num: -999 to 10

# c40_01	haven_labelled	Grado de acuerdo: Estado trabaja por bienestar de personas	Num: -999 to 5
# c40_02	haven_labelled	Grado de acuerdo: Todos piensan en si mismos y no ayudan a otros	Num: -999 to 5
# c40_03	haven_labelled	Grado de acuerdo: El Estado usa su poder legitimamente	Num: -999 to 5
# c40_04	haven_labelled	Grado de acuerdo: Exito a veces requiere trampa	Num: -999 to 5
# c40_05	haven_labelled	Grado de acuerdo: Personas son amigas de otras por su propio beneficio	Num: -888 to 5
# c40_06	haven_labelled	Grado de acuerdo: Las autoridades protegen a los vulnerables y debiles	Num: -999 to 5
# c41_01	haven_labelled	Grado de rabia: Actuales niveles de desigualdad en Chile	Num: -999 to 5
# c41_02	haven_labelled	Grado de rabia: El costo de la vida en Chile	Num: -999 to 5
# c41_03	haven_labelled	Grado de rabia: Los manifestantes violentos en las protestas	Num: -999 to 5
# c41_04	haven_labelled	Grado de rabia: El accionar de las Fuerzas de Seguridad en las manifestaciones	Num: -999 to 5
# c42_01	haven_labelled	Grado de miedo: Actuales niveles de desigualdad en Chile	Num: -999 to 5
# c42_02	haven_labelled	Grado de miedo: El costo de la vida en Chile	Num: -999 to 5
# c42_03	haven_labelled	Grado de miedo: Los manifestantes violentos en las protestas	Num: -999 to 5
# c42_04	haven_labelled	Grado de miedo: El accionar de las Fuerzas de Seguridad en las manifestaciones	Num: -999 to 5

# c13	haven_labelled	Interes en la politica	Num: -999 to 5
# c14_01	haven_labelled	Frecuencia: Habla de politica con familiares o amigos	Num: -999 to 5
# c14_02	haven_labelled	Frecuencia: Se informa sobre politica en medios de comunicacion	Num: -999 to 5

# c15	haven_labelled	Autoubicacion escala izquierda-derecha	Num: -999 to 12
# c16	haven_labelled	Identificacion partidaria	Num: -999 to 15
# c17	haven_labelled	Identificacion con coaliciones politicas	Num: -999 to 5
# c17_otro	character	Identificacion con coaliciones politicas. Otra: especifique

# c25	haven_labelled	Preferencia entre Autoritarismo y Democracia	Num: -999 to 4
# c26	haven_labelled	Conformidad con actual Constitucion	Num: -999 to 5
# c27	haven_labelled	Importancia de cambio de Constitucion actual	Num: -999 to 5
# c28	haven_labelled	Grado de acuerdo: cambiar la Constitucion	Num: -999 to 5
# c29	haven_labelled	Mecanismo de cambio de Constitucion	Num: -999 to 3
# c30	haven_labelled	Frecuencia de conversacion sobre el cambio constitucional	Num: -999 to 5
# c31	haven_labelled	Participacion en proceso constituyente	Num: -999 to 2
# c32_01	haven_labelled	Grado de acuerdo: Me siento orgulloso de ser chileno	Num: -999 to 5
# c32_02	haven_labelled	Grado de acuerdo: Me identifico con Chile	Num: -999 to 5

# d02_01	haven_labelled	Grado de acuerdo: Justicia distributiva en pensiones	Num: -999 to 5
# d02_02	haven_labelled	Grado de acuerdo: Justicia distributiva en educacion	Num: -999 to 5
# d02_03	haven_labelled	Grado de acuerdo: Justicia distributiva en salud	Num: -999 to 5
# d24_01	haven_labelled	Grado de acuerdo: Poderosos no le importan personas como uno	Num: -999 to 5
# d24_02	haven_labelled	Grado de acuerdo: Poderosos indolentes con problemas graves en mi barrio	Num: -999 to 5
# d24_03	haven_labelled	Grado de acuerdo: Poderosos actuan segun intereses de grandes empresarios	Num: -999 to 5
# d24_04	haven_labelled	Grado de acuerdo: Justicia siempre favorece a poderosos	Num: -999 to 5

# d03_01	numeric	Salario percibido: Gerente gran empresa	Num: -999 to 1e+20
# d03_02	numeric	Salario percibido: Obrero no calificado	Num: -999 to 1e+20

# c43	haven_labelled	Participacion electoral retrospectiva Plebiscito Nueva Constitucion	Num: -999 to 3
# c44	haven_labelled	Voto retrospectivo aprobacion redaccion Nueva Constitucion	Num: -999 to 4
# c45	haven_labelled	Voto retrospectivo Mecanimo de cambio constitucional	Num: -999 to 4
# c46_01	haven_labelled	Optimismo constitucional: Desigualdad en salud y educacion	Num: -999 to 5
# c46_02	haven_labelled	Optimismo constitucional: Condiciones economicas	Num: -999 to 5
# c46_03	haven_labelled	Optimismo constitucional: Corrupcion	Num: -999 to 5
# c46_04	haven_labelled	Optimismo constitucional: Calidad de vida	Num: -999 to 5
# c47	haven_labelled	Intencion de voto para 2021	Num: -888 to 3
# c48	haven_labelled	Cumplimiento del aislamiento social	Num: -999 to 5
