

packages <- c("haven", "texreg","labelled", "tidyverse", "vtable", "patchwork", "gtsummary", "devtools", "stargazer")
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

data <- read_sav(paste0("C:/Users/Temporal/Desktop/sol198/data/ELSOC_Long_2016_2021_v1.00_SPSS.sav"))
data$c15

data$c38 %>% unique

eliminar = c(-666, -777, -888, -999)
data = data %>%
  sapply(function(x){ ifelse(x %in% eliminar, NA, as.numeric(x)) }) %>%
  as.data.frame %>%
  tibble

# Selección de variables
data = data %>%
  
  ### SELECCIÓN Variables Dependientes ----
  select(
        # Gobierno:
         c05_01,
        # Partidos Políticos 
         c05_02, 
        # Carabineros
         c05_03,
        # Presidente
         c05_08,
        # Fuerzas Armadas
         c05_10,
        
  ### SELECCIÓN Variables Independientes ----
         m01,
         c05_10, c40_01, ola, c15,c13, c38) %>% 
  
  # Para mayor comodidad, renombramos de la forma:
  rename(gobierno = c05_01,
         partidos = c05_02,
         carabineros = c05_03,
         presidente = c05_08,
         educ = m01,
         id_partidaria = c15,
         benevolencia = c40_01,
         interes = c13,
         corrupcion = c38) %>%
  mutate(anio = factor(ola, labels = c(2016, 2017, 2018, 2019, 2021), levels = c(1,2,3,4,5)),
  pp = case_when(id_partidaria %in% c(0, 1, 2, 3)~"Izquierda",
                id_partidaria %in% c(4, 5, 6)~"Centro",
                id_partidaria %in% c(7, 8, 9, 10)~"Derecha",
                id_partidaria == 11~"Independiente",
                id_partidaria == 12~"Ninguno"))

data$gobierno = as.numeric(data$gobierno)

#
data %>%
select(anio, educ, pp, interes, corrupcion) %>%
  tbl_summary(missing = "no",
  type = list(interes ~ "continuous", 
              corrupcion ~ "continuous"),
  statistic = all_continuous() ~ c("{mean} ({sd})"))
  
# Provisorio
data %>%
select(gobierno, partidos, carabineros, presidente)%>%
  tbl_summary(missing = "no",
  type = list(gobierno ~ "continuous",
              partidos ~ "continuous",
              carabineros ~ "continuous",
              presidente ~ "continuous"),
  statistic = all_continuous() ~ c("{mean} ({sd})"))

data %>%
select(gobierno, partidos, carabineros, presidente, anio) %>%
  tbl_summary(missing = "no", by = anio,
  type = list(gobierno ~ "continuous",
              partidos ~ "continuous",
              carabineros ~ "continuous",
              presidente ~ "continuous"),
  statistic = all_continuous() ~ c("{mean} ({sd})")) %>%
  gtsummary::add_ci()

# Tiempo
data %>%
select(gobierno, partidos, carabineros, presidente,FFAA, pp) %>%
  tbl_summary(by=pp, missing = "no",
  type = list(gobierno ~ "continuous",
              partidos ~ "continuous",
              carabineros ~ "continuous",
              presidente ~ "continuous",
              FFAA ~ "continuous"))

data$id_partidaria = ifelse(data$id_partidaria %in% c(11, 12), NA, data$id_partidaria)


# Gobierno
data$ola = as.numeric(data$ola)
m1 = lm(gobierno~ola, data = data)
m2 = lm(gobierno~ola+id_partidaria, data = data)
m3 = lm(gobierno~ola + interes, data = data)
m4 = lm(gobierno~ola + id_partidaria+interes, data = data)
m5 = lm(gobierno~ola + id_partidaria+interes+corrupcion, data = data)
stargazer(m1, m2, m3, m4, m5, type="html")


# Partidos
data$ola = as.numeric(data$ola)
m1 = lm(partidos~ola, data = data)
m2 = lm(partidos~ola+id_partidaria, data = data)
m3 = lm(partidos~ola + interes, data = data)
m4 = lm(partidos~ola + id_partidaria+interes, data = data)
m5 = lm(partidos~ola + id_partidaria+interes+corrupcion, data = data)
stargazer(m1, m2, m3, m4, m5, type="html")


# Carabineros
data$ola = as.numeric(data$ola)
m1 = lm(carabineros~ola, data = data)
m2 = lm(carabineros~ola+id_partidaria, data = data)
m3 = lm(carabineros~ola + interes, data = data)
m4 = lm(carabineros~ola + id_partidaria+interes, data = data)
m5 = lm(carabineros~ola + id_partidaria+interes+corrupcion, data = data)
stargazer(m1, m2, m3, m4, m5, type="html")


# Presidente
data$ola = as.numeric(data$ola)
m1 = lm(presidente~ola, data = data)
m2 = lm(presidente~ola+id_partidaria, data = data)
m3 = lm(presidente~ola + interes, data = data)
m4 = lm(presidente~ola + id_partidaria+interes, data = data)
m5 = lm(presidente~ola + id_partidaria+interes+corrupcion, data = data)
stargazer(m1, m2, m3, m4, m5, type="html")

table(data$educ)
