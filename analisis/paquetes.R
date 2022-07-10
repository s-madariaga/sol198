
packages <- c("haven", "labelled", "tidyverse", "vtable", "patchwork", "gtsummary", "devtools", "stargazer")
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
ipak(packages)

install.packages("devtools",type="win.binary") 
devtools::install_github("MilesMcBain/paint", force = TRUE)
library(paint)
devtools::install_github("desuc/desuctools")
library(desuctools)
