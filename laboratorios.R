library(tidyverse)
library(microdatasus)

dados <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "RJ", information_system = "SIM-DO")
dados <- process_sim(dados)

dados_sia<- 
  
  fetch_datasus(year_start = 2014, month_start = 1,
                year_end = 2014, month_end = 1,
                uf = c("RJ"),
                information_system = "SIA-PA")

dados_sia <- process_sia(dados_sia)
