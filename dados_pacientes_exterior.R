
library(microdatasus)
library(tidyverse)


estados<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB", "PR","PE","PI","RJ","RS","RN","RO","RR","SC","SP","SE","TO")




dados_sih_1<-
  map_dfr(estados[1:13],function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2025,
                                          year_end = 2025,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 12,
                                          information_system = "SIH-RD"))
    
    if (inherits(res, "try-error")){
      return()
    }
    
    microdatasus::process_sih(res)
  })

saveRDS(dados_sih_1,file = "dados_sih_1.RDS")

dados_sih_2<-
  map_dfr(estados[14:18],function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2025,
                                          year_end = 2025,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 12,
                                          information_system = "SIH-RD"))
    
    if (inherits(res, "try-error")){
      return()
    }
    
    microdatasus::process_sih(res)
  })


saveRDS(dados_sih_2,file = "dados_sih_2.RDS")

estados<- c("RJ",  "PR", "SC", "RS", "MS", "MT", "GO", "DF") #Exclusão de SP da lista

dados_sih_3<-
  map_dfr(estados,function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2025,
                                          year_end = 2025,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 12,
                                          information_system = "SIH-RD"))
    
    if (inherits(res, "try-error")){
      return()
    }
    
    microdatasus::process_sih(res)
  })


saveRDS(dados_sih_3,file = "dados_sih_3.RDS")


#processamento de SIH

#TEntativa de pegar 80% dos atendimentos em um período

#Estados com demanda que correspondem a aproximadamente 80% em novembro de 2025

estados<- c("SP", "MG", "PR", "BA", "RJ", "RS", "SC", "PE", "CE", "PA", "GO")


#Toda da demanda de SP em 2024 e 2025 nos meses entre abril e novembro

estados<- c("SP")




dados_sih_2024_abril_novembro_sp<-
  map_dfr(estados,function(estado){
    print(estado)
    res<- try(microdatasus::fetch_datasus(year_start = 2024,
                                          year_end = 2024,
                                          uf = estado,
                                          month_start = 4,
                                          month_end = 11,
                                          information_system = "SIH-RD"))
    
    if (inherits(res, "try-error")){
      return()
    }
    
    microdatasus::process_sih(res)
  })


dados_sih_2024_abril_novembro_sp <- janitor::clean_names(dados_sih_2024_abril_novembro_sp)

dados_sih_2024_abril_novembro_sp %>%
  summarise(quantidade = n(),
            .by = nacional) %>%
  arrange(desc(quantidade))

dados_sih_2025_abril_novembro_sp<-
  map_dfr(estados,function(estado){
    print(estado)
    res<- try(microdatasus::fetch_datasus(year_start = 2025,
                                          year_end = 2025,
                                          uf = estado,
                                          month_start = 4,
                                          month_end = 11,
                                          information_system = "SIH-RD"))
    
    if (inherits(res, "try-error")){
      return()
    }
    
    microdatasus::process_sih(res)
  })


dados_sih_2025_abril_novembro_sp <- janitor::clean_names(dados_sih_2025)

dados_sih_2025_abril_novembro_sp %>%
  summarise(quantidade = n(),
            .by = nacional) %>%
  arrange(desc(quantidade))

saveRDS(dados_sih_2025_abril_novembro_sp, "dados_sih_2025_abril_novembro_sp.rds")
saveRDS(dados_sih_2024_abril_novembro_sp, "dados_sih_2024_abril_novembro_sp.rds")
