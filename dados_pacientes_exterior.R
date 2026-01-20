
library(microdatasus)
library(tidyverse)


estados<- c("AC","AL","AM", "AP", "BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB", "PR","PE","PI","RJ","RS","RN","RO","RR","SC","SP","SE","TO")


download.file("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Doc/IT_SIHSUS_1603.pdf", destfile = "documentacao.pdf", mode= "wb", method = "libcurl" )



dados_sih_1<-
  map_dfr(estados[1:13],function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2024,
                                          year_end = 2024,
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
    res<- try(microdatasus::fetch_datasus(year_start = 2024,
                                          year_end = 2024,
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


dados_sih_3<-
  map_dfr(estados[c(19:24,26:27)],function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2024,
                                          year_end = 2024,
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

#estados<- c("SP", "MG", "PR", "BA", "RJ", "RS", "SC", "PE", "CE", "PA", "GO")


###Demanda específica de SP ebtre janeiro e março de 2024

estados<- c("SP")


dados_sih_2024_janeiro_marco_sp<-
  map_dfr(estados,function(estado){
    print(estado)
    res<- try(microdatasus::fetch_datasus(year_start = 2024,
                                          year_end = 2024,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 3,
                                          information_system = "SIH-RD"))
    
    if (inherits(res, "try-error")){
      return()
    }
    
    microdatasus::process_sih(res)
  })


dados_sih_2024_janeiro_marco_sp <- janitor::clean_names(dados_sih_2024_janeiro_marco_sp)


#Toda da demanda de SP em 2024 e 2025 nos meses entre abril e novembro


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

saveRDS(dados_sih_2024_janeiro_marco_sp, "dados_sih_2024_janeiro_marco_sp.rds")
saveRDS(dados_sih_2025_abril_novembro_sp, "dados_sih_2025_abril_novembro_sp.rds")
saveRDS(dados_sih_2024_abril_novembro_sp, "dados_sih_2024_abril_novembro_sp.rds")



############Dados de Roraima

dados_sih_RR_2025<-
  map_dfr("RR",function(estado){
    res<- try(microdatasus::fetch_datasus(year_start = 2025,
                                          year_end = 2025,
                                          uf = estado,
                                          month_start = 1,
                                          month_end = 10,
                                          information_system = "SIH-RD"))
    
    if (inherits(res, "try-error")){
      return()
    }
    
    res
    
    #microdatasus::process_sih(res)
  })

dados_sih_RR_2025 <- janitor::clean_names(dados_sih_RR_2025)

#Venezuela, código =92 
#Brasil, código =10

total_linhas<- NROW(dados_sih_RR_2025) 

dados_sih_RR_2025 %>%
  filter(nacional %in% c("010","092")) %>%
  summarise(proporcao = n()/total_linhas,
            .by = nacional)

##########################
#Carga dos datasets previamente salvos. Os datasets estão disponíveis no googledrive a partir do link:
#https://drive.google.com/drive/folders/1HQRcsMtddN_oVkEhtGAIuJIfE3XbG68E?usp=sharing

dados_sih_2025_abril_novembro_sp <- readRDS("~/github/labs_microdatasus/dados_sih_2025_abril_novembro_sp.rds")

dados_sih_2024_janeiro_marco_sp <- readRDS("~/Github/labs_microdatasus/dados_sih_2024_janeiro_marco_sp.rds")
dados_sih_2024_abril_novembro_sp <- readRDS("~/Github/labs_microdatasus/dados_sih_2024_abril_novembro_sp.rds")

dados_sp_2024<-
  bind_rows(dados_sih_2024_janeiro_marco_sp, dados_sih_2024_abril_novembro_sp)

dados_sih_1 <- readRDS("~/Github/labs_microdatasus/dados_sih_1.RDS")

dados_sih_1<- janitor::clean_names(dados_sih_1)

dados_sih_2 <- readRDS("~/Github/labs_microdatasus/dados_sih_2.RDS")

dados_sih_2<- janitor::clean_names(dados_sih_2)

dados_sih_3 <- readRDS("~/Github/labs_microdatasus/dados_sih_3.RDS")

dados_sih_3<- janitor::clean_names(dados_sih_3)

################### sumarizações


#O período de abril a novembro de 2025 corresponde aos meses em que estão valendo as regras de fim de reciprocidade para visto dos EUA

df_2025_comparacao<-
dados_sih_2025_abril_novembro_sp %>%
  mutate(uf = str_sub(uf_zi,1,2)) %>%
  mutate(idade = as.numeric(idade)) %>%
  mutate(idade = case_when(
    cod_idade %in% c( "Meses","Dias" ) ~ 0,
    cod_idade == "Centena de anos (100 + idade)" ~ idade + 100,
    cod_idade == "Anos" ~idade
  )) %>%
  summarise(quantidade_2025 = n(),
            .by = c(nacional)) %>%
  arrange(desc(quantidade_2025))

df_2024_comparacao<-
dados_sih_2024_abril_novembro_sp %>%
  mutate(uf = str_sub(uf_zi,1,2)) %>%
  mutate(idade = as.numeric(idade)) %>%
  mutate(idade = case_when(
    cod_idade %in% c( "Meses","Dias" ) ~ 0,
    cod_idade == "Centena de anos (100 + idade)" ~ idade + 100,
    cod_idade == "Anos" ~idade
  )) %>%
  summarise(quantidade_2024 = n(),
            .by = c(nacional)) %>%
  arrange(desc(quantidade_2024)) 


comparacao_2025_2024_abr_nov_sp<-
df_2025_comparacao %>%
  inner_join(df_2024_comparacao) %>%
  mutate(var = quantidade_2025- quantidade_2024,
         var_perc = ((quantidade_2025/quantidade_2024)-1) * 100 ) %>%
  arrange(desc(var_perc))

saveRDS(comparacao_2025_2024_abr_nov_sp, "comparacao_2025_2024_abr_nov_sp.rds")

### Sínteses de dados de 2024


sintese_sp_2024<-
dados_sp_2024 %>%
  mutate(uf = str_sub(uf_zi,1,2)) %>%
  mutate(idade = as.numeric(idade)) %>%
  mutate(idade = case_when(
    cod_idade %in% c( "Meses","Dias" ) ~ 0,
    cod_idade == "Centena de anos (100 + idade)" ~ idade + 100,
    cod_idade == "Anos" ~idade
  )) %>%
  summarise(quantidade = n(),
            media_idade = mean(idade),
            .by = c(uf, nacional)) %>%
  arrange(desc(quantidade)) 

saveRDS(sintese_sp_2024,"sintese_sp_2024.rds")

dados_sp_2024_sem_brasil<-
  dados_sp_2024 %>%
  filter(nacional  != "Brasil")
  


sintese_sih_1<-
dados_sih_1 %>%
  mutate(uf = str_sub(uf_zi,1,2)) %>%
  mutate(idade = as.numeric(idade)) %>%
  mutate(idade = case_when(
    cod_idade %in% c( "Meses","Dias" ) ~ 0,
    cod_idade == "Centena de anos (100 + idade)" ~ idade + 100,
    cod_idade == "Anos" ~idade
  )) %>%
  summarise(quantidade = n(),
            media_idade = mean(idade),
            .by = c(uf, nacional)) %>%
  arrange(desc(quantidade)) 


saveRDS(sintese_sih_1,"sintese_sih_1.rds")

dados_sih_1_sem_brasil<-
  dados_sih_1 %>%
  filter(nacional  != "Brasil")


sintese_sih_2<-
  dados_sih_2 %>%
  mutate(uf = str_sub(uf_zi,1,2)) %>%
  mutate(idade = as.numeric(idade)) %>%
  mutate(idade = case_when(
    cod_idade %in% c( "Meses","Dias" ) ~ 0,
    cod_idade == "Centena de anos (100 + idade)" ~ idade + 100,
    cod_idade == "Anos" ~idade
  )) %>%
  summarise(quantidade = n(),
            media_idade = mean(idade),
            .by = c(uf, nacional)) %>%
  arrange(desc(quantidade)) 


saveRDS(sintese_sih_2,"sintese_sih_2.rds")

dados_sih_2_sem_brasil<-
  dados_sih_2 %>%
  filter(nacional  != "Brasil")



sintese_sih_3<-
  dados_sih_3 %>%
  mutate(uf = str_sub(uf_zi,1,2)) %>%
  mutate(idade = as.numeric(idade)) %>%
  mutate(idade = case_when(
    cod_idade %in% c( "Meses","Dias" ) ~ 0,
    cod_idade == "Centena de anos (100 + idade)" ~ idade + 100,
    cod_idade == "Anos" ~idade
  )) %>%
  summarise(quantidade = n(),
            media_idade = mean(idade,na.rm =TRUE),
            .by = c(uf, nacional)) %>%
  arrange(desc(quantidade)) 


saveRDS(sintese_sih_3,"sintese_sih_3.rds")

dados_sih_3_sem_brasil<-
  dados_sih_3 %>%
  filter(nacional  != "Brasil")


dados_2024_sem_brasil<-
  dados_sp_2024_sem_brasil %>%
  bind_rows(
    dados_sih_1_sem_brasil,
    dados_sih_2_sem_brasil,
    dados_sih_3_sem_brasil
  )


saveRDS(dados_2024_sem_brasil, "dados_2024_sem_brasil.rds")
