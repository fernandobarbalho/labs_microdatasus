dados_sih_1 <- readRDS("~/Github/labs_microdatasus/dados_sih_1.RDS")
dados_sih_2 <- readRDS("~/Github/labs_microdatasus/dados_sih_2.RDS")
dados_sih_3 <- readRDS("~/Github/labs_microdatasus/dados_sih_3.RDS")
dados_sih_2024_janeiro_marco_sp <- readRDS("~/Github/labs_microdatasus/dados_sih_2024_janeiro_marco_sp.rds")
dados_sih_2024_abril_novembro_sp <- readRDS("~/Github/labs_microdatasus/dados_sih_2024_abril_novembro_sp.rds")
dados_sih_2024_dezembro_sp<-  readRDS("~/Github/dados_sih_2024_dezembro_sp.rds")

dados_sih_1 <- janitor::clean_names(dados_sih_1)
dados_sih_2 <- janitor::clean_names(dados_sih_2)
dados_sih_3 <- janitor::clean_names(dados_sih_3)
dados_sih_2024_janeiro_marco_sp <- janitor::clean_names(dados_sih_2024_janeiro_marco_sp)
dados_sih_2024_abril_novembro_sp <- janitor::clean_names(dados_sih_2024_abril_novembro_sp )
dados_sih_2024_dezembro_sp <- janitor::clean_names(dados_sih_2024_dezembro_sp)

total_municipio_aparelho_respiratorio_grupo<-
dados_sih_1 %>%
  bind_rows(dados_sih_2,
            dados_sih_3,
            dados_sih_2024_janeiro_marco_sp,
            dados_sih_2024_abril_novembro_sp,
            dados_sih_2024_dezembro_sp)  %>%
  filter(str_sub(diag_princ,1,1)== "J") %>%
  summarise(total_2024 = n(),
            .by = munic_res)






saveRDS(total_municipio_aparelho_respiratorio_grupo, "total_municipio_aparelho_respiratorio_grupo.rds")

acumulado_1<-
dados_sih_1%>%
  mutate(mes_internacao = month(dt_inter)) %>%
           filter(str_sub(diag_princ,1,1)== "J") %>%
           summarise(total_2024 = n(),
                     .by = c(munic_res, mes_internacao) )
         

acumulado_2<-
  dados_sih_2%>%
  mutate(mes_internacao = month(dt_inter)) %>%
  filter(str_sub(diag_princ,1,1)== "J") %>%
  summarise(total_2024 = n(),
            .by = c(munic_res, mes_internacao) )


acumulado_3<-
  dados_sih_3%>%
  mutate(mes_internacao = month(dt_inter)) %>%
  filter(str_sub(diag_princ,1,1)== "J") %>%
  summarise(total_2024 = n(),
            .by = c(munic_res, mes_internacao) )


acumulado_4<-
  dados_sih_2024_janeiro_marco_sp%>%
  mutate(mes_internacao = month(dt_inter)) %>%
  filter(str_sub(diag_princ,1,1)== "J") %>%
  summarise(total_2024 = n(),
            .by = c(munic_res, mes_internacao) )


acumulado_5<-
  dados_sih_2024_abril_novembro_sp%>%
  mutate(mes_internacao = month(dt_inter)) %>%
  filter(str_sub(diag_princ,1,1)== "J") %>%
  summarise(total_2024 = n(),
            .by = c(munic_res, mes_internacao) )


acumulado_6<-
  dados_sih_2024_dezembro_sp%>%
  mutate(mes_internacao = month(dt_inter)) %>%
  filter(str_sub(diag_princ,1,1)== "J") %>%
  summarise(total_2024 = n(),
            .by = c(munic_res, mes_internacao) )


acumulados_mes_total<-
  acumulado_1 %>%
  bind_rows(acumulado_2,
            acumulado_3,
            acumulado_4,
            acumulado_5,
            acumulado_6) %>%
  summarise(total_mes = sum(total_2024),
            .by = c(munic_res, mes_internacao) )

saveRDS(acumulados_mes_total, "acumulados_mes_total.rds")

participacao_mes<-
  acumulados_mes_total %>%
  inner_join(total_municipio_aparelho_respiratorio_grupo) %>%
  mutate(participacao_internacao_mes = total_mes/total_2024)

saveRDS(participacao_mes, "participacao_mes.rds")
