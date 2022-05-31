##################################################
###### Consulta à Comunidade Acadêmica 2021 ######
##################################################

setwd("C:/Users/igorB/Desktop/ArquivosPesquisaUNB")
options(OutDec = ",")
install.packages("googlesheets")
install.packages("dplyr")
library('googlesheets')
library('dplyr')
library('ggplot2')
library('readxl')
library('stringr')
library('scales')
library('tidyr')
library('readr')



# Localização do arquivo xlsx para leitura dos dados

consulta_tecnico <- read_excel("C:/Users/igorB/Desktop/ArquivosPesquisaUNB/TecnicoConsulta.xlsx")
consulta_docente <- read_excel("C:/Users/igorB/Desktop/ArquivosPesquisaUNB/DocenteConsulta.xlsx")
consulta_aluno   <- read_excel("C:/Users/igorB/Desktop/ArquivosPesquisaUNB/AlunosConsulta.xlsx")




# QUADROUNIDADE ------------------------------------------------------
consulta_tec <- consulta_tecnico%>% filter(`Unidade` == "FACE")
consulta_doc <- consulta_docente%>% filter(`Unidade` == "FACE")
consulta_al   <- consulta_aluno%>% filter(`Unidade` == "FACE")

quadro1_te <- consulta_tecnico %>% group_by(`Unidade`) %>% filter(!is.na(`Unidade`)) %>%  tally()
quadro1_do <- consulta_docente %>% group_by(`Unidade`) %>% filter(!is.na(`Unidade`)) %>%  tally()
quadro1_al <- consulta_aluno %>% group_by(`Unidade`) %>% filter(!is.na(`Unidade`)) %>%  tally()

quadro1 <- dplyr:::full_join(quadro1_al, quadro1_do, by = "Unidade") %>% 
  full_join(quadro1_te, by = "Unidade") %>% 
  arrange("Unidade") %>% 
  rename("Alunos" = n.x, "Docentes" = n.y, "Tecnicos" = n)

quadro1 <- rbind(quadro1, data.frame(Unidade = "Total",
                                     t(colSums(quadro1[, -1], na.rm = T))))
quadro1 <- cbind(quadro1, data.frame(Total = rowSums(quadro1[, -1], na.rm = T)))

quadrounidade <- quadro1

rm(quadro1_al, quadro1_do, quadro1_te)

# QUADROTEMPO ----------------------------------------------------------------

quadro_do <- consulta_docente %>% filter(!is.na(`Há quanto tempo você trabalha na UnB?`)) %>% 
  rename(Tempo = "Há quanto tempo você trabalha na UnB?") %>% 
  select(Tempo) %>% 
  mutate(Classe = "Docentes") 

quadro_te <- consulta_tecnico %>% filter(!is.na(`Há quanto tempo você trabalha na UnB?`)) %>% 
  rename(Tempo = `Há quanto tempo você trabalha na UnB?`) %>% 
  select(Tempo) %>% 
  mutate(Classe = "Técnicos") 

quadrotempo <- rbind(quadro_do, quadro_te) %>% 
  drop_na(Tempo) %>% 
  group_by(Classe, Tempo) %>% 
  tally() %>% mutate(freq_relat=n/sum(n)) %>%  
  mutate(freq_relat=percent((freq_relat), 4)) %>%
  select(-n) %>% 
  spread(Classe, freq_relat)

quadrotempo <- rbind(quadrotempo, (c("Total", "100%", "100%")))


# FIGURAPDA ----------------------------------------------------------------

figuracpa_al <- consulta_aluno %>% 
  rename("Categoria" = `Você já ouviu falar da Comissão Própria de Avaliação (CPA) da UnB?`) %>% 
  select(Categoria) %>% 
  filter(!is.na(Categoria)) %>% 
  mutate(Classe = "Discentes")

figuracpa_do <- consulta_docente %>% 
  rename("Categoria" = `Você já ouviu falar da Comissão Própria de Avaliação (CPA) da UnB?`) %>% 
  select(Categoria) %>% 
  filter(!is.na(Categoria)) %>% 
  mutate(Classe = "Docentes")

figuracpa_te <- consulta_tecnico %>%
  rename("Categoria" = `Você já ouviu falar da Comissão Própria de Avaliação (CPA) da UnB?`) %>% 
  select(Categoria) %>% 
  filter(!is.na(Categoria)) %>% 
  mutate(Classe = "Tecnicos")

figuracpa_dados <- rbind(figuracpa_al, figuracpa_do, figuracpa_te)



figuracpa <- figuracpa_dados %>% 
  ggplot(aes(x = Categoria, group = Classe)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count", show.legend = F) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust = -0.05) +
  scale_y_continuous(labels = percent) +
  labs(y = "Percentual", fill = "", x = "", caption = "Fonte: CAI/DAI/DPO, 2021 UnB") +
  facet_grid(~Classe) +
  theme_bw()

figuracpa


rm(figuracpa_al, figuracpa_do, figuracpa_te)


# FIGURAPDI ----------------------------------------------------------------

figura1_di <- consulta_aluno %>% 
  rename("Categoria" = `Você já ouviu falar do Plano de Desenvolvimento Institucional (PDI) da UnB?`) %>% 
  select(Categoria) %>% 
  filter(!is.na(Categoria)) %>% 
  mutate(Classe = "Discentes")

figura1_do <- consulta_docente %>% 
  rename("Categoria" = `Você já ouviu falar do Plano de Desenvolvimento Institucional (PDI) da UnB?`) %>% 
  select(Categoria) %>% 
  filter(!is.na(Categoria)) %>% 
  mutate(Classe = "Docentes")

figura1_te <- consulta_tecnico %>%
  rename("Categoria" = `Você já ouviu falar do Plano de Desenvolvimento Institucional (PDI) da UnB?`) %>% 
  select(Categoria) %>% 
  filter(!is.na(Categoria)) %>% 
  mutate(Classe = "Tecnicos")



figura1_dados <- rbind(figura1_di, figura1_do, figura1_te)

figura1 <- figura1_dados %>% 
  ggplot(aes(x = Categoria, group = Classe)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count", show.legend = F) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust = -0.5) +
  scale_y_continuous(labels = percent) +
  labs(y = "Percentual", fill = "", x = "", caption = "Fonte: CAI/DAI/DPO, 2021 UnB") +
  facet_grid(~Classe) +
  theme_bw()

figurapdi<- figura1

# QuadroDOCGESTAOTOTAL5 ----------------------------------------------------------------

# pergunta 1

consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`))

consulta_docenteg$"1" <- NA
consulta_docenteg$"1" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "1"), 1, 0)
consulta_docenteg$"2" <- NA
consulta_docenteg$"2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "2"), 1, 0)
consulta_docenteg$"3" <- NA
consulta_docenteg$"3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "3"), 1, 0)
consulta_docenteg$"4" <- NA
consulta_docenteg$"4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "4"), 1, 0)
consulta_docenteg$"5" <- NA
consulta_docenteg$"5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "5"), 1, 0)
consulta_docenteg$"Não sei responder" <- NA
consulta_docenteg$"Não sei responder" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                                           "Não sei responder"), 1, 0)

quadrodocgestao <- consulta_docenteg %>% 
  select("1":"Não sei responder") %>% 
  mutate(Classe = "A comunidade acadêmica é incentivada a participar da definição das políticas de gestão")


quadrodocgestao <- quadrodocgestao%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1`, na.rm = T)/length(`1`)),
            "2" = percent(sum(`2`, na.rm = T)/length(`2`)),
            "3" = percent(sum(`3`, na.rm = T)/length(`3`)),
            "4" = percent(sum(`4`, na.rm = T)/length(`4`)),
            "5" = percent(sum(`5`, na.rm = T)/length(`5`)),
            "Não sei responder" = percent(sum(`Não sei responder`, na.rm = T)/length(`Não sei responder`)))

# Pergunta 2

consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`))

consulta_docenteg$"1.2" <- NA
consulta_docenteg$"1.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.2" <- NA
consulta_docenteg$"2.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.2" <- NA
consulta_docenteg$"3.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.2" <- NA
consulta_docenteg$"4.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.2" <- NA
consulta_docenteg$"5.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder.2" <- NA
consulta_docenteg$"Não sei responder.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                                             "Não sei responder"), 1, 0)

quadrodocgestao2 <- consulta_docenteg %>% 
  select("1.2":"Não sei responder.2") %>% 
  mutate(Classe = "As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ")

quadrodocgestao2 <- quadrodocgestao2%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.2`, na.rm = T)/length(`1.2`)),
            "2" = percent(sum(`2.2`, na.rm = T)/length(`2.2`)),
            "3" = percent(sum(`3.2`, na.rm = T)/length(`3.2`)),
            "4" = percent(sum(`4.2`, na.rm = T)/length(`4.2`)),
            "5" = percent(sum(`5.2`, na.rm = T)/length(`5.2`)),
            "Não sei responder" = percent(sum(`Não sei responder.2`, na.rm = T)/length(`Não sei responder.2`)))

quadrodocgestaototal <- rbind(quadrodocgestao, quadrodocgestao2)


# Pergunta 3

consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`))

consulta_docenteg$"1.3" <- NA
consulta_docenteg$"1.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.3" <- NA
consulta_docenteg$"2.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.3" <- NA
consulta_docenteg$"3.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.3" <- NA
consulta_docenteg$"4.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.3" <- NA
consulta_docenteg$"5.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder3" <- NA
consulta_docenteg$"Não sei responder3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                                            "Não sei responder"), 1, 0)

quadrodocgestao3 <- consulta_docenteg %>% 
  select("1.3":"Não sei responder3") %>% 
  mutate(Classe = "Os meios de comunicação da Instituição são eficazes")

quadrodocgestao3 <- quadrodocgestao3%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.3`, na.rm = T)/length(`1.3`)),
            "2" = percent(sum(`2.3`, na.rm = T)/length(`2.3`)),
            "3" = percent(sum(`3.3`, na.rm = T)/length(`3.3`)),
            "4" = percent(sum(`4.3`, na.rm = T)/length(`4.3`)),
            "5" = percent(sum(`5.3`, na.rm = T)/length(`5.3`)),
            "Não sei responder" = percent(sum(`Não sei responder3`, na.rm = T)/length(`Não sei responder3`)))


quadrodocgestaototal2 <- rbind(quadrodocgestaototal, quadrodocgestao3)


# Pergunta 4


consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`))

consulta_docenteg$"1.4" <- NA
consulta_docenteg$"1.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.4" <- NA
consulta_docenteg$"2.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.4" <- NA
consulta_docenteg$"3.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.4" <- NA
consulta_docenteg$"4.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.4" <- NA
consulta_docenteg$"5.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder4" <- NA
consulta_docenteg$"Não sei responder4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                                            "Não sei responder"), 1, 0)

quadrodocgestao4 <- consulta_docenteg %>% 
  select("1.4":"Não sei responder4") %>% 
  mutate(Classe = "Os canais de comunicação oficiais da UnB são confiáveis.")

quadrodocgestao4 <- quadrodocgestao4%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.4`, na.rm = T)/length(`1.4`)),
            "2" = percent(sum(`2.4`, na.rm = T)/length(`2.4`)),
            "3" = percent(sum(`3.4`, na.rm = T)/length(`3.4`)),
            "4" = percent(sum(`4.4`, na.rm = T)/length(`4.4`)),
            "5" = percent(sum(`5.4`, na.rm = T)/length(`5.4`)),
            "Não sei responder" = percent(sum(`Não sei responder4`, na.rm = T)/length(`Não sei responder4`)))


quadrodocgestaototal3 <- rbind(quadrodocgestaototal2, quadrodocgestao4)


# pergunta 5


consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`))

consulta_docenteg$"1.5" <- NA
consulta_docenteg$"1.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.5" <- NA
consulta_docenteg$"2.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.5" <- NA
consulta_docenteg$"3.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.5" <- NA
consulta_docenteg$"4.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.5" <- NA
consulta_docenteg$"5.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder5" <- NA
consulta_docenteg$"Não sei responder5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                                            "Não sei responder"), 1, 0)

quadrodocgestao5 <- consulta_docenteg %>% 
  select("1.5":"Não sei responder5") %>% 
  mutate(Classe = "Os programas de apoio aos estudantes funcionam de modo satisfatório.")

quadrodocgestao5 <- quadrodocgestao5%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.5`, na.rm = T)/length(`1.5`)),
            "2" = percent(sum(`2.5`, na.rm = T)/length(`2.5`)),
            "3" = percent(sum(`3.5`, na.rm = T)/length(`3.5`)),
            "4" = percent(sum(`4.5`, na.rm = T)/length(`4.5`)),
            "5" = percent(sum(`5.5`, na.rm = T)/length(`5.5`)),
            "Não sei responder" = percent(sum(`Não sei responder5`, na.rm = T)/length(`Não sei responder5`)))

quadrodocgestaototal4 <- rbind(quadrodocgestaototal3, quadrodocgestao5)

# QuadroDISGESTAOTOTAL4 ----------------------------------------------------------------



# pergunta 1


consulta_discenteg <- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`))

consulta_discenteg$"1" <- NA
consulta_discenteg$"1" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                            "1"), 1, 0)
consulta_discenteg$"2" <- NA
consulta_discenteg$"2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                            "2"), 1, 0)
consulta_discenteg$"3" <- NA
consulta_discenteg$"3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                            "3"), 1, 0)
consulta_discenteg$"4" <- NA
consulta_discenteg$"4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                            "4"), 1, 0)
consulta_discenteg$"5" <- NA
consulta_discenteg$"5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                            "5"), 1, 0)
consulta_discenteg$"Não sei responder" <- NA
consulta_discenteg$"Não sei responder" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                                            "Não sei responder"), 1, 0)


quadrodisgestao <- consulta_discenteg %>% 
  select("1":"Não sei responder") %>% 
  mutate(Classe = "A comunidade acadêmica é incentivada a participar da definição das políticas de gestão")

quadrodisgestao <- quadrodisgestao%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1`, na.rm = T)/length(`1`)),
            "2" = percent(sum(`2`, na.rm = T)/length(`2`)),
            "3" = percent(sum(`3`, na.rm = T)/length(`3`)),
            "4" = percent(sum(`4`, na.rm = T)/length(`4`)),
            "5" = percent(sum(`5`, na.rm = T)/length(`5`)),
            "Não sei responder" = percent(sum(`Não sei responder`, na.rm = T)/length(`Não sei responder`)))

# Pergunta 2

consulta_discenteg <- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`))

consulta_discenteg$"1.2" <- NA
consulta_discenteg$"1.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.2" <- NA
consulta_discenteg$"2.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.2" <- NA
consulta_discenteg$"3.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.2" <- NA
consulta_discenteg$"4.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.2" <- NA
consulta_discenteg$"5.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder.2" <- NA
consulta_discenteg$"Não sei responder.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                                              "Não sei responder"), 1, 0)

quadrodisgestao2 <- consulta_discenteg %>% 
  select("1.2":"Não sei responder.2") %>% 
  mutate(Classe = "As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ")

quadrodisgestao2 <- quadrodisgestao2%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.2`, na.rm = T)/length(`1.2`)),
            "2" = percent(sum(`2.2`, na.rm = T)/length(`2.2`)),
            "3" = percent(sum(`3.2`, na.rm = T)/length(`3.2`)),
            "4" = percent(sum(`4.2`, na.rm = T)/length(`4.2`)),
            "5" = percent(sum(`5.2`, na.rm = T)/length(`5.2`)),
            "Não sei responder" = percent(sum(`Não sei responder.2`, na.rm = T)/length(`Não sei responder.2`)))

quadrodisgestaototal <- rbind(quadrodisgestao, quadrodisgestao2)


# Pergunta 3

consulta_discenteg <- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`))

consulta_discenteg$"1.3" <- NA
consulta_discenteg$"1.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.3" <- NA
consulta_discenteg$"2.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.3" <- NA
consulta_discenteg$"3.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.3" <- NA
consulta_discenteg$"4.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.3" <- NA
consulta_discenteg$"5.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder3" <- NA
consulta_discenteg$"Não sei responder3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                                             "Não sei responder"), 1, 0)

quadrodisgestao3 <- consulta_discenteg %>% 
  select("1.3":"Não sei responder3") %>% 
  mutate(Classe = "Os meios de comunicação da Instituição são eficazes")
quadrodisgestao3 <- quadrodisgestao3%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.3`, na.rm = T)/length(`1.3`)),
            "2" = percent(sum(`2.3`, na.rm = T)/length(`2.3`)),
            "3" = percent(sum(`3.3`, na.rm = T)/length(`3.3`)),
            "4" = percent(sum(`4.3`, na.rm = T)/length(`4.3`)),
            "5" = percent(sum(`5.3`, na.rm = T)/length(`5.3`)),
            "Não sei responder" = percent(sum(`Não sei responder3`, na.rm = T)/length(`Não sei responder3`)))

quadrodisgestaototal2 <- rbind(quadrodisgestaototal, quadrodisgestao3)


# Pergunta 4


consulta_discenteg <- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`))

consulta_discenteg$"1.4" <- NA
consulta_discenteg$"1.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.4" <- NA
consulta_discenteg$"2.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.4" <- NA
consulta_discenteg$"3.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.4" <- NA
consulta_discenteg$"4.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.4" <- NA
consulta_discenteg$"5.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder4" <- NA
consulta_discenteg$"Não sei responder4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                                             "Não sei responder"), 1, 0)


quadrodisgestao4 <- consulta_discenteg %>% 
  select("1.4":"Não sei responder4") %>% 
  mutate(Classe = "Os canais de comunicação oficiais da UnB são confiáveis.")

quadrodisgestao4 <- quadrodisgestao4%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.4`, na.rm = T)/length(`1.4`)),
            "2" = percent(sum(`2.4`, na.rm = T)/length(`2.4`)),
            "3" = percent(sum(`3.4`, na.rm = T)/length(`3.4`)),
            "4" = percent(sum(`4.4`, na.rm = T)/length(`4.4`)),
            "5" = percent(sum(`5.4`, na.rm = T)/length(`5.4`)),
            "Não sei responder" = percent(sum(`Não sei responder4`, na.rm = T)/length(`Não sei responder4`)))

quadrodisgestaototal3 <- rbind(quadrodisgestaototal2, quadrodisgestao4)


# pergunta 5


consulta_discenteg<- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`))

consulta_discenteg$"1.5" <- NA
consulta_discenteg$"1.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.5" <- NA
consulta_discenteg$"2.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.5" <- NA
consulta_discenteg$"3.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.5" <- NA
consulta_discenteg$"4.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.5" <- NA
consulta_discenteg$"5.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder5" <- NA
consulta_discenteg$"Não sei responder5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                                             "Não sei responder"), 1, 0)

quadrodisgestao5 <- consulta_discenteg %>% 
  select("1.5":"Não sei responder5") %>% 
  mutate(Classe = "Os programas de apoio aos estudantes funcionam de modo satisfatório.")

quadrodisgestao5 <- quadrodisgestao5%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.5`, na.rm = T)/length(`1.5`)),
            "2" = percent(sum(`2.5`, na.rm = T)/length(`2.5`)),
            "3" = percent(sum(`3.5`, na.rm = T)/length(`3.5`)),
            "4" = percent(sum(`4.5`, na.rm = T)/length(`4.5`)),
            "5" = percent(sum(`5.5`, na.rm = T)/length(`5.5`)),
            "Não sei responder" = percent(sum(`Não sei responder5`, na.rm = T)/length(`Não sei responder5`)))

quadrodisgestaototal4 <- rbind(quadrodisgestaototal3, quadrodisgestao5)




# QuadroTECGESTAOTOTAL5 ----------------------------------------------------------------

consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`))

consulta_tecnicog$"1" <- NA
consulta_tecnicog$"1" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "1"), 1, 0)
consulta_tecnicog$"2" <- NA
consulta_tecnicog$"2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "2"), 1, 0)
consulta_tecnicog$"3" <- NA
consulta_tecnicog$"3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "3"), 1, 0)
consulta_tecnicog$"4" <- NA
consulta_tecnicog$"4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "4"), 1, 0)
consulta_tecnicog$"5" <- NA
consulta_tecnicog$"5" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "5"), 1, 0)
consulta_tecnicog$"Não sei responder" <- NA
consulta_tecnicog$"Não sei responder" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                                           "Não sei responder"), 1, 0)


quadrotecgestao <- consulta_tecnicog %>% 
  select("1":"Não sei responder") %>% 
  mutate(Classe = "A comunidade acadêmica é incentivada a participar da definição das políticas de gestão")

quadrotecgestao <- quadrotecgestao%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1`, na.rm = T)/length(`1`)),
            "2" = percent(sum(`2`, na.rm = T)/length(`2`)),
            "3" = percent(sum(`3`, na.rm = T)/length(`3`)),
            "4" = percent(sum(`4`, na.rm = T)/length(`4`)),
            "5" = percent(sum(`5`, na.rm = T)/length(`5`)),
            "Não sei responder" = percent(sum(`Não sei responder`, na.rm = T)/length(`Não sei responder`)))


# Pergunta 2

consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`))

consulta_tecnicog$"1.2" <- NA
consulta_tecnicog$"1.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.2" <- NA
consulta_tecnicog$"2.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.2" <- NA
consulta_tecnicog$"3.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.2" <- NA
consulta_tecnicog$"4.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.2" <- NA
consulta_tecnicog$"5.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder.2" <- NA
consulta_tecnicog$"Não sei responder.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                                             "Não sei responder"), 1, 0)

quadrotecgestao2 <- consulta_tecnicog %>% 
  select("1.2":"Não sei responder.2") %>% 
  mutate(Classe = "As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ")

quadrotecgestao2 <- quadrotecgestao2%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.2`, na.rm = T)/length(`1.2`)),
            "2" = percent(sum(`2.2`, na.rm = T)/length(`2.2`)),
            "3" = percent(sum(`3.2`, na.rm = T)/length(`3.2`)),
            "4" = percent(sum(`4.2`, na.rm = T)/length(`4.2`)),
            "5" = percent(sum(`5.2`, na.rm = T)/length(`5.2`)),
            "Não sei responder" = percent(sum(`Não sei responder.2`, na.rm = T)/length(`Não sei responder.2`)))

quadrotecgestaototal <- rbind(quadrotecgestao, quadrotecgestao2)


# Pergunta 3

consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`))

consulta_tecnicog$"1.3" <- NA
consulta_tecnicog$"1.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.3" <- NA
consulta_tecnicog$"2.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.3" <- NA
consulta_tecnicog$"3.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.3" <- NA
consulta_tecnicog$"4.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.3" <- NA
consulta_tecnicog$"5.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder3" <- NA
consulta_tecnicog$"Não sei responder3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                                            "Não sei responder"), 1, 0)


quadrotecgestao3 <- consulta_tecnicog %>% 
  select("1.3":"Não sei responder3") %>% 
  mutate(Classe = "Os meios de comunicação da Instituição são eficazes")

quadrotecgestao3 <- quadrotecgestao3%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.3`, na.rm = T)/length(`1.3`)),
            "2" = percent(sum(`2.3`, na.rm = T)/length(`2.3`)),
            "3" = percent(sum(`3.3`, na.rm = T)/length(`3.3`)),
            "4" = percent(sum(`4.3`, na.rm = T)/length(`4.3`)),
            "5" = percent(sum(`5.3`, na.rm = T)/length(`5.3`)),
            "Não sei responder" = percent(sum(`Não sei responder3`, na.rm = T)/length(`Não sei responder3`)))

quadrotecgestaototal2 <- rbind(quadrotecgestaototal, quadrotecgestao3)



# Pergunta 4


consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`))

consulta_tecnicog$"1.4" <- NA
consulta_tecnicog$"1.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.4" <- NA
consulta_tecnicog$"2.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.4" <- NA
consulta_tecnicog$"3.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.4" <- NA
consulta_tecnicog$"4.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.4" <- NA
consulta_tecnicog$"5.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder4" <- NA
consulta_tecnicog$"Não sei responder4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                                            "Não sei responder"), 1, 0)

quadrotecgestao4 <- consulta_tecnicog %>% 
  select("1.4":"Não sei responder4") %>% 
  mutate(Classe = "Os canais de comunicação oficiais da UnB são confiáveis.")

quadrotecgestao4 <- quadrotecgestao4%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.4`, na.rm = T)/length(`1.4`)),
            "2" = percent(sum(`2.4`, na.rm = T)/length(`2.4`)),
            "3" = percent(sum(`3.4`, na.rm = T)/length(`3.4`)),
            "4" = percent(sum(`4.4`, na.rm = T)/length(`4.4`)),
            "5" = percent(sum(`5.4`, na.rm = T)/length(`5.4`)),
            "Não sei responder" = percent(sum(`Não sei responder4`, na.rm = T)/length(`Não sei responder4`)))

quadrotecgestaototal3 <- rbind(quadrotecgestaototal2, quadrotecgestao4)

# pergunta 5


consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`))

consulta_tecnicog$"1.5" <- NA
consulta_tecnicog$"1.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.5" <- NA
consulta_tecnicog$"2.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.5" <- NA
consulta_tecnicog$"3.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.5" <- NA
consulta_tecnicog$"4.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.5" <- NA
consulta_tecnicog$"5.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder5" <- NA
consulta_tecnicog$"Não sei responder5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                                            "Não sei responder"), 1, 0)

quadrotecgestao5 <- consulta_tecnicog %>% 
  select("1.5":"Não sei responder5") %>% 
  mutate(Classe = "Os programas de apoio aos estudantes funcionam de modo satisfatório.")

quadrotecgestao5 <- quadrotecgestao5%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1.5`, na.rm = T)/length(`1.5`)),
            "2" = percent(sum(`2.5`, na.rm = T)/length(`2.5`)),
            "3" = percent(sum(`3.5`, na.rm = T)/length(`3.5`)),
            "4" = percent(sum(`4.5`, na.rm = T)/length(`4.5`)),
            "5" = percent(sum(`5.5`, na.rm = T)/length(`5.5`)),
            "Não sei responder" = percent(sum(`Não sei responder5`, na.rm = T)/length(`Não sei responder5`)))

quadrotecgestaototal4 <- rbind(quadrotecgestaototal3, quadrotecgestao5)





# QUADROTURNO ------------------------------------
consulta_tecnico$Manhã <- NA
consulta_tecnico$Manhã <- ifelse(str_detect(consulta_tecnico$`Qual é o seu turno de trabalho?`, 
                                            "Manhã"), 1, 0)

consulta_tecnico$Tarde <- NA
consulta_tecnico$Tarde <- ifelse(str_detect(consulta_tecnico$`Qual é o seu turno de trabalho?`, 
                                            "Tarde"), 1, 0)

consulta_tecnico$Noturno <- NA
consulta_tecnico$Noturno <- ifelse(str_detect(consulta_tecnico$`Qual é o seu turno de trabalho?`, 
                                              "Noturno"), 1, 0)

consulta_tecnico$Dia <- NA
consulta_tecnico$Dia <- ifelse(str_detect(consulta_tecnico$`Qual é o seu turno de trabalho?`, 
                                          "Dia"), 1, 0)


quadroturno <- consulta_tecnico %>% 
  select(Manhã:Dia) %>% 
  mutate(Classe = "Técnicos")

quadroturno <- quadroturno%>% 
  group_by(Classe) %>% 
  summarise("Manhã" = percent(sum(Manhã, na.rm = T)/length(Manhã)),
            Tarde = percent(sum(Tarde, na.rm = T)/length(Tarde)),
            Noturno = percent(sum(Noturno, na.rm = T)/length(Noturno)),
            Dia = percent(sum(Dia, na.rm = T)/length(Dia)))

# QUADROMEIOSDIS e QUADROMEIOSDOCTEC ----------------------------------------------------------------

consulta_aluno$SiteOficial <- NA
consulta_aluno$SiteOficial <- ifelse(str_detect(consulta_aluno$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                                "Site oficial da UnB"), 1, 0)

consulta_aluno$Facebook <- NA
consulta_aluno$Facebook <- ifelse(str_detect(consulta_aluno$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                             "Perfil oficial do Facebook da UnB"), 1, 0)

consulta_aluno$Instagram <- NA
consulta_aluno$Instagram <- ifelse(str_detect(consulta_aluno$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                              "Perfil oficial do Instagram da UnB"), 1, 0)

consulta_aluno$Sigaa <- NA
consulta_aluno$Sigaa <- ifelse(str_detect(consulta_aluno$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                          "Sigaa"), 1, 0)
consulta_aluno$Teams <- NA
consulta_aluno$Teams <- ifelse(str_detect(consulta_aluno$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                          "Teams"), 1, 0)

consulta_aluno$Twitter <- NA
consulta_aluno$Twitter <- ifelse(str_detect(consulta_aluno$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                            "Perfil oficial do Twitter da UnB"), 1, 0)

# docente

consulta_docente$SiteOficial <- NA
consulta_docente$SiteOficial <- ifelse(str_detect(consulta_docente$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                                  "Site oficial da UnB"), 1, 0)

consulta_docente$Facebook <- NA
consulta_docente$Facebook <- ifelse(str_detect(consulta_docente$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                               "Perfil oficial do Facebook da UnB"), 1, 0)

consulta_docente$Instagram <- NA
consulta_docente$Instagram <- ifelse(str_detect(consulta_docente$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                                "Perfil oficial do Instagram da UnB"), 1, 0)

consulta_docente$Sigaa <- NA
consulta_docente$Sigaa <- ifelse(str_detect(consulta_docente$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                            "Sigaa"), 1, 0)
consulta_docente$Teams <- NA
consulta_docente$Teams <- ifelse(str_detect(consulta_docente$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                            "Teams"), 1, 0)

consulta_docente$Twitter <- NA
consulta_docente$Twitter <- ifelse(str_detect(consulta_docente$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                              "Perfil oficial do Twitter da UnB"), 1, 0)

# tecnico

consulta_tecnico$SiteOficial <- NA
consulta_tecnico$SiteOficial <- ifelse(str_detect(consulta_tecnico$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                                  "Site oficial da UnB"), 1, 0)

consulta_tecnico$Facebook <- NA
consulta_tecnico$Facebook <- ifelse(str_detect(consulta_tecnico$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                               "Pefil oficial do Facebook da UnB"), 1, 0)

consulta_tecnico$Instagram <- NA
consulta_tecnico$Instagram <- ifelse(str_detect(consulta_tecnico$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                                "Pefil oficial  do Instagram da UnB"), 1, 0)

consulta_tecnico$Sigaa <- NA
consulta_tecnico$Sigaa <- ifelse(str_detect(consulta_tecnico$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                            "Sigaa"), 1, 0)
consulta_tecnico$Teams <- NA
consulta_tecnico$Teams <- ifelse(str_detect(consulta_tecnico$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                            "Teams"), 1, 0)

consulta_tecnico$Twitter <- NA
consulta_tecnico$Twitter <- ifelse(str_detect(consulta_tecnico$`Quais os meios de comunicação da UnB você utiliza para obter informações institucionais?`, 
                                              "Perfil oficial do Twitter da UnB"), 1, 0)

quadro5_di <- consulta_aluno %>% 
  select(SiteOficial:Twitter) %>% 
  mutate(Classe = "Discentes")

quadro5_do <- consulta_docente %>% 
  select(SiteOficial:Twitter) %>% 
  mutate(Classe = "Docentes")

quadro5_te <- consulta_tecnico %>% 
  select(SiteOficial:Twitter) %>% 
  mutate(Classe = "Técnicos")


quadromeiosdoctecdis <- rbind(quadro5_do, quadro5_te, quadro5_di) %>% 
  group_by(Classe) %>% 
  summarise("Site Oficial" = percent(sum(SiteOficial, na.rm = T)/length(SiteOficial)),
            Facebook = percent(sum(Facebook, na.rm = T)/length(Facebook)),
            "Perfil oficial  do Instagram da UnB" = percent(sum(Instagram, na.rm = T)/length(Instagram)),
            Sigaa = percent(sum(Sigaa, na.rm = T)/length(Sigaa)),
            Teams = percent(sum(Teams, na.rm = T)/length(Teams)),
            Twitter = percent(sum(Twitter, na.rm = T)/length(Twitter)))

# Infraestrutura

# Alunos  Rede UnB Wireless



consulta_discenter <- consulta_aluno %>% filter(!is.na(`Rede UnB Wireless`))

consulta_discenter$"Ótima" <- NA
consulta_discenter$"Ótima" <- ifelse(str_detect(consulta_discenter$`Rede UnB Wireless`, 
                                                "Ótima"), 1, 0)
consulta_discenter$"Boa" <- NA
consulta_discenter$"Boa" <- ifelse(str_detect(consulta_discenter$`Rede UnB Wireless`, 
                                              "Boa"), 1, 0)

consulta_discenter$"Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Rede UnB Wireless`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"Ruim" <- NA
consulta_discenter$"Ruim" <- ifelse(str_detect(consulta_discenter$`Rede UnB Wireless`, 
                                               "Ruim"), 1, 0)

consulta_discenter$"Péssima" <- NA
consulta_discenter$"Péssima" <- ifelse(str_detect(consulta_discenter$`Rede UnB Wireless`, 
                                                  "Péssima"), 1, 0)


quadrodisgestaor <- consulta_discenter %>% 
  mutate(Classe = "Rede UnB Wireless")

quadrodisgestaor <- quadrodisgestaor%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`Ótima`, na.rm = T)/length(`Ótima`)),
             Boa = percent(sum(`Boa`, na.rm = T)/length(`Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`Não utilizei o local/serviço este ano.`, na.rm = T)/length(`Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`Ruim`, na.rm = T)/length(`Ruim`)),
             Péssima = percent(sum(`Péssima`, na.rm = T)/length(`Péssima`)))

# Alunos  Acessibilidade fisica

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Condições de acessibilidade`))

consulta_discenter$"AÓtima" <- NA
consulta_discenter$"AÓtima" <- ifelse(str_detect(consulta_discenter$`Condições de acessibilidade`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"ABoa" <- NA
consulta_discenter$"ABoa" <- ifelse(str_detect(consulta_discenter$`Condições de acessibilidade`, 
                                               "Boa"), 1, 0)

consulta_discenter$"ANão utilizei o local/serviço este ano." <- NA
consulta_discenter$"ANão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Condições de acessibilidade`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"ARuim" <- NA
consulta_discenter$"ARuim" <- ifelse(str_detect(consulta_discenter$`Condições de acessibilidade`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"APéssima" <- NA
consulta_discenter$"APéssima" <- ifelse(str_detect(consulta_discenter$`Condições de acessibilidade`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor1 <- consulta_discenter %>% 
  mutate(Classe = "Condições de acessibilidade")

quadrodisgestaor1 <- quadrodisgestaor1%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`AÓtima`, na.rm = T)/length(`AÓtima`)),
             Boa = percent(sum(`ABoa`, na.rm = T)/length(`ABoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`ANão utilizei o local/serviço este ano.`, na.rm = T)/length(`ANão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`ARuim`, na.rm = T)/length(`ARuim`)),
             Péssima = percent(sum(`APéssima`, na.rm = T)/length(`APéssima`)))

# Alunos  Auditorio

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Condições de gerais de auditorio`))

consulta_discenter$"BÓtima" <- NA
consulta_discenter$"BÓtima" <- ifelse(str_detect(consulta_discenter$`Condições de gerais de auditorio`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"BBoa" <- NA
consulta_discenter$"BBoa" <- ifelse(str_detect(consulta_discenter$`Condições de acessibilidade`, 
                                               "Boa"), 1, 0)

consulta_discenter$"BNão utilizei o local/serviço este ano." <- NA
consulta_discenter$"BNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Condições de gerais de auditorio`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"BRuim" <- NA
consulta_discenter$"BRuim" <- ifelse(str_detect(consulta_discenter$`Condições de gerais de auditorio`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"BPéssima" <- NA
consulta_discenter$"BPéssima" <- ifelse(str_detect(consulta_discenter$`Condições de gerais de auditorio`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor2 <- consulta_discenter %>% 
  mutate(Classe = "Condições de gerais de auditorio")

quadrodisgestaor2 <- quadrodisgestaor2%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`BÓtima`, na.rm = T)/length(`BÓtima`)),
             Boa = percent(sum(`BBoa`, na.rm = T)/length(`BBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`BNão utilizei o local/serviço este ano.`, na.rm = T)/length(`BNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`BRuim`, na.rm = T)/length(`BRuim`)),
             Péssima = percent(sum(`BPéssima`, na.rm = T)/length(`BPéssima`)))

# Alunos  Espaço de convivencia

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Espaço de Convivencia e alimentação (exceto RU)`))

consulta_discenter$"CÓtima" <- NA
consulta_discenter$"CÓtima" <- ifelse(str_detect(consulta_discenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"CBoa" <- NA
consulta_discenter$"CBoa" <- ifelse(str_detect(consulta_discenter$`Condições de acessibilidade`, 
                                               "Boa"), 1, 0)

consulta_discenter$"CNão utilizei o local/serviço este ano." <- NA
consulta_discenter$"CNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"CRuim" <- NA
consulta_discenter$"CRuim" <- ifelse(str_detect(consulta_discenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"CPéssima" <- NA
consulta_discenter$"CPéssima" <- ifelse(str_detect(consulta_discenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor3 <- consulta_discenter %>% 
  mutate(Classe = "Espaço de Convivencia e alimentação (exceto RU)")

quadrodisgestaor3 <- quadrodisgestaor3%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`CÓtima`, na.rm = T)/length(`CÓtima`)),
             Boa = percent(sum(`CBoa`, na.rm = T)/length(`CBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`CNão utilizei o local/serviço este ano.`, na.rm = T)/length(`CNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`CRuim`, na.rm = T)/length(`CRuim`)),
             Péssima = percent(sum(`CPéssima`, na.rm = T)/length(`CPéssima`)))


# BCE Estudo

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Espaço de Estudo da BCE`))

consulta_discenter$"DÓtima" <- NA
consulta_discenter$"DÓtima" <- ifelse(str_detect(consulta_discenter$`Espaço de Estudo da BCE`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"DBoa" <- NA
consulta_discenter$"DBoa" <- ifelse(str_detect(consulta_discenter$`Espaço de Estudo da BCE`, 
                                               "Boa"), 1, 0)

consulta_discenter$"DNão utilizei o local/serviço este ano." <- NA
consulta_discenter$"DNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Espaço de Estudo da BCE`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"DRuim" <- NA
consulta_discenter$"DRuim" <- ifelse(str_detect(consulta_discenter$`Espaço de Estudo da BCE`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"DPéssima" <- NA
consulta_discenter$"DPéssima" <- ifelse(str_detect(consulta_discenter$`Espaço de Estudo da BCE`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor4 <- consulta_discenter %>% 
  mutate(Classe = "Espaço de Estudo da BCE")

quadrodisgestaor4 <- quadrodisgestaor4%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`DÓtima`, na.rm = T)/length(`DÓtima`)),
             Boa = percent(sum(`DBoa`, na.rm = T)/length(`DBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`DNão utilizei o local/serviço este ano.`, na.rm = T)/length(`DNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`DRuim`, na.rm = T)/length(`DRuim`)),
             Péssima = percent(sum(`DPéssima`, na.rm = T)/length(`DPéssima`)))



# Estacionamento

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Estacionamento`))

consulta_discenter$"FÓtima" <- NA
consulta_discenter$"FÓtima" <- ifelse(str_detect(consulta_discenter$`Estacionamento`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"FDBoa" <- NA
consulta_discenter$"FBoa" <- ifelse(str_detect(consulta_discenter$`Estacionamento`, 
                                               "Boa"), 1, 0)

consulta_discenter$"FNão utilizei o local/serviço este ano." <- NA
consulta_discenter$"FNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Estacionamento`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"FRuim" <- NA
consulta_discenter$"FRuim" <- ifelse(str_detect(consulta_discenter$`Estacionamento`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"FPéssima" <- NA
consulta_discenter$"FPéssima" <- ifelse(str_detect(consulta_discenter$`Estacionamento`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor6 <- consulta_discenter %>% 
  mutate(Classe = "Estacionamento")

quadrodisgestaor6 <- quadrodisgestaor6%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`FÓtima`, na.rm = T)/length(`FÓtima`)),
             Boa = percent(sum(`FBoa`, na.rm = T)/length(`FBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`FNão utilizei o local/serviço este ano.`, na.rm = T)/length(`FNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`FRuim`, na.rm = T)/length(`FRuim`)),
             Péssima = percent(sum(`FPéssima`, na.rm = T)/length(`FPéssima`)))


# Iluminacao Publica

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Iluminação pública`))

consulta_discenter$"GÓtima" <- NA
consulta_discenter$"GÓtima" <- ifelse(str_detect(consulta_discenter$`Iluminação pública`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"GDBoa" <- NA
consulta_discenter$"GBoa" <- ifelse(str_detect(consulta_discenter$`Iluminação pública`, 
                                               "Boa"), 1, 0)

consulta_discenter$"GNão utilizei o local/serviço este ano." <- NA
consulta_discenter$"GNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Iluminação pública`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"GRuim" <- NA
consulta_discenter$"GRuim" <- ifelse(str_detect(consulta_discenter$`Iluminação pública`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"GPéssima" <- NA
consulta_discenter$"GPéssima" <- ifelse(str_detect(consulta_discenter$`Iluminação pública`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor7 <- consulta_discenter %>% 
  mutate(Classe = "Iluminação pública")

quadrodisgestaor7 <- quadrodisgestaor7%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`GÓtima`, na.rm = T)/length(`GÓtima`)),
             Boa = percent(sum(`GBoa`, na.rm = T)/length(`GBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`GNão utilizei o local/serviço este ano.`, na.rm = T)/length(`GNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`GRuim`, na.rm = T)/length(`GRuim`)),
             Péssima = percent(sum(`GPéssima`, na.rm = T)/length(`GPéssima`)))

# Instalações sanitarias

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Instalações Sanitarias`))

consulta_discenter$"HÓtima" <- NA
consulta_discenter$"HÓtima" <- ifelse(str_detect(consulta_discenter$`Instalações Sanitarias`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"HDBoa" <- NA
consulta_discenter$"HBoa" <- ifelse(str_detect(consulta_discenter$`Instalações Sanitarias`, 
                                               "Boa"), 1, 0)

consulta_discenter$"HNão utilizei o local/serviço este ano." <- NA
consulta_discenter$"HNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Instalações Sanitarias`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"HRuim" <- NA
consulta_discenter$"HRuim" <- ifelse(str_detect(consulta_discenter$`Instalações Sanitarias`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"HPéssima" <- NA
consulta_discenter$"HPéssima" <- ifelse(str_detect(consulta_discenter$`Instalações Sanitarias`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor8 <- consulta_discenter %>% 
  mutate(Classe = "Instalações Sanitarias")

quadrodisgestaor8 <- quadrodisgestaor8%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`HÓtima`, na.rm = T)/length(`HÓtima`)),
             Boa = percent(sum(`HBoa`, na.rm = T)/length(`HBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`HNão utilizei o local/serviço este ano.`, na.rm = T)/length(`HNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`HRuim`, na.rm = T)/length(`HRuim`)),
             Péssima = percent(sum(`HPéssima`, na.rm = T)/length(`HPéssima`)))

# Ru

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Restaurante Universitario`))

consulta_discenter$"IÓtima" <- NA
consulta_discenter$"IÓtima" <- ifelse(str_detect(consulta_discenter$`Restaurante Universitario`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"IBoa" <- NA
consulta_discenter$"IBoa" <- ifelse(str_detect(consulta_discenter$`Restaurante Universitario`, 
                                               "Boa"), 1, 0)

consulta_discenter$"INão utilizei o local/serviço este ano." <- NA
consulta_discenter$"INão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Restaurante Universitario`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"IRuim" <- NA
consulta_discenter$"IRuim" <- ifelse(str_detect(consulta_discenter$`Restaurante Universitario`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"IPéssima" <- NA
consulta_discenter$"IPéssima" <- ifelse(str_detect(consulta_discenter$`Restaurante Universitario`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor9 <- consulta_discenter %>% 
  mutate(Classe = "Restaurante Universitario")

quadrodisgestaor9 <- quadrodisgestaor9%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`IÓtima`, na.rm = T)/length(`IÓtima`)),
             Boa = percent(sum(`IBoa`, na.rm = T)/length(`IBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`INão utilizei o local/serviço este ano.`, na.rm = T)/length(`INão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`IRuim`, na.rm = T)/length(`IRuim`)),
             Péssima = percent(sum(`IPéssima`, na.rm = T)/length(`IPéssima`)))

# Salas de Aula

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Salas de aula`))

consulta_discenter$"1Ótima" <- NA
consulta_discenter$"1Ótima" <- ifelse(str_detect(consulta_discenter$`Salas de aula`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"1Boa" <- NA
consulta_discenter$"1Boa" <- ifelse(str_detect(consulta_discenter$`Salas de aula`, 
                                               "Boa"), 1, 0)

consulta_discenter$"1Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"1Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Salas de aula`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"1Ruim" <- NA
consulta_discenter$"1Ruim" <- ifelse(str_detect(consulta_discenter$`Salas de aula`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"1Péssima" <- NA
consulta_discenter$"1Péssima" <- ifelse(str_detect(consulta_discenter$`Salas de aula`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor10 <- consulta_discenter %>% 
  mutate(Classe = "Salas de aula")

quadrodisgestaor10 <- quadrodisgestaor10%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`1Ótima`, na.rm = T)/length(`1Ótima`)),
             Boa = percent(sum(`1Boa`, na.rm = T)/length(`1Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`1Não utilizei o local/serviço este ano.`, na.rm = T)/length(`1Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`1Ruim`, na.rm = T)/length(`1Ruim`)),
             Péssima = percent(sum(`1Péssima`, na.rm = T)/length(`1Péssima`)))

# Sala dos professores

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Sala dos Professores`))

consulta_discenter$"2Ótima" <- NA
consulta_discenter$"2Ótima" <- ifelse(str_detect(consulta_discenter$`Sala dos Professores`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"2Boa" <- NA
consulta_discenter$"2Boa" <- ifelse(str_detect(consulta_discenter$`Sala dos Professores`, 
                                               "Boa"), 1, 0)

consulta_discenter$"2Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"2Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Sala dos Professores`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"2Ruim" <- NA
consulta_discenter$"2Ruim" <- ifelse(str_detect(consulta_discenter$`Sala dos Professores`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"2Péssima" <- NA
consulta_discenter$"2Péssima" <- ifelse(str_detect(consulta_discenter$`Sala dos Professores`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor11 <- consulta_discenter %>% 
  mutate(Classe = "Sala dos Professores")

quadrodisgestaor11 <- quadrodisgestaor11%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`2Ótima`, na.rm = T)/length(`2Ótima`)),
             Boa = percent(sum(`2Boa`, na.rm = T)/length(`2Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`2Não utilizei o local/serviço este ano.`, na.rm = T)/length(`2Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`2Ruim`, na.rm = T)/length(`2Ruim`)),
             Péssima = percent(sum(`2Péssima`, na.rm = T)/length(`2Péssima`)))

# Segurança Campus

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Segurança do Campus`))

consulta_discenter$"3Ótima" <- NA
consulta_discenter$"3Ótima" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"3Boa" <- NA
consulta_discenter$"3Boa" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                               "Boa"), 1, 0)

consulta_discenter$"3Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"3Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"3Ruim" <- NA
consulta_discenter$"3Ruim" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"3Péssima" <- NA
consulta_discenter$"3Péssima" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor12 <- consulta_discenter %>% 
  mutate(Classe = "Segurança do Campus")

quadrodisgestaor12 <- quadrodisgestaor12%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`3Ótima`, na.rm = T)/length(`3Ótima`)),
             Boa = percent(sum(`3Boa`, na.rm = T)/length(`3Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`3Não utilizei o local/serviço este ano.`, na.rm = T)/length(`3Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`3Ruim`, na.rm = T)/length(`3Ruim`)),
             Péssima = percent(sum(`3Péssima`, na.rm = T)/length(`3Péssima`)))

# Segurança Campus

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Segurança do Campus`))

consulta_discenter$"3Ótima" <- NA
consulta_discenter$"3Ótima" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"3Boa" <- NA
consulta_discenter$"3Boa" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                               "Boa"), 1, 0)

consulta_discenter$"3Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"3Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"3Ruim" <- NA
consulta_discenter$"3Ruim" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"3Péssima" <- NA
consulta_discenter$"3Péssima" <- ifelse(str_detect(consulta_discenter$`Segurança do Campus`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor12 <- consulta_discenter %>% 
  mutate(Classe = "Segurança do Campus")

quadrodisgestaor12 <- quadrodisgestaor12%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`3Ótima`, na.rm = T)/length(`3Ótima`)),
             Boa = percent(sum(`3Boa`, na.rm = T)/length(`3Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`3Não utilizei o local/serviço este ano.`, na.rm = T)/length(`3Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`3Ruim`, na.rm = T)/length(`3Ruim`)),
             Péssima = percent(sum(`3Péssima`, na.rm = T)/length(`3Péssima`)))


# Suporte tecnologico

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Suporte tecnologico para administração`))

consulta_discenter$"4Ótima" <- NA
consulta_discenter$"4Ótima" <- ifelse(str_detect(consulta_discenter$`Suporte tecnologico para administração`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"4Boa" <- NA
consulta_discenter$"4Boa" <- ifelse(str_detect(consulta_discenter$`Suporte tecnologico para administração`, 
                                               "Boa"), 1, 0)

consulta_discenter$"4Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"4Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Suporte tecnologico para administração`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"4Ruim" <- NA
consulta_discenter$"4Ruim" <- ifelse(str_detect(consulta_discenter$`Suporte tecnologico para administração`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"4Péssima" <- NA
consulta_discenter$"4Péssima" <- ifelse(str_detect(consulta_discenter$`Suporte tecnologico para administração`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor13 <- consulta_discenter %>% 
  mutate(Classe = "Suporte tecnologico para administração")

quadrodisgestaor13 <- quadrodisgestaor13%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`4Ótima`, na.rm = T)/length(`4Ótima`)),
             Boa = percent(sum(`4Boa`, na.rm = T)/length(`4Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`4Não utilizei o local/serviço este ano.`, na.rm = T)/length(`4Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`4Ruim`, na.rm = T)/length(`4Ruim`)),
             Péssima = percent(sum(`4Péssima`, na.rm = T)/length(`4Péssima`)))


# Acervo fisico BCE

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Acervo Fisico BCE`))

consulta_discenter$"5Ótima" <- NA
consulta_discenter$"5Ótima" <- ifelse(str_detect(consulta_discenter$`Acervo Fisico BCE`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"5Boa" <- NA
consulta_discenter$"5Boa" <- ifelse(str_detect(consulta_discenter$`Acervo Fisico BCE`, 
                                               "Boa"), 1, 0)

consulta_discenter$"5Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"5Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Acervo Fisico BCE`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"5Ruim" <- NA
consulta_discenter$"5Ruim" <- ifelse(str_detect(consulta_discenter$`Acervo Fisico BCE`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"5Péssima" <- NA
consulta_discenter$"5Péssima" <- ifelse(str_detect(consulta_discenter$`Acervo Fisico BCE`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor14 <- consulta_discenter %>% 
  mutate(Classe = "Acervo Fisico BCE")

quadrodisgestaor14 <- quadrodisgestaor14%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`5Ótima`, na.rm = T)/length(`5Ótima`)),
             Boa = percent(sum(`5Boa`, na.rm = T)/length(`5Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`5Não utilizei o local/serviço este ano.`, na.rm = T)/length(`5Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`5Ruim`, na.rm = T)/length(`5Ruim`)),
             Péssima = percent(sum(`5Péssima`, na.rm = T)/length(`5Péssima`)))


# Acervo Digital BCE

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Acervo Virtual BCE`))

consulta_discenter$"6Ótima" <- NA
consulta_discenter$"6Ótima" <- ifelse(str_detect(consulta_discenter$`Acervo Virtual BCE`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"6Boa" <- NA
consulta_discenter$"6Boa" <- ifelse(str_detect(consulta_discenter$`Acervo Virtual BCE`, 
                                               "Boa"), 1, 0)

consulta_discenter$"6Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"6Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Acervo Virtual BCE`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"6Ruim" <- NA
consulta_discenter$"6Ruim" <- ifelse(str_detect(consulta_discenter$`Acervo Virtual BCE`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"6Péssima" <- NA
consulta_discenter$"6Péssima" <- ifelse(str_detect(consulta_discenter$`Acervo Virtual BCE`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor14 <- consulta_discenter %>% 
  mutate(Classe = "Acervo Virtual BCE")

quadrodisgestaor14 <- quadrodisgestaor14%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`6Ótima`, na.rm = T)/length(`6Ótima`)),
             Boa = percent(sum(`6Boa`, na.rm = T)/length(`6Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`6Não utilizei o local/serviço este ano.`, na.rm = T)/length(`6Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`6Ruim`, na.rm = T)/length(`6Ruim`)),
             Péssima = percent(sum(`6Péssima`, na.rm = T)/length(`6Péssima`)))

rm(list=ls())
consulta_aluno   <- read_excel("C:/Users/igorB/Desktop/ArquivosPesquisaUNB/Rpibic/AlunosConsulta.xlsx")