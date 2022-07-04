##################################################
###### Consulta à Comunidade Acadêmica 2021 ######
##################################################
rm(list=ls())
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

## Recomeçar a parte de tecnio doscente e dicente 

# Localização do arquivo xlsx para leitura dos dados

consulta_tecnico <- read_excel("C:/Users/igorB/Desktop/ArquivosPesquisaUNB/Rpibic/TecnicoConsulta.xlsx")
consulta_docente <- read_excel("C:/Users/igorB/Desktop/ArquivosPesquisaUNB/Rpibic/DocenteConsulta.xlsx")
consulta_aluno   <- read_excel("C:/Users/igorB/Desktop/ArquivosPesquisaUNB/Rpibic/AlunosConsulta.xlsx")




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
  rename(Tempo = `Há quanto tempo você trabalha na UnB?`) %>% 
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

consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`))

consulta_docenteg$"1" <- NA
consulta_docenteg$"1" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "1"), 1, 0)
consulta_docenteg$"2" <- NA
consulta_docenteg$"2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "2"), 1, 0)
consulta_docenteg$"3" <- NA
consulta_docenteg$"3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "3"), 1, 0)
consulta_docenteg$"4" <- NA
consulta_docenteg$"4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "4"), 1, 0)
consulta_docenteg$"5" <- NA
consulta_docenteg$"5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                           "5"), 1, 0)
consulta_docenteg$"Não sei responder" <- NA
consulta_docenteg$"Não sei responder" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]`, 
                                                           "Não sei responder"), 1, 0)

quadrodocgestao <- consulta_docenteg %>% 
  select("1":"Não sei responder") %>% 
  mutate(Classe = "Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [A comunidade acadêmica é incentivada a participar da definição das políticas de gestão]")


quadrodocgestao <- quadrodocgestao%>% 
  group_by(Classe) %>% 
  summarise("1" = percent(sum(`1`, na.rm = T)/length(`1`)),
            "2" = percent(sum(`2`, na.rm = T)/length(`2`)),
            "3" = percent(sum(`3`, na.rm = T)/length(`3`)),
            "4" = percent(sum(`4`, na.rm = T)/length(`4`)),
            "5" = percent(sum(`5`, na.rm = T)/length(`5`)),
            "Não sei responder" = percent(sum(`Não sei responder`, na.rm = T)/length(`Não sei responder`)))

# Pergunta 2

consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`))

consulta_docenteg$"1.2" <- NA
consulta_docenteg$"1.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.2" <- NA
consulta_docenteg$"2.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.2" <- NA
consulta_docenteg$"3.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.2" <- NA
consulta_docenteg$"4.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.2" <- NA
consulta_docenteg$"5.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder.2" <- NA
consulta_docenteg$"Não sei responder.2" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                                             "Não sei responder"), 1, 0)

quadrodocgestao2 <- consulta_docenteg %>% 
  select("1.2":"Não sei responder.2") %>% 
  mutate(Classe = "Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]")

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

consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`))

consulta_docenteg$"1.3" <- NA
consulta_docenteg$"1.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.3" <- NA
consulta_docenteg$"2.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.3" <- NA
consulta_docenteg$"3.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.3" <- NA
consulta_docenteg$"4.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.3" <- NA
consulta_docenteg$"5.3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder3" <- NA
consulta_docenteg$"Não sei responder3" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                                            "Não sei responder"), 1, 0)

quadrodocgestao3 <- consulta_docenteg %>% 
  select("1.3":"Não sei responder3") %>% 
  mutate(Classe = "Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]")

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


consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`))

consulta_docenteg$"1.4" <- NA
consulta_docenteg$"1.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.4" <- NA
consulta_docenteg$"2.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.4" <- NA
consulta_docenteg$"3.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.4" <- NA
consulta_docenteg$"4.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.4" <- NA
consulta_docenteg$"5.4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder4" <- NA
consulta_docenteg$"Não sei responder4" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                                            "Não sei responder"), 1, 0)

quadrodocgestao4 <- consulta_docenteg %>% 
  select("1.4":"Não sei responder4") %>% 
  mutate(Classe = "Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]")

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


consulta_docenteg <- consulta_docente %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`))

consulta_docenteg$"1.5" <- NA
consulta_docenteg$"1.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "1"), 1, 0)
consulta_docenteg$"2.5" <- NA
consulta_docenteg$"2.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "2"), 1, 0)
consulta_docenteg$"3.5" <- NA
consulta_docenteg$"3.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "3"), 1, 0)
consulta_docenteg$"4.5" <- NA
consulta_docenteg$"4.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "4"), 1, 0)
consulta_docenteg$"5.5" <- NA
consulta_docenteg$"5.5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "5"), 1, 0)
consulta_docenteg$"Não sei responder5" <- NA
consulta_docenteg$"Não sei responder5" <- ifelse(str_detect(consulta_docenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                                            "Não sei responder"), 1, 0)

quadrodocgestao5 <- consulta_docenteg %>% 
  select("1.5":"Não sei responder5") %>% 
  mutate(Classe = "Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]")

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

consulta_discenteg <- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`))

consulta_discenteg$"1.2" <- NA
consulta_discenteg$"1.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.2" <- NA
consulta_discenteg$"2.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.2" <- NA
consulta_discenteg$"3.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.2" <- NA
consulta_discenteg$"4.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.2" <- NA
consulta_discenteg$"5.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder.2" <- NA
consulta_discenteg$"Não sei responder.2" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
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

consulta_discenteg <- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`))

consulta_discenteg$"1.3" <- NA
consulta_discenteg$"1.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.3" <- NA
consulta_discenteg$"2.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.3" <- NA
consulta_discenteg$"3.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.3" <- NA
consulta_discenteg$"4.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.3" <- NA
consulta_discenteg$"5.3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder3" <- NA
consulta_discenteg$"Não sei responder3" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
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


consulta_discenteg <- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`))

consulta_discenteg$"1.4" <- NA
consulta_discenteg$"1.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.4" <- NA
consulta_discenteg$"2.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.4" <- NA
consulta_discenteg$"3.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.4" <- NA
consulta_discenteg$"4.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.4" <- NA
consulta_discenteg$"5.4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder4" <- NA
consulta_discenteg$"Não sei responder4" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
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


consulta_discenteg<- consulta_aluno %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`))

consulta_discenteg$"1.5" <- NA
consulta_discenteg$"1.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "1"), 1, 0)
consulta_discenteg$"2.5" <- NA
consulta_discenteg$"2.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "2"), 1, 0)
consulta_discenteg$"3.5" <- NA
consulta_discenteg$"3.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "3"), 1, 0)
consulta_discenteg$"4.5" <- NA
consulta_discenteg$"4.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "4"), 1, 0)
consulta_discenteg$"5.5" <- NA
consulta_discenteg$"5.5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                              "5"), 1, 0)
consulta_discenteg$"Não sei responder5" <- NA
consulta_discenteg$"Não sei responder5" <- ifelse(str_detect(consulta_discenteg$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
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

consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`))

consulta_tecnicog$"1.2" <- NA
consulta_tecnicog$"1.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.2" <- NA
consulta_tecnicog$"2.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.2" <- NA
consulta_tecnicog$"3.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.2" <- NA
consulta_tecnicog$"4.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.2" <- NA
consulta_tecnicog$"5.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder.2" <- NA
consulta_tecnicog$"Não sei responder.2" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [As ações institucionais estão de acordo com o Plano de Desenvolvimento Institucional (PDI) da UnB ]`, 
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

consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`))

consulta_tecnicog$"1.3" <- NA
consulta_tecnicog$"1.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.3" <- NA
consulta_tecnicog$"2.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.3" <- NA
consulta_tecnicog$"3.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.3" <- NA
consulta_tecnicog$"4.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.3" <- NA
consulta_tecnicog$"5.3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder3" <- NA
consulta_tecnicog$"Não sei responder3" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os meios de comunicação da Instituição são eficazes]`, 
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


consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`))

consulta_tecnicog$"1.4" <- NA
consulta_tecnicog$"1.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.4" <- NA
consulta_tecnicog$"2.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.4" <- NA
consulta_tecnicog$"3.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.4" <- NA
consulta_tecnicog$"4.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.4" <- NA
consulta_tecnicog$"5.4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder4" <- NA
consulta_tecnicog$"Não sei responder4" <- ifelse(str_detect(consulta_tecnicog$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os canais de comunicação oficiais da UnB são confiáveis.]`, 
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


consulta_tecnicog <- consulta_tecnico %>% filter(!is.na(`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`))

consulta_tecnicog$"1.5" <- NA
consulta_tecnicog$"1.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "1"), 1, 0)
consulta_tecnicog$"2.5" <- NA
consulta_tecnicog$"2.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "2"), 1, 0)
consulta_tecnicog$"3.5" <- NA
consulta_tecnicog$"3.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "3"), 1, 0)
consulta_tecnicog$"4.5" <- NA
consulta_tecnicog$"4.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "4"), 1, 0)
consulta_tecnicog$"5.5" <- NA
consulta_tecnicog$"5.5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB: Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
                                             "5"), 1, 0)
consulta_tecnicog$"Não sei responder5" <- NA
consulta_tecnicog$"Não sei responder5" <- ifelse(str_detect(consulta_tecnico$`Julgue as assertivas relacionadas à Gestão da UnB:  Considere a escala de 1 a 5, onde 1 = Discordo Totalmente e 5 = Concordo Totalmente [Os programas de apoio aos estudantes funcionam de modo satisfatório.]`, 
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

# Infraestrutura ---------- Alunos

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


quadrodisgestaor5 <- consulta_discenter %>% 
  mutate(Classe = "Estacionamento")

quadrodisgestaor5 <- quadrodisgestaor5%>%
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


quadrodisgestaor6 <- consulta_discenter %>% 
  mutate(Classe = "Iluminação pública")

quadrodisgestaor6 <- quadrodisgestaor6%>%
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


quadrodisgestaor7 <- consulta_discenter %>% 
  mutate(Classe = "Instalações Sanitarias")

quadrodisgestaor7 <- quadrodisgestaor7%>%
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


quadrodisgestaor8 <- consulta_discenter %>% 
  mutate(Classe = "Restaurante Universitario")

quadrodisgestaor8 <- quadrodisgestaor8%>%
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


quadrodisgestaor9 <- consulta_discenter %>% 
  mutate(Classe = "Salas de aula")

quadrodisgestaor9 <- quadrodisgestaor9%>%
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


quadrodisgestaor10 <- consulta_discenter %>% 
  mutate(Classe = "Sala dos Professores")

quadrodisgestaor10 <- quadrodisgestaor10%>%
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


quadrodisgestaor11 <- consulta_discenter %>% 
  mutate(Classe = "Segurança do Campus")

quadrodisgestaor11 <- quadrodisgestaor11%>%
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


quadrodisgestaor15 <- consulta_discenter %>% 
  mutate(Classe = "Acervo Virtual BCE")

quadrodisgestaor15 <- quadrodisgestaor15%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`6Ótima`, na.rm = T)/length(`6Ótima`)),
             Boa = percent(sum(`6Boa`, na.rm = T)/length(`6Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`6Não utilizei o local/serviço este ano.`, na.rm = T)/length(`6Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`6Ruim`, na.rm = T)/length(`6Ruim`)),
             Péssima = percent(sum(`6Péssima`, na.rm = T)/length(`6Péssima`)))


# Funcionamento Teams

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Funcionamento Teams`))

consulta_discenter$"7Ótima" <- NA
consulta_discenter$"7Ótima" <- ifelse(str_detect(consulta_discenter$`Funcionamento Teams`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"7Boa" <- NA
consulta_discenter$"7Boa" <- ifelse(str_detect(consulta_discenter$`Funcionamento Teams`, 
                                               "Boa"), 1, 0)

consulta_discenter$"7Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"7Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Funcionamento Teams`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"7Ruim" <- NA
consulta_discenter$"7Ruim" <- ifelse(str_detect(consulta_discenter$`Funcionamento Teams`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"7Péssima" <- NA
consulta_discenter$"7Péssima" <- ifelse(str_detect(consulta_discenter$`Funcionamento Teams`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor16 <- consulta_discenter %>% 
  mutate(Classe = "Funcionamento Teams")

quadrodisgestaor16 <- quadrodisgestaor16%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`7Ótima`, na.rm = T)/length(`7Ótima`)),
             Boa = percent(sum(`7Boa`, na.rm = T)/length(`7Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`7Não utilizei o local/serviço este ano.`, na.rm = T)/length(`7Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`7Ruim`, na.rm = T)/length(`7Ruim`)),
             Péssima = percent(sum(`7Péssima`, na.rm = T)/length(`7Péssima`)))


# Funcionamento Aprender

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Funcionamento da Plataforma Institucional Aprender`))

consulta_discenter$"8Ótima" <- NA
consulta_discenter$"8Ótima" <- ifelse(str_detect(consulta_discenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"8Boa" <- NA
consulta_discenter$"8Boa" <- ifelse(str_detect(consulta_discenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                               "Boa"), 1, 0)

consulta_discenter$"8Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"8Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"8Ruim" <- NA
consulta_discenter$"8Ruim" <- ifelse(str_detect(consulta_discenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"8Péssima" <- NA
consulta_discenter$"8Péssima" <- ifelse(str_detect(consulta_discenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor17 <- consulta_discenter %>% 
  mutate(Classe = "Funcionamento da Plataforma Institucional Aprender")

quadrodisgestaor17 <- quadrodisgestaor17%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`8Ótima`, na.rm = T)/length(`8Ótima`)),
             Boa = percent(sum(`8Boa`, na.rm = T)/length(`8Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`8Não utilizei o local/serviço este ano.`, na.rm = T)/length(`8Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`8Ruim`, na.rm = T)/length(`8Ruim`)),
             Péssima = percent(sum(`8Péssima`, na.rm = T)/length(`8Péssima`)))


# Funcionamento SIGAA

consulta_discenter <- consulta_aluno %>% filter(!is.na(`Funcionamento SIGAA`))

consulta_discenter$"9Ótima" <- NA
consulta_discenter$"9Ótima" <- ifelse(str_detect(consulta_discenter$`Funcionamento SIGAA`, 
                                                 "Ótima"), 1, 0)
consulta_discenter$"9Boa" <- NA
consulta_discenter$"9Boa" <- ifelse(str_detect(consulta_discenter$`Funcionamento SIGAA`, 
                                               "Boa"), 1, 0)

consulta_discenter$"9Não utilizei o local/serviço este ano." <- NA
consulta_discenter$"9Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_discenter$`Funcionamento SIGAA`, 
                                                                                  "Não utilizei o local/serviço este ano."), 1, 0)

consulta_discenter$"9Ruim" <- NA
consulta_discenter$"9Ruim" <- ifelse(str_detect(consulta_discenter$`Funcionamento SIGAA`, 
                                                "Ruim"), 1, 0)

consulta_discenter$"9Péssima" <- NA
consulta_discenter$"9Péssima" <- ifelse(str_detect(consulta_discenter$`Funcionamento SIGAA`, 
                                                   "Péssima"), 1, 0)


quadrodisgestaor18 <- consulta_discenter %>% 
  mutate(Classe = "Funcionamento SIGAA")

quadrodisgestaor18 <- quadrodisgestaor18%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`9Ótima`, na.rm = T)/length(`9Ótima`)),
             Boa = percent(sum(`9Boa`, na.rm = T)/length(`9Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`9Não utilizei o local/serviço este ano.`, na.rm = T)/length(`9Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`9Ruim`, na.rm = T)/length(`9Ruim`)),
             Péssima = percent(sum(`9Péssima`, na.rm = T)/length(`9Péssima`)))

quadrodisgestaortotal <- rbind(quadrodisgestaor, quadrodisgestaor1)
quadrodisgestaortotal1 <- rbind(quadrodisgestaortotal, quadrodisgestaor2)
quadrodisgestaortotal2 <- rbind(quadrodisgestaortotal1, quadrodisgestaor3)
quadrodisgestaortotal3 <- rbind(quadrodisgestaortotal2, quadrodisgestaor4)
quadrodisgestaortotal4 <- rbind(quadrodisgestaortotal3, quadrodisgestaor5)
quadrodisgestaortotal5 <- rbind(quadrodisgestaortotal4, quadrodisgestaor6)
quadrodisgestaortotal6 <- rbind(quadrodisgestaortotal5, quadrodisgestaor7)
quadrodisgestaortotal7 <- rbind(quadrodisgestaortotal6, quadrodisgestaor8)
quadrodisgestaortotal8 <- rbind(quadrodisgestaortotal7, quadrodisgestaor9)
quadrodisgestaortotal9 <- rbind(quadrodisgestaortotal8, quadrodisgestaor10)
quadrodisgestaortotal10 <- rbind(quadrodisgestaortotal9, quadrodisgestaor11)
quadrodisgestaortotal11 <- rbind(quadrodisgestaortotal10, quadrodisgestaor12)
quadrodisgestaortotal12 <- rbind(quadrodisgestaortotal11, quadrodisgestaor13)
quadrodisgestaortotal13 <- rbind(quadrodisgestaortotal12, quadrodisgestaor14)
quadrodisgestaortotal14 <- rbind(quadrodisgestaortotal13, quadrodisgestaor15)
quadrodisgestaortotal15 <- rbind(quadrodisgestaortotal14, quadrodisgestaor16)
quadrodisgestaortotal16 <- rbind(quadrodisgestaortotal15, quadrodisgestaor17)

quadrodisgestaortotal <- rbind(quadrodisgestaor, quadrodisgestaor16)



# Infraestrutura ---------- Tecmocp

# tecnico  Rede UnB Wireless



consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Rede UnB Wireless`))

consulta_tecnicor$"Ótima" <- NA
consulta_tecnicor$"Ótima" <- ifelse(str_detect(consulta_tecnicor$`Rede UnB Wireless`, 
                                               "Ótima"), 1, 0)
consulta_tecnicor$"Boa" <- NA
consulta_tecnicor$"Boa" <- ifelse(str_detect(consulta_tecnicor$`Rede UnB Wireless`, 
                                             "Boa"), 1, 0)

consulta_tecnicor$"Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Rede UnB Wireless`, 
                                                                                "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"Ruim" <- NA
consulta_tecnicor$"Ruim" <- ifelse(str_detect(consulta_tecnicor$`Rede UnB Wireless`, 
                                              "Ruim"), 1, 0)

consulta_tecnicor$"Péssima" <- NA
consulta_tecnicor$"Péssima" <- ifelse(str_detect(consulta_tecnicor$`Rede UnB Wireless`, 
                                                 "Péssima"), 1, 0)


quadrotecgestaor <- consulta_tecnicor %>% 
  mutate(Classe = "Rede UnB Wireless")

quadrotecgestaor <- quadrotecgestaor%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`Ótima`, na.rm = T)/length(`Ótima`)),
             Boa = percent(sum(`Boa`, na.rm = T)/length(`Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`Não utilizei o local/serviço este ano.`, na.rm = T)/length(`Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`Ruim`, na.rm = T)/length(`Ruim`)),
             Péssima = percent(sum(`Péssima`, na.rm = T)/length(`Péssima`)))

# Tecnico  Acessibilidade fisica

consulta_tecnicor<- consulta_tecnico %>% filter(!is.na(`Condições de acessibilidade`))

consulta_tecnicor$"AÓtima" <- NA
consulta_tecnicor$"AÓtima" <- ifelse(str_detect(consulta_tecnicor$`Condições de acessibilidade`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"ABoa" <- NA
consulta_tecnicor$"ABoa" <- ifelse(str_detect(consulta_tecnicor$`Condições de acessibilidade`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"ANão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"ANão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Condições de acessibilidade`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"ARuim" <- NA
consulta_tecnicor$"ARuim" <- ifelse(str_detect(consulta_tecnicor$`Condições de acessibilidade`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"APéssima" <- NA
consulta_tecnicor$"APéssima" <- ifelse(str_detect(consulta_tecnicor$`Condições de acessibilidade`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor1 <- consulta_tecnicor %>% 
  mutate(Classe = "Condições de acessibilidade")

quadrotecgestaor1 <- quadrotecgestaor1%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`AÓtima`, na.rm = T)/length(`AÓtima`)),
             Boa = percent(sum(`ABoa`, na.rm = T)/length(`ABoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`ANão utilizei o local/serviço este ano.`, na.rm = T)/length(`ANão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`ARuim`, na.rm = T)/length(`ARuim`)),
             Péssima = percent(sum(`APéssima`, na.rm = T)/length(`APéssima`)))



# tecnico Auditorio

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Condições de gerais de auditorio`))

consulta_tecnicor$"BÓtima" <- NA
consulta_tecnicor$"BÓtima" <- ifelse(str_detect(consulta_tecnicor$`Condições de gerais de auditorio`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"BBoa" <- NA
consulta_tecnicor$"BBoa" <- ifelse(str_detect(consulta_tecnicor$`Condições de acessibilidade`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"BNão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"BNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Condições de gerais de auditorio`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"BRuim" <- NA
consulta_tecnicor$"BRuim" <- ifelse(str_detect(consulta_tecnicor$`Condições de gerais de auditorio`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"BPéssima" <- NA
consulta_tecnicor$"BPéssima" <- ifelse(str_detect(consulta_tecnicor$`Condições de gerais de auditorio`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor2 <- consulta_tecnicor %>% 
  mutate(Classe = "Condições de gerais de auditorio")

quadrotecgestaor2 <- quadrotecgestaor2%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`BÓtima`, na.rm = T)/length(`BÓtima`)),
             Boa = percent(sum(`BBoa`, na.rm = T)/length(`BBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`BNão utilizei o local/serviço este ano.`, na.rm = T)/length(`BNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`BRuim`, na.rm = T)/length(`BRuim`)),
             Péssima = percent(sum(`BPéssima`, na.rm = T)/length(`BPéssima`)))

# Tecnico  Espaço de convivencia

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Espaço de Convivencia e alimentação (exceto RU)`))

consulta_tecnicor$"CÓtima" <- NA
consulta_tecnicor$"CÓtima" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"CBoa" <- NA
consulta_tecnicor$"CBoa" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"CNão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"CNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"CRuim" <- NA
consulta_tecnicor$"CRuim" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"CPéssima" <- NA
consulta_tecnicor$"CPéssima" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor3 <- consulta_tecnicor %>% 
  mutate(Classe = "Espaço de Convivencia e alimentação (exceto RU)")

quadrotecgestaor3 <- quadrotecgestaor3%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`CÓtima`, na.rm = T)/length(`CÓtima`)),
             Boa = percent(sum(`CBoa`, na.rm = T)/length(`CBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`CNão utilizei o local/serviço este ano.`, na.rm = T)/length(`CNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`CRuim`, na.rm = T)/length(`CRuim`)),
             Péssima = percent(sum(`CPéssima`, na.rm = T)/length(`CPéssima`)))


# BCE Estudo

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Espaço de Estudo da BCE`))

consulta_tecnicor$"DÓtima" <- NA
consulta_tecnicor$"DÓtima" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Estudo da BCE`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"DBoa" <- NA
consulta_tecnicor$"DBoa" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Estudo da BCE`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"DNão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"DNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Espaço de Estudo da BCE`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"DRuim" <- NA
consulta_tecnicor$"DRuim" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Estudo da BCE`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"DPéssima" <- NA
consulta_tecnicor$"DPéssima" <- ifelse(str_detect(consulta_tecnicor$`Espaço de Estudo da BCE`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor4 <- consulta_tecnicor %>% 
  mutate(Classe = "Espaço de Estudo da BCE")

quadrotecgestaor4 <- quadrotecgestaor4%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`DÓtima`, na.rm = T)/length(`DÓtima`)),
             Boa = percent(sum(`DBoa`, na.rm = T)/length(`DBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`DNão utilizei o local/serviço este ano.`, na.rm = T)/length(`DNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`DRuim`, na.rm = T)/length(`DRuim`)),
             Péssima = percent(sum(`DPéssima`, na.rm = T)/length(`DPéssima`)))



# Estacionamento

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Estacionamento`))

consulta_tecnicor$"FÓtima" <- NA
consulta_tecnicor$"FÓtima" <- ifelse(str_detect(consulta_tecnicor$`Estacionamento`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"FDBoa" <- NA
consulta_tecnicor$"FBoa" <- ifelse(str_detect(consulta_tecnicor$`Estacionamento`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"FNão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"FNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Estacionamento`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"FRuim" <- NA
consulta_tecnicor$"FRuim" <- ifelse(str_detect(consulta_tecnicor$`Estacionamento`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"FPéssima" <- NA
consulta_tecnicor$"FPéssima" <- ifelse(str_detect(consulta_tecnicor$`Estacionamento`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor5 <- consulta_tecnicor %>% 
  mutate(Classe = "Estacionamento")

quadrotecgestaor5 <- quadrotecgestaor5%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`FÓtima`, na.rm = T)/length(`FÓtima`)),
             Boa = percent(sum(`FBoa`, na.rm = T)/length(`FBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`FNão utilizei o local/serviço este ano.`, na.rm = T)/length(`FNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`FRuim`, na.rm = T)/length(`FRuim`)),
             Péssima = percent(sum(`FPéssima`, na.rm = T)/length(`FPéssima`)))


# Iluminacao Publica

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Iluminação pública`))

consulta_tecnicor$"GÓtima" <- NA
consulta_tecnicor$"GÓtima" <- ifelse(str_detect(consulta_tecnicor$`Iluminação pública`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"GDBoa" <- NA
consulta_tecnicor$"GBoa" <- ifelse(str_detect(consulta_tecnicor$`Iluminação pública`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"GNão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"GNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Iluminação pública`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"GRuim" <- NA
consulta_tecnicor$"GRuim" <- ifelse(str_detect(consulta_tecnicor$`Iluminação pública`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"GPéssima" <- NA
consulta_tecnicor$"GPéssima" <- ifelse(str_detect(consulta_tecnicor$`Iluminação pública`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor6 <- consulta_tecnicor %>% 
  mutate(Classe = "Iluminação pública")

quadrotecgestaor6 <- quadrotecgestaor6%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`GÓtima`, na.rm = T)/length(`GÓtima`)),
             Boa = percent(sum(`GBoa`, na.rm = T)/length(`GBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`GNão utilizei o local/serviço este ano.`, na.rm = T)/length(`GNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`GRuim`, na.rm = T)/length(`GRuim`)),
             Péssima = percent(sum(`GPéssima`, na.rm = T)/length(`GPéssima`)))

# Instalações sanitarias

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Instalações Sanitarias`))

consulta_tecnicor$"HÓtima" <- NA
consulta_tecnicor$"HÓtima" <- ifelse(str_detect(consulta_tecnicor$`Instalações Sanitarias`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"HDBoa" <- NA
consulta_tecnicor$"HBoa" <- ifelse(str_detect(consulta_tecnicor$`Instalações Sanitarias`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"HNão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"HNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Instalações Sanitarias`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"HRuim" <- NA
consulta_tecnicor$"HRuim" <- ifelse(str_detect(consulta_tecnicor$`Instalações Sanitarias`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"HPéssima" <- NA
consulta_tecnicor$"HPéssima" <- ifelse(str_detect(consulta_tecnicor$`Instalações Sanitarias`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor7 <- consulta_tecnicor %>% 
  mutate(Classe = "Instalações Sanitarias")

quadrotecgestaor7 <- quadrotecgestaor7%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`HÓtima`, na.rm = T)/length(`HÓtima`)),
             Boa = percent(sum(`HBoa`, na.rm = T)/length(`HBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`HNão utilizei o local/serviço este ano.`, na.rm = T)/length(`HNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`HRuim`, na.rm = T)/length(`HRuim`)),
             Péssima = percent(sum(`HPéssima`, na.rm = T)/length(`HPéssima`)))

# Ru

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Restaurante Universitario`))

consulta_tecnicor$"IÓtima" <- NA
consulta_tecnicor$"IÓtima" <- ifelse(str_detect(consulta_tecnicor$`Restaurante Universitario`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"IBoa" <- NA
consulta_tecnicor$"IBoa" <- ifelse(str_detect(consulta_tecnicor$`Restaurante Universitario`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"INão utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"INão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Restaurante Universitario`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"IRuim" <- NA
consulta_tecnicor$"IRuim" <- ifelse(str_detect(consulta_tecnicor$`Restaurante Universitario`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"IPéssima" <- NA
consulta_tecnicor$"IPéssima" <- ifelse(str_detect(consulta_tecnicor$`Restaurante Universitario`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor8 <- consulta_tecnicor %>% 
  mutate(Classe = "Restaurante Universitario")

quadrotecgestaor8 <- quadrotecgestaor8%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`IÓtima`, na.rm = T)/length(`IÓtima`)),
             Boa = percent(sum(`IBoa`, na.rm = T)/length(`IBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`INão utilizei o local/serviço este ano.`, na.rm = T)/length(`INão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`IRuim`, na.rm = T)/length(`IRuim`)),
             Péssima = percent(sum(`IPéssima`, na.rm = T)/length(`IPéssima`)))

# Salas de Aula

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Salas de aula`))

consulta_tecnicor$"1Ótima" <- NA
consulta_tecnicor$"1Ótima" <- ifelse(str_detect(consulta_tecnicor$`Salas de aula`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"1Boa" <- NA
consulta_tecnicor$"1Boa" <- ifelse(str_detect(consulta_tecnicor$`Salas de aula`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"1Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"1Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Salas de aula`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"1Ruim" <- NA
consulta_tecnicor$"1Ruim" <- ifelse(str_detect(consulta_tecnicor$`Salas de aula`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"1Péssima" <- NA
consulta_tecnicor$"1Péssima" <- ifelse(str_detect(consulta_tecnicor$`Salas de aula`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor9 <- consulta_tecnicor %>% 
  mutate(Classe = "Salas de aula")

quadrotecgestaor9 <- quadrotecgestaor9%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`1Ótima`, na.rm = T)/length(`1Ótima`)),
             Boa = percent(sum(`1Boa`, na.rm = T)/length(`1Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`1Não utilizei o local/serviço este ano.`, na.rm = T)/length(`1Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`1Ruim`, na.rm = T)/length(`1Ruim`)),
             Péssima = percent(sum(`1Péssima`, na.rm = T)/length(`1Péssima`)))

# Sala dos professores

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Sala dos Professores`))

consulta_tecnicor$"2Ótima" <- NA
consulta_tecnicor$"2Ótima" <- ifelse(str_detect(consulta_tecnicor$`Sala dos Professores`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"2Boa" <- NA
consulta_tecnicor$"2Boa" <- ifelse(str_detect(consulta_tecnicor$`Sala dos Professores`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"2Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"2Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Sala dos Professores`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"2Ruim" <- NA
consulta_tecnicor$"2Ruim" <- ifelse(str_detect(consulta_tecnicor$`Sala dos Professores`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"2Péssima" <- NA
consulta_tecnicor$"2Péssima" <- ifelse(str_detect(consulta_tecnicor$`Sala dos Professores`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor10 <- consulta_tecnicor %>% 
  mutate(Classe = "Sala dos Professores")

quadrotecgestaor10 <- quadrotecgestaor10%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`2Ótima`, na.rm = T)/length(`2Ótima`)),
             Boa = percent(sum(`2Boa`, na.rm = T)/length(`2Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`2Não utilizei o local/serviço este ano.`, na.rm = T)/length(`2Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`2Ruim`, na.rm = T)/length(`2Ruim`)),
             Péssima = percent(sum(`2Péssima`, na.rm = T)/length(`2Péssima`)))

# Segurança Campus

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Segurança do Campus`))

consulta_tecnicor$"3Ótima" <- NA
consulta_tecnicor$"3Ótima" <- ifelse(str_detect(consulta_tecnicor$`Segurança do Campus`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"3Boa" <- NA
consulta_tecnicor$"3Boa" <- ifelse(str_detect(consulta_tecnicor$`Segurança do Campus`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"3Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"3Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Segurança do Campus`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"3Ruim" <- NA
consulta_tecnicor$"3Ruim" <- ifelse(str_detect(consulta_tecnicor$`Segurança do Campus`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"3Péssima" <- NA
consulta_tecnicor$"3Péssima" <- ifelse(str_detect(consulta_tecnicor$`Segurança do Campus`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor11 <- consulta_tecnicor %>% 
  mutate(Classe = "Segurança do Campus")

quadrotecgestaor11 <- quadrotecgestaor11%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`3Ótima`, na.rm = T)/length(`3Ótima`)),
             Boa = percent(sum(`3Boa`, na.rm = T)/length(`3Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`3Não utilizei o local/serviço este ano.`, na.rm = T)/length(`3Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`3Ruim`, na.rm = T)/length(`3Ruim`)),
             Péssima = percent(sum(`3Péssima`, na.rm = T)/length(`3Péssima`)))



# Suporte tecnologico

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Suporte tecnologico para administração`))

consulta_tecnicor$"4Ótima" <- NA
consulta_tecnicor$"4Ótima" <- ifelse(str_detect(consulta_tecnicor$`Suporte tecnologico para administração`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"4Boa" <- NA
consulta_tecnicor$"4Boa" <- ifelse(str_detect(consulta_tecnicor$`Suporte tecnologico para administração`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"4Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"4Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Suporte tecnologico para administração`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"4Ruim" <- NA
consulta_tecnicor$"4Ruim" <- ifelse(str_detect(consulta_tecnicor$`Suporte tecnologico para administração`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"4Péssima" <- NA
consulta_tecnicor$"4Péssima" <- ifelse(str_detect(consulta_tecnicor$`Suporte tecnologico para administração`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor12 <- consulta_tecnicor %>% 
  mutate(Classe = "Suporte tecnologico para administração")

quadrotecgestaor12 <- quadrotecgestaor12%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`4Ótima`, na.rm = T)/length(`4Ótima`)),
             Boa = percent(sum(`4Boa`, na.rm = T)/length(`4Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`4Não utilizei o local/serviço este ano.`, na.rm = T)/length(`4Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`4Ruim`, na.rm = T)/length(`4Ruim`)),
             Péssima = percent(sum(`4Péssima`, na.rm = T)/length(`4Péssima`)))


# Acervo fisico BCE

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Acervo Fisico BCE`))

consulta_tecnicor$"5Ótima" <- NA
consulta_tecnicor$"5Ótima" <- ifelse(str_detect(consulta_tecnicor$`Acervo Fisico BCE`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"5Boa" <- NA
consulta_tecnicor$"5Boa" <- ifelse(str_detect(consulta_tecnicor$`Acervo Fisico BCE`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"5Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"5Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Acervo Fisico BCE`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"5Ruim" <- NA
consulta_tecnicor$"5Ruim" <- ifelse(str_detect(consulta_tecnicor$`Acervo Fisico BCE`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"5Péssima" <- NA
consulta_tecnicor$"5Péssima" <- ifelse(str_detect(consulta_tecnicor$`Acervo Fisico BCE`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor13 <- consulta_tecnicor %>% 
  mutate(Classe = "Acervo Fisico BCE")

quadrotecgestaor13 <- quadrotecgestaor13%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`5Ótima`, na.rm = T)/length(`5Ótima`)),
             Boa = percent(sum(`5Boa`, na.rm = T)/length(`5Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`5Não utilizei o local/serviço este ano.`, na.rm = T)/length(`5Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`5Ruim`, na.rm = T)/length(`5Ruim`)),
             Péssima = percent(sum(`5Péssima`, na.rm = T)/length(`5Péssima`)))


# Acervo Digital BCE

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Acervo Virtual BCE`))

consulta_tecnicor$"6Ótima" <- NA
consulta_tecnicor$"6Ótima" <- ifelse(str_detect(consulta_tecnicor$`Acervo Virtual BCE`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"6Boa" <- NA
consulta_tecnicor$"6Boa" <- ifelse(str_detect(consulta_tecnicor$`Acervo Virtual BCE`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"6Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"6Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Acervo Virtual BCE`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"6Ruim" <- NA
consulta_tecnicor$"6Ruim" <- ifelse(str_detect(consulta_tecnicor$`Acervo Virtual BCE`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"6Péssima" <- NA
consulta_tecnicor$"6Péssima" <- ifelse(str_detect(consulta_tecnicor$`Acervo Virtual BCE`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor14 <- consulta_tecnicor %>% 
  mutate(Classe = "Acervo Virtual BCE")

quadrotecgestaor14 <- quadrotecgestaor14%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`6Ótima`, na.rm = T)/length(`6Ótima`)),
             Boa = percent(sum(`6Boa`, na.rm = T)/length(`6Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`6Não utilizei o local/serviço este ano.`, na.rm = T)/length(`6Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`6Ruim`, na.rm = T)/length(`6Ruim`)),
             Péssima = percent(sum(`6Péssima`, na.rm = T)/length(`6Péssima`)))


# Funcionamento Teams

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Funcionamento Teams`))

consulta_tecnicor$"7Ótima" <- NA
consulta_tecnicor$"7Ótima" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento Teams`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"7Boa" <- NA
consulta_tecnicor$"7Boa" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento Teams`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"7Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"7Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Funcionamento Teams`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"7Ruim" <- NA
consulta_tecnicor$"7Ruim" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento Teams`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"7Péssima" <- NA
consulta_tecnicor$"7Péssima" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento Teams`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor15 <- consulta_tecnicor %>% 
  mutate(Classe = "Funcionamento Teams")

quadrotecgestaor15 <- quadrotecgestaor15%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`7Ótima`, na.rm = T)/length(`7Ótima`)),
             Boa = percent(sum(`7Boa`, na.rm = T)/length(`7Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`7Não utilizei o local/serviço este ano.`, na.rm = T)/length(`7Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`7Ruim`, na.rm = T)/length(`7Ruim`)),
             Péssima = percent(sum(`7Péssima`, na.rm = T)/length(`7Péssima`)))


# Funcionamento Aprender

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Funcionamento da Plataforma Institucional Aprender`))

consulta_tecnicor$"8Ótima" <- NA
consulta_tecnicor$"8Ótima" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento da Plataforma Institucional Aprender`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"8Boa" <- NA
consulta_tecnicor$"8Boa" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento da Plataforma Institucional Aprender`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"8Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"8Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Funcionamento da Plataforma Institucional Aprender`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"8Ruim" <- NA
consulta_tecnicor$"8Ruim" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento da Plataforma Institucional Aprender`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"8Péssima" <- NA
consulta_tecnicor$"8Péssima" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento da Plataforma Institucional Aprender`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor16 <- consulta_tecnicor %>% 
  mutate(Classe = "Funcionamento da Plataforma Institucional Aprender")

quadrotecgestaor16 <- quadrotecgestaor16%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`8Ótima`, na.rm = T)/length(`8Ótima`)),
             Boa = percent(sum(`8Boa`, na.rm = T)/length(`8Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`8Não utilizei o local/serviço este ano.`, na.rm = T)/length(`8Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`8Ruim`, na.rm = T)/length(`8Ruim`)),
             Péssima = percent(sum(`8Péssima`, na.rm = T)/length(`8Péssima`)))


# Funcionamento SIGAA - Estamos aqui

consulta_tecnicor <- consulta_tecnico %>% filter(!is.na(`Funcionamento SIGAA`))

consulta_tecnicor$"9Ótima" <- NA
consulta_tecnicor$"9Ótima" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento SIGAA`, 
                                                "Ótima"), 1, 0)
consulta_tecnicor$"9Boa" <- NA
consulta_tecnicor$"9Boa" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento SIGAA`, 
                                              "Boa"), 1, 0)

consulta_tecnicor$"9Não utilizei o local/serviço este ano." <- NA
consulta_tecnicor$"9Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_tecnicor$`Funcionamento SIGAA`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_tecnicor$"9Ruim" <- NA
consulta_tecnicor$"9Ruim" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento SIGAA`, 
                                               "Ruim"), 1, 0)

consulta_tecnicor$"9Péssima" <- NA
consulta_tecnicor$"9Péssima" <- ifelse(str_detect(consulta_tecnicor$`Funcionamento SIGAA`, 
                                                  "Péssima"), 1, 0)


quadrotecgestaor17 <- consulta_tecnicor %>% 
  mutate(Classe = "Funcionamento SIGAA")

quadrotecgestaor17 <- quadrotecgestaor17%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`9Ótima`, na.rm = T)/length(`9Ótima`)),
             Boa = percent(sum(`9Boa`, na.rm = T)/length(`9Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`9Não utilizei o local/serviço este ano.`, na.rm = T)/length(`9Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`9Ruim`, na.rm = T)/length(`9Ruim`)),
             Péssima = percent(sum(`9Péssima`, na.rm = T)/length(`9Péssima`)))

quadrotecgestaortotal <- rbind(quadrotecgestaor, quadrotecgestaor1)
quadrotecgestaortotal1 <- rbind(quadrotecgestaortotal, quadrotecgestaor2)
quadrotecgestaortotal2 <- rbind(quadrotecgestaortotal1, quadrotecgestaor3)
quadrotecgestaortotal3 <- rbind(quadrotecgestaortotal2, quadrotecgestaor4)
quadrotecgestaortotal4 <- rbind(quadrotecgestaortotal3, quadrotecgestaor5)
quadrotecgestaortotal5 <- rbind(quadrotecgestaortotal4, quadrotecgestaor6)
quadrotecgestaortotal6 <- rbind(quadrotecgestaortotal5, quadrotecgestaor7)
quadrotecgestaortotal7 <- rbind(quadrotecgestaortotal6, quadrotecgestaor8)
quadrotecgestaortotal8 <- rbind(quadrotecgestaortotal7, quadrotecgestaor9)
quadrotecgestaortotal9 <- rbind(quadrotecgestaortotal8, quadrotecgestaor10)
quadrotecgestaortotal10 <- rbind(quadrotecgestaortotal9, quadrotecgestaor11)
quadrotecgestaortotal11 <- rbind(quadrotecgestaortotal10, quadrotecgestaor12)
quadrotecgestaortotal12 <- rbind(quadrotecgestaortotal11, quadrotecgestaor13)
quadrotecgestaortotal13 <- rbind(quadrotecgestaortotal12, quadrotecgestaor14)
quadrotecgestaortotal14 <- rbind(quadrotecgestaortotal13, quadrotecgestaor15)
quadrotecgestaortotal15 <- rbind(quadrotecgestaortotal14, quadrotecgestaor16)
quadrotecgestaortotal16 <- rbind(quadrotecgestaortotal15, quadrotecgestaor17)

quadrotecgestaortotal <- rbind(quadrotecgestaor, quadrotecgestaor1)




# Infraestrutura ---------- Docenter

# docente  Rede UnB Wireless



consulta_docenter <- consulta_docente %>% filter(!is.na(`Rede UnB Wireless`))

consulta_docenter$"Ótima" <- NA
consulta_docenter$"Ótima" <- ifelse(str_detect(consulta_docenter$`Rede UnB Wireless`, 
                                               "Ótima"), 1, 0)
consulta_docenter$"Boa" <- NA
consulta_docenter$"Boa" <- ifelse(str_detect(consulta_docenter$`Rede UnB Wireless`, 
                                             "Boa"), 1, 0)

consulta_docenter$"Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Rede UnB Wireless`, 
                                                                                "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"Ruim" <- NA
consulta_docenter$"Ruim" <- ifelse(str_detect(consulta_docenter$`Rede UnB Wireless`, 
                                              "Ruim"), 1, 0)

consulta_docenter$"Péssima" <- NA
consulta_docenter$"Péssima" <- ifelse(str_detect(consulta_docenter$`Rede UnB Wireless`, 
                                                 "Péssima"), 1, 0)


quadrodocgestaor <- consulta_docenter %>% 
  mutate(Classe = "Rede UnB Wireless")

quadrodocgestaor <- quadrodocgestaor%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`Ótima`, na.rm = T)/length(`Ótima`)),
             Boa = percent(sum(`Boa`, na.rm = T)/length(`Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`Não utilizei o local/serviço este ano.`, na.rm = T)/length(`Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`Ruim`, na.rm = T)/length(`Ruim`)),
             Péssima = percent(sum(`Péssima`, na.rm = T)/length(`Péssima`)))

# docente  Acessibilidade fisica

consulta_docenter<- consulta_docente %>% filter(!is.na(`Condições de acessibilidade`))

consulta_docenter$"AÓtima" <- NA
consulta_docenter$"AÓtima" <- ifelse(str_detect(consulta_docenter$`Condições de acessibilidade`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"ABoa" <- NA
consulta_docenter$"ABoa" <- ifelse(str_detect(consulta_docenter$`Condições de acessibilidade`, 
                                              "Boa"), 1, 0)

consulta_docenter$"ANão utilizei o local/serviço este ano." <- NA
consulta_docenter$"ANão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Condições de acessibilidade`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"ARuim" <- NA
consulta_docenter$"ARuim" <- ifelse(str_detect(consulta_docenter$`Condições de acessibilidade`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"APéssima" <- NA
consulta_docenter$"APéssima" <- ifelse(str_detect(consulta_docenter$`Condições de acessibilidade`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor1 <- consulta_docenter %>% 
  mutate(Classe = "Condições de acessibilidade")

quadrodocgestaor1 <- quadrodocgestaor1%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`AÓtima`, na.rm = T)/length(`AÓtima`)),
             Boa = percent(sum(`ABoa`, na.rm = T)/length(`ABoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`ANão utilizei o local/serviço este ano.`, na.rm = T)/length(`ANão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`ARuim`, na.rm = T)/length(`ARuim`)),
             Péssima = percent(sum(`APéssima`, na.rm = T)/length(`APéssima`)))



# docente Auditorio

consulta_docenter <- consulta_docente %>% filter(!is.na(`Condições de gerais de auditorio`))

consulta_docenter$"BÓtima" <- NA
consulta_docenter$"BÓtima" <- ifelse(str_detect(consulta_docenter$`Condições de gerais de auditorio`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"BBoa" <- NA
consulta_docenter$"BBoa" <- ifelse(str_detect(consulta_docenter$`Condições de gerais de auditorio`, 
                                              "Boa"), 1, 0)

consulta_docenter$"BNão utilizei o local/serviço este ano." <- NA
consulta_docenter$"BNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Condições de gerais de auditorio`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"BRuim" <- NA
consulta_docenter$"BRuim" <- ifelse(str_detect(consulta_docenter$`Condições de gerais de auditorio`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"BPéssima" <- NA
consulta_docenter$"BPéssima" <- ifelse(str_detect(consulta_docenter$`Condições de gerais de auditorio`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor2 <- consulta_docenter %>% 
  mutate(Classe = "Condições de gerais de auditorio")

quadrodocgestaor2 <- quadrodocgestaor2%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`BÓtima`, na.rm = T)/length(`BÓtima`)),
             Boa = percent(sum(`BBoa`, na.rm = T)/length(`BBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`BNão utilizei o local/serviço este ano.`, na.rm = T)/length(`BNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`BRuim`, na.rm = T)/length(`BRuim`)),
             Péssima = percent(sum(`BPéssima`, na.rm = T)/length(`BPéssima`)))

# Docente  Espaço de convivencia

consulta_docenter <- consulta_docente %>% filter(!is.na(`Espaço de Convivencia e alimentação (exceto RU)`))

consulta_docenter$"CÓtima" <- NA
consulta_docenter$"CÓtima" <- ifelse(str_detect(consulta_docenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"CBoa" <- NA
consulta_docenter$"CBoa" <- ifelse(str_detect(consulta_docenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                              "Boa"), 1, 0)

consulta_docenter$"CNão utilizei o local/serviço este ano." <- NA
consulta_docenter$"CNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"CRuim" <- NA
consulta_docenter$"CRuim" <- ifelse(str_detect(consulta_docenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"CPéssima" <- NA
consulta_docenter$"CPéssima" <- ifelse(str_detect(consulta_docenter$`Espaço de Convivencia e alimentação (exceto RU)`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor3 <- consulta_docenter %>% 
  mutate(Classe = "Espaço de Convivencia e alimentação (exceto RU)")

quadrodocgestaor3 <- quadrodocgestaor3%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`CÓtima`, na.rm = T)/length(`CÓtima`)),
             Boa = percent(sum(`CBoa`, na.rm = T)/length(`CBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`CNão utilizei o local/serviço este ano.`, na.rm = T)/length(`CNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`CRuim`, na.rm = T)/length(`CRuim`)),
             Péssima = percent(sum(`CPéssima`, na.rm = T)/length(`CPéssima`)))


# BCE Estudo

consulta_docenter <- consulta_docente %>% filter(!is.na(`Espaço de Estudo da BCE`))

consulta_docenter$"1DÓtima" <- NA
consulta_docenter$"1DÓtima" <- ifelse(str_detect(consulta_docenter$`Espaço de Estudo da BCE`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"DBoa" <- NA
consulta_docenter$"DBoa" <- ifelse(str_detect(consulta_docenter$`Espaço de Estudo da BCE`, 
                                              "Boa"), 1, 0)

consulta_docenter$"DNão utilizei o local/serviço este ano." <- NA
consulta_docenter$"DNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Espaço de Estudo da BCE`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"DRuim" <- NA
consulta_docenter$"DRuim" <- ifelse(str_detect(consulta_docenter$`Espaço de Estudo da BCE`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"DPéssima" <- NA
consulta_docenter$"DPéssima" <- ifelse(str_detect(consulta_docenter$`Espaço de Estudo da BCE`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor4 <- consulta_docenter %>% 
  mutate(Classe = "Espaço de Estudo da BCE")

quadrodocgestaor4 <- quadrodocgestaor4%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`1DÓtima`, na.rm = T)/length(`1DÓtima`)),
             Boa = percent(sum(`DBoa`, na.rm = T)/length(`DBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`DNão utilizei o local/serviço este ano.`, na.rm = T)/length(`DNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`DRuim`, na.rm = T)/length(`DRuim`)),
             Péssima = percent(sum(`DPéssima`, na.rm = T)/length(`DPéssima`)))


# Estacionamento

consulta_docenter <- consulta_docente %>% filter(!is.na(`Estacionamento`))

consulta_docenter$"FÓtima" <- NA
consulta_docenter$"FÓtima" <- ifelse(str_detect(consulta_docenter$`Estacionamento`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"FDBoa" <- NA
consulta_docenter$"FBoa" <- ifelse(str_detect(consulta_docenter$`Estacionamento`, 
                                              "Boa"), 1, 0)

consulta_docenter$"FNão utilizei o local/serviço este ano." <- NA
consulta_docenter$"FNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Estacionamento`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"FRuim" <- NA
consulta_docenter$"FRuim" <- ifelse(str_detect(consulta_docenter$`Estacionamento`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"FPéssima" <- NA
consulta_docenter$"FPéssima" <- ifelse(str_detect(consulta_docenter$`Estacionamento`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor5 <- consulta_docenter %>% 
  mutate(Classe = "Estacionamento")

quadrodocgestaor5 <- quadrodocgestaor5%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`FÓtima`, na.rm = T)/length(`FÓtima`)),
             Boa = percent(sum(`FBoa`, na.rm = T)/length(`FBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`FNão utilizei o local/serviço este ano.`, na.rm = T)/length(`FNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`FRuim`, na.rm = T)/length(`FRuim`)),
             Péssima = percent(sum(`FPéssima`, na.rm = T)/length(`FPéssima`)))


# Iluminacao Publica

consulta_docenter <- consulta_docente %>% filter(!is.na(`Iluminação pública`))

consulta_docenter$"GÓtima" <- NA
consulta_docenter$"GÓtima" <- ifelse(str_detect(consulta_docenter$`Iluminação pública`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"GDBoa" <- NA
consulta_docenter$"GBoa" <- ifelse(str_detect(consulta_docenter$`Iluminação pública`, 
                                              "Boa"), 1, 0)

consulta_docenter$"GNão utilizei o local/serviço este ano." <- NA
consulta_docenter$"GNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Iluminação pública`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"GRuim" <- NA
consulta_docenter$"GRuim" <- ifelse(str_detect(consulta_docenter$`Iluminação pública`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"GPéssima" <- NA
consulta_docenter$"GPéssima" <- ifelse(str_detect(consulta_docenter$`Iluminação pública`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor6 <- consulta_docenter %>% 
  mutate(Classe = "Iluminação pública")

quadrodocgestaor6 <- quadrodocgestaor6%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`GÓtima`, na.rm = T)/length(`GÓtima`)),
             Boa = percent(sum(`GBoa`, na.rm = T)/length(`GBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`GNão utilizei o local/serviço este ano.`, na.rm = T)/length(`GNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`GRuim`, na.rm = T)/length(`GRuim`)),
             Péssima = percent(sum(`GPéssima`, na.rm = T)/length(`GPéssima`)))

# Instalações sanitarias

consulta_docenter <- consulta_docente%>% filter(!is.na(`Instalações Sanitarias`))

consulta_docenter$"HÓtima" <- NA
consulta_docenter$"HÓtima" <- ifelse(str_detect(consulta_docenter$`Instalações Sanitarias`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"HDBoa" <- NA
consulta_docenter$"HBoa" <- ifelse(str_detect(consulta_docenter$`Instalações Sanitarias`, 
                                              "Boa"), 1, 0)

consulta_docenter$"HNão utilizei o local/serviço este ano." <- NA
consulta_docenter$"HNão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Instalações Sanitarias`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"HRuim" <- NA
consulta_docenter$"HRuim" <- ifelse(str_detect(consulta_docenter$`Instalações Sanitarias`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"HPéssima" <- NA
consulta_docenter$"HPéssima" <- ifelse(str_detect(consulta_docenter$`Instalações Sanitarias`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor7 <- consulta_docenter %>% 
  mutate(Classe = "Instalações Sanitarias")

quadrodocgestaor7 <- quadrodocgestaor7%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`HÓtima`, na.rm = T)/length(`HÓtima`)),
             Boa = percent(sum(`HBoa`, na.rm = T)/length(`HBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`HNão utilizei o local/serviço este ano.`, na.rm = T)/length(`HNão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`HRuim`, na.rm = T)/length(`HRuim`)),
             Péssima = percent(sum(`HPéssima`, na.rm = T)/length(`HPéssima`)))

# Ru

consulta_docenter <- consulta_docente %>% filter(!is.na(`Restaurante Universitario`))

consulta_docenter$"IÓtima" <- NA
consulta_docenter$"IÓtima" <- ifelse(str_detect(consulta_docenter$`Restaurante Universitario`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"IBoa" <- NA
consulta_docenter$"IBoa" <- ifelse(str_detect(consulta_docenter$`Restaurante Universitario`, 
                                              "Boa"), 1, 0)

consulta_docenter$"INão utilizei o local/serviço este ano." <- NA
consulta_docenter$"INão utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Restaurante Universitario`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"IRuim" <- NA
consulta_docenter$"IRuim" <- ifelse(str_detect(consulta_docenter$`Restaurante Universitario`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"IPéssima" <- NA
consulta_docenter$"IPéssima" <- ifelse(str_detect(consulta_docenter$`Restaurante Universitario`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor8 <- consulta_docenter %>% 
  mutate(Classe = "Restaurante Universitario")

quadrodocgestaor8 <- quadrodocgestaor8%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`IÓtima`, na.rm = T)/length(`IÓtima`)),
             Boa = percent(sum(`IBoa`, na.rm = T)/length(`IBoa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`INão utilizei o local/serviço este ano.`, na.rm = T)/length(`INão utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`IRuim`, na.rm = T)/length(`IRuim`)),
             Péssima = percent(sum(`IPéssima`, na.rm = T)/length(`IPéssima`)))

# Salas de Aula

consulta_docenter <- consulta_docente %>% filter(!is.na(`Salas de aula`))

consulta_docenter$"1Ótima" <- NA
consulta_docenter$"1Ótima" <- ifelse(str_detect(consulta_docenter$`Salas de aula`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"1Boa" <- NA
consulta_docenter$"1Boa" <- ifelse(str_detect(consulta_docenter$`Salas de aula`, 
                                              "Boa"), 1, 0)

consulta_docenter$"1Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"1Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Salas de aula`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"1Ruim" <- NA
consulta_docenter$"1Ruim" <- ifelse(str_detect(consulta_docenter$`Salas de aula`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"1Péssima" <- NA
consulta_docenter$"1Péssima" <- ifelse(str_detect(consulta_docenter$`Salas de aula`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor9 <- consulta_docenter %>% 
  mutate(Classe = "Salas de aula")

quadrodocgestaor9 <- quadrodocgestaor9%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`1Ótima`, na.rm = T)/length(`1Ótima`)),
             Boa = percent(sum(`1Boa`, na.rm = T)/length(`1Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`1Não utilizei o local/serviço este ano.`, na.rm = T)/length(`1Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`1Ruim`, na.rm = T)/length(`1Ruim`)),
             Péssima = percent(sum(`1Péssima`, na.rm = T)/length(`1Péssima`)))

# Sala dos professores

consulta_docenter <- consulta_docente %>% filter(!is.na(`Sala dos Professores`))

consulta_docenter$"2Ótima" <- NA
consulta_docenter$"2Ótima" <- ifelse(str_detect(consulta_docenter$`Sala dos Professores`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"2Boa" <- NA
consulta_docenter$"2Boa" <- ifelse(str_detect(consulta_docenter$`Sala dos Professores`, 
                                              "Boa"), 1, 0)

consulta_docenter$"2Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"2Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Sala dos Professores`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"2Ruim" <- NA
consulta_docenter$"2Ruim" <- ifelse(str_detect(consulta_docenter$`Sala dos Professores`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"2Péssima" <- NA
consulta_docenter$"2Péssima" <- ifelse(str_detect(consulta_docenter$`Sala dos Professores`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor10 <- consulta_docenter %>% 
  mutate(Classe = "Sala dos Professores")

quadrodocgestaor10 <- quadrodocgestaor10%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`2Ótima`, na.rm = T)/length(`2Ótima`)),
             Boa = percent(sum(`2Boa`, na.rm = T)/length(`2Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`2Não utilizei o local/serviço este ano.`, na.rm = T)/length(`2Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`2Ruim`, na.rm = T)/length(`2Ruim`)),
             Péssima = percent(sum(`2Péssima`, na.rm = T)/length(`2Péssima`)))

# Segurança Campus

consulta_docenter <- consulta_docente %>% filter(!is.na(`Segurança do Campus`))

consulta_docenter$"3Ótima" <- NA
consulta_docenter$"3Ótima" <- ifelse(str_detect(consulta_docenter$`Segurança do Campus`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"3Boa" <- NA
consulta_docenter$"3Boa" <- ifelse(str_detect(consulta_docenter$`Segurança do Campus`, 
                                              "Boa"), 1, 0)

consulta_docenter$"3Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"3Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Segurança do Campus`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"3Ruim" <- NA
consulta_docenter$"3Ruim" <- ifelse(str_detect(consulta_docenter$`Segurança do Campus`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"3Péssima" <- NA
consulta_docenter$"3Péssima" <- ifelse(str_detect(consulta_docenter$`Segurança do Campus`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor11 <- consulta_docenter %>% 
  mutate(Classe = "Segurança do Campus")

quadrodocgestaor11 <- quadrodocgestaor11%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`3Ótima`, na.rm = T)/length(`3Ótima`)),
             Boa = percent(sum(`3Boa`, na.rm = T)/length(`3Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`3Não utilizei o local/serviço este ano.`, na.rm = T)/length(`3Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`3Ruim`, na.rm = T)/length(`3Ruim`)),
             Péssima = percent(sum(`3Péssima`, na.rm = T)/length(`3Péssima`)))



# Suporte tecnologico

consulta_docenter <- consulta_docente %>% filter(!is.na(`Suporte tecnologico para administração`))

consulta_docenter$"4Ótima" <- NA
consulta_docenter$"4Ótima" <- ifelse(str_detect(consulta_docenter$`Suporte tecnologico para administração`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"4Boa" <- NA
consulta_docenter$"4Boa" <- ifelse(str_detect(consulta_docenter$`Suporte tecnologico para administração`, 
                                              "Boa"), 1, 0)

consulta_docenter$"4Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"4Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Suporte tecnologico para administração`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"4Ruim" <- NA
consulta_docenter$"4Ruim" <- ifelse(str_detect(consulta_docenter$`Suporte tecnologico para administração`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"4Péssima" <- NA
consulta_docenter$"4Péssima" <- ifelse(str_detect(consulta_docenter$`Suporte tecnologico para administração`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor12 <- consulta_docenter %>% 
  mutate(Classe = "Suporte tecnologico para administração")

quadrodocgestaor12 <- quadrodocgestaor12%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`4Ótima`, na.rm = T)/length(`4Ótima`)),
             Boa = percent(sum(`4Boa`, na.rm = T)/length(`4Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`4Não utilizei o local/serviço este ano.`, na.rm = T)/length(`4Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`4Ruim`, na.rm = T)/length(`4Ruim`)),
             Péssima = percent(sum(`4Péssima`, na.rm = T)/length(`4Péssima`)))


# Acervo fisico BCE

consulta_docenter <- consulta_docente %>% filter(!is.na(`Acervo Fisico BCE`))

consulta_docenter$"5Ótima" <- NA
consulta_docenter$"5Ótima" <- ifelse(str_detect(consulta_docenter$`Acervo Fisico BCE`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"5Boa" <- NA
consulta_docenter$"5Boa" <- ifelse(str_detect(consulta_docenter$`Acervo Fisico BCE`, 
                                              "Boa"), 1, 0)

consulta_docenter$"5Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"5Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Acervo Fisico BCE`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"5Ruim" <- NA
consulta_docenter$"5Ruim" <- ifelse(str_detect(consulta_docenter$`Acervo Fisico BCE`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"5Péssima" <- NA
consulta_docenter$"5Péssima" <- ifelse(str_detect(consulta_docenter$`Acervo Fisico BCE`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor13 <- consulta_docenter %>% 
  mutate(Classe = "Acervo Fisico BCE")

quadrodocgestaor13 <- quadrodocgestaor13%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`5Ótima`, na.rm = T)/length(`5Ótima`)),
             Boa = percent(sum(`5Boa`, na.rm = T)/length(`5Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`5Não utilizei o local/serviço este ano.`, na.rm = T)/length(`5Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`5Ruim`, na.rm = T)/length(`5Ruim`)),
             Péssima = percent(sum(`5Péssima`, na.rm = T)/length(`5Péssima`)))


# Acervo Digital BCE

consulta_docenter <- consulta_docente %>% filter(!is.na(`Acervo Virtual BCE`))

consulta_docenter$"6Ótima" <- NA
consulta_docenter$"6Ótima" <- ifelse(str_detect(consulta_docenter$`Acervo Virtual BCE`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"6Boa" <- NA
consulta_docenter$"6Boa" <- ifelse(str_detect(consulta_docenter$`Acervo Virtual BCE`, 
                                              "Boa"), 1, 0)

consulta_docenter$"6Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"6Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Acervo Virtual BCE`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"6Ruim" <- NA
consulta_docenter$"6Ruim" <- ifelse(str_detect(consulta_docenter$`Acervo Virtual BCE`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"6Péssima" <- NA
consulta_docenter$"6Péssima" <- ifelse(str_detect(consulta_docenter$`Acervo Virtual BCE`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor14 <- consulta_docenter %>% 
  mutate(Classe = "Acervo Virtual BCE")

quadrodocgestaor14 <- quadrodocgestaor14%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`6Ótima`, na.rm = T)/length(`6Ótima`)),
             Boa = percent(sum(`6Boa`, na.rm = T)/length(`6Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`6Não utilizei o local/serviço este ano.`, na.rm = T)/length(`6Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`6Ruim`, na.rm = T)/length(`6Ruim`)),
             Péssima = percent(sum(`6Péssima`, na.rm = T)/length(`6Péssima`)))


# Funcionamento Teams

consulta_docenter <- consulta_docente %>% filter(!is.na(`Funcionamento Teams`))

consulta_docenter$"7Ótima" <- NA
consulta_docenter$"7Ótima" <- ifelse(str_detect(consulta_docenter$`Funcionamento Teams`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"7Boa" <- NA
consulta_docenter$"7Boa" <- ifelse(str_detect(consulta_docenter$`Funcionamento Teams`, 
                                              "Boa"), 1, 0)

consulta_docenter$"7Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"7Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Funcionamento Teams`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"7Ruim" <- NA
consulta_docenter$"7Ruim" <- ifelse(str_detect(consulta_docenter$`Funcionamento Teams`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"7Péssima" <- NA
consulta_docenter$"7Péssima" <- ifelse(str_detect(consulta_docenter$`Funcionamento Teams`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor15 <- consulta_docenter %>% 
  mutate(Classe = "Funcionamento Teams")

quadrodocgestaor15 <- quadrodocgestaor15%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`7Ótima`, na.rm = T)/length(`7Ótima`)),
             Boa = percent(sum(`7Boa`, na.rm = T)/length(`7Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`7Não utilizei o local/serviço este ano.`, na.rm = T)/length(`7Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`7Ruim`, na.rm = T)/length(`7Ruim`)),
             Péssima = percent(sum(`7Péssima`, na.rm = T)/length(`7Péssima`)))


# Funcionamento Aprender

consulta_docenter <- consulta_docente %>% filter(!is.na(`Funcionamento da Plataforma Institucional Aprender`))

consulta_docenter$"8Ótima" <- NA
consulta_docenter$"8Ótima" <- ifelse(str_detect(consulta_docenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"8Boa" <- NA
consulta_docenter$"8Boa" <- ifelse(str_detect(consulta_docenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                              "Boa"), 1, 0)

consulta_docenter$"8Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"8Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"8Ruim" <- NA
consulta_docenter$"8Ruim" <- ifelse(str_detect(consulta_docenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"8Péssima" <- NA
consulta_docenter$"8Péssima" <- ifelse(str_detect(consulta_docenter$`Funcionamento da Plataforma Institucional Aprender`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor16 <- consulta_docenter %>% 
  mutate(Classe = "Funcionamento da Plataforma Institucional Aprender")

quadrodocgestaor16 <- quadrodocgestaor16%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`8Ótima`, na.rm = T)/length(`8Ótima`)),
             Boa = percent(sum(`8Boa`, na.rm = T)/length(`8Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`8Não utilizei o local/serviço este ano.`, na.rm = T)/length(`8Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`8Ruim`, na.rm = T)/length(`8Ruim`)),
             Péssima = percent(sum(`8Péssima`, na.rm = T)/length(`8Péssima`)))


# Funcionamento SIGAA

consulta_docenter <- consulta_docente %>% filter(!is.na(`Funcionamento SIGAA`))

consulta_docenter$"9Ótima" <- NA
consulta_docenter$"9Ótima" <- ifelse(str_detect(consulta_docenter$`Funcionamento SIGAA`, 
                                                "Ótima"), 1, 0)
consulta_docenter$"9Boa" <- NA
consulta_docenter$"9Boa" <- ifelse(str_detect(consulta_docenter$`Funcionamento SIGAA`, 
                                              "Boa"), 1, 0)

consulta_docenter$"9Não utilizei o local/serviço este ano." <- NA
consulta_docenter$"9Não utilizei o local/serviço este ano." <- ifelse(str_detect(consulta_docenter$`Funcionamento SIGAA`, 
                                                                                 "Não utilizei o local/serviço este ano."), 1, 0)

consulta_docenter$"9Ruim" <- NA
consulta_docenter$"9Ruim" <- ifelse(str_detect(consulta_docenter$`Funcionamento SIGAA`, 
                                               "Ruim"), 1, 0)

consulta_docenter$"9Péssima" <- NA
consulta_docenter$"9Péssima" <- ifelse(str_detect(consulta_docenter$`Funcionamento SIGAA`, 
                                                  "Péssima"), 1, 0)


quadrodocgestaor17 <- consulta_docenter %>% 
  mutate(Classe = "Funcionamento SIGAA")

quadrodocgestaor17 <- quadrodocgestaor17%>%
  group_by(Classe) %>% 
  summarise(Ótima = percent(sum(`9Ótima`, na.rm = T)/length(`9Ótima`)),
             Boa = percent(sum(`9Boa`, na.rm = T)/length(`9Boa`)),
             "Não utilizei o local/serviço este ano." = percent(sum(`9Não utilizei o local/serviço este ano.`, na.rm = T)/length(`9Não utilizei o local/serviço este ano.`)),
             Ruim = percent(sum(`9Ruim`, na.rm = T)/length(`9Ruim`)),
             Péssima = percent(sum(`9Péssima`, na.rm = T)/length(`9Péssima`)))

quadrodocgestaortotal <- rbind(quadrodocgestaor, quadrodocgestaor1)
quadrodocgestaortotal1 <- rbind(quadrodocgestaortotal, quadrodocgestaor2)
quadrodocgestaortotal2 <- rbind(quadrodocgestaortotal1, quadrodocgestaor3)
quadrodocgestaortotal3 <- rbind(quadrodocgestaortotal2, quadrodocgestaor4)
quadrodocgestaortotal4 <- rbind(quadrodocgestaortotal3, quadrodocgestaor5)
quadrodocgestaortotal5 <- rbind(quadrodocgestaortotal4, quadrodocgestaor6)
quadrodocgestaortotal6 <- rbind(quadrodocgestaortotal5, quadrodocgestaor7)
quadrodocgestaortotal7 <- rbind(quadrodocgestaortotal6, quadrodocgestaor8)
quadrodocgestaortotal8 <- rbind(quadrodocgestaortotal7, quadrodocgestaor9)
quadrodocgestaortotal9 <- rbind(quadrodocgestaortotal8, quadrodocgestaor10)
quadrodocgestaortotal10 <- rbind(quadrodocgestaortotal9, quadrodocgestaor11)
quadrodocgestaortotal11 <- rbind(quadrodocgestaortotal10, quadrodocgestaor12)
quadrodocgestaortotal12 <- rbind(quadrodocgestaortotal11, quadrodocgestaor13)
quadrodocgestaortotal13 <- rbind(quadrodocgestaortotal12, quadrodocgestaor14)
quadrodocgestaortotal14 <- rbind(quadrodocgestaortotal13, quadrodocgestaor15)
quadrodocgestaortotal15 <- rbind(quadrodocgestaortotal14, quadrodocgestaor16)
quadrodocgestaortotal16 <- rbind(quadrodocgestaortotal15, quadrodocgestaor17)

quadrotecgestaortotal <- rbind(quadrodocgestaor, quadrodocgestaor1)



#Aluno Assistencia Pergunta 0


consulta_alunor <- consulta_aluno %>% filter(!is.na(`O programa de assistência estudantil tem regras claras e simples.`))

consulta_alunor $"Concordo Totalmente" <- NA
consulta_alunor $"Concordo Totalmente" <- ifelse(str_detect(consulta_alunor$`O programa de assistência estudantil tem regras claras e simples.`, 
                                                 "Concordo Totalmente"), 1, 0)
consulta_alunor $"Concordo" <- NA
consulta_alunor $"Concordo" <- ifelse(str_detect(consulta_alunor$`O programa de assistência estudantil tem regras claras e simples.`, 
                                               "Concordo"), 1, 0)

consulta_alunor $"Discordo" <- NA
consulta_alunor $"Discordo" <- ifelse(str_detect(consulta_alunor$`O programa de assistência estudantil tem regras claras e simples.`, 
                                                                                  "Discordo"), 1, 0)

consulta_alunor $"Discordo Totalmente" <- NA
consulta_alunor $"Discordo Totalmente" <- ifelse(str_detect(consulta_alunor$`O programa de assistência estudantil tem regras claras e simples.`, 
                                                "Discordo Totalmente"), 1, 0)

consulta_alunor $"Nem Concordo, Nem Discordo" <- NA
consulta_alunor $"Nem Concordo, Nem Discordo" <- ifelse(str_detect(consulta_alunor$`O programa de assistência estudantil tem regras claras e simples.`, 
                                                   "Nem Concordo, Nem Discordo"), 1, 0)

consulta_alunor $"Não Sei" <- NA
consulta_alunor $"Não Sei" <- ifelse(str_detect(consulta_alunor$`O programa de assistência estudantil tem regras claras e simples.`, 
                                                                     "Não Sei"), 1, 0)


quadroaluno1 <- consulta_alunor %>% 
  mutate(Classe = "O programa de assistência estudantil tem regras claras e simples.")

quadroaluno1 <- quadroaluno1%>%
  group_by(Classe) %>% 
  summarise("Concordo Totalmente" = percent(sum(`Concordo Totalmente`, na.rm = T)/length(`Concordo Totalmente`)),
            Concordo = percent(sum(`Concordo`, na.rm = T)/length(`Concordo`)),
            "Nem Concordo, Nem Discordo" = percent(sum(`Nem Concordo, Nem Discordo`, na.rm = T)/length(`Nem Concordo, Nem Discordo`)),
            Discordo = percent(sum(`Discordo`, na.rm = T)/length(`Discordo`)),
            "Discordo Totalmente" = percent(sum(`Discordo Totalmente`, na.rm = T)/length(`Discordo Totalmente`)),
            "Não Sei" = percent(sum(`Não Sei`, na.rm = T)/length(`Não Sei`)))




# Pergunta 2


consulta_alunor <- consulta_aluno %>% filter(!is.na(`As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.`))

consulta_alunor $"1Concordo Totalmente" <- NA
consulta_alunor $"1Concordo Totalmente" <- ifelse(str_detect(consulta_alunor$`As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.`, 
                                                            "Concordo Totalmente"), 1, 0)
consulta_alunor $"1Concordo" <- NA
consulta_alunor $"1Concordo" <- ifelse(str_detect(consulta_alunor$`As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.`, 
                                                 "Concordo"), 1, 0)

consulta_alunor $"1Discordo" <- NA
consulta_alunor $"1Discordo" <- ifelse(str_detect(consulta_alunor$`As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.`, 
                                                 "Discordo"), 1, 0)

consulta_alunor $"1Discordo Totalmente" <- NA
consulta_alunor $"1Discordo Totalmente" <- ifelse(str_detect(consulta_alunor$`As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.`, 
                                                            "Discordo Totalmente"), 1, 0)

consulta_alunor $"1Nem Concordo, Nem Discordo" <- NA
consulta_alunor $"1Nem Concordo, Nem Discordo" <- ifelse(str_detect(consulta_alunor$`As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.`, 
                                                                   "Nem Concordo, Nem Discordo"), 1, 0)

consulta_alunor $"1Não Sei" <- NA
consulta_alunor $"1Não Sei" <- ifelse(str_detect(consulta_alunor$`As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.`, 
                                                "Não Sei"), 1, 0)


quadroaluno2 <- consulta_alunor %>% 
  mutate(Classe = "As disciplinas cursadas contribuíram para sua formação integral, como cidadão e profissional.")

quadroaluno2 <- quadroaluno2%>%
  group_by(Classe) %>% 
  summarise("Concordo Totalmente" = percent(sum(`1Concordo Totalmente`, na.rm = T)/length(`1Concordo Totalmente`)),
            Concordo = percent(sum(`1Concordo`, na.rm = T)/length(`1Concordo`)),
            "Nem Concordo, Nem Discordo" = percent(sum(`1Nem Concordo, Nem Discordo`, na.rm = T)/length(`1Nem Concordo, Nem Discordo`)),
            Discordo = percent(sum(`1Discordo`, na.rm = T)/length(`1Discordo`)),
            "Discordo Totalmente" = percent(sum(`1Discordo Totalmente`, na.rm = T)/length(`1Discordo Totalmente`)),
            "Não Sei" = percent(sum(`1Não Sei`, na.rm = T)/length(`1Não Sei`)))




# Pergunta 3


consulta_alunor <- consulta_aluno %>% filter(!is.na(`Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.`))

consulta_alunor $"2Concordo Totalmente" <- NA
consulta_alunor $"2Concordo Totalmente" <- ifelse(str_detect(consulta_alunor$`Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.`, 
                                                            "Concordo Totalmente"), 1, 0)
consulta_alunor $"2Concordo" <- NA
consulta_alunor $"2Concordo" <- ifelse(str_detect(consulta_alunor$`Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.`, 
                                                  "Concordo"), 1, 0)

consulta_alunor $"2Discordo" <- NA
consulta_alunor $"2Discordo" <- ifelse(str_detect(consulta_alunor$`Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.`, 
                                                  "Discordo"), 1, 0)

consulta_alunor $"2Discordo Totalmente" <- NA
consulta_alunor $"2Discordo Totalmente" <- ifelse(str_detect(consulta_alunor$`Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.`, 
                                                             "Discordo Totalmente"), 1, 0)

consulta_alunor $"2Nem Concordo, Nem Discordo" <- NA
consulta_alunor $"2Nem Concordo, Nem Discordo" <- ifelse(str_detect(consulta_alunor$`Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.`, 
                                                                    "Nem Concordo, Nem Discordo"), 1, 0)

consulta_alunor $"2Não Sei" <- NA
consulta_alunor $"2Não Sei" <- ifelse(str_detect(consulta_alunor$`Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.`, 
                                                 "Não Sei"), 1, 0)


quadroaluno2 <- consulta_alunor %>% 
  mutate(Classe = "Os conteúdos abordados nas disciplinas do curso favoreceram sua atuação em estágios ou em atividades de iniciação profissional.")

quadroaluno2 <- quadroaluno2%>%
  group_by(Classe) %>% 
  summarise("Concordo Totalmente" = percent(sum(`2Concordo Totalmente`, na.rm = T)/length(`2Concordo Totalmente`)),
            Concordo = percent(sum(`2Concordo`, na.rm = T)/length(`2Concordo`)),
            "Nem Concordo, Nem Discordo" = percent(sum(`2Nem Concordo, Nem Discordo`, na.rm = T)/length(`2Nem Concordo, Nem Discordo`)),
            Discordo = percent(sum(`2Discordo`, na.rm = T)/length(`2Discordo`)),
            "Discordo Totalmente" = percent(sum(`2Discordo Totalmente`, na.rm = T)/length(`2Discordo Totalmente`)),
            "Não Sei" = percent(sum(`2Não Sei`, na.rm = T)/length(`2Não Sei`)))



# Pergunta 4


consulta_alunor <- consulta_aluno %>% filter(!is.na(`As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.`))

consulta_alunor $"3Concordo Totalmente" <- NA
consulta_alunor $"3Concordo Totalmente" <- ifelse(str_detect(consulta_alunor$`As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.`, 
                                                             "Concordo Totalmente"), 1, 0)
consulta_alunor $"3Concordo" <- NA
consulta_alunor $"3Concordo" <- ifelse(str_detect(consulta_alunor$`As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.`, 
                                                  "Concordo"), 1, 0)

consulta_alunor $"3Discordo" <- NA
consulta_alunor $"3Discordo" <- ifelse(str_detect(consulta_alunor$`As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.`, 
                                                  "Discordo"), 1, 0)

consulta_alunor $"3Discordo Totalmente" <- NA
consulta_alunor $"3Discordo Totalmente" <- ifelse(str_detect(consulta_alunor$`As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.`, 
                                                             "Discordo Totalmente"), 1, 0)

consulta_alunor $"3Nem Concordo, Nem Discordo" <- NA
consulta_alunor $"3Nem Concordo, Nem Discordo" <- ifelse(str_detect(consulta_alunor$`As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.`, 
                                                                    "Nem Concordo, Nem Discordo"), 1, 0)

consulta_alunor $"3Não Sei" <- NA
consulta_alunor $"3Não Sei" <- ifelse(str_detect(consulta_alunor$`As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.`, 
                                                 "Não Sei"), 1, 0)


quadroaluno3 <- consulta_alunor %>% 
  mutate(Classe = "As metodologias de ensino utilizadas no curso desafiam você a aprofundar conhecimentos e desenvolver competências reflexivas e críticas.")

quadroaluno3 <- quadroaluno3%>%
  group_by(Classe) %>% 
  summarise("Concordo Totalmente" = percent(sum(`3Concordo Totalmente`, na.rm = T)/length(`3Concordo Totalmente`)),
            Concordo = percent(sum(`3Concordo`, na.rm = T)/length(`3Concordo`)),
            "Nem Concordo, Nem Discordo" = percent(sum(`3Nem Concordo, Nem Discordo`, na.rm = T)/length(`3Nem Concordo, Nem Discordo`)),
            Discordo = percent(sum(`3Discordo`, na.rm = T)/length(`3Discordo`)),
            "Discordo Totalmente" = percent(sum(`3Discordo Totalmente`, na.rm = T)/length(`3Discordo Totalmente`)),
            "Não Sei" = percent(sum(`3Não Sei`, na.rm = T)/length(`3Não Sei`)))



# Pergunta 5


consulta_alunor <- consulta_aluno %>% filter(!is.na(`A UnB incentiva o(a) estudante a participar de atividades de monitoria.`))

consulta_alunor $"4Concordo Totalmente" <- NA
consulta_alunor $"4Concordo Totalmente" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de monitoria.`, 
                                                             "Concordo Totalmente"), 1, 0)
consulta_alunor $"4Concordo" <- NA
consulta_alunor $"4Concordo" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de monitoria.`, 
                                                  "Concordo"), 1, 0)

consulta_alunor $"4Discordo" <- NA
consulta_alunor $"4Discordo" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de monitoria.`, 
                                                  "Discordo"), 1, 0)

consulta_alunor $"4Discordo Totalmente" <- NA
consulta_alunor $"4Discordo Totalmente" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de monitoria.`, 
                                                             "Discordo Totalmente"), 1, 0)

consulta_alunor $"4Nem Concordo, Nem Discordo" <- NA
consulta_alunor $"4Nem Concordo, Nem Discordo" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de monitoria.`, 
                                                                    "Nem Concordo, Nem Discordo"), 1, 0)

consulta_alunor $"4Não Sei" <- NA
consulta_alunor $"4Não Sei" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de monitoria.`, 
                                                 "Não Sei"), 1, 0)


quadroaluno4 <- consulta_alunor %>% 
  mutate(Classe = "A UnB incentiva o(a) estudante a participar de atividades de monitoria.")

quadroaluno4 <- quadroaluno4%>%
  group_by(Classe) %>% 
  summarise("Concordo Totalmente" = percent(sum(`4Concordo Totalmente`, na.rm = T)/length(`4Concordo Totalmente`)),
            Concordo = percent(sum(`4Concordo`, na.rm = T)/length(`4Concordo`)),
            "Nem Concordo, Nem Discordo" = percent(sum(`4Nem Concordo, Nem Discordo`, na.rm = T)/length(`4Nem Concordo, Nem Discordo`)),
            Discordo = percent(sum(`4Discordo`, na.rm = T)/length(`4Discordo`)),
            "Discordo Totalmente" = percent(sum(`4Discordo Totalmente`, na.rm = T)/length(`4Discordo Totalmente`)),
            "Não Sei" = percent(sum(`4Não Sei`, na.rm = T)/length(`4Não Sei`)))


# Pergunta 6


consulta_alunor <- consulta_aluno %>% filter(!is.na(`A UnB incentiva o(a) estudante a participar de atividades de tutoria.`))

consulta_alunor $"5Concordo Totalmente" <- NA
consulta_alunor $"5Concordo Totalmente" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de tutoria.`, 
                                                             "Concordo Totalmente"), 1, 0)
consulta_alunor $"5Concordo" <- NA
consulta_alunor $"5Concordo" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de tutoria.`, 
                                                  "Concordo"), 1, 0)

consulta_alunor $"5Discordo" <- NA
consulta_alunor $"5Discordo" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de tutoria.`, 
                                                  "Discordo"), 1, 0)

consulta_alunor $"5Discordo Totalmente" <- NA
consulta_alunor $"5Discordo Totalmente" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de tutoria.`, 
                                                             "Discordo Totalmente"), 1, 0)

consulta_alunor $"5Nem Concordo, Nem Discordo" <- NA
consulta_alunor $"5Nem Concordo, Nem Discordo" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de tutoria.`, 
                                                                    "Nem Concordo, Nem Discordo"), 1, 0)

consulta_alunor $"5Não Sei" <- NA
consulta_alunor $"5Não Sei" <- ifelse(str_detect(consulta_alunor$`A UnB incentiva o(a) estudante a participar de atividades de tutoria.`, 
                                                 "Não Sei"), 1, 0)


quadroaluno5 <- consulta_alunor %>% 
  mutate(Classe = "A UnB incentiva o(a) estudante a participar de atividades de tutoria.")

quadroaluno5 <- quadroaluno5%>%
  group_by(Classe) %>% 
  summarise("Concordo Totalmente" = percent(sum(`5Concordo Totalmente`, na.rm = T)/length(`5Concordo Totalmente`)),
            Concordo = percent(sum(`5Concordo`, na.rm = T)/length(`5Concordo`)),
            "Nem Concordo, Nem Discordo" = percent(sum(`5Nem Concordo, Nem Discordo`, na.rm = T)/length(`5Nem Concordo, Nem Discordo`)),
            Discordo = percent(sum(`5Discordo`, na.rm = T)/length(`5Discordo`)),
            "Discordo Totalmente" = percent(sum(`5Discordo Totalmente`, na.rm = T)/length(`5Discordo Totalmente`)),
            "Não Sei" = percent(sum(`5Não Sei`, na.rm = T)/length(`5Não Sei`)))