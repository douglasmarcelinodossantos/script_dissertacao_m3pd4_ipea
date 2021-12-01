################ Levantando os dados para a Dissertação(2) #####################
################        03 de outubro de 2021              #####################

# Instalando os pacotes 

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("readxl")
#install.packages("purrr")
#install.packages("data.table")
#install.packages("deflateBR")
#install.packages("BETS")
#install.packages("sidrar")
#install.packages("maddison")
#install.packages("ggplot2")
#install.packages("ggrepel")
#install.packages("scales")
#install.packages("xlsx")
#install.packages("gdata")
#install.packages("shiny")
#install.packages("flexdashboard")
#install.packages("markdown")
#install.packages("lubridate")
#install.packages("tibble")
#install.packages("xlsx")

# Carregando os pacotes
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(purrr)
library(data.table)
library(deflateBR)
library(BETS)
library(sidrar)
library(maddison)
library(ggplot2)
library(ggrepel)
library(scales)
library(readr)
library(readxl)
library(xlsx)
library(gdata)
library(shiny)
library(flexdashboard)
library(markdown)
library(lubridate)
library(tibble)
library(magrittr) 

# Desligando notacao cientifica
options(scipen = 999)
options(digits=2)

# Definindo o diretório
# setwd("C:/Users/Adriana/Documents/DOUGLAS/PESSOAIS/MESTRADOIPEA/M3PD4IPEA/
# Dissertação/Qualificação/Insumos")
setwd("D:/NOTEBOOK HP PAVILION/MESTRADOIPEA/M3PD4IPEA/Dissertação/Dados")

################################################################################

# Carregando dados do Resultado Primário do Governo Central (anual) - abaixo da 
# linha (RTN, aba 2.2, item 8: 5 + 6 + 7) - (2010 a 2020) - Preços Correntes

primario_2010_2020_prcorrentes <- 
  read_xlsx("primario_2010_2020_prcorrentes.xlsx")
str(primario_2010_2020_prcorrentes)
head(primario_2010_2020_prcorrentes, n= 11)
View(primario_2010_2020_prcorrentes)

# Deflacionando os valores a preços de agosto de 2021, usando o pacote 
# deflateBR, função auxiliar "ipca" (valores referentes Resultado Primário do 
# Governo Central  (anual) - abaixo da linha (RTN, aba 2.2, item 8: 5 + 6 + 7) - 
# (2010 a 2020) 

primario_2010_2020_prcorrentes = primario_2010_2020_prcorrentes %>% 
  mutate(Ano = as.Date(Ano))
str(primario_2010_2020_prcorrentes)
primario_2010_2020_pragosto2021 <- primario_2010_2020_prcorrentes %>% 
  mutate(Valor_real = ipca(primario_2010_2020_prcorrentes$Valor, 
                           primario_2010_2020_prcorrentes$Ano, "08/2021"))

primario_2010_2020_pragosto2021
View(primario_2010_2020_pragosto2021)

################################################################################
# Carregando dados referente aos dividendos pagos à União, aos aportes recebidos 
#da União, e aos lucros líquidos, ativos totais e patrimônio líquido 
#apresentados pelas empresas estatais federais no período 2010 a 2020 - 
# Preços Correntes

dividendos_jcp_aportes <- read_excel("dividendos_jcp_aportes.xlsx")
str(dividendos_jcp_aportes)
head(dividendos_jcp_aportes, n = 19)
View(dividendos_jcp_aportes)

# Transformando a variável "Ano" em data (variável tipo "Date" )

dividendos_jcp_aportes = dividendos_jcp_aportes %>% mutate(Ano = as.Date(Ano))
str(dividendos_jcp_aportes)

# Transformando as variáveis "Data_Criacao" e "Data_Constituicao" em variável 
# tipo "Date" 
# "Data_Criacao"
# dividendos_jcp_aportes = dividendos_jcp_aportes %>% mutate(Data_Criacao = 
# as.numeric(Data_Criacao))

str(dividendos_jcp_aportes)

# "Data_Constituicao"
# dividendos_jcp_aportes = dividendos_jcp_aportes %>% mutate(Data_Constituicao = 
# as.numeric(Data_Constituicao))

str(dividendos_jcp_aportes)
View(dividendos_jcp_aportes)

####################################################################################################################################################################

# Avaliando as datas de criações das atuais empresas estatais federais

# Selecionando as variáveis necessárias para essa avaliação. Base de dados: 
# dividendos_jcp_aportes

eef_criadas_por_presidente <- dividendos_jcp_aportes %>% 
  select (Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
          Classificacao)
str(eef_criadas_por_presidente)  
eef_criadas_por_presidente
View(eef_criadas_por_presidente)

# Criando as variáveis necessárias para essa avaliação
# Identificando as atuais estatais federais que foram criadas nos Governos de 
# cada Presidente/Governante

eef_criadas_por_presidente <- eef_criadas_por_presidente %>%
  mutate(Criada_no_Governo = 
           case_when(Sigla_Empresa == "Amazul" ~ "Dilma", Sigla_Empresa == 
                       "CEITEC" ~ "Lula", Sigla_Empresa == "CBTU" ~ 
                       "João Figueiredo", Sigla_Empresa == "CODEVASF" ~ 
                       "Ernesto Geisel", Sigla_Empresa == "CPRM" ~ 
                       "Costa e Silva", Sigla_Empresa == "CONAB" ~ 
                       "Fernando Collor", Sigla_Empresa == "EBC" ~ "Lula", 
                     Sigla_Empresa == "EMBRAPA" ~ "Emílio Médici", Sigla_Empresa 
                     == "EBSERH" ~ "Dilma", Sigla_Empresa == "EPE" ~ "Lula",
                     Sigla_Empresa == "EPL" ~ "Dilma", Sigla_Empresa == 
                       "TRENSURB" ~ "João Figueiredo", Sigla_Empresa == "HCPA" ~
                       "Emílio Médici", Sigla_Empresa == "CONCEIÇÃO" ~ 
                       "Juscelino Kubitschek", Sigla_Empresa == "IMBEL" ~ 
                       "Ernesto Geisel", Sigla_Empresa == "INB" ~ "José Sarney",
                     Sigla_Empresa == "NUCLEP" ~ "Ernesto Geisel", Sigla_Empresa 
                     == "TELEBRAS" ~ "Emílio Médici", Sigla_Empresa == "VALEC" ~ 
                       "José Sarney",	Sigla_Empresa == "ABGF" ~ "Dilma", 
                     Sigla_Empresa == "CODESP" ~ "João Figueiredo", 
                     Sigla_Empresa == "BANCO DA AMAZÔNIA" ~ "Getúlio Vargas",
                     Sigla_Empresa == "BB" ~ "D. João", Sigla_Empresa == "BNB" ~ 
                       "Getúlio Vargas", Sigla_Empresa == "BNDES" ~ 
                       "Getúlio Vargas", Sigla_Empresa == "CAIXA" ~ "D. Pedro II", 
                     Sigla_Empresa == "CMB" ~ "Emílio Médici", Sigla_Empresa == 
                       "CEAGESP" ~ "Costa e Silva", Sigla_Empresa == 
                       "CEASAMINAS" ~ "Emílio Médici", Sigla_Empresa == 
                       "ELETROBRAS" ~ "Jânio da Silva Quadros", Sigla_Empresa == 
                       "CODEBA" ~ "Ernesto Geisel", Sigla_Empresa == "CDC" ~ 
                       "Castello Branco", Sigla_Empresa == "CODESA" ~ 
                       "João Figueiredo", Sigla_Empresa == "CDP" ~ 
                       "Castello Branco", Sigla_Empresa == "CDRJ" ~ 
                       "Castello Branco",  Sigla_Empresa == "CODERN" ~ 
                       "Costa e Silva", Sigla_Empresa == "PPSA" ~ "Dilma", 
                     Sigla_Empresa == "ECT" ~ "Costa e Silva", Sigla_Empresa == 
                       "HEMOBRÁS" ~ "Lula", Sigla_Empresa == "INFRAERO" ~ 
                       "Emílio Médici", Sigla_Empresa == "DATAPREV" ~ 
                       "Ernesto Geisel", Sigla_Empresa == "EMGEPRON" ~ 
                       "João Figueiredo", Sigla_Empresa == "EMGEA" ~ 
                       "Fernando Henrique Cardoso", Sigla_Empresa == "FINEP" ~ 
                       "Costa e Silva", Sigla_Empresa == "PETROBRAS" ~ 
                       "Getúlio Vargas", Sigla_Empresa == "SERPRO" ~ 
                       "Castello Branco"))
eef_criadas_por_presidente
View(eef_criadas_por_presidente)

# Filtrando uma observação para cada empresa estatal federal

eef_criadas_por_presidente = filter(eef_criadas_por_presidente, 
                                    Ano == "2020-12-31")
str(eef_criadas_por_presidente)
eef_criadas_por_presidente
View(eef_criadas_por_presidente)

# Para salvar a base de dados no PC:
# write.xlsx(eef_criadas_por_presidente, "eef_criadas_por_presidente.xlsx")

eef_criadas_por_presidente <- 
  read_xlsx("eef_criadas_por_presidente.xlsx")
str(eef_criadas_por_presidente)
View(eef_criadas_por_presidente)


# Contando as Estatais Federais Criadas por cada Presidente/Governante

table_eef_por_presidente_agrupadas <- eef_criadas_por_presidente %>%
  group_by(Criada_no_Governo) %>%
  summarize(n())
str(table_eef_por_presidente_agrupadas)
table_eef_por_presidente_agrupadas
str(table_eef_por_presidente_agrupadas)
View(table_eef_por_presidente_agrupadas)
summary(table_eef_por_presidente_agrupadas)

# Para salvar a base de dados no PC:
# write.xlsx(table_eef_por_presidente_agrupadas, 
#           "table_eef_por_presidente_agrupadas.xlsx") 
# write.xlsx(table_eef_por_presidente_agrupadas, 
#"F:/NOTEBOOK HP PAVILION/MESTRADOIPEA/M3PD4IPEA/Dissertação/Dados/
# table_eef_por_presidente_agrupadas.xlsx") ## para salvar a base de dados em 
# outro diretório

## carregando a base de dados

table_eef_por_presidente_agrupadas <- 
  read_xlsx("table_eef_por_presidente_agrupadas.xlsx")  
View(table_eef_por_presidente_agrupadas)  

# "table_eef_por_presente.xlsx" que foi ordenada pela quantidade de estatais 
# criadas, de forma decrescente. 

str(table_eef_por_presidente_agrupadas)
names(table_eef_por_presidente_agrupadas)
table_eef_por_presidente_agrupadas <- table_eef_por_presidente_agrupadas %>% 
  select(Criada_no_Governo, Qtde_eef_criadas)
View(table_eef_por_presidente_agrupadas)
table_eef_por_presidente_agrupadas

# Plotando um gráfico de barras para contar a história das estatais criadas por
# cada Governo brasileiro

ggplot(table_eef_por_presidente_agrupadas, 
       aes(x=factor(Criada_no_Governo), y=(Qtde_eef_criadas))) + 
  geom_bar(position=position_dodge(), stat="identity",  colour="blue", 
           width = 0.5) + theme(legend.position="none",
                                axis.text.x = element_blank(), 
                                axis.ticks.x = element_blank(), 
                                axis.ticks.y = element_blank()) + xlab("") + 
  ylab("") + theme(axis.text.x = element_text(angle = 90, vjust = 0.9, 
                                              hjust= 1))

# Plotando um gráfico de barras para contar a história das estatais criadas por
# cada Governo brasileiro ordenado de forma decrescente

ggplot(table_eef_por_presidente_agrupadas, aes(x = reorder(Criada_no_Governo, 
                                                 -Qtde_eef_criadas), 
                                     y = Qtde_eef_criadas)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Estatais Federais atuais e os Governos nos quais foram criadas",
       subtitle = "(Empresas sob controle direto da União)",
       x = "Presidente/Governante",
       y = "Quantidade de Estatais Federais criadas") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################################################################
# Deflacionando os valores a preços de agosto de 2021, usando o pacote deflateBR, 
# função auxiliar ipca. Dados: dividendos pagos à União, aos aportes recebidos 
# da União, e aos lucros líquidos, ativos totais e patrimônio líquido 
# apresentados pelas empresas estatais federais no período 2010 a 2020)

# Deflacionando os dados acerca dos dividendos pagos à União (2010 - 2020)

str(dividendos_jcp_aportes)
dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes %>% 
  mutate(Dividendos_real = ipca(dividendos_jcp_aportes$Dividendos, 
                                dividendos_jcp_aportes$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna Dividendos_real ao lado da coluna Dividendos

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca dos aportes de capital recebidos da União 
# (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(Aporte_real = ipca(dividendos_jcp_aportes_pragosto2021$Aporte, 
                            dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "Aporte_real" ao lado da coluna "Aporte" (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca das subvenções recebidas, pelas empresas 
# estatais federais, da União (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(Subvencoes_real = ipca(dividendos_jcp_aportes_pragosto2021$Subvencoes, 
                                dividendos_jcp_aportes_pragosto2021$Ano, 
                                "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "Subvencoes_real" ao lado da coluna "Subvencoes"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca dos Juros de IHCD/IECP pagos pelas Instituições 
# Financeiras à União (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(Juros_Pagto_IHCD_real = 
           ipca(dividendos_jcp_aportes_pragosto2021$Juros_Pagto_IHCD,
                dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "Juros_Pagto_IHCD_real" ao lado da coluna "Juros_Pagto_IHCD"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca dos valores principais referentes aos contratos 
# IHCD/IECP devolvidos pelas Instituições Financeiras à União (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(Principal_Pgto_IHCD_real = 
           ipca(dividendos_jcp_aportes_pragosto2021$Principal_Pgto_IHCD,
                dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "Principal_Pgto_IHCD_real" ao lado da coluna 
# "Principal_Pgto_IHCD"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca dos lucros líquidos apresentados pelas empresas 
# estatais federais (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(LL_real = ipca(dividendos_jcp_aportes_pragosto2021$Lucro_Liquido, 
                        dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "LL_real" ao lado da coluna "Lucro_Liquido"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         Lucro_Liquido, LL_real, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca das despesas totais apresentadas pelas empresas 
# estatais federais (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(DT_real = ipca(dividendos_jcp_aportes_pragosto2021$Despesas_Totais, 
                        dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "DT_real" ao lado da coluna "Despesas_Totais"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         Lucro_Liquido, LL_real, Despesas_Totais, DT_real, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca das despesas de pessoal apresentadas pelas 
# empresas estatais federais (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(DP_real = ipca(dividendos_jcp_aportes_pragosto2021$Despesas_Pessoal, 
                        dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "DP_real" ao lado da coluna "Despesas_Pessoal"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         Lucro_Liquido, LL_real, Despesas_Totais, DT_real, Despesas_Pessoal, 
         DP_real, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca dos patrimônios líquidos apresentados pelas 
# empresas estatais federais (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(PL_real = ipca(dividendos_jcp_aportes_pragosto2021$Patrimonio_Liquido, 
                        dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "PL_real" ao lado da coluna "Patrimonio_Liquido"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         Lucro_Liquido, LL_real, Despesas_Totais, DT_real, Despesas_Pessoal, 
         DP_real, Patrimonio_Liquido, PL_real, everything())
                                                                                      
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Deflacionando os dados acerca dos ativos totais apresentados pelas empresas 
# estatais federais (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(AT_real = ipca(dividendos_jcp_aportes_pragosto2021$Ativos_Totais, 
                        dividendos_jcp_aportes_pragosto2021$Ano, "08/2021"))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "AT_real" ao lado da coluna "Ativos_Totais"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         Lucro_Liquido, LL_real, Despesas_Totais, DT_real, Despesas_Pessoal, 
         DP_real, Patrimonio_Liquido, PL_real, Ativos_Totais, AT_real, 
         everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Calculando a diferença entre os dividendos pagos à União e os juros e 
# principais referentes IHCD/IECP  e os valores para aportes de capital 
# recebidos da União (preços correntes)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(Dif_div_ihcd_aportes_subv = (dividendos_jcp_aportes_pragosto2021$Dividendos +
                                        dividendos_jcp_aportes_pragosto2021$Juros_Pagto_IHCD + 
                                        dividendos_jcp_aportes_pragosto2021$Principal_Pgto_IHCD - 
                              dividendos_jcp_aportes_pragosto2021$Aporte - 
                              dividendos_jcp_aportes_pragosto2021$Subvencoes))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "Dif_div_ihcd_aportes_subv" ao lado da coluna 
# "Principal_Pgto_IHCD_real" (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         Dif_div_ihcd_aportes_subv, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Calculando a diferença entre os dividendos pagos à União e os juros e 
# principais referentes IHCD/IECP e os valores para aportes de capital recebidos 
# da União (a preços de agosto/2021)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  mutate(Dif_div_ihcd_aportes_subv_real = 
           (dividendos_jcp_aportes_pragosto2021$Dividendos_real + 
              dividendos_jcp_aportes_pragosto2021$Juros_Pagto_IHCD_real + 
              dividendos_jcp_aportes_pragosto2021$Principal_Pgto_IHCD_real - 
                                   dividendos_jcp_aportes_pragosto2021$Aporte_real - 
                                   dividendos_jcp_aportes_pragosto2021$Subvencoes_real))
str(dividendos_jcp_aportes_pragosto2021)
head(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "Dif_div_ihcd_aportes_subv_real" ao lado da coluna 
# "Dif_div_ihcd_aportes_subv" (2010 - 2020)

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Dividendos, Dividendos_real, Aporte, Aporte_real, 
         Modalidade__Aporte, Subvencoes, Subvencoes_real, Juros_Pagto_IHCD, 
         Juros_Pagto_IHCD_real, Principal_Pgto_IHCD, Principal_Pgto_IHCD_real, 
         Dif_div_ihcd_aportes_subv, Dif_div_ihcd_aportes_subv_real, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Criando a variável setor de atuação das estatais federais

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>%
  mutate(Setor_Atuacao = 
           case_when(Sigla_Empresa == "Amazul" ~ "Pesquisa e Desenvolvimento", 
                     Sigla_Empresa == "CEITEC" ~ "Indústria", 
                     Sigla_Empresa == "CBTU" ~ "Transportes", 
                     Sigla_Empresa == "CODEVASF" ~ "Pesquisa e Desenvolvimento",
                     Sigla_Empresa == "CPRM" ~ "Pesquisa e Desenvolvimento",
                     Sigla_Empresa == "CONAB" ~ "Abastecimento", 
                     Sigla_Empresa == "EBC" ~ "Comunicações", 
                     Sigla_Empresa == "EMBRAPA" ~ "Pesquisa e Desenvolvimento", 
                     Sigla_Empresa == "EBSERH" ~ "Saúde", 
                     Sigla_Empresa == "EPE" ~ "Pesquisa e Desenvolvimento",
                     Sigla_Empresa == "EPL" ~ "Pesquisa e Desenvolvimento", 
                     Sigla_Empresa == "TRENSURB" ~ "Transportes", 
                     Sigla_Empresa == "HCPA" ~ "Saúde", 
                     Sigla_Empresa == "CONCEIÇÃO" ~ "Saúde", Sigla_Empresa == 
                       "IMBEL" ~ "Indústria", Sigla_Empresa == "INB" ~ "Indústria",
                     Sigla_Empresa == "NUCLEP" ~ "Indústria", Sigla_Empresa == 
                       "TELEBRAS" ~ "Comunicações", Sigla_Empresa == "VALEC" ~ 
                       "Transportes",	Sigla_Empresa == "ABGF" ~ "Serviços", 
                     Sigla_Empresa == "CODESP" ~ "Portos e Aeroportos", 
                     Sigla_Empresa == "BANCO DA AMAZÔNIA" ~ "Bancos e Serviços Financeiros",
                     Sigla_Empresa == "BB" ~ "Bancos e Serviços Financeiros", 
                     Sigla_Empresa == "BNB" ~ "Bancos e Serviços Financeiros", 
                     Sigla_Empresa == "BNDES" ~ "Bancos e Serviços Financeiros", 
                     Sigla_Empresa == "CAIXA" ~ "Bancos e Serviços Financeiros", 
                     Sigla_Empresa == "CMB" ~ "Indústria", Sigla_Empresa == 
                       "CEAGESP" ~ "Abastecimento", Sigla_Empresa == 
                       "CEASAMINAS" ~ "Abastecimento", Sigla_Empresa == 
                       "ELETROBRAS" ~ "Petróleo, Gás e Energia Elétrica", 
                     Sigla_Empresa == "CODEBA" ~ "Portos e Aeroportos", 
                     Sigla_Empresa == "CDC" ~ "Portos e Aeroportos", 
                     Sigla_Empresa == "CODESA" ~ "Portos e Aeroportos", 
                     Sigla_Empresa == "CDP" ~ "Portos e Aeroportos", 
                     Sigla_Empresa == "CDRJ" ~ "Portos e Aeroportos",  
                     Sigla_Empresa == "CODERN" ~ "Portos e Aeroportos", 
                     Sigla_Empresa == "PPSA" ~ "Serviços", Sigla_Empresa == 
                       "ECT" ~ "Comunicações", Sigla_Empresa == 
                       "HEMOBRÁS" ~ "Indústria", Sigla_Empresa == "INFRAERO" ~ 
                       "Portos e Aeroportos", Sigla_Empresa == "DATAPREV" ~ 
                       "Serviços", Sigla_Empresa == "EMGEPRON" ~ 
                       "Pesquisa e Desenvolvimento", Sigla_Empresa == "EMGEA" ~ 
                       "Serviços", Sigla_Empresa == "FINEP" ~ 
                       "Bancos e Serviços Financeiros", Sigla_Empresa == 
                       "PETROBRAS" ~ "Petróleo, Gás e Energia", Sigla_Empresa == 
                       "SERPRO" ~ "Serviços"))
dividendos_jcp_aportes_pragosto2021
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna Setor_Atuacao ao lado da coluna Classificacao

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Setor_Atuacao, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Salvando o dataframe "dividendos_jcp_aportes_pragosto2021" no computador
# write.xlsx(dividendos_jcp_aportes_pragosto2021, "F:/NOTEBOOK HP PAVILION/
# MESTRADOIPEA/M3PD4IPEA/Dissertação/Dados/
# dividendos_jcp_aportes_pragosto2021.xlsx")
# write.xlsx(dividendos_jcp_aportes_pragosto2021, 
           #"dividendos_jcp_aportes_pragosto2021.xlsx")

# Analisando se a relação entre dividendos pagos à União e aportes de capital 
# recebidos pelas estatais federais, no período 2010 - 2020, foi superavitária 
# ou deficitária

# Iniciando-se pelas estatais dependentes de recursos do Tesouro Nacional

# Filtrando os dados referentes às estatais dependentes 

dividendos_jcp_aportes_pragosto2021_dependentes = 
  filter(dividendos_jcp_aportes_pragosto2021, Classificacao %in% ("Dependente"))
str(dividendos_jcp_aportes_pragosto2021_dependentes)
head(dividendos_jcp_aportes_pragosto2021_dependentes)
View(dividendos_jcp_aportes_pragosto2021_dependentes)
summary(dividendos_jcp_aportes_pragosto2021_dependentes)

# Adicionando os dados da Telebras para o período 2010 - 2019, ou seja, os dados 
# referente ao período no qual a Telebras era classificada como "não dependente"

telebras_2010_2019 = filter(dividendos_jcp_aportes_pragosto2021, 
                            Sigla_Empresa %in% ("TELEBRAS")) %>% 
                            filter(Classificacao %in% ("Não Dependente")) 
head(telebras_2010_2019, n=11)
View(telebras_2010_2019)

# Unindo os dois data frames

dividendos_jcp_aportes_pragosto2021_dependentes <- 
  rbind(dividendos_jcp_aportes_pragosto2021_dependentes, telebras_2010_2019)
View(dividendos_jcp_aportes_pragosto2021_dependentes)
str(dividendos_jcp_aportes_pragosto2021_dependentes)
dividendos_jcp_aportes_pragosto2021_dependentes <- 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% arrange(Ano)
dividendos_jcp_aportes_pragosto2021_dependentes <- 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% arrange(Empresa)
View(dividendos_jcp_aportes_pragosto2021_dependentes)
str(dividendos_jcp_aportes_pragosto2021_dependentes)

# Agrupando os valores referentes às subvenções recebidos da União por cada 
# estatal dependente, no período 2010 - 2020 (preços de agosto de 2021)

subvencoes_dependentes_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Subvencoes_real = sum(Subvencoes_real))
View(subvencoes_dependentes_pragosto2021)
str(subvencoes_dependentes_pragosto2021)
head(subvencoes_dependentes_pragosto2021, n=19)
summary(subvencoes_dependentes_pragosto2021)

# Somando as subvenções (a preços de agosto/2021) realizadas entre 2010 e 2020
sum(subvencoes_dependentes_pragosto2021$Soma_Subvencoes_real)

# Salvando a tabela subvencoes_dependentes_pragosto2021 no PC
# write.xlsx(subvencoes_dependentes_pragosto2021, 
#           "subvencoes_dependentes_pragosto2021.xlsx")

# Agrupando os valores referentes às subvenções recebidos da União por cada 
# estatal dependente, no período 2010 - 2020 (valores nominais)

subvencoes_dependentes_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Subvencoes = sum(Subvencoes))
View(subvencoes_dependentes_vlnominal)
str(subvencoes_dependentes_vlnominal)
head(subvencoes_dependentes_vlnominal, n=19)
summary(subvencoes_dependentes_vlnominal)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com as 
# subvenções reais e nominais recebidas da União, por cada estatal dependente

subvencoes_dependentes <- subvencoes_dependentes_vlnominal %>% 
  inner_join(subvencoes_dependentes_pragosto2021)
str(subvencoes_dependentes)
head(subvencoes_dependentes, n=19)
View(subvencoes_dependentes)

# Calculando os % das subvenções (a preços de 08/2021) em relação ao total de 
# subvenções recebidas pelas estatais dependentes

subvencoes_dependentes <- subvencoes_dependentes %>% 
  mutate(Percentual_Total = (subvencoes_dependentes$Soma_Subvencoes_real / 
                               sum(Soma_Subvencoes_real) * 100))
head(subvencoes_dependentes, n=19)
View(subvencoes_dependentes)

# Salvando a tabela subvencoes_dependentes no PC
# write.xlsx(subvencoes_dependentes, "subvencoes_dependentes.xlsx")

# Agrupando os valores referentes aos aportes de capital recebidos da União por 
# cada estatal dependente, no período 2010 - 2020 (preços de agosto de 2021)

View(dividendos_jcp_aportes_pragosto2021_dependentes)
aportes_dependentes_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Aportes_real = sum(Aporte_real))
View(aportes_dependentes_pragosto2021)
str(aportes_dependentes_pragosto2021)
head(aportes_dependentes_pragosto2021, n=19)
summary(aportes_dependentes_pragosto2021)

# Somando os aportes (a preços de agosto/2021) realizadas entre 2010 e 2020

sum(aportes_dependentes_pragosto2021$Soma_Aportes_real)

#write.xlsx(aportes_dependentes_pragosto2021, 
#           "aportes_dependentes_pragosto2021.xlsx")

# Agrupando os valores referentes aos aportes de capital recebidos da União por 
# cada estatal dependente, no período 2010 - 2020 (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_dependentes)
aportes_dependentes_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_aportes = sum(Aporte))
View(aportes_dependentes_vlnominal)
str(aportes_dependentes_vlnominal)
head(aportes_dependentes_vlnominal, n=19)
summary(aportes_dependentes_vlnominal)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# aportes reais e nominais recebidas da União, por cada estatal dependente 

aportes_dependentes <- aportes_dependentes_vlnominal %>% 
  inner_join(aportes_dependentes_pragosto2021)
str(aportes_dependentes)
head(aportes_dependentes, n=19)
View(aportes_dependentes)

aportes_dependentes <- aportes_dependentes %>% 
  mutate(Percentual_Total = 
           (aportes_dependentes$Soma_Aportes_real /  sum(Soma_Aportes_real) *
              100))
head(aportes_dependentes, n=19)
View(aportes_dependentes)

# Somando os aportes (valores nominais) realizadas entre 2010 e 2020

sum(aportes_dependentes$Soma_aportes)
                                                    
# Salvando a tabela aportes_dependentes no PC
# write.xlsx(aportes_dependentes, "aportes_dependentes.xlsx")

# Agrupando os valores referentes aos dividendos pagos à União por cada estatal
# dependente, no período 2010 - 2020 (preços de agosto de 2021)

dividendos_dependentes_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dividendos_real = sum(Dividendos_real))
View(dividendos_dependentes_pragosto2021)
str(dividendos_dependentes_pragosto2021)
head(dividendos_dependentes_pragosto2021, n=19)
summary(dividendos_dependentes_pragosto2021)

# Somando os dividendos (a preços de agosto/2021) realizadas entre 2010 e 2020

sum(dividendos_dependentes_pragosto2021$Soma_Dividendos_real)

# Agrupando os valores referentes aos dividendos pagos à União por cada estatal,
# no período 2010 - 2020 (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_dependentes)
dividendos_dependentes_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dividendos = sum(Dividendos))
View(dividendos_dependentes_vlnominal)
str(dividendos_dependentes_vlnominal)
head(dividendos_dependentes_vlnominal, n=19)
summary(dividendos_dependentes_vlnominal)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# dividendos reais e nominais pagos à União, por cada estatal dependente 

dividendos_dependentes <- dividendos_dependentes_vlnominal %>% 
  inner_join(dividendos_dependentes_pragosto2021)
str(dividendos_dependentes)
head(dividendos_dependentes)
View(dividendos_dependentes)

# Calculando os % dos dividendos (a preços de 08/2021) em relação ao total de 
# dividendos pagos à União pelas estatais dependentes

dividendos_dependentes <- dividendos_dependentes %>% 
  mutate(Percentual_Total = (dividendos_dependentes$Soma_Dividendos_real / 
                               sum(Soma_Dividendos_real) * 100))
head(dividendos_dependentes, n=19)
View(dividendos_dependentes)

# Salvando a tabela dividendos_dependentes no PC
# write.xlsx(dividendos_dependentes, "dividendos_dependentes.xlsx")

# Agrupando os valores referentes à diferença de dividendos pagos e aportes e 
# subvenções recebidas da União por cada estatal dependente, no período 2010 - 
# 2020 (preços de agosto de 2021)

str(dividendos_jcp_aportes_pragosto2021_dependentes)
dif_div_aportes_subv_dependentes_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dif_div_aportes_subv_real = sum(Dif_div_ihcd_aportes_subv_real))
View(dif_div_aportes_subv_dependentes_pragosto2021)
str(dif_div_aportes_subv_dependentes_pragosto2021)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# dividendos pagos à União e aportes e subvenções recebidos da União, por cada 
# estatal dependente

dif_dividendos_aportes_dependentes <- dividendos_dependentes_pragosto2021 %>% 
  inner_join(aportes_dependentes_pragosto2021)
str(dif_dividendos_aportes_dependentes)
head(dif_dividendos_aportes_dependentes)
View(dif_dividendos_aportes_dependentes)

dif_dividendos_aportes_dependentes <- dif_dividendos_aportes_dependentes %>% 
  inner_join(subvencoes_dependentes_pragosto2021)
str(dif_dividendos_aportes_dependentes)
head(dif_dividendos_aportes_dependentes)
View(dif_dividendos_aportes_dependentes)

dif_dividendos_aportes_dependentes <- dif_dividendos_aportes_dependentes %>% 
  mutate(Dif_dividendos_aportes_subv = Soma_Dividendos_real - Soma_Aportes_real - 
           Soma_Subvencoes_real)
str(dif_dividendos_aportes_dependentes)
head(dif_dividendos_aportes_dependentes, n = 19)
View(dif_dividendos_aportes_dependentes)

# Calculando os % da diferença entre dividendos e a soma das subvenções e 
# aportes (a preços de 08/2021) em relação ao total dessa diferença 

dif_dividendos_aportes_dependentes <- dif_dividendos_aportes_dependentes %>% 
  mutate(Percentual_Total = 
           (dif_dividendos_aportes_dependentes$Dif_dividendos_aportes_subv / 
                               sum(Dif_dividendos_aportes_subv) * 100))
head(dif_dividendos_aportes_dependentes, n=19)
View(dif_dividendos_aportes_dependentes)

# Somando as diferenças entre as somas das subvenções e os aportes e 
# os dividendos (a preços de agosto/2021) realizadas entre 2010 e 2020

sum(dif_dividendos_aportes_dependentes$Dif_dividendos_aportes_subv)

# Salvando o dataframe dif_dividendos_aportes_dependentes no computador
# write.xlsx(dif_dividendos_aportes_dependentes, 
#           "dif_dividendos_aportes_dependentes.xlsx")

# Plotando os gráficos

ggplot(dif_div_aportes_subv_dependentes_pragosto2021, 
       aes(x=factor(Sigla_Empresa), y=Soma_Dif_div_aportes_subv_real, 
           label=sprintf("%0.2f", round(Soma_Dif_div_aportes_subv_real, 
                                        digits = 2)))) +
  geom_bar(position=position_dodge(), stat="identity",  colour="blue", 
           width = 0.5) + theme(legend.position="none", 
                                axis.text.x = element_blank(), 
                                axis.ticks.x = element_blank(), 
                                axis.ticks.y = element_blank()) +
  #geom_text(size = 4, hjust = 1.2) +
  #coord_flip(ylim = c(1,6))+
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1)) 

# Criando uma dummy para indicar se a relação dividendos/JCP vs. Aportes/
# Subvenções é superavitária ou deficitária

dif_div_aportes_subv_dependentes_pragosto2021$positivo_negativo = 
  as.factor(ifelse(dif_div_aportes_subv_dependentes_pragosto2021$Soma_Dif_div_aportes_subv_real > 
                     0, yes = 1, no = 0))
head(dif_div_aportes_subv_dependentes_pragosto2021, n = 19)
str(dif_div_aportes_subv_dependentes_pragosto2021)

# Gráfico dos dados com a Valec

str(dif_dividendos_aportes_dependentes)
str(dif_div_aportes_subv_dependentes_pragosto2021)

ggplot(dif_div_aportes_subv_dependentes_pragosto2021, 
       aes(Sigla_Empresa, Soma_Dif_div_aportes_subv_real)) + 
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))

# Gráfico dos dados sem a Valec

dif_dividendos_aportes_dependentes_semvalec <- 
  dif_div_aportes_subv_dependentes_pragosto2021[!dif_div_aportes_subv_dependentes_pragosto2021$Sigla_Empresa== "VALEC",]
head(dif_dividendos_aportes_dependentes_semvalec, n = 19)
str(dif_dividendos_aportes_dependentes_semvalec)  
ggplot(dif_dividendos_aportes_dependentes_semvalec, 
       aes(Sigla_Empresa, Soma_Dif_div_aportes_subv_real)) + 
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))


# Plotando os gráficos de barras ordenado de forma decrescente

ggplot(dif_div_aportes_subv_dependentes_pragosto2021, 
       aes(x = reorder(Sigla_Empresa, +Soma_Dif_div_aportes_subv_real), 
           y=Soma_Dif_div_aportes_subv_real, 
           label=sprintf("%0.2f", round(Soma_Dif_div_aportes_subv_real, 
                                        digits = 2)))) +
  geom_bar(position=position_dodge(), stat="identity",  colour="blue", 
           width = 0.5) + theme(legend.position="none", 
                                axis.text.x = element_blank(), 
                                axis.ticks.x = element_blank(), 
                                axis.ticks.y = element_blank()) +
  #geom_text(size = 4, hjust = 1.2) +
  #coord_flip(ylim = c(1,6))+
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1)) 

ggplot(dif_div_aportes_subv_dependentes_pragosto2021, 
       aes(x = reorder(Sigla_Empresa, +Soma_Dif_div_aportes_subv_real),
           y=Soma_Dif_div_aportes_subv_real, 
           label=sprintf("%0.2f", round(Soma_Dif_div_aportes_subv_real, 
                                        digits = 2)))) +
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  labs(title = "Aportes e Subvenções Subtraídos dos Dividendos Pagos",
       subtitle = "(em R$ bilhões)",
       x = "Estatal Dependente",
       y = "R$ a preços de 08/2021") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))

# Descrevendo o Lucro Líquido das estatais federais dependentes
# Agrupando os valores referentes aos lucros líquidos de cada estatal dependente, 
# no período 2010 - 2020 (preços de agosto de 2021)

str(dividendos_jcp_aportes_pragosto2021_dependentes)
lucroliq_dependentes_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_LL_real = sum(LL_real))
View(lucroliq_dependentes_pragosto2021)
str(lucroliq_dependentes_pragosto2021)
head(lucroliq_dependentes_pragosto2021, n=19)
summary(lucroliq_dependentes_pragosto2021)

# Somando os Lucro Líquidos das estatais federais dependentes (a preços de 
# agosto/2021) realizadas entre 2010 e 2020

sum(lucroliq_dependentes_pragosto2021$Soma_LL_real)

# Salvando a tabela lucroliq_dependentes_pragosto2021 no PC
# write.xlsx(lucroliq_dependentes_pragosto2021, 
#           "lucroliq_dependentes_pragosto2021.xlsx")

# Agrupando os valores referentes ao lucro líquido de cada estatal dependente, 
# no período 2010 - 2020 (valores nominais)

lucroliq_dependentes_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_dependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Lucro_Liquido = sum(Lucro_Liquido))
View(lucroliq_dependentes_vlnominal)
str(lucroliq_dependentes_vlnominal)
head(lucroliq_dependentes_vlnominal, n=19)
summary(lucroliq_dependentes_vlnominal)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# lucros reais e nominais de cada estatal dependente 

lucroliq_dependentes <- lucroliq_dependentes_vlnominal %>% 
  inner_join(lucroliq_dependentes_pragosto2021)
str(lucroliq_dependentes)
head(lucroliq_dependentes, n=19)
View(lucroliq_dependentes)

# Salvando a tabela lucroliq_dependentes no PC
#write.xlsx(lucroliq_dependentes, 
           #"lucroliq_dependentes.xlsx")

# Calculando os % dos lucros líquidos (a preços de 08/2021) em relação ao total 
# de lucros líquidos reais das estatais dependentes
#lucroliq_dependentes <- lucroliq_dependentes %>% 
  #mutate(Percentual_Total = (lucroliq_dependentes$Soma_LL_real / 
                               #sum(Soma_LL_real) * 100))
#head(lucroliq_dependentes, n=19)
#View(lucroliq_dependentes)
#sum(lucroliq_dependentes$Percentual_Total)

# Observando os Patrimônios Líquidos das Estatais Federais dependentes - 
# referência 2020
str(dividendos_jcp_aportes_pragosto2021_dependentes)
View(dividendos_jcp_aportes_pragosto2021_dependentes)

# Filtrando as observações referentes ao ano de 2020

pl2020_pragosto2021_dependentes <- dividendos_jcp_aportes_pragosto2021_dependentes %>% 
  filter(Ano == "2020-12-31")
str(pl2020_pragosto2021_dependentes)
head(pl2020_pragosto2021_dependentes, n=19)
View(pl2020_pragosto2021_dependentes)
summary(pl2020_pragosto2021_dependentes)

pl2020_pragosto2021_dependentes <- select(pl2020_pragosto2021_dependentes, Ano, 
                                          Empresa, Sigla_Empresa, 
                                          Patrimonio_Liquido, PL_real)
pl2020_pragosto2021_dependentes
View(pl2020_pragosto2021_dependentes)

# Salvando a tabela pl2020_pragosto2021_dependentes no PC
#write.xlsx(pl2020_pragosto2021_dependentes, 
           #"pl2020_pragosto2021_dependentes.xlsx")

# Plotando o gráfico dos patrimônios líquidos das estatais dependentes em 2020
# Criando uma dummy para indicar se  patriônio líquido de cada estatal
# federal dependente foi positivo ou negativo

pl2020_pragosto2021_dependentes$positivo_negativo = 
  as.factor(ifelse(pl2020_pragosto2021_dependentes$Patrimonio_Liquido > 
                     0, yes = 1, no = 0))
head(pl2020_pragosto2021_dependentes, n = 19)
str(pl2020_pragosto2021_dependentes)

ggplot(pl2020_pragosto2021_dependentes, 
       aes(x = reorder(Sigla_Empresa, -Patrimonio_Liquido),
           y=Patrimonio_Liquido, 
           label=sprintf("%0.2f", round(Patrimonio_Liquido, 
                                        digits = 2)))) +
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  labs(title = "Patrimônio Líquido das Estatais Dependentes em 2020",
       subtitle = "(em R$ bilhões)",
       x = "Estatal Dependente",
       y = "R$ - valores nominais") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))

################################################################################

# Prosseguindo para as estatais não dependentes de recursos do Tesouro Nacional
                  
# Filtrando os dados referentes às estatais não dependentes

dividendos_jcp_aportes_pragosto2021_naodependentes = 
  filter(dividendos_jcp_aportes_pragosto2021, Classificacao %in% ("Não Dependente"))
str(dividendos_jcp_aportes_pragosto2021_naodependentes)
head(dividendos_jcp_aportes_pragosto2021_naodependentes)
View(dividendos_jcp_aportes_pragosto2021_naodependentes)
summary(dividendos_jcp_aportes_pragosto2021_naodependentes)

# Removendo as 10 primeiras observacoes do banco de dados - linhas referentes à 
# Telebras

dividendos_jcp_aportes_pragosto2021_naodependentes <- 
  slice(dividendos_jcp_aportes_pragosto2021_naodependentes, -c(1:10))
str(dividendos_jcp_aportes_pragosto2021_naodependentes)
head(dividendos_jcp_aportes_pragosto2021_naodependentes)
View(dividendos_jcp_aportes_pragosto2021_naodependentes)

# Salvando a tabela dividendos_jcp_aportes_pragosto2021_naodependentes no PC

#write.xlsx(dividendos_jcp_aportes_pragosto2021_naodependentes, 
#           "dividendos_jcp_aportes_pragosto2021_naodependentes.xlsx")

# Agrupando os valores referentes aos aportes de capital recebidos da União por 
# cada estatal não dependente, no período 2010 - 2020 (preços de agosto de 2021)

aportes_naodependentes_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_naodependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Aportes_real = sum(Aporte_real))
View(aportes_naodependentes_pragosto2021)
str(aportes_naodependentes_pragosto2021)
head(aportes_naodependentes_pragosto2021, n=27)
summary(aportes_naodependentes_pragosto2021)

# Somando os aportes (a preços de agosto/2021) realizadas entre 2010 e 2020

sum(aportes_naodependentes_pragosto2021$Soma_Aportes_real)

# Agrupando os valores referentes aos aportes de capital recebidos da União por 
# cada estatal dependente, no período 2010 - 2020 (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_naodependentes)
aportes_naodependentes_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_naodependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_aportes = sum(Aporte))
View(aportes_naodependentes_vlnominal)
str(aportes_naodependentes_vlnominal)
head(aportes_naodependentes_vlnominal, n=20)
summary(aportes_naodependentes_vlnominal)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# aportes reais e nominais recebidas da União, por cada estatal dependente 

aportes_naodependentes <- aportes_naodependentes_vlnominal %>% 
  inner_join(aportes_naodependentes_pragosto2021)
str(aportes_naodependentes)
head(aportes_naodependentes, n=20)
View(aportes_naodependentes)

aportes_naodependentes <- aportes_naodependentes %>% 
  mutate(Percentual_Total = 
           (aportes_naodependentes$Soma_Aportes_real /  sum(Soma_Aportes_real) *
              100))
head(aportes_naodependentes, n=20)
View(aportes_naodependentes)
aportes_naodependentes <- 
  aportes_naodependentes %>% arrange(desc(Soma_Aportes_real))
head(aportes_naodependentes, n=20)
View(aportes_naodependentes)

# Somando os aportes recebidos pelas estatas federais não dependentes

sum(aportes_naodependentes$Soma_aportes)
sum(aportes_naodependentes$Soma_Aportes_real)

# Salvando a tabela aportes_naodependentes no PC
#write.xlsx(aportes_naodependentes, "aportes_naodependentes.xlsx")

# Agrupando os valores referentes aos dividendos pagos à União por cada estatal 
# não dependente, no período 2010 - 2020 (preços de agosto de 2021)

dividendos_naodependentes_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_naodependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dividendos_real = sum(Dividendos_real))
View(dividendos_naodependentes_pragosto2021)
str(dividendos_naodependentes_pragosto2021)
head(dividendos_naodependentes_pragosto2021, n=19)
summary(dividendos_naodependentes_pragosto2021)

# Somando os dividendos (a preços de agosto/2021) realizadas entre 2010 e 2020

sum(dividendos_naodependentes_pragosto2021$Soma_Dividendos_real)

# Agrupando os valores referentes aos dividendos pagos à União por cada estatal
# não dependente, no período 2010 - 2020 (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_dependentes)
dividendos_naodependentes_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_naodependentes %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dividendos = sum(Dividendos))
View(dividendos_naodependentes_vlnominal)
str(dividendos_naodependentes_vlnominal)
head(dividendos_naodependentes_vlnominal, n=19)
summary(dividendos_naodependentes_vlnominal)

# Somando os dividendos (valores nominais) realizadas entre 2010 e 2020

sum(dividendos_naodependentes_vlnominal$Soma_Dividendos)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# dividendos reais e nominais pagos à União, por cada estatal dependente 

dividendos_naodependentes <- dividendos_naodependentes_vlnominal %>% 
  inner_join(dividendos_naodependentes_pragosto2021)
str(dividendos_naodependentes)
head(dividendos_naodependentes)
View(dividendos_naodependentes)

# Calculando os % dos dividendos (a preços de 08/2021) em relação ao total de 
# dividendos pagos à União pelas estatais dependentes

dividendos_naodependentes <- dividendos_naodependentes %>% 
  mutate(Percentual_Total = (dividendos_naodependentes$Soma_Dividendos_real / 
                               sum(Soma_Dividendos_real) * 100))
head(dividendos_naodependentes, n=19)
View(dividendos_naodependentes)

# Salvando a tabela dividendos_naodependentes no PC
#write.xlsx(dividendos_naodependentes, "dividendos_naodependentes.xlsx")

# Analisando o Relacionamento Financeiro entre a União e suas estatais não 
# dependentes

# Iniciando pelas Instituições Financeiras (IFs) públicas federais

# Filtrando os dados referentes as IFs públicas federais

str(dividendos_jcp_aportes_pragosto2021)
dividendos_jcp_aportes_pragosto2021_ifs = 
  filter(dividendos_jcp_aportes_pragosto2021, Sigla_Empresa %in% 
           c("BANCO DA AMAZÔNIA", "BB", "BNB", "BNDES", "CAIXA"))
str(dividendos_jcp_aportes_pragosto2021_ifs)
head(dividendos_jcp_aportes_pragosto2021_ifs)
View(dividendos_jcp_aportes_pragosto2021_ifs)
summary(dividendos_jcp_aportes_pragosto2021_ifs)

# Salvando a tabela dividendos_jcp_aportes_pragosto2021_ifs no PC
#write.xlsx(dividendos_jcp_aportes_pragosto2021_ifs, 
#           "dividendos_jcp_aportes_pragosto2021_ifs.xlsx")

# Agrupando os valores referentes aos aportes de capital,leia-se IHCD/IECP, 
# recebidos da União por cada Insttuição Pública federal (IF), no período 
# 2010 - 2020 (preços de agosto de 2021)

str(dividendos_jcp_aportes_pragosto2021_ifs)
View(dividendos_jcp_aportes_pragosto2021_ifs)
aportes_ihcd_ifs_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Aportes_real = sum(Aporte_real))
View(aportes_ihcd_ifs_pragosto2021)
str(aportes_ihcd_ifs_pragosto2021)
head(aportes_ihcd_ifs_pragosto2021)
summary(aportes_ihcd_ifs_pragosto2021)

# Somando os aportes (a preços de agosto/2021) realizados nas IFs entre 2010 e 
# 2020

sum(aportes_ihcd_ifs_pragosto2021$Soma_Aportes_real)

# write.xlsx(aportes_ihcd_ifs_pragosto2021, 
#           "aportes_ihcd_ifs_pragosto2021.xlsx")

# Agrupando os valores referentes aos aportes de capital,leia-se IHCD/IECP, 
# recebidos da União por cada Insttuição Pública federal (IF), no período 
# 2010 - 2020 (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_ifs)
aportes_ihcd_ifs_vlnominal = dividendos_jcp_aportes_pragosto2021_ifs %>% 
  group_by(Sigla_Empresa) %>% summarise(Soma_aportes = sum(Aporte))
View(aportes_ihcd_ifs_vlnominal)
str(aportes_ihcd_ifs_vlnominal)
head(aportes_ihcd_ifs_vlnominal)
summary(aportes_ihcd_ifs_vlnominal)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# aportes reais e nominais recebidas da União, por cada IF 

aportes_ifs <- aportes_ihcd_ifs_vlnominal %>% 
  inner_join(aportes_ihcd_ifs_pragosto2021)
str(aportes_ifs)
head(aportes_ifs)
View(aportes_ifs)

# Calculando o Percentual de Participação dos Aportes Real de cada IF em relação
# ao valor total de aportes

aportes_ifs <- aportes_ifs %>% mutate(Percentual_Total = 
           (aportes_ifs$Soma_Aportes_real /  sum(Soma_Aportes_real) * 100))
head(aportes_ifs)
View(aportes_ifs)

# Somando os aportes (valores nominais) realizadas nas IFs entre 2010 e 2020

sum(aportes_ifs$Soma_aportes)

# Salvando a tabela aportes_ifs no PC
# write.xlsx(aportes_ifs, "aportes_ifs.xlsx")

# Agrupando os valores referentes aos dividendos pagos à União por cada IF
# pública federal, no período 2010 - 2020 (preços de agosto de 2021)

dividendos_ifs_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dividendos_real = sum(Dividendos_real))
View(dividendos_ifs_pragosto2021)
str(dividendos_ifs_pragosto2021)
head(dividendos_ifs_pragosto2021)
summary(dividendos_ifs_pragosto2021)

# Somando os dividendos (a preços de agosto/2021) à União, por parte das IFs 
# públicas federais, entre 2010 e 2020

sum(dividendos_ifs_pragosto2021$Soma_Dividendos_real)

# Agrupando os valores referentes aos dividendos pagos à União por cada IF 
# pública federal, no período 2010 - 2020 (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_ifs)
dividendos_ifs_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dividendos = sum(Dividendos))
View(dividendos_ifs_vlnominal)
str(dividendos_ifs_vlnominal)
head(dividendos_ifs_vlnominal)
summary(dividendos_ifs_vlnominal)

# Somando os dividendos (em valores nominais) à União, por parte das IFs 
# públicas federais, entre 2010 e 2020

sum(dividendos_ifs_vlnominal$Soma_Dividendos)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# dividendos reais e nominais pagos à União, por cada IF pública federal 

dividendos_ifs <- dividendos_ifs_vlnominal %>% 
  inner_join(dividendos_ifs_pragosto2021)
str(dividendos_ifs)
head(dividendos_ifs)
View(dividendos_ifs)

# Calculando os % dos dividendos (a preços de 08/2021) em relação ao total de 
# dividendos pagos à União pelas IFs públicas federais

dividendos_ifs <- dividendos_ifs %>% 
  mutate(Percentual_Total = (dividendos_ifs$Soma_Dividendos_real / 
                               sum(Soma_Dividendos_real) * 100))
head(dividendos_ifs)
View(dividendos_ifs)

# Salvando a tabela dividendos_dependentes no PC
# write.xlsx(dividendos_ifs, "dividendos_ifs.xlsx")

# Agrupando os valores referentes à remuneração paga à União por cada IF 
# pública federal, referente aos contratos de IHCD/IECP, no período 2010 - 2020 
# (valores a preços de 08/2021)

str(dividendos_jcp_aportes_pragosto2021_ifs)
juros_ihcd_ifs_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Juros_Pagto_IHCD_real = sum(Juros_Pagto_IHCD_real))
View(juros_ihcd_ifs_pragosto2021)
str(juros_ihcd_ifs_pragosto2021)
head(juros_ihcd_ifs_pragosto2021)
summary(juros_ihcd_ifs_pragosto2021)

# Somando os dividendos (a preços de agosto/2021) à União, por parte das IFs 
# públicas federais, entre 2010 e 2020

sum(juros_ihcd_ifs_pragosto2021$Soma_Juros_Pagto_IHCD_real)

# Agrupando os valores referentes à remuneração paga à União por cada IF 
# pública federal, referente aos contratos de IHCD/IECP, no período 2010 - 2020 
# (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_ifs)
juros_ihcd_ifs_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Juros_Pagto_IHCD = sum(Juros_Pagto_IHCD))
View(juros_ihcd_ifs_vlnominal)
str(juros_ihcd_ifs_vlnominal)
head(juros_ihcd_ifs_vlnominal)
summary(juros_ihcd_ifs_vlnominal)

# Somando os dividendos (em valores nominais) à União, por parte das IFs 
# públicas federais, entre 2010 e 2020

sum(juros_ihcd_ifs_vlnominal$Soma_Juros_Pagto_IHCD)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com as
# remunerações de contratos de IHCD/IECP (valores reais e nominais) destinadas 
# à União, por cada IF pública federal 

juros_ihcd_ifs <- juros_ihcd_ifs_vlnominal %>% 
  inner_join(juros_ihcd_ifs_pragosto2021)
str(juros_ihcd_ifs)
head(juros_ihcd_ifs)
View(juros_ihcd_ifs)

# Calculando os % das remunerações (a preços de 08/2021) em relação ao total de 
# remunerações de contatos de IHCD/IECP destinadas à União pelas IFs públicas 
# federais, entre 2010 e 2020

juros_ihcd_ifs <- juros_ihcd_ifs %>% 
  mutate(Percentual_Total = (juros_ihcd_ifs$Soma_Juros_Pagto_IHCD_real / 
                               sum(Soma_Juros_Pagto_IHCD_real) * 100))
head(juros_ihcd_ifs)
View(juros_ihcd_ifs)

# Salvando a tabela juros_ihcd_ifs no PC
# write.xlsx(juros_ihcd_ifs, "juros_ihcd_ifs.xlsx")

# Agrupando os valores associados às devoluções de valores referentes aos 
# principais dos contratos de IHCD/IECP realizados com as IFs (a preços de 
# agosto de 2021)

str(dividendos_jcp_aportes_pragosto2021_ifs)
devolucao_ihcd_ifs_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Principal_Pgto_IHCD_real = sum(Principal_Pgto_IHCD_real))
View(devolucao_ihcd_ifs_pragosto2021)
str(devolucao_ihcd_ifs_pragosto2021)
head(devolucao_ihcd_ifs_pragosto2021)
summary(devolucao_ihcd_ifs_pragosto2021)

# Somando os dividendos (a preços de agosto/2021) à União, por parte das IFs 
# públicas federais, entre 2010 e 2020

sum(devolucao_ihcd_ifs_pragosto2021$Soma_Principal_Pgto_IHCD_real)

# Agrupando os valores referentes à remuneração paga à União por cada IF 
# pública federal, referente aos contratos de IHCD/IECP, no período 2010 - 2020 
# (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_ifs)
devolucao_ihcd_ifs_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Principal_Pgto_IHCD = sum(Principal_Pgto_IHCD))
View(devolucao_ihcd_ifs_vlnominal)
str(devolucao_ihcd_ifs_vlnominal)
head(devolucao_ihcd_ifs_vlnominal)
summary(devolucao_ihcd_ifs_vlnominal)

# Somando os dividendos (em valores nominais) à União, por parte das IFs 
# públicas federais, entre 2010 e 2020

sum(devolucao_ihcd_ifs_vlnominal$Soma_Principal_Pgto_IHCD)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com as
# remunerações de contratos de IHCD/IECP (valores reais e nominais) destinadas 
# à União, por cada IF pública federal 

devolucao_ihcd_ifs <- devolucao_ihcd_ifs_vlnominal %>% 
  inner_join(devolucao_ihcd_ifs_pragosto2021)
str(devolucao_ihcd_ifs)
head(devolucao_ihcd_ifs)
View(devolucao_ihcd_ifs)

# Calculando os % das remunerações (a preços de 08/2021) em relação ao total de 
# remunerações de contatos de IHCD/IECP destinadas à União pelas IFs públicas 
# federais, entre 2010 e 2020

devolucao_ihcd_ifs <- devolucao_ihcd_ifs %>% 
  mutate(Percentual_Total = (devolucao_ihcd_ifs$Soma_Principal_Pgto_IHCD_real / 
                               sum(Soma_Principal_Pgto_IHCD_real) * 100))
head(devolucao_ihcd_ifs)
View(devolucao_ihcd_ifs)

# Salvando a tabela dividendos_dependentes no PC
# write.xlsx(devolucao_ihcd_ifs, "devolucao_ihcd_ifs.xlsx")

# Agrupando os valores referentes à os dividendos, juros sobre IHCD/IECP e 
# devoluções de IHCD/IECP destinadas à União, subtraídos dos aportes por meio 
# dos IHCD/IECP recebidos da União por cada Instituição Financeira pública 
# federal, no período 2010 - 2020 (preços de agosto de 2021)

str(dividendos_jcp_aportes_pragosto2021_ifs)
dif_div_juros_ihcd_ifs_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_ifs %>% group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dif_div_ihcd_aportes_subv_real = 
              sum(Dif_div_ihcd_aportes_subv_real))
View(dif_div_juros_ihcd_ifs_pragosto2021)
str(dif_div_juros_ihcd_ifs_pragosto2021)
head(dif_div_juros_ihcd_ifs_pragosto2021)

# Salvando a tabela dif_div_juros_ihcd_ifs_pragosto2021 no PC
#write.xlsx(dif_div_juros_ihcd_ifs_pragosto2021, 
#           "dif_div_juros_ihcd_ifs_pragosto2021.xlsx")

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# dividendos, juros e devoluções de IHCD à União e aportes, via IHCD, 
# recebidos da União, por cada IF pública federal

dif_dividendos_ihcd_ifs <- dividendos_ifs_pragosto2021 %>% 
  inner_join(juros_ihcd_ifs_pragosto2021)
str(dif_dividendos_ihcd_ifs)
head(dif_dividendos_ihcd_ifs)
View(dif_dividendos_ihcd_ifs)

dif_dividendos_ihcd_ifs <- dif_dividendos_ihcd_ifs %>% 
  inner_join(devolucao_ihcd_ifs_pragosto2021)
str(dif_dividendos_ihcd_ifs)
head(dif_dividendos_ihcd_ifs)
View(dif_dividendos_ihcd_ifs)

dif_dividendos_ihcd_ifs <- dif_dividendos_ihcd_ifs %>% 
  inner_join(aportes_ihcd_ifs_pragosto2021)
str(dif_dividendos_ihcd_ifs)
head(dif_dividendos_ihcd_ifs)
View(dif_dividendos_ihcd_ifs)

# Somando os resultados do relacionamento financeiro das IFs públicas federais 
# com a União, entre 2010 e 2020

sum(dif_div_juros_ihcd_ifs_pragosto2021$Soma_Dif_div_ihcd_aportes_subv_real)

# Criando uma dummy para indicar se o relacionamento financeiro entre as IFs
# públicas federais e a União foi superavitária ou deficitária entre 2010 e 2020

dif_div_juros_ihcd_ifs_pragosto2021$positivo_negativo = 
  as.factor(ifelse(dif_div_juros_ihcd_ifs_pragosto2021$Soma_Dif_div_ihcd_aportes_subv_real > 
                     0, yes = 1, no = 0))
head(dif_div_juros_ihcd_ifs_pragosto2021)
str(dif_div_juros_ihcd_ifs_pragosto2021)
View(dif_div_juros_ihcd_ifs_pragosto2021)

# Plotando o gráfico de barras ordenado de forma decrescente

ggplot(dif_div_juros_ihcd_ifs_pragosto2021, 
       aes(x = reorder(Sigla_Empresa, -Soma_Dif_div_ihcd_aportes_subv_real),
           y=Soma_Dif_div_ihcd_aportes_subv_real, 
           label=sprintf("%0.2f", round(Soma_Dif_div_ihcd_aportes_subv_real, 
                                        digits = 2)))) +
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  labs(title = "Relacionamento Financeiro entre as IFs públicas federais e a União",
       subtitle = "(em R$ - a preços de agosto/2021)",
       x = "IF pública federal",
       y = "R$ a preços de 08/2021") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))

# Analisando o relacionamento financeiro entre as demais estatais federais 
# dependentes (sem as IFs públicas federais) e a União 

# Filtrando os dados referentes às demais estatais não dependentes

str(dividendos_jcp_aportes_pragosto2021)
str(dividendos_jcp_aportes_pragosto2021_naodependentes)
View(dividendos_jcp_aportes_pragosto2021_naodependentes)

dividendos_jcp_aportes_pragosto2021_demais_ndependentes = 
  filter(dividendos_jcp_aportes_pragosto2021_naodependentes, Sigla_Empresa %in% 
           c("ABGF", "CODESP", "CMB", "CEAGESP", "CEASAMINAS", "ELETROBRAS",
             "CODEBA", "CDC", "CODESA", "CDP", "CDRJ", "CODERN", "PPSA", "ECT",
             "HEMOBRÁS", "INFRAERO", "DATAPREV", "EMGEPRON", "EMGEA", "FINEP",
             "PETROBRAS", "SERPRO"))
head(dividendos_jcp_aportes_pragosto2021_demais_ndependentes)
str(dividendos_jcp_aportes_pragosto2021_demais_ndependentes)
View(dividendos_jcp_aportes_pragosto2021_demais_ndependentes)

# Agrupando os valores referentes aos aportes de capital recebidos da União por 
# cada uma das demais estatais federais não dependentes, no período 2010 - 2020 
# (valores a preços de agosto de 2021)

str(dividendos_jcp_aportes_pragosto2021_demais_ndependentes)
aportes_demais_ndep_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_demais_ndependentes %>% 
  group_by(Sigla_Empresa) %>% summarise(Soma_Aportes_real = sum(Aporte_real))
View(aportes_demais_ndep_pragosto2021)
str(aportes_demais_ndep_pragosto2021)
head(aportes_demais_ndep_pragosto2021)
summary(aportes_demais_ndep_pragosto2021)

# Somando os aportes (a preços de agosto/2021) realizadas entre 2010 e 2020

sum(aportes_demais_ndep_pragosto2021$Soma_Aportes_real)

# Agrupando os valores referentes aos aportes de capital recebidos da União por 
# cada uma das demais estatais federais não dependentes, no período 2010 - 2020 
# (valores nominais)

str(dividendos_jcp_aportes_pragosto2021_demais_ndependentes)
aportes_demais_ndep_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_demais_ndependentes %>%
  group_by(Sigla_Empresa) %>% summarise(Soma_aportes = sum(Aporte))
View(aportes_demais_ndep_vlnominal)
str(aportes_demais_ndep_vlnominal)
head(aportes_demais_ndep_vlnominal)
summary(aportes_demais_ndep_vlnominal)

# Somando os aportes (valores nominais) realizadas nas IFs entre 2010 e 2020

sum(aportes_demais_ndep_vlnominal$Soma_aportes)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# aportes reais e nominais recebidas da União, por cada uma das demais estatais
# federais não dependentes

aportes_demais_ndep <- aportes_demais_ndep_vlnominal %>% 
  inner_join(aportes_demais_ndep_pragosto2021)
str(aportes_demais_ndep)
head(aportes_demais_ndep)
View(aportes_demais_ndep)

# Calculando o Percentual de Participação dos Aportes Real de cada uma das 
# demais estatais federais não dependentes em relação ao valor total de aportes

aportes_demais_ndep <- aportes_demais_ndep %>% 
  mutate(Percentual_Total = (aportes_demais_ndep$Soma_Aportes_real / 
                               sum(Soma_Aportes_real) * 100))
head(aportes_demais_ndep)
View(aportes_demais_ndep)

# Salvando a tabela aportes_demais_ndep no PC
#write.xlsx(aportes_demais_ndep, "aportes_demais_ndep.xlsx")

# Agrupando os valores referentes aos dividendos/JCP pagos à União por cada uma 
# das demais estatais federais não dependentes, no período 2010 - 2020 (preços 
# de agosto de 2021)

dividendos_demais_ndep_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_demais_ndependentes %>% 
  group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dividendos_real = sum(Dividendos_real))
View(dividendos_demais_ndep_pragosto2021)
str(dividendos_demais_ndep_pragosto2021)
head(dividendos_demais_ndep_pragosto2021)
summary(dividendos_demais_ndep_pragosto2021)

# Somando os dividendos (a preços de agosto/2021) à União, por parte das IFs 
# públicas federais, entre 2010 e 2020

sum(dividendos_demais_ndep_pragosto2021$Soma_Dividendos_real)

# Agrupando os valores referentes aos dividendos/JCP pagos à União por cada uma 
# das demais estatais federais não dependentes, no período 2010 - 2020 
# (valores nominais)

str(dividendos_demais_ndep_pragosto2021)
dividendos_demais_ndep_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_demais_ndependentes %>% 
  group_by(Sigla_Empresa) %>% summarise(Soma_Dividendos = sum(Dividendos))
View(dividendos_demais_ndep_vlnominal)
str(dividendos_demais_ndep_vlnominal)
head(dividendos_demais_ndep_vlnominal)
summary(dividendos_demais_ndep_vlnominal)

# Somando os dividendos (em valores nominais) à União, por cada uma das demais 
# estatais federais não dependentes, no período 2010 - 2020 

sum(dividendos_demais_ndep_vlnominal$Soma_Dividendos)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# dividendos reais e nominais pagos à União, por cada uma das demais estatais 
# federais não dependentes, no período 2010 - 2020  

dividendos_demais_ndep <- dividendos_demais_ndep_vlnominal %>% 
  inner_join(dividendos_demais_ndep_pragosto2021)
str(dividendos_demais_ndep)
head(dividendos_demais_ndep)
View(dividendos_demais_ndep)

# Calculando os % dos dividendos (a preços de 08/2021) em relação ao total de 
# dividendos pagos à União por cada uma das demais estatais federais não 
# dependentes, no período 2010 - 2020

dividendos_demais_ndep <- dividendos_demais_ndep %>% 
  mutate(Percentual_Total = (dividendos_demais_ndep$Soma_Dividendos_real / 
                               sum(Soma_Dividendos_real) * 100))
head(dividendos_demais_ndep)
View(dividendos_demais_ndep)

# Salvando a tabela dividendos_demais_ndep no PC
#write.xlsx(dividendos_demais_ndep, "dividendos_demais_ndep.xlsx")

# Identificando se o relacionamento financeiro entre a União e as demais 
# estatais federais não dependentes (com exceção das IFs) foi superavitário ou 
# deficitário  

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# dividendos e aportes recebidos da União, por cada uma das demais estatais 
# não federais dependentes

dif_dividendos_aporte_ndep <- dividendos_demais_ndep_pragosto2021 %>% 
  inner_join(aportes_demais_ndep_pragosto2021)
str(dif_dividendos_aporte_ndep)
head(dif_dividendos_aporte_ndep)
View(dif_dividendos_aporte_ndep)

dif_dividendos_aporte_ndep <- dif_dividendos_aporte_ndep %>% 
  mutate(Resultado_Relac_Financ = (Soma_Dividendos_real - Soma_Aportes_real))
head(dif_dividendos_aporte_ndep)
View(dif_dividendos_aporte_ndep)

dif_dividendos_aporte_ndep <- dif_dividendos_aporte_ndep %>% 
  mutate(Percentual_Total = (dif_dividendos_aporte_ndep$Resultado_Relac_Financ / 
                                sum(Resultado_Relac_Financ) * 100))
head(dif_dividendos_aporte_ndep)
View(dif_dividendos_aporte_ndep)

sum(dif_dividendos_aporte_ndep$Resultado_Relac_Financ)
sum(dif_dividendos_aporte_ndep$Percentual_Total)

# Salvando a tabela dif_dividendos_aporte_ndep no PC
#write.xlsx(dif_dividendos_aporte_ndep, 
#           "dif_dividendos_aporte_ndep.xlsx")

# Calculando o saldo do relacionamento financeiro entre as demais estatais 
# federais não dependentes
str(dividendos_jcp_aportes_pragosto2021_demais_ndependentes)
dif_div_aportes_ndep_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_demais_ndependentes %>% 
  group_by(Sigla_Empresa) %>% 
  summarise(Soma_Dif_div_ihcd_aportes_subv_real = 
              sum(Dif_div_ihcd_aportes_subv_real))
View(dif_div_aportes_ndep_pragosto2021)
str(dif_div_aportes_ndep_pragosto2021)
head(dif_div_aportes_ndep_pragosto2021)

# Somando os resultados do relacionamento financeiro das IFs públicas federais 
# com a União, entre 2010 e 2020

sum(dif_div_aportes_ndep_pragosto2021$Soma_Dif_div_ihcd_aportes_subv_real)
str(dividendos_jcp_aportes_pragosto2021_naodependentes)

# Salvando a tabela dif_div_aportes_ndep_pragosto2021 no PC
#write.xlsx(dif_div_aportes_ndep_pragosto2021, 
#           "dif_div_aportes_ndep_pragosto2021.xlsx")

# Criando uma dummy para indicar se o relacionamento financeiro entre as IFs
# públicas federais e a União foi superavitária ou deficitária entre 2010 e 2020

dif_div_aportes_ndep_pragosto2021$positivo_negativo = 
  as.factor(ifelse(dif_div_aportes_ndep_pragosto2021$Soma_Dif_div_ihcd_aportes_subv_real > 
                     0, yes = 1, no = 0))
head(dif_div_aportes_ndep_pragosto2021)
str(dif_div_aportes_ndep_pragosto2021)
View(dif_div_aportes_ndep_pragosto2021)

# Plotando o gráfico de barras ordenado de forma decrescente

ggplot(dif_div_aportes_ndep_pragosto2021, 
       aes(x = reorder(Sigla_Empresa, -Soma_Dif_div_ihcd_aportes_subv_real),
           y=Soma_Dif_div_ihcd_aportes_subv_real, 
           label=sprintf("%0.2f", round(Soma_Dif_div_ihcd_aportes_subv_real, 
                                        digits = 2)))) +
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  labs(title = "Relacionamento Financeiro entre as Demais Estatais Federais 
       Não Dependentes e a União",
       subtitle = "(em R$ - a preços de agosto/2021)",
       x = "Estatal Federal",
       y = "R$ a preços de 08/2021") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))

# Deflacionando o valor pago pela Petrobras na Cessão Onerosa para exploração de 
# até 5 bilhões de barris de óleo equivalente em áreas não licitadas do Pré-Sal.
# Do total captado na OPA realizada em 2010, R$ 45,5 bilhões foram para o caixa 
# da empresa e R$ 74,8 bilhões destinaram-se ao pagamento da cessão onerosa dos 
# direitos de produção desses 5 bilhões de barris de óleo (PETROBRAS, 2010, 
# p. 34).

dev_opa_ces_oner_petro <- data.frame(Ano = as.Date("20101231", 
                                                   format = "%Y%m%d"),
                                     Valor_Ces_Onerosa = 74808000000)
       
dev_opa_ces_oner_petro
View(dev_opa_ces_oner_petro)
str(dev_opa_ces_oner_petro)

dev_opa_ces_oner_petro <- dev_opa_ces_oner_petro %>% 
  mutate(Valor_real = ipca(dev_opa_ces_oner_petro$Valor_Ces_Onerosa, 
                           dev_opa_ces_oner_petro$Ano, "08/2021"))

dev_opa_ces_oner_petro
View(dev_opa_ces_oner_petro)
str(dev_opa_ces_oner_petro)

# Descrevendo o Lucro Líquido das estatais federais não dependentes
# Agrupando os valores referentes aos lucros líquidos de cada estatal não 
# dependente, no período 2010 - 2020 (preços de agosto de 2021)

str(dividendos_jcp_aportes_pragosto2021_naodependentes)
lucroliq_ndep_pragosto2021 = 
  dividendos_jcp_aportes_pragosto2021_naodependentes %>% 
  group_by(Sigla_Empresa) %>% summarise(Soma_LL_real = sum(LL_real))
View(lucroliq_ndep_pragosto2021)
str(lucroliq_ndep_pragosto2021)
head(lucroliq_ndep_pragosto2021)
summary(lucroliq_ndep_pragosto2021)

# Somando os Lucro Líquidos das estatais federais não dependentes (a preços de 
# agosto/2021) realizadas entre 2010 e 2020

sum(lucroliq_ndep_pragosto2021$Soma_LL_real)

# Salvando a tabela lucroliq_ndep_pragosto2021 no PC
#write.xlsx(lucroliq_ndep_pragosto2021, 
#           "lucroliq_ndep_pragosto2021.xlsx")

# Agrupando os valores referentes ao lucro líquido de cada estatal não 
# dependente, no período 2010 - 2020 (valores nominais)

lucroliq_ndep_vlnominal = 
  dividendos_jcp_aportes_pragosto2021_naodependentes %>% 
  group_by(Sigla_Empresa) %>% summarise(Soma_Lucro_Liquido = sum(Lucro_Liquido))
View(lucroliq_ndep_vlnominal)
str(lucroliq_ndep_vlnominal)
head(lucroliq_ndep_vlnominal)
summary(lucroliq_ndep_vlnominal)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# lucros reais e nominais de cada estatal não dependente 

lucroliq_ndep <- lucroliq_ndep_vlnominal %>% 
  inner_join(lucroliq_ndep_pragosto2021)
str(lucroliq_ndep)
head(lucroliq_ndep)
View(lucroliq_ndep)

# Calculando os % dos lucros líquidos (a preços de 08/2021) em relação ao total 
# de lucros líquidos reais das estatais não dependentes
lucroliq_ndep <- lucroliq_ndep %>% 
  mutate(Percentual_Total = (lucroliq_ndep$Soma_LL_real / 
                               sum(Soma_LL_real) * 100))
head(lucroliq_ndep)
View(lucroliq_ndep)
sum(lucroliq_ndep$Percentual_Total)
sum(lucroliq_ndep$Soma_LL_real)

# Salvando a tabela lucroliq_ndep no PC
#write.xlsx(lucroliq_ndep, "lucroliq_ndep.xlsx")

# Plotando o gráfico de barras ordenado de forma decrescente - LL a preços de 
# agosto/2021 

# Criando uma dummy para indicar se o relacionamento financeiro entre as IFs
# públicas federais e a União foi superavitária ou deficitária entre 2010 e 2020

lucroliq_ndep$positivo_negativo = 
  as.factor(ifelse(lucroliq_ndep$Soma_LL_real > 
                     0, yes = 1, no = 0))
head(lucroliq_ndep)
str(lucroliq_ndep)
View(lucroliq_ndep)

ggplot(lucroliq_ndep, 
       aes(x = reorder(Sigla_Empresa, -Soma_LL_real),
           y=Soma_LL_real, 
           label=sprintf("%0.2f", round(Soma_LL_real, 
                                        digits = 2)))) +
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  labs(title = "Resultado Econômico Acumulado das Estatais Federais Não 
       Dependentes",
       subtitle = "(em R$ - a preços de agosto/2021)",
       x = "Estatal Federal",
       y = "R$ a preços de 08/2021") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))

# Observando os Patrimônios Líquidos das Estatais Federais não dependentes - 
# referência 2020
str(dividendos_jcp_aportes_pragosto2021_naodependentes)
View(dividendos_jcp_aportes_pragosto2021_naodependentes)

# Filtrando as observações referentes ao ano de 2020

pl2020_pragosto2021_ndep <- dividendos_jcp_aportes_pragosto2021_naodependentes %>% 
  filter(Ano == "2020-12-31")
str(pl2020_pragosto2021_ndep)
head(pl2020_pragosto2021_ndep)
View(pl2020_pragosto2021_ndep)
summary(pl2020_pragosto2021_ndep)

pl2020_pragosto2021_ndep <- select(pl2020_pragosto2021_ndep, Ano, 
                                          Empresa, Sigla_Empresa, 
                                          Patrimonio_Liquido, PL_real)
pl2020_pragosto2021_ndep
View(pl2020_pragosto2021_ndep)

# Salvando a tabela pl2020_pragosto2021_ndep no PC
# write.xlsx(pl2020_pragosto2021_ndep, 
#           "pl2020_pragosto2021_ndep.xlsx")

# Plotando o gráfico dos patrimônios líquidos das estatais não dependentes em 
# 2020
# Criando uma dummy para indicar se  patriônio líquido de cada estatal
# federal não dependente foi positivo ou negativo

pl2020_pragosto2021_ndep$positivo_negativo = 
  as.factor(ifelse(pl2020_pragosto2021_ndep$Patrimonio_Liquido > 
                     0, yes = 1, no = 0))
head(pl2020_pragosto2021_ndep, n = 19)
str(pl2020_pragosto2021_ndep)

ggplot(pl2020_pragosto2021_ndep, 
       aes(x = reorder(Sigla_Empresa, -Patrimonio_Liquido),
           y=Patrimonio_Liquido, 
           label=sprintf("%0.2f", round(Patrimonio_Liquido, 
                                        digits = 2)))) +
  geom_col(aes(fill = positivo_negativo)) +
  geom_hline(yintercept = 0) +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
  labs(title = "Patrimônio Líquido das Estatais Não Dependentes em 2020",
       subtitle = "(em R$ bilhões)",
       x = "Estatal Dependente",
       y = "R$ - valores nominais") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.9, hjust= 1))


# Valor de Mercado: BB, Petrobras e Eletrobras

# Carregando dados - posição 23.11.2021

valor_mercado_k_aberto <- 
  read_xlsx("valor_mercado_k_aberto.xlsx")
str(valor_mercado_k_aberto)
head(valor_mercado_k_aberto)
View(valor_mercado_k_aberto)

# calculando o valor de mercado do BB, Petrobras e Eletrobras
valor_mercado_k_aberto <- valor_mercado_k_aberto %>% 
  mutate(Valor_mercado = (valor_mercado_k_aberto$Cotação_23112021 * 
                            valor_mercado_k_aberto$Qtde_acoes_totais)) 
              
str(valor_mercado_k_aberto)
head(valor_mercado_k_aberto)
View(valor_mercado_k_aberto)

# calculando o valor da União em caso de alienação do BB, Petrobras e Eletrobras

valor_mercado_k_aberto <- valor_mercado_k_aberto %>% 
  mutate(Valor_Uniao = (valor_mercado_k_aberto$Valor_mercado * 
                          (valor_mercado_k_aberto$Percentual_Uniao/100)))

str(valor_mercado_k_aberto)
head(valor_mercado_k_aberto)
View(valor_mercado_k_aberto)

# Agrupando os valores referentes aos valores de mercado do BB, Petrobras e   
# Eletrobras

valor_mercado_k_aberto_vm = 
  valor_mercado_k_aberto %>% 
  group_by(Estatal) %>% summarise(Valor_mercado= sum(Valor_mercado))
View(valor_mercado_k_aberto_vm)
str(valor_mercado_k_aberto_vm)
head(valor_mercado_k_aberto_vm)
summary(valor_mercado_k_aberto_vm)

# Agrupando os valores referentes aos valores de %T da União no BB, Petrobras e   
# Eletrobras

valor_mercado_k_aberto_vu = 
  valor_mercado_k_aberto %>% 
  group_by(Estatal) %>% summarise(Valor_Uniao = sum(Valor_Uniao))
View(valor_mercado_k_aberto_vu)
str(valor_mercado_k_aberto_vu)
head(valor_mercado_k_aberto_vu)
summary(valor_mercado_k_aberto_vu)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com os 
# valores de merccado e de paraticipação da União nos capitais sociais das
# Estatais

valor_mercado_k_aberto_group <- valor_mercado_k_aberto_vm %>% 
  inner_join(valor_mercado_k_aberto_vu)
str(valor_mercado_k_aberto_group)
head(valor_mercado_k_aberto_group)
View(valor_mercado_k_aberto_group)

# Salvando a tabela valor_mercado_k_aberto_group no PC
#write.xlsx(valor_mercado_k_aberto_group, 
#           "valor_mercado_k_aberto_group.xlsx")

# Avaliando os aportes e subvenções considerando os Governos de criação das 
# estatais

# Selecionando as variáveis necessárias para essa avaliação. Base de dados: 
# dividendos_jcp_aportes

dividendos_jcp_aportes_pragosto2021
str(dividendos_jcp_aportes_pragosto2021)
gastos_criadas_por_presidente <- dividendos_jcp_aportes_pragosto2021 %>% 
  select (Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
          Classificacao, Setor_Atuacao, Aporte_real, Subvencoes_real)
str(gastos_criadas_por_presidente)  
gastos_criadas_por_presidente
View(gastos_criadas_por_presidente)

# Criando as variáveis necessárias para essa avaliação
# Identificando as atuais estatais federais que foram criadas nos Governos de 
# cada Presidente/Governante

# Criando uma variável para demonstrar a soma com despesas para aporte de 
# capital e subvenções

gastos_eef_setores <- gastos_criadas_por_presidente %>%
  mutate(Aportes_Subvencoes_real = Aporte_real + Subvencoes_real)
          
gastos_eef_setores
View(gastos_eef_setores)

gastos_eef_setores$Setor_Atuacao[gastos_eef_setores$Setor_Atuacao == 
                                   "Petróleo, Gás e Energia Elétrica"] <- 
  "Petróleo, Gás e Energia"

View(gastos_eef_setores)
head(gastos_eef_setores)

# Agrupando os valores referentes aos Aportes e Subvenções a preços de 08/2021   
# por ano e por setor de atuação

gastos_eef_setores = 
  gastos_eef_setores %>% group_by(Ano, Setor_Atuacao) %>% 
  summarise(Aportes_Subvencoes_real_ano = sum(Aportes_Subvencoes_real))
View(gastos_eef_setores)
str(gastos_eef_setores)
head(gastos_eef_setores)
summary(gastos_eef_setores)

# Salvando a tabela gastos_eef_setores no PC
#write.csv(gastos_eef_setores, 
#           "gastos_eef_setores.csv")


# Agrupando os valores referentes aos Aportes e Subvenções a preços de 08/2021   
# por Governo de criação

eef_criadas_por_pres <- dividendos_jcp_aportes %>% 
  select (Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
          Classificacao)
str(eef_criadas_por_pres)  
eef_criadas_por_pres
View(eef_criadas_por_pres)

eef_criadas_por_pres <- eef_criadas_por_pres %>% mutate(Criada_no_Governo = 
           case_when(Sigla_Empresa == "Amazul" ~ "Dilma", Sigla_Empresa == 
                       "CEITEC" ~ "Lula", Sigla_Empresa == "CBTU" ~ 
                       "João Figueiredo", Sigla_Empresa == "CODEVASF" ~ 
                       "Ernesto Geisel", Sigla_Empresa == "CPRM" ~ 
                       "Costa e Silva", Sigla_Empresa == "CONAB" ~ 
                       "Fernando Collor", Sigla_Empresa == "EBC" ~ "Lula", 
                     Sigla_Empresa == "EMBRAPA" ~ "Emílio Médici", Sigla_Empresa 
                     == "EBSERH" ~ "Dilma", Sigla_Empresa == "EPE" ~ "Lula",
                     Sigla_Empresa == "EPL" ~ "Dilma", Sigla_Empresa == 
                       "TRENSURB" ~ "João Figueiredo", Sigla_Empresa == "HCPA" ~
                       "Emílio Médici", Sigla_Empresa == "CONCEIÇÃO" ~ 
                       "Juscelino Kubitschek", Sigla_Empresa == "IMBEL" ~ 
                       "Ernesto Geisel", Sigla_Empresa == "INB" ~ "José Sarney",
                     Sigla_Empresa == "NUCLEP" ~ "Ernesto Geisel", Sigla_Empresa 
                     == "TELEBRAS" ~ "Emílio Médici", Sigla_Empresa == "VALEC" ~ 
                       "José Sarney",	Sigla_Empresa == "ABGF" ~ "Dilma", 
                     Sigla_Empresa == "CODESP" ~ "João Figueiredo", 
                     Sigla_Empresa == "BANCO DA AMAZÔNIA" ~ "Getúlio Vargas",
                     Sigla_Empresa == "BB" ~ "D. João", Sigla_Empresa == "BNB" ~ 
                       "Getúlio Vargas", Sigla_Empresa == "BNDES" ~ 
                       "Getúlio Vargas", Sigla_Empresa == "CAIXA" ~ "D. Pedro II", 
                     Sigla_Empresa == "CMB" ~ "Emílio Médici", Sigla_Empresa == 
                       "CEAGESP" ~ "Costa e Silva", Sigla_Empresa == 
                       "CEASAMINAS" ~ "Emílio Médici", Sigla_Empresa == 
                       "ELETROBRAS" ~ "Jânio da Silva Quadros", Sigla_Empresa == 
                       "CODEBA" ~ "Ernesto Geisel", Sigla_Empresa == "CDC" ~ 
                       "Castello Branco", Sigla_Empresa == "CODESA" ~ 
                       "João Figueiredo", Sigla_Empresa == "CDP" ~ 
                       "Castello Branco", Sigla_Empresa == "CDRJ" ~ 
                       "Castello Branco",  Sigla_Empresa == "CODERN" ~ 
                       "Costa e Silva", Sigla_Empresa == "PPSA" ~ "Dilma", 
                     Sigla_Empresa == "ECT" ~ "Costa e Silva", Sigla_Empresa == 
                       "HEMOBRÁS" ~ "Lula", Sigla_Empresa == "INFRAERO" ~ 
                       "Emílio Médici", Sigla_Empresa == "DATAPREV" ~ 
                       "Ernesto Geisel", Sigla_Empresa == "EMGEPRON" ~ 
                       "João Figueiredo", Sigla_Empresa == "EMGEA" ~ 
                       "Fernando Henrique Cardoso", Sigla_Empresa == "FINEP" ~ 
                       "Costa e Silva", Sigla_Empresa == "PETROBRAS" ~ 
                       "Getúlio Vargas", Sigla_Empresa == "SERPRO" ~ 
                       "Castello Branco"))
eef_criadas_por_pres
View(eef_criadas_por_pres)

eef_criadas_por_pres_resumida <- eef_criadas_por_pres %>% 
  select (Ano, Empresa, Classificacao, Criada_no_Governo)
str(eef_criadas_por_pres_resumida)  
eef_criadas_por_pres_resumida
View(eef_criadas_por_pres_resumida)

# Realizando o inner join (cruzamento) dos agrupamentos realizados com as 
# as tabelas eef_criadas_por_presidente_resumida e gastos_criadas_por_presidente
gastos_criadas_por_presidente
gastos_criadas_por_presidente <- gastos_criadas_por_presidente %>%
  mutate(Aportes_Subvencoes_real = Aporte_real + Subvencoes_real)
View(gastos_criadas_por_presidente)

str(gastos_criadas_por_presidente)
gastos_criadas_por_presidente = gastos_criadas_por_presidente %>% 
  mutate(Ano = as.Date(Ano))

str(dividendos_jcp_aportes)
gastos_eef_gov_criacao <- gastos_criadas_por_presidente %>%
  mutate(Governo_Aporte = 
           case_when(Ano == "2010-12-31" ~ "Lula", 
                     Ano == "2011-12-31" ~ "Dilma", 
                     Ano == "2012-12-31" ~ "Dilma", 
                     Ano == "2013-12-31" ~ "Dilma",
                     Ano == "2014-12-31" ~ "Dilma",
                     Ano == "2015-12-31" ~ "Dilma", 
                     Ano == "2016-12-31" ~ "Michel Temer", 
                     Ano == "2017-12-31" ~ "Michel Temer", 
                     Ano == "2018-12-31" ~ "Michel Temer", 
                     Ano == "2019-12-31" ~ "Jair Bolsonaro",
                     Ano == "2020-12-31" ~ "Jair Bolsonaro"))
gastos_eef_gov_criacao
View(gastos_eef_gov_criacao)

View(eef_criadas_por_pres_resumida)

gastos_eef_gov_criacao <- gastos_eef_gov_criacao %>% 
  inner_join(eef_criadas_por_pres_resumida)
str(gastos_eef_gov_criacao)
head(gastos_eef_gov_criacao)
View(gastos_eef_gov_criacao)

# Agrupando os dados por Ano e por Governos de criação
gastos_eef_gov_criacao_group = gastos_eef_gov_criacao %>% 
  group_by(Criada_no_Governo, Governo_Aporte) %>% 
  summarise(Aportes_Subvencoes_Governo_real = sum(Aportes_Subvencoes_real))
View(gastos_eef_gov_criacao_group)
str(ggastos_eef_gov_criacao_group)
head(gastos_eef_gov_criacao_group)
summary(gastos_eef_gov_criacao_group)

# Salvando a tabela gastos_eef_setores no PC
#write.xlsx2(gastos_eef_gov_criacao_group, 
#          "gastos_eef_gov_criacao_group.xlsx")


# Parei aqui 25/11/2021 - ajustar o script



# Avaliando os aportes realizados por cada Governo em relação aos setores de 
# atuações das empresas estatais federais

# Selecionando as variáveis necessárias para essa avaliação. Base de dados: 
# dividendos_jcp_aportes

dividendos_jcp_aportes_pragosto2021
str(dividendos_jcp_aportes_pragosto2021)
gastos_criadas_por_presidente_ap_real <- dividendos_jcp_aportes_pragosto2021 %>% 
  select (Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
          Classificacao, Setor_Atuacao, Aporte_real)
str(gastos_criadas_por_presidente_ap_real)  
gastos_criadas_por_presidente_ap_real
View(gastos_criadas_por_presidente_ap_real)
gastos_criadas_por_presidente_ap_real$Setor_Atuacao[gastos_criadas_por_presidente_ap_real$Setor_Atuacao == 
                                   "Petróleo, Gás e Energia Elétrica"] <- "Petróleo, Gás e Energia"

View(gastos_criadas_por_presidente_ap_real)
head(gastos_criadas_por_presidente_ap_real)

# Agrupando os valores referentes aos Aportes a preços de 08/2021 por ano e por 
# setor de atuação

gastos_criadas_por_presidente_ap_real_group = 
  gastos_criadas_por_presidente_ap_real %>% group_by(Ano, Setor_Atuacao) %>% 
  summarise(Aportes_real = sum(Aporte_real))
View(gastos_criadas_por_presidente_ap_real_group)
str(gastos_criadas_por_presidente_ap_real_group)
head(gastos_criadas_por_presidente_ap_real_group)
summary(gastos_criadas_por_presidente_ap_real_group)

# Salvando a tabela gastos_criadas_por_presidente_ap_real_group no PC
#write.csv(gastos_criadas_por_presidente_ap_real_group, 
#           "gastos_criadas_por_presidente_ap_real_group.csv")


# Agrupando os valores referentes aos Aportes a preços de 08/2021 por Governo de 
# criação

# Agrupando os dados por Ano e por Governos de criação

View(gastos_eef_gov_criacao)
gastos_eef_gov_criacao
gastos_eef_gov_criacao_ap_group = gastos_eef_gov_criacao %>% 
  group_by(Criada_no_Governo, Governo_Aporte) %>% 
  summarise(Soma_Aporte_real = sum(Aporte_real))
View(gastos_eef_gov_criacao_ap_group)
str(gastos_eef_gov_criacao_ap_group)
head(gastos_eef_gov_criacao_ap_group)
summary(gastos_eef_gov_criacao_ap_group)


# Analisando os dados à luz do tamanho das estatais (maiores vs. menores)


# Agrupando os aportes pelas estatais maiores (BB, BNDES, CEF, ELETROBRÁS e 
# PETROBRÁs) e pelas outras (menores_estatais)

# Criando a variável "Tamanho_estatal"
dividendos_jcp_aportes_pragosto2021 <- 
  mutate(dividendos_jcp_aportes_pragosto2021, Tamanho_estatal = 
           if_else(Sigla_Empresa == "BB", "maiores_estatais", "menores_estatais"))
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

# Colocando a coluna "Tamanho_estatal" ao lado da coluna "Setor_Atuacao"

dividendos_jcp_aportes_pragosto2021 <- dividendos_jcp_aportes_pragosto2021 %>% 
  select(Ano, Empresa, Sede, Sigla_Empresa, Data_Criacao, Data_Constituicao, 
         Classificacao, Setor_Atuacao, Tamanho_estatal, everything())
str(dividendos_jcp_aportes_pragosto2021)
View(dividendos_jcp_aportes_pragosto2021)

dividendos_jcp_aportes_pragosto2021$Tamanho_estatal[dividendos_jcp_aportes_pragosto2021$Sigla_Empresa == 
                                                      'BNDES'] <- "maiores_estatais"
dividendos_jcp_aportes_pragosto2021$Tamanho_estatal[dividendos_jcp_aportes_pragosto2021$Sigla_Empresa == 
                                                      'CAIXA'] <- "maiores_estatais"
dividendos_jcp_aportes_pragosto2021$Tamanho_estatal[dividendos_jcp_aportes_pragosto2021$Sigla_Empresa == 
                                                      'ELETROBRAS'] <- "maiores_estatais"
dividendos_jcp_aportes_pragosto2021$Tamanho_estatal[dividendos_jcp_aportes_pragosto2021$Sigla_Empresa == 
                                                      'PETROBRAS'] <- "maiores_estatais"

View(dividendos_jcp_aportes_pragosto2021)
str(dividendos_jcp_aportes_pragosto2021)

partic_tamanho_estatais_aportes <- group_by(dividendos_jcp_aportes_pragosto2021, 
                                            Ano, Tamanho_estatal) %>% 
  summarise(Soma_Aporte_real=sum(Aporte_real))

View(partic_tamanho_estatais_aportes)

ggplot(partic_tamanho_estatais_aportes, aes(x = Ano, 
                                            y = Soma_Aporte_real, 
                                            fill = Tamanho_estatal)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = Soma_Aporte_real, accuracy = 0.1), vjust = -1) +
  labs(title = "Aportes de K: BB, BNDES, CEF, Eletrobras e Petrobras e demais 
       estatais entre 2010 a 2020 (em R$)", x="Ano", 
       y="Total de dividendos e JCP (em R$)") + 
  theme_classic(base_size = 10)


# Plotando o Gráfico do % de Aportes de K pelas estatais maiores (BB, BNDES, 
# CEF, Eletrobras e Petrobras) e pelas demais estatais à União

partic_tamanho_estatais_aportes_perc = partic_tamanho_estatais_aportes %>% 
  mutate(Perc_part_ap = Soma_Aporte_real/sum(Soma_Aporte_real, Ano))
str(partic_tamanho_estatais_aportes_perc)
View(partic_tamanho_estatais_aportes_perc)

ggplot(partic_tamanho_estatais_aportes_perc, aes(x = Ano, y = Soma_Aporte_real, 
                                                 fill = Tamanho_estatal)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(Perc_part_ap, accuracy = 0.1)), vjust = -1, ) +
  labs(title = "Dividendos e JCP BB, BNDES, CEF, Eletrobras e Petrobras e demais 
       estatais entre 2015 a 2019 (em R$)", x="Ano", y="Percentual de 
       participação nos div. e JCP") + 
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom')


# Salvando a tabela partic_tamanho_estatais_aportes_perc no PC
#write.csv2(partic_tamanho_estatais_aportes_perc, 
#          "partic_tamanho_estatais_aportes_perc.csv")


# Agrupando as Subvenções por ano

subvencoes_group_ano <- group_by(dividendos_jcp_aportes_pragosto2021, Ano) %>% 
  summarise(Soma_Subv_real = sum(Subvencoes_real))

View(subvencoes_group_ano)

ggplot(subvencoes_group_ano, aes(x = Ano, y = Soma_Subv_real, 
                                            fill = Soma_Subv_real)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = Soma_Subv_real), vjust = -1) +
  labs(title = "Subvenções Anuais - Estatais Dependentes - entre 2010 a 2020 
       (em R$)", x="Ano", 
       y="Total de Subvenções (em R$)") + 
  theme_classic(base_size = 10)

# Salvando a tabela subvencoes_group_ano no PC
#write.csv2(subvencoes_group_ano, 
#           "subvencoes_group_ano.csv")


# Agrupando os dividendos/jcp pagos à União pelas estatais maiores (BB, BNDES, 
# CEF, ELETROBRÁS e PETROBRÁs) e pelas outras (menores_estatais)

str(dividendos_jcp_aportes_pragosto2021)
partic_tamanho_estatais_div_jcp <- group_by(dividendos_jcp_aportes_pragosto2021, 
                                            Ano, Tamanho_estatal) %>% 
  summarise(Soma_Dividendos_real = sum(Dividendos_real))

View(partic_tamanho_estatais_div_jcp)

ggplot(partic_tamanho_estatais_div_jcp, aes(x = Ano, 
                                            y = Soma_Dividendos_real, 
                                            fill = Tamanho_estatal)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = Soma_Aporte_real, accuracy = 0.1), vjust = -1) +
  labs(title = "Dividendos/JCP: BB, BNDES, CEF, Eletrobras e Petrobras e demais 
       estatais entre 2010 a 2020 (em R$)", x="Ano", 
       y="Total de dividendos e JCP (em R$)") + 
  theme_classic(base_size = 10)


# Plotando o Gráfico do % de Aportes de K pelas estatais maiores (BB, BNDES, 
# CEF, Eletrobras e Petrobras) e pelas demais estatais à União

partic_tamanho_estatais_div_jcp_perc = partic_tamanho_estatais_div_jcp %>% 
  mutate(Perc_part_div_jcp = Soma_Dividendos_real/sum(Soma_Dividendos_real, Ano))
str(partic_tamanho_estatais_div_jcp_perc)
View(partic_tamanho_estatais_div_jcp_perc)

ggplot(partic_tamanho_estatais_div_jcp_perc, aes(x = Ano, 
                                                 y = Soma_Dividendos_real, 
                                                 fill = Tamanho_estatal)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(Perc_part_div_jcp, accuracy = 0.1)), vjust = -1, ) +
  labs(title = "Dividendos e JCP BB, BNDES, CEF, Eletrobras e Petrobras e demais 
       estatais entre 2015 a 2019 (em R$)", x="Ano", y="Percentual de 
       participação nos div. e JCP") + 
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom')


# Salvando a tabela partic_tamanho_estatais_div_jcp_perc no PC
#write.csv2(partic_tamanho_estatais_div_jcp_perc, 
     #     "partic_tamanho_estatais_div_jcp_perc.csv")


# Agrupando as juros pagos pelas IFs referentes aos contratos de IHCD/IECP

str(dividendos_jcp_aportes_pragosto2021)
juros_ihcd_group_ano <- group_by(dividendos_jcp_aportes_pragosto2021, Ano) %>% 
  summarise(Soma_Juros_Pagto_IHCD_real = sum(Juros_Pagto_IHCD_real))

View(juros_ihcd_group_ano)

ggplot(juros_ihcd_group_ano, aes(x = Ano, y = Soma_Juros_Pagto_IHCD_real, 
                                 fill = Soma_Juros_Pagto_IHCD_real)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = Soma_Subv_real), vjust = -1) +
  labs(title = "Juros s/ Contratos IHCD/IECP - IFs públicas federais - 
  entre 2010 a 2020 (em R$)", x="Ano", 
       y="Total de Juros Pagos à União (em R$)") + 
  theme_classic(base_size = 10)

# Salvando a tabela juros_ihcd_group_ano no PC
#write.csv2(juros_ihcd_group_ano, 
#           "juros_ihcd_group_ano.csv")


# Agrupando os Resultados do Relacionamento Financeiro pelas estatais maiores 
# (BB, BNDES, CEF, ELETROBRÁS e PETROBRÁs) e pelas outras (menores_estatais)

str(dividendos_jcp_aportes_pragosto2021)
partic_tamanho_estatais_relac_financ <- 
  group_by(dividendos_jcp_aportes_pragosto2021, 
                                            Ano, Tamanho_estatal) %>% 
  summarise(Soma_Dif_div_ihcd_aportes_subv_real = 
              sum(Dif_div_ihcd_aportes_subv_real))

View(partic_tamanho_estatais_relac_financ)

ggplot(partic_tamanho_estatais_relac_financ, 
       aes(x = Ano, y = Soma_Dif_div_ihcd_aportes_subv_real, 
           fill = Tamanho_estatal)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = Soma_Aporte_real, accuracy = 0.1), vjust = -1) +
  labs(title = "Relacionamento Financeiro: BB, BNDES, CEF, Eletrobras e 
  Petrobras e demais estatais entre 2010 a 2020 (em R$)", x="Ano", 
       y="Resultado do Relacionamento Financeiro (em R$)") + 
  theme_classic(base_size = 10)


# Plotando o Gráfico do % de Aportes de K pelas estatais maiores (BB, BNDES, 
# CEF, Eletrobras e Petrobras) e pelas demais estatais à União

partic_tamanho_estatais_relac_financ_perc = 
  partic_tamanho_estatais_relac_financ %>% 
  mutate(Perc_part_relac_financ = 
           Soma_Dif_div_ihcd_aportes_subv_real/
           sum(Soma_Dif_div_ihcd_aportes_subv_real, Ano))

str(partic_tamanho_estatais_relac_financ_perc)
View(partic_tamanho_estatais_relac_financ_perc)

ggplot(partic_tamanho_estatais_relac_financ_perc, 
       aes(x = Ano, y = Soma_Dif_div_ihcd_aportes_subv_real, 
           fill = Tamanho_estatal)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(Perc_part_relac_financ, 
                                accuracy = 0.1)), vjust = -1, ) +
  labs(title = "Relacionamento Financeiro BB, BNDES, CEF, Eletrobras e Petrobras 
  e demais estatais entre 2015 a 2019 (em R$)", x="Ano", 
       y="Resultado do Relacionamento Financeiro (em R$)") + 
  theme_classic(base_size = 10) +
  theme(legend.position = 'bottom')


# Salvando a tabela partic_tamanho_estatais_relac_financ_perc no PC
#write.csv2(partic_tamanho_estatais_relac_financ_perc, 
#     "partic_tamanho_estatais_relac_financ_perc.csv")

