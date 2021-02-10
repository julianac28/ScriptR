#######Vers�o 26-02 com parametriza��o da RCL
#######Vers�o 28-02 com ajustes nas receitas da Sa�de. Depois tem que corrigir o num_ano, e o nome da receita base
###Vers�o01-03: Com receita or�ada
#######Vers�o 12-03: Acrescentei 2020 a 2022
########Vers�o 07-03: Acrescentei UGs (Para poder identificar os poderes)
#Vers�o 25-03: Corre��o de receita patrimonial
###Vers�o 15-03 Acrescentei Receita de capital
###Vers�o 09-04: Valores constantes 2018
#Vers�o 12-04: Acrescentado o campo com o Tipo da Fonte de Recursos (d�gitos 3 e 4)
#Vers�o 26/04: Escrever tabela no Boa Vista
#Vers�o 12/02/2020: Retirei ITBI
#Vers�o 13/02/2020: Ajustei as receitas patrimoniais n�o financeiras para que elas levem em considera��o os primeiros meses de 2020
#Vers�o 02/03/2020: Ajustei a FR 124
#Vers�o 05/03/2020: Ajustei o link para o novo ambiente do BoaVista
#Vers�o 17/03/2020: Relat�rio FOCUS 17/03

#Vers�o 19/03/2020: Tentando estimar impactos do COVID-19

#Vers�es 2021: Deixar a parte de receita de aliena��es com input manual

#Vers�o 30-03: Tentando modelar o impacto do Covid-19 sobre o emprego e receita da FR 120, como consequ�ncia. Mas isso s� ser� incorporado na LOA 2021 e n�o na LDO

library(RODBC)
library(bizdays)
library(sqldf)

con <- odbcConnect("BoaVista",uid="bsinzato-sefaz",pwd="crisis",believeNRows=FALSE)


##################################
#PAR�METROS MACROECON�MICOS
#####################################

#https://www.valor.com.br/brasil/6135743/mercado-ve-inflacao-mais-baixa-em-2019-e-avanco-de-248-para-o-pib
#https://www1.folha.uol.com.br/mercado/2019/01/industria-automotiva-projeta-alta-de-114-nas-vendas-em-2019.shtml
#http://www.revistaportuaria.com.br/noticia/19257

#http://www.automotivebusiness.com.br/noticia/30400/para-anfavea-vendas-serao-maiores-em-2020
#https://valorinveste.globo.com/objetivo/de-olho-no-mercado/noticia/2020/01/07/anfavea-preve-crescimento-de-quase-10percent-nas-vendas-internas-de-veiculos-em-2020.ghtml

#Relat�rio FOCUS 17-03-2020
#https://www.infomoney.com.br/economia/jpmorgan-e-goldman-derrubam-projecao-do-pib-do-brasil-em-2020-e-veem-queda-de-ate-1-em-2020/

#Centro de Macroeconomia Aplicada da Funda��o Get�lio Vargas (FGV)

#Com informa��es FOCUS 23-03-2020

IPCA2020 <- 1+3.05/100
PIB2020 <- 1+(0.02/100) #Cen�rio do Governo Federal em 23/03
EF2020 <- 1+0.00
CVFS2020 <- 1+0.05
ANFAVEA2020 <- 0.97 #Substitui os dados da ANFAVEA de jan de 2020 pois estavam muito otimistas diante do quadro atual. Agora apenas considero desvaloriza��o da frota atual

IPCA2021 <- 1+0.0357
PIB2021 <- 1+2.5/100
EF2021 <- 1+0.00
CVFS2021 <- 1+0.05
ANFAVEA2021 <- IPCA2021*PIB2021*0.97

IPCA2022 <- 1+0.0350
PIB2022 <- 1+2.50/100
EF2022 <- 1+0.00
CVFS2022 <- 1+0.05
ANFAVEA2022 <- IPCA2022*PIB2022*0.97

IPCA2023 <- 1+0.0350
PIB2023 <- 1+2.50/100
EF2023 <- 1+0.00
CVFS2023 <- 1+0.05
ANFAVEA2023 <- IPCA2022*PIB2022*0.97

PIB_SC2017 <- 275400000000
PIB_SC2018 <- 298381000000
PIB_SC2019 <- 317300000000
PIB_SC2020 <- PIB_SC2019*PIB2020*IPCA2020
PIB_SC2021 <- PIB_SC2020*PIB2021*IPCA2021
PIB_SC2022 <- PIB_SC2021*PIB2022*IPCA2022
PIB_SC2023 <- PIB_SC2022*PIB2023*IPCA2023

SELIC2019 <- 6.5/100
SELIC2020 <- 3.75/100
SELIC2021 <- 5.25/100
SELIC2022 <- 6.0/100
SELIC2023 <- 6.25/100

TaxaCambio2019 <- 4.1
TaxaCambio2020 <- 4.5
TaxaCambio2021 <- 4.29
TaxaCambio2022 <- 4.23
TaxaCambio2023 <- 4.3

#Efeitos da Covid sobre o emprego e, consequentemente, sobre a FR 120
EfeitoCovid19Emprego2020 <- 1
EfeitoCovid19Emprego2021 <- 1
EfeitoCovid19Emprego2022 <- 1
EfeitoCovid19Emprego2023 <- 1

###############################################

###################################################
#PAR�METROS DA DIRETORIA DA D�VIDA E CAPTA��O
###############################################

IngressosExternos2020 <- 37695571
IngressosExternos2021 <- 0 
IngressosExternos2022 <- 0 
IngressosExternos2023 <- 0 


IngressosInternos2020 <- 262799084
IngressosInternos2021 <- 512000000
IngressosInternos2022 <- 550000000
IngressosInternos2023 <- 0
####################################################################

#4.ICMS 
string <- paste("
 SELECT  
 cod_unidade_gestora, cod_conta_contabil, 
 cod_origem, cod_fonte_recurso, 
 cod_detalhamento, nom_detalhamento, 
 num_ano, 
 (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, 
 (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 	
 WHERE
 cod_origem LIKE '11'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%' OR cod_conta_contabil LIKE '62132%')
 AND ((nom_detalhamento LIKE '%ICMS%') AND (nom_detalhamento NOT LIKE '%Juro%') AND (nom_detalhamento NOT LIKE '%Multa%') AND (nom_detalhamento NOT LIKE '%D�vida Ativa%'))
 AND (cod_conta_contabil NOT LIKE '%62131020201%') 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "4.ICMS"
dataframe$memoria_calculo <- "Execu��o2019+PIB+IPCA+EF. Para 2021-2023 utilizado PIB+IPCA. EF igualado a zero."
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*(IPCA2020)*(PIB2020)*(EF2020)

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*PIB2021*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*PIB2022*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*PIB2023*IPCA2023

receita <- dataframe

#############################################################################################################


###############################################
#3.ITCMD
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '11'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND ((nom_detalhamento LIKE '%ITCMD%') AND (nom_detalhamento NOT LIKE '%Juro%') AND (nom_detalhamento NOT LIKE '%Multa%') AND (nom_detalhamento NOT LIKE '%D�vida Ativa%'))
 AND (num_mes > 6)

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x
dataframe$receita_orcada_liquida <- 0

dataframe0<-dataframe

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, '0' as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '11'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND ((nom_detalhamento LIKE '%ITCMD%') AND (nom_detalhamento NOT LIKE '%Juro%') AND (nom_detalhamento NOT LIKE '%Multa%') AND (nom_detalhamento NOT LIKE '%D�vida Ativa%'))
 AND num_mes < 2

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe <- rbind(dataframe,dataframe0)



dataframe$nome_receita_ldo <- "3.ITCMD"
dataframe$memoria_calculo <- "2� Semestre de 2019 x 2. Para 2021-2023 utilizado IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*2

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#2.IPVA
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '11'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND ((nom_detalhamento LIKE '%IPVA%') AND (nom_detalhamento NOT LIKE '%Juro%') AND (nom_detalhamento NOT LIKE '%Multa%') AND (nom_detalhamento NOT LIKE '%D�vida Ativa%'))
 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "2.IPVA"
dataframe$memoria_calculo <- "Dados da ANFAVEA. Para 2021-2023 PIB+IPCA+3%desvaloriza��o"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*ANFAVEA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*ANFAVEA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*ANFAVEA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*ANFAVEA2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#1.IRRF
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '11'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND ((nom_detalhamento LIKE '%Imposto sobre a Renda%') AND (nom_detalhamento NOT LIKE '%Juro%') AND (nom_detalhamento NOT LIKE '%Multa%') AND (nom_detalhamento NOT LIKE '%D�vida Ativa%'))
 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "1.IRRF"
dataframe$memoria_calculo <- "Crescimento vegetativo da folha de sal�rios"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*CVFS2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*CVFS2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*CVFS2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*CVFS2023


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#5.Taxas
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '11'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND ((nom_detalhamento NOT LIKE '%ITBI%') AND (nom_detalhamento NOT LIKE '%ITCMD%') AND (nom_detalhamento NOT LIKE '%IPVA%') AND (nom_detalhamento NOT LIKE '%ICMS%') AND (nom_detalhamento NOT LIKE '%Imposto sobre a Renda%') AND (nom_detalhamento NOT LIKE '%Juro%') AND (nom_detalhamento NOT LIKE '%Multa%') AND (nom_detalhamento NOT LIKE '%D�vida Ativa%'))
 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "5.Taxas"
dataframe$memoria_calculo <- "Receita executada de 2019+IPCA+PIB"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#6.Outras Receitas tribut�rias
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '11'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND ((nom_detalhamento LIKE '%Juro%') OR (nom_detalhamento LIKE '%Multa%') OR (nom_detalhamento LIKE '%D�vida Ativa%'))
 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "6.Outras receitas tribut�rias"
dataframe$memoria_calculo <- "Receita executada de 2019+IPCA+PIB+Esfor�o Fiscal. Para 2021-2023 IPCA+PIB. EF igualado a zero"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*EF2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#7.Contribui��es
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '12'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "7.Contribui��es"
dataframe$memoria_calculo <- "Receita executada de 2019+CVFS-2% (A receita de contribui��es tem crescido a uma taxa mais baixa que CVFS. Para 2021-2023 CVFS"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*(CVFS2020-0.02)

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*CVFS2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*CVFS2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*CVFS2023


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#8.Receita Patrimonial - Rendimento de aplica��es financeiras
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '13'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND (nom_detalhamento LIKE '%Remunera��o%')
 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "8.Receita Patrimonial - Rendimento de aplica��es financeiras"
dataframe$memoria_calculo <- "Receita executada de 2019+IPCA+Aumento da SELIC. Para 2021-2023 IPCA+SELIC."
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*(SELIC2021/SELIC2020)
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*(SELIC2022/SELIC2021)
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*(SELIC2023/SELIC2022)


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#9.Receita Patrimonial - N�o financeiras
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '13'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND (nom_detalhamento NOT LIKE '%Remunera��o%')
 and num_mes < 2 
 and num_ano > 2019 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- " 9.Receita Patrimonial - N�o financeiras"
dataframe$memoria_calculo <- "In�cio de 2020 x 12. Para 2021-2023 IPCA."
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2020),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2020),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

receita <- rbind(receita,dataframe)

#Verificar em qual natureza da receita elas se encaixam
dataframe <- data.frame('cod_unidade_gestora'='520099','cod_conta_contabil'='62121000000','cod_origem'='16',
 'cod_fonte_recurso'='0160000000','cod_detalhamento'='1360011101','nom_detalhamento'='Cess�o Direito de Operacionaliza��o da Folha Pagto de Pessoal -Adm. Direta -P. Executivo',
 'num_ano'='2019','receita_arrecadada_liquida'=0,'receita_orcada_liquida'=0,'nome_receita_ldo'=' 9.Receita Patrimonial - N�o financeiras',
 'memoria_calculo'='Informa��o Luiz sobre contrato BB',
 'projecao_receita_arrecadada_liquida2020'=12*(101055919.23/40),
 'projecao_receita_arrecadada_liquida2021'=12*(101055919.23/40),
 'projecao_receita_arrecadada_liquida2022'=12*(101055919.23/40),
 'projecao_receita_arrecadada_liquida2023'=0*(101055919.23/40)  #Confirmar se em 2023 n�o teremos essa receita.
 ) #Retirei o marcador de ldo='N'. Depois tenho que verificar se n�o precisa colocar de volta

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#10. Receita Agropecu�ria
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '14'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- " 10. Receita Agropecu�ria"
dataframe$memoria_calculo <- "Receita executada de 2019+PIB+IPCA. Para 2021-2023 PIB+IPCA."
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#11. Receita Industrial
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '15'
 

 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "11. Receita Industrial"
dataframe$memoria_calculo <- "Receita executada de 2019+PIB+IPCA. Para 2021-2023 PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#12. Receita de Servi�os (Vers�o II. Corre��o por CVFS para Contribui��es. Corre��o por PIB+IPCA para os demais. Linha espec�fica para SUS)

#12.a Contribui��o
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '16'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 and nom_detalhamento LIKE '%Contribui��o%'

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "12. Receita de Servi�os"
dataframe$memoria_calculo <- "Para contribui��es: CVFS. Para os demais PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*CVFS2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*CVFS2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*CVFS2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*CVFS2023

receita <- rbind(receita,dataframe)

#12.b SUS
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '16'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 and nom_detalhamento LIKE '%SUS%'
 and num_mes > 7
 
 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- 12*(x/5)

##Inclus�o do Or�amento
dataframe1 <- dataframe

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, '0' as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '16'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND num_mes < 2
 AND nom_detalhamento LIKE '%SUS%'
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

dataframe <- rbind(dataframe1,dataframe)


x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x
##Fim Inclus�o do Or�amento

dataframe$nome_receita_ldo <- "12. Receita de Servi�os"
dataframe$memoria_calculo <- "Para contribui��es: CVFS. Para os demais PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#12.c Demais que n�o s�o SUS nem Contribui��o
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '16'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 and nom_detalhamento NOT LIKE '%SUS%' and nom_detalhamento NOT LIKE '%Contribui��o%'

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "12. Receita de Servi�os"
dataframe$memoria_calculo <- "Para contribui��es: CVFS. Para os demais PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#13. Transfer�ncias Correntes - Cota-Parte do Fundo Participa��o Estado e DF
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (SUM(val_receita_arrecadada) - SUM(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 and nom_detalhamento LIKE '%Cota-Parte do Fundo de Participa��o%'


 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "13. Transfer�ncias Correntes - Cota-Parte do Fundo Participa��o Estado e DF "
dataframe$memoria_calculo <- "Receita de 2019 corrigida por PIB+IPCA. Para 2021-2023 PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#14. Transfer�ncias Correntes - Cota-Parte do IPI
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%IPI%'
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "14. Transfer�ncias Correntes - Cota-Parte do IPI"
dataframe$memoria_calculo <- "Receita executada de 2019+PIB+IPCA. Para 2021-2023 PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#15. Transfer�ncias Correntes - Outras Transfer�ncias da Uni�o - FEX (Aux. Fom.Export) Tesouro
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%FEX%'
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "15. Transfer�ncias Correntes - Outras Transfer�ncias da Uni�o - FEX (Aux. Fom.Export) Tesouro"
dataframe$memoria_calculo <- "Legisla��o Federal - Uni�o interrompeu o envio da FEX"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- 0

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021*0
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022*0
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023*0


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#16. Transfer�ncias Correntes - Transf. Financeiras do ICMS - Desonera��o - L.C. N. 87/96
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (MAX(val_receita_arrecadada) - MAX(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%87/96%'
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "16. Transfer�ncias Correntes - Transf. Financeiras do ICMS - Desonera��o - L.C. N. 87/96"
dataframe$memoria_calculo <- "Legisla��o Federal"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*0

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021*0
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022*0
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023*0

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#17. Transfer�ncias Correntes - Outras Transfer�ncias Dir. Fundo Nacional do Desenv. da Educa��o - FNDE (Simplificado - Melhorar)
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%FNDE%'
 AND num_mes > 1 AND num_mes < 3 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

##Inclus�o do Or�amento
dataframe1 <- dataframe

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, '0' as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND num_mes < 2
 AND nom_detalhamento LIKE '%FNDE%'
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

dataframe <- rbind(dataframe1,dataframe)


x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x
##Fim Inclus�o do Or�amento

dataframe$nome_receita_ldo <- "17. Transfer�ncias Correntes - Outras Transfer�ncias Dir. Fundo Nacional do Desenv. da Educa��o - FNDE"
dataframe$memoria_calculo <- "Fevereiro de 2020 vezes 10 apenas para a parte de alimenta��o (PNAE). A de obras foi zerada por falta de visibilidade na receita."
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2020),]$projecao_receita_arrecadada_liquida2020 <- 10*dataframe[which(dataframe$num_ano==2020),]$receita_arrecadada_liquida

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#18. Transfer�ncias Correntes - Transfer�ncias do Sal�rio-Educa��o (Precisa melhorar)
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%Sal�rio-Educa��o%'
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "18. Transfer�ncias Correntes - Transfer�ncias do Sal�rio-Educa��o"
dataframe$memoria_calculo <- "receita de 2019 + IPCA. Para 2021-2023 IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*EfeitoCovid19Emprego2020 

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*EfeitoCovid19Emprego2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*EfeitoCovid19Emprego2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*EfeitoCovid19Emprego2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#19. Transfer�ncias Correntes - Cota-Parte CIDE - Contrib. Interven��o no Dom�nio Econ�mico
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%CIDE%'

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "19. Transfer�ncias Correntes - Cota-Parte CIDE - Contrib. Interven��o no Dom�nio Econ�mico"
dataframe$memoria_calculo <- "Receita de 2019 corrigido pelo IPCA + PIB. Para 2021-2023 PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020*PIB2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#20. Transfer�ncias Correntes - Transfer�ncias de Recursos do FUNDEB
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%FUNDEB%'
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "20. Transfer�ncias Correntes - Transfer�ncias de Recursos do FUNDEB"
dataframe$memoria_calculo <- "Receita de 2019+IPCA+PIB+EF+ previs�o de que o coeficiente munic�pio/estado ser� est�vel nos pr�ximos ano. Para 2021-2023 PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*PIB2020*IPCA2020*EF2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#21. Transfer�ncias Correntes - Recursos da Sa�de
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (SUM(val_receita_arrecadada) - SUM(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND cod_fonte_recurso LIKE '%223%'
 and num_mes < 2 
 and num_ano > 2019

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x



dataframe$nome_receita_ldo <- "21. Transfer�ncias Correntes - Recursos da Sa�de"
dataframe$memoria_calculo <- "Receita de 2019+PIB+IPCA. Para 2021-2023 PIB+IPCA"   #Conversei com a Alba e ela s� atualiza os valores em maio para a LOA 2020
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2020),]$projecao_receita_arrecadada_liquida2020 <- 12*dataframe[which(dataframe$num_ano==2020),]$receita_arrecadada_liquida*IPCA2020
#dataframe$num_ano <- 2019

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021*PIB2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022*PIB2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023*PIB2023

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#22. Transfer�ncias Correntes - Conv�nios (transfer�ncias volunt�rias)
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND (cod_fonte_recurso LIKE '%128%' OR cod_fonte_recurso LIKE '%228%')
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "22. Transfer�ncias Correntes - Conv�nios (transfer�ncias volunt�rias)"
dataframe$memoria_calculo <- "Receita de 2019+IPCA. Para 2021-2023 IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#23. Transfer�ncias Correntes - Outras Transfer�ncias
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '17'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND ((cod_fonte_recurso NOT LIKE '0128%') AND (cod_fonte_recurso NOT LIKE '0228%') AND (cod_fonte_recurso NOT LIKE '0223%'))
 AND (nom_detalhamento NOT LIKE '%FUNDEB%' AND nom_detalhamento NOT LIKE '%CIDE%' AND nom_detalhamento NOT LIKE '%Sal�rio-Educa��o%' AND nom_detalhamento NOT LIKE '%Sal�rio-Educa��o%' AND nom_detalhamento NOT LIKE '%FNDE%' AND nom_detalhamento NOT LIKE '%87/96%' AND nom_detalhamento NOT LIKE '%FEX%' AND nom_detalhamento NOT LIKE '%IPI%' AND nom_detalhamento NOT LIKE '%Cota-Parte do Fundo de Participa��o%' AND nom_detalhamento NOT LIKE '%12.276%')

  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "23. Transfer�ncias Correntes - Outras Transfer�ncias"
dataframe$memoria_calculo <- "Receita de 2019+IPCA. Para 2021-2023 IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023


receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#24. OUTRAS RECEITAS CORRENTES
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '19'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND (nom_detalhamento NOT LIKE '%Recupera��o%' AND nom_detalhamento NOT LIKE '%Restitui��es de Conv�nios%' AND nom_detalhamento NOT LIKE '%PRODEC%')
 
 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

#Lembrando que os acr�scimos contratuais do PRODEC s�o inseridos depois

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "24. OUTRAS RECEITAS CORRENTES"
dataframe$memoria_calculo <- "Receita de 2019+IPCA. Para 2021-2023 IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

receita <- rbind(receita,dataframe)

receita$ldo <- "S"

#############################################################################################################


#########################################
#RECEITAS INTRAOR�AMENT�RIAS
#########################################

###############################################
#30. Receitas intra-or�ament�rias de contribui��es
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '72'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND num_mes < 2
  
 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "30. Receitas intra-or�ament�rias de contribui��es"
dataframe$memoria_calculo <- "13 vezes receita de janeiro para 2020. Para 2021-2023 CVFS"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2020),]$projecao_receita_arrecadada_liquida2020 <- 13*dataframe[which(dataframe$num_ano==2020),]$receita_arrecadada_liquida

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*CVFS2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*CVFS2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*CVFS2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#31. Receitas intra-or�ament�rias patrimoniais
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '73'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "31. Receitas intra-or�ament�rias patrimoniais"
dataframe$memoria_calculo <- "Receita de 2019+IPCA. Para 2021-2023 IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

#############################################################################################################

###############################################
#32. Receitas intra-or�ament�rias de servi�os
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '76'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND num_mes > 1 AND num_mes < 7
 AND num_ano < 2020 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x/5  #Divis�o por 5 (5 meses entre janeiro e julho)

dataframe1<-dataframe

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, 0 as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '76'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND num_mes < 2
 AND num_ano > 2019  

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
dataframe <- rbind(dataframe1,dataframe)

x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x


dataframe$nome_receita_ldo <- "32. Receitas intra-or�ament�rias de servi�os"
dataframe$memoria_calculo <- "Receita de 2019+CVFS. Para 2021-2023 CVFS"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*11.14*CVFS2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*CVFS2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*CVFS2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*CVFS2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

#############################################################################################################


###############################################
#33. Receitas intra-or�ament�rias - outras receitas correntes
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (sum(val_receita_arrecadada) - sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '79'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND (nom_detalhamento NOT LIKE '%Recupera��o%') 
 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "33. Receitas intra-or�ament�rias - outras receitas correntes
"
dataframe$memoria_calculo <- "Receita de 2019+IPCA. Para 2021-2023 IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

#############################################################################################################


###Atualiza os registros com receita l�quida ou proje��o de receita l�quida para 2020

receita[which(substr(receita$cod_conta_contabil,0,3)=='621'),]$num_ano <- 2020

##

###################################################
##Fim receitas intraor�ament�rias#############
########################################################


#########################################
#ACR�SCIMOS DO PRODEC NA FR 299
#########################################

#Acr�scimos PRODEC
#
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '19'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nome_detalhamento LIKE '%PRODEC%'

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "24. OUTRAS RECEITAS CORRENTES"
dataframe$memoria_calculo <- "Informa��es do FADESC sobre arrecada��o para 2020. Para o per�odo 2021-2023 reajustado por PIB+IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0

#Inicializando as informa��es 2020-2023 com valores zerados. A seguir s�o inputados os valores manualmente.
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- 0
dataframe$projecao_receita_arrecadada_liquida2021 <- 0
dataframe$projecao_receita_arrecadada_liquida2022 <- 0
dataframe$projecao_receita_arrecadada_liquida2023 <- 0

dataframe$ldo <- 'N'

receita <- rbind(receita,dataframe)

#Verificar em qual natureza da receita elas se encaixam
d <- data.frame('cod_unidade_gestora'='520091','cod_conta_contabil'='62121000000','cod_origem'='19',
 'cod_fonte_recurso'='0299000000','cod_detalhamento'='1990992101','nom_detalhamento'='Acr�scimos Contratuais PRODEC',
 'num_ano'='2019','receita_arrecadada_liquida'=0,'receita_orcada_liquida'=0,'nome_receita_ldo'='24. OUTRAS RECEITAS CORRENTES',
 'memoria_calculo'='Informa��es do FADESC sobre arrecada��o para 2020','projecao_receita_arrecadada_liquida2020'=28962264,
 'projecao_receita_arrecadada_liquida2021'=28962264*IPCA2021,
 'projecao_receita_arrecadada_liquida2022'=28962264*IPCA2021*IPCA2022,
 'projecao_receita_arrecadada_liquida2023'=28962264*IPCA2021*IPCA2022*IPCA2023,
 'ldo'='N')

receita <- rbind(receita,d)

#########################################
#RECEITAS DE CAPITAL
#########################################

#Opera��es de cr�dito
#
string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '21'
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)

	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "Opera��es de cr�dito"
dataframe$memoria_calculo <- "Informa��es da DICD sobre opera��es de cr�dito contratadas para os pr�ximos 3 anos"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- 0

dataframe$projecao_receita_arrecadada_liquida2021 <- 0
dataframe$projecao_receita_arrecadada_liquida2022 <- 0
dataframe$projecao_receita_arrecadada_liquida2023 <- 0

dataframe$ldo <- 'N'

receita <- rbind(receita,dataframe)

#Verificar em qual natureza da receita elas se encaixam
d <- data.frame('cod_unidade_gestora'='520099','cod_conta_contabil'='62121000000','cod_origem'='21',
 'cod_fonte_recurso'='0191000000','cod_detalhamento'='2119001100','nom_detalhamento'='Espa�o Fiscal',
 'num_ano'='2019','receita_arrecadada_liquida'=0,'receita_orcada_liquida'=0,'nome_receita_ldo'='Opera��es de cr�dito',
 'memoria_calculo'='Informa��es da GEDIP sobre espa�o fiscal do Estado','projecao_receita_arrecadada_liquida2020'=IngressosInternos2020,
 'projecao_receita_arrecadada_liquida2021'=IngressosInternos2021,
 'projecao_receita_arrecadada_liquida2022'=IngressosInternos2022,
 'projecao_receita_arrecadada_liquida2023'=IngressosInternos2023,
 'ldo'='N')

receita <- rbind(receita,d)

d <- data.frame('cod_unidade_gestora'='520099','cod_conta_contabil'='62121000000','cod_origem'='21',
 'cod_fonte_recurso'='0192000000','cod_detalhamento'='2128015100','nom_detalhamento'='BID/CAF/PROFISCO...',
 'num_ano'='2019','receita_arrecadada_liquida'=0,'receita_orcada_liquida'=0,'nome_receita_ldo'='Opera��es de cr�dito',
 'memoria_calculo'='Informa��es da GEDIP','projecao_receita_arrecadada_liquida2020'=IngressosExternos2020,
 'projecao_receita_arrecadada_liquida2021'=IngressosExternos2021,
 'projecao_receita_arrecadada_liquida2022'=IngressosExternos2022,
 'projecao_receita_arrecadada_liquida2023'=IngressosExternos2023,
 'ldo'='N')

receita <- rbind(receita,d)

#Aliena��o de bens
con <- odbcConnect("BoaVista",uid="bsinzato-sefaz",pwd="crisis",believeNRows=FALSE)

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '22' 
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')
 AND nom_detalhamento LIKE '%Poder Executivo%' 

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "Aliena��o de bens"
dataframe$memoria_calculo <- "IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

#Amortiza��o de empr�stimos

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '23' 
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "Amortiza��o de empr�stimos"
dataframe$memoria_calculo <- "IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

#Transfer�ncias de capital 

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '24' 
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "Transfer�ncias de capital "
dataframe$memoria_calculo <- "IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

#Outras Receitas de Capital (Intraor�ament�rias) 

string <- paste("
 SELECT  cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano, (Sum(val_receita_arrecadada) - Sum(val_receita_arrecadada_deducao)) as receita_arrecadada_liquida, (Sum(val_receita_orcada) - Sum(val_receita_orcada_deducao)) as receita_orcada_liquida
 
 FROM
 dcog_share.fat_receita_orcamentaria_coedt 
 WHERE
 cod_origem LIKE '89' 
 AND (cod_conta_contabil LIKE '%521%' OR cod_conta_contabil LIKE '62121%' OR cod_conta_contabil LIKE '62131%' OR cod_conta_contabil LIKE '62132%')

 GROUP BY cod_unidade_gestora, cod_conta_contabil, cod_origem, cod_fonte_recurso, cod_detalhamento, nom_detalhamento, num_ano
 ")

# Query the database and put the results into the data frame "dataframe"
 dataframe <- sqlQuery(con, string, as.is=T)
	
x<-as.character(dataframe[,8])
x <- gsub(",",".",x)
x <- as.numeric(x)
dataframe$receita_arrecadada_liquida <- x

dataframe$nome_receita_ldo <- "Outras Receitas de Capital"
dataframe$memoria_calculo <- "IPCA"
dataframe$projecao_receita_arrecadada_liquida2020 <- 0
dataframe[which(dataframe$num_ano==2019),]$projecao_receita_arrecadada_liquida2020 <- dataframe[which(dataframe$num_ano==2019),]$receita_arrecadada_liquida*IPCA2020

dataframe$projecao_receita_arrecadada_liquida2021 <- dataframe$projecao_receita_arrecadada_liquida2020*IPCA2021
dataframe$projecao_receita_arrecadada_liquida2022 <- dataframe$projecao_receita_arrecadada_liquida2021*IPCA2022
dataframe$projecao_receita_arrecadada_liquida2023 <- dataframe$projecao_receita_arrecadada_liquida2022*IPCA2023

dataframe$ldo <- "N"

receita <- rbind(receita,dataframe)

##################
#FIM RECEITAS DE CAPITAL
#######################

###Atualiza os registros com receita l�quida ou proje��o de receita l�quida para 2020

receita[which(substr(receita$cod_conta_contabil,0,3)=='621'),]$num_ano <- 2020

##

#################################################
#Parametriza��o da RCL
#################################################
receita$rcl <- ""
parametrizacao <- ""

codigos_receitas <- receita[,5]
parametro_rcl <- "1118021"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp

parametro_rcl <- "1118012"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao

parametro_rcl <- "1118013"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "111303"
sub_codigos_receitas <- substr(codigos_receitas,0,6)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1118014"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "112"
sub_codigos_receitas <- substr(codigos_receitas,0,3)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "113"
sub_codigos_receitas <- substr(codigos_receitas,0,3)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "13"
sub_codigos_receitas <- substr(codigos_receitas,0,2)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "14"
sub_codigos_receitas <- substr(codigos_receitas,0,2)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "15"
sub_codigos_receitas <- substr(codigos_receitas,0,2)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "16"
sub_codigos_receitas <- substr(codigos_receitas,0,2)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718011"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718061"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718016"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "17580111"
sub_codigos_receitas <- substr(codigos_receitas,0,8)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718017"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718018"
sub_codigos_receitas <- substr(codigos_receitas,0,7)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "171802"
sub_codigos_receitas <- substr(codigos_receitas,0,6)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "171803"
sub_codigos_receitas <- substr(codigos_receitas,0,6)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "171812"
sub_codigos_receitas <- substr(codigos_receitas,0,6)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "171805"
sub_codigos_receitas <- substr(codigos_receitas,0,6)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "171807"
sub_codigos_receitas <- substr(codigos_receitas,0,6)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1728"
sub_codigos_receitas <- substr(codigos_receitas,0,4)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "173"
sub_codigos_receitas <- substr(codigos_receitas,0,3)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "175899"
sub_codigos_receitas <- substr(codigos_receitas,0,6)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "174"
sub_codigos_receitas <- substr(codigos_receitas,0,3)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "176"
sub_codigos_receitas <- substr(codigos_receitas,0,3)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "177"
sub_codigos_receitas <- substr(codigos_receitas,0,3)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "17181"
sub_codigos_receitas <- substr(codigos_receitas,0,5)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "19"
sub_codigos_receitas <- substr(codigos_receitas,0,2)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "22200012"
sub_codigos_receitas <- substr(codigos_receitas,0,8)
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

receita[parametrizacao,]$rcl <- "S"

###################################################


#################################################
#Parametriza��o da RLI
#################################################

receita$RLI <- ""
parametrizacao <- ""

codigos_receitas <- receita[,5]  #Ideal � n�o utilizar n�meros e sim o nome da coluna
parametro_rcl <- "1118021"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp
sum(parametrizacao)

parametro_rcl <- "11180212"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180214"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180213"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180131"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180132"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180133"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180134"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180121"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180122"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180123"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "11180124"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "111303"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718011"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "171806"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718016"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

parametro_rcl <- "1718018"
sub_codigos_receitas <- substr(codigos_receitas,0,nchar(parametro_rcl))
parametrizacao_temp <- sub_codigos_receitas %in% parametro_rcl
parametrizacao <- parametrizacao_temp | parametrizacao
sum(parametrizacao)

receita[parametrizacao,][which(substr(receita[parametrizacao,]$cod_conta_contabil,0,7) != '6213101' & substr(receita[parametrizacao,]$cod_conta_contabil,0,9) != '521120101'),]$RLI <- "S"


###############################################
###FIM Parametriza��o RLI
###################################################

colnames(receita)[8]<-"valor_base"

receita[which(receita$cod_fonte_recurso=='0161000000'),]$cod_fonte_recurso <- "0100000000"
receita[which(receita$cod_fonte_recurso=='0162000000'),]$cod_fonte_recurso <- "0100000000"

receita$cod_fonte_reduzida <- substr(receita$cod_fonte_recurso,0,4)
receita$cod_tipo_fr <- substr(receita$cod_fonte_recurso,3,4)


receita$projecao2020aValoresDe2019 <- receita$receita_orcada_liquida/IPCA2020
receita$projecao2021aValoresDe2019 <- receita$projecao_receita_arrecadada_liquida2021/(IPCA2020*IPCA2021)
receita$projecao2022aValoresDe2019 <- receita$projecao_receita_arrecadada_liquida2022/(IPCA2020*IPCA2021*IPCA2022)
receita$projecao2023aValoresDe2019 <- receita$projecao_receita_arrecadada_liquida2023/(IPCA2020*IPCA2021*IPCA2022*IPCA2023)


#Carregar no banco PostgreSQL
require("RPostgreSQL")
require("ImportExport")

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "postgres",
                 host = "localhost", port = 5432,
                 user = "postgres", password = "pgf")

dbRemoveTable(con, "GEORC_ReceitaLDO2021")

receita$DataAtualizacao <- toString(Sys.time())

# writes df to the PostgreSQL database "postgres", table  
dbWriteTable(con, "GEORC_ReceitaLDO2021", 
             value = receita, append = TRUE, row.names = FALSE)


inserirdados <- "N"

if (inserirdados == "S")
{

#############################################
#Inser��o de dados no Hadoop User Experience (HUE)
########################################

channel <- odbcConnect("HUE")
sqlDrop(channel, "dior_share.ReceitaLDO2021")

#Inser�ao linha a linha (feito dessa forma para salvar no formato parquet e porqu� utilizando a biblioteca RODBC estava gerando erros)
string <- ""
for (j in 1:ncol(receita))
{
   string <- paste(string,colnames(receita)[j],typeof(receita[1,j]),",",sep=" ")	
}
string <- substr(string,1,nchar(string)-1)
string <- gsub("character","string",string)
receita_colnames <- paste(string,")",sep="")
string <- paste("create table dior_share.ReceitaLDO2021(",receita_colnames," stored as parquet", sep="")

con = dbConnect(odbc::odbc(), "HUE")
dbGetQuery(conn = con, string)


receita_colnames <- paste(colnames(receita),collapse=",")
receita_colnames <- paste("(",receita_colnames,")",sep="")
values <- paste(receita[1,],collapse=",")

for (j in 1:nrow(receita))
{
string <- ""
for (k in 1:ncol(receita))
 {
 value <- receita[j,k]
  if (typeof(value)=="character")
   {
   value <- paste("'",value,"'",sep="")
   }  
  if (typeof(value)=="double")
   {
   value <- value
   }

 string <- paste(string,value,sep=",")

 }

values <- substr(string,2,nchar(string))

string <- paste("insert into dior_share.ReceitaLDO2021 ",receita_colnames," values (", values, ")",sep="")

dbGetQuery(conn = con, string)

}

table <- dbGetQuery(conn = con, "SELECT * FROM dior_share.ReceitaLDO2021")

#dbGetQuery(conn = con, "INVALIDATE METADATA dior_share.ReceitaLDO2021")

}#End If de inser��o no Boa Vista

