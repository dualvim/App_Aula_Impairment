#####################################################
# script_aux_03_gerar_respostas_exercicio.R         #
#####################################################

source("script_aux_02_classe_testeImpairmentAtivo.R")




##############################################
# --> Arrays com os dados dos enunciados     #
##############################################
#ARR_NOMES_ONIBUS <- enc2utf8(c("Marcopolo Paradiso Ld G7 Scania Ano 2014/2014", "Dd Comil Campione 2015/2016", "Ônibus Of 1724 - Carroceria Marcopolo - Viaggio 1050 2015", "Marcopolo Sênior G7 Executivo – Rodoviário – 2010", "Micro-ônibus Mascarello - padrão ARTESP - 2014"))
ARR_NOMES_ONIBUS <- enc2utf8(c("Marcopolo Paradiso Ld G7 (2014)", "Dd Comil Campione (2015/2016)", "Marcopolo Viaggio 1050 (2015)", "Marcopolo Sênior G7 (2010)", "Micro-ônibus Mascarello (2014)"))
ARR_DATA_CPA <- as.Date(c("26/05/2014", "04/01/2016", "02/07/2016", "02/06/2010", "24/10/2014"), format="%d/%m/%Y")
ARR_VLR_COMPRA <- c(950000, 850000, 620000, 345000, 400000)
ARR_PERC_VLR_RES <- c(0.15, 0.12, 0.2, 0.15, 0.06)
ARR_QTDE <- c(2, 2, 1, 2, 1)
ARR_VIDA_UTIL <- c(7, 10, 10, 10, 9)
ARR_VLR_VDA <- c(680000, 755000, 490000, 138500, 255000)
ARR_DATA_FIM_VU <- as.Date(c("31/05/2021", "31/12/2025", "30/06/2026", "31/05/2020", "31/10/2023"), format="%d/%m/%Y")


##################################################
# --> Lista com as receitas de cada ativo        #
##################################################
LST_SERIE_RECEITAS <- list()
LST_SERIE_RECEITAS[[1]] <- c(270000, 220000, 200000, 190000, 140000)
LST_SERIE_RECEITAS[[2]] <- c(300000, 320000, 315000, 270000, 295000, 200000, 150000, 110000, 100000)
LST_SERIE_RECEITAS[[3]] <- c(275000, 290000, 285000, 250000, 210000, 195000, 175000, 125000, 110000, 100000)
LST_SERIE_RECEITAS[[4]] <- c(140000, 125000, 100000, 65000)
LST_SERIE_RECEITAS[[5]] <- c(200000, 175000, 160000, 130000, 115500, 95000, 80000)


###############################################
# --> Lista com os gastos de cada ativo:      #
###############################################
LST_SERIE_GASTOS <- list()
LST_SERIE_GASTOS[[1]] <- c(-90000, -95000, -85000, -70000, -85000)
LST_SERIE_GASTOS[[2]] <- c(-150000, -120000, -105000, -190000, -160000, -110000, -90000, -80000, -85000)
LST_SERIE_GASTOS[[3]] <- c(-125000, -110000, -125000, -115000, -120000, -110000, -135000, -115000, -90000, -75000)
LST_SERIE_GASTOS[[4]] <- c(-76000, -70000, -80000, -37500)
LST_SERIE_GASTOS[[5]] <- c(-75000, -90000, -95500, -60000, -60500, -55000, -60000)




########################################################################
# Lista de listas com os valores iniciais de cada um dos exercicios    #
########################################################################
LST_LST_VALS_INI <- list()
for(idx in 1:5){
      LST_VALS_INI <- list()
      LST_VALS_INI[["NOME_ONIBUS"]] <- ARR_NOMES_ONIBUS[idx]
      LST_VALS_INI[["DATA_CPA"]] <- as.Date(ARR_DATA_CPA[idx])
      LST_VALS_INI[["VLR_COMPRA"]] <- ARR_VLR_COMPRA[idx]
      LST_VALS_INI[["QTDE"]] <- ARR_QTDE[idx]
      LST_VALS_INI[["PERC_VLR_RES"]] <- ARR_PERC_VLR_RES[idx]
      LST_VALS_INI[["VIDA_UTIL"]] <- ARR_VIDA_UTIL[idx]
      LST_VALS_INI[["DT_BAL"]] <- as.Date("2016-12-31")
      LST_VALS_INI[["TX_DESC"]] <- 0.17
      LST_VALS_INI[["VLR_MERCADO"]] <- ARR_VLR_VDA[idx]
      LST_VALS_INI[["SERIE_RECEITAS"]] <- LST_SERIE_RECEITAS[[idx]]
      LST_VALS_INI[["SERIE_GASTOS"]] <- LST_SERIE_GASTOS[[idx]]
      
      # Inserir 'LST_VALS_INI' em 'LST_LST_VALS_INI':
      LST_LST_VALS_INI[[idx]] <- LST_VALS_INI
      
}

# Nomear os elementos da lista 'LST_LST_VALS_INI'
names(LST_LST_VALS_INI) <- ARR_NOMES_ONIBUS

# Apagar os arrays que nao forem mais usados
rm(ARR_VLR_VDA, ARR_VIDA_UTIL, ARR_PERC_VLR_RES, ARR_QTDE, ARR_VLR_COMPRA, ARR_DATA_CPA, LST_SERIE_RECEITAS, LST_SERIE_GASTOS)


retornar_obj_testeImpairmetAtivo_exerc <- function(nome_ativo, lst_dados=LST_LST_VALS_INI){
      LST_VALS_INI <- lst_dados[[nome_ativo]]
      
      # Objeto da classe "testeImpairmentAtivo"
      OBJ_TESTE_IMPAIRM <- testeImpairmentAtivo(i_dtCpa=LST_VALS_INI[["DATA_CPA"]], 
                                                i_vlrCpa=LST_VALS_INI[["VLR_COMPRA"]],
                                                i_qtde=LST_VALS_INI[["QTDE"]],
                                                i_perVlrRes=LST_VALS_INI[["PERC_VLR_RES"]],
                                                i_vidaUt=LST_VALS_INI[["VIDA_UTIL"]],
                                                i_vlrMerc=LST_VALS_INI[["VLR_MERCADO"]],
                                                i_dtBal=LST_VALS_INI[["DT_BAL"]],
                                                i_txDesc=LST_VALS_INI[["TX_DESC"]],
                                                i_arrRecs=LST_VALS_INI[["SERIE_RECEITAS"]],
                                                i_arrGsts=LST_VALS_INI[["SERIE_GASTOS"]])
      return(OBJ_TESTE_IMPAIRM)
}
