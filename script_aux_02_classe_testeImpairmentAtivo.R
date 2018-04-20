#####################################################
# script_aux_02_classe_testeImpairmentAtivo.R       #
#####################################################
# --> Define a classe 'testeImpairmentAtivo', que realiza os testes de impairment




##################################################
# Definicao da classe 'testeImpairmentAtivo'     #
##################################################
testeImpairmentAtivo <- function(i_dtCpa, i_vlrCpa, i_qtde, i_perVlrRes, i_vidaUt, i_vlrMerc, i_dtBal, i_txDesc, i_arrRecs, i_arrGsts){
      ###########################################################
      # --> Classe 'testeImpairmentAtivo'                       #
      # Argumentos Recebidos: Todos os inputs selecionaveis     #
      #           no menu lateral                               #
      ###########################################################
      # --> Environment da classe no R:
      thisEnv <- environment()
      
      
      
      
      ####################################################
      # Inicializar as variaveis do teste de impairment  #
      ####################################################
      # --> Inicializar as variaveis referentes aos inputs recebidos
      DT_CPA <- i_dtCpa # Data da compra do ativo
      VLR_CPA <- i_vlrCpa # Valor de compra do ativo (unitario) 
      QTDE <- i_qtde # Quantidade de ativos
      PERC_RES <- i_perVlrRes # Percentual do valor residual
      VU <- i_vidaUt # Vida Util do ativo
      VLR_VDA_UNIT <- i_vlrMerc # Valor da Venda (unitario)
      DT_BAL <- i_dtBal # Data do balanco
      TX_DESC <- i_txDesc # Taxa de desconto dos fluxos de caixa (%a.a.)
      ARR_REC_UNIT <- i_arrRecs # Array com as receitas esperadas futuras
      ARR_GST_UNIT <- i_arrGsts # Array com os custos esperados futuros
      
      
      
      
      #######################
      # Funcoes Auxiliares  #
      #######################
      fun_aux_retornarValoresFormat <- function(vlr_inp, tipo_valor=c("geral", "moeda", "taxa")){
            ##################################################################################
            # --> Retorna o valor formatado em 2 casas decimais, usando como separador       #
            #           de decimal ',' e o separador de milhar '.'                           #
            ##################################################################################
            tp_vlr <- match.arg(tipo_valor)
            
            switch(tp_vlr,
                   geral = {return(paste(format(vlr_inp, decimal.mark=",", big.mark=".", nsmall=2)))},
                   moeda = {return(paste("R$", format(vlr_inp, decimal.mark=",", big.mark=".", nsmall=2)))},
                   taxa = {return(paste0(format(vlr_inp*100, decimal.mark=",", nsmall=2), "% a.a."))}
            )
      }
      
      
      fun_aux_gerar_seq_anos_vida_util_ativo <- function(data_cpa, vida_util){
            #########################################################
            # Retorna um array com os anos referentes a vida util   #
            #     do ativo                                          #
            #########################################################
            ano_cpa <- as.numeric(format(as.Date(data_cpa, origin="1970-01-01"), "%Y"))
            ult_ano <- ano_cpa + vida_util
            seq_anos <- seq(from=ano_cpa, to=ult_ano)
            return(seq_anos)
      }
      
      
      fun_aux_data_fim_vida_util <- function(data_inicio, vida_util){
            ##################################################
            # Estima a data com o im da vida util do ativo   #
            ##################################################
            # Meses referentes a vida util do ativo:
            meses_vu <- vida_util * 12
            
            # Converter a data para "POSIXlt"
            data_fim <- as.POSIXlt(as.Date(data_inicio, origin="1970-01-01"))
            # Pegar o dia do mes em 'data_inicio2'
            dia_mes <- data_fim$mday
            
            # Somar "meses_vu" a "data_inicio2"
            data_fim$mon <- data_fim$mon + meses_vu
            
            if(dia_mes <= 15){
                  # Se a compra for antes do dia 15, o mes da data final sera o ultimo 
                  #     dia do mes anterior da data da compra
                  data_fim$mday <- 1
                  data_fim <- as.Date(data_fim, origin="1970-01-01") - 1
            } else{
                  # Se a compra ocorrer apos o dia 15, o mes da data final ser o ultimo dia do
                  #     mes em que ocorreu a compra
                  data_fim$mday <- 1
                  data_fim$mon <- data_fim$mon + 1
                  data_fim <- as.Date(data_fim, origin="1970-01-01") - 1
            }
            
            # Converter "data_fim" para Date:
            return(data_fim)
      }
      
      
      fun_aux_gerar_sequencia_datas_fluxo_caixa <- function(data_balanco, data_fim){
            ################################################################
            # Retorna um array com as datas dos fluxos de caixa futuros    #
            #           do ativo                                           #
            ################################################################
            ano_data_bal <- as.numeric(format(as.Date(data_balanco), "%Y"))
            ano_data_fim <- as.numeric(format(as.Date(data_fim), "%Y"))
            
            # Array com os anos apos a data do balanco
            anos_datas <- seq(from=ano_data_bal+1, to=ano_data_fim-1, by=1)
            
            # Array com as datas dos fluxos de caixa
            datas_fluxos <- paste0(anos_datas, "-12-31")
            datas_fluxos <- as.Date(datas_fluxos)
            
            # Acrescentar data_fim
            datas_fluxos <- c(datas_fluxos, as.Date(data_fim))
            return(datas_fluxos)
      }
      
      
      fun_aux_meses_decorridos <- function(data_ini, data_fim){
            ###########################################################
            # Retorna o numero de meses decorridos entre duas datas   #
            ###########################################################
            data_ini <- as.POSIXlt(as.Date(data_ini), origin="1970-01-01")
            data_fim <- as.POSIXlt(as.Date(data_fim), origin="1970-01-01")
            
            # Diferenca entre os anos das duas datas
            dif_anos <- data_fim$year - data_ini$year
            # Diferenca entre os meses das duas datas
            dif_meses <- data_fim$mon - data_ini$mon
            # Booleano indicando se ainda nao completou o mes ou nao
            #mes_inc <- (data_fim$mday < data_ini$mday)
            
            # Calcular e retornar n_meses
            n_meses <- dif_anos*12 + dif_meses #- mes_inc
            return(n_meses)
      }
      
      fun_aux_valor_residual <- function(vlr_compra, perc_vlr_res, qtde){
            #########################################
            # Calcula o valor residual do ativo     #
            #########################################
            vlr_res <- vlr_compra * perc_vlr_res 
            vlr_res <- round(vlr_res * qtde, digits=2)
            return(vlr_res)
      }
      
      
      fun_aux_depreciacao_mensal <- function(vlr_compra, perc_vlr_res, vida_util, qtde){
            ############################################
            # Calcula o valor da depreciacao mensal    #
            ############################################
            meses_vu <- vida_util * 12
            vlr_deprec <- vlr_compra * (1 - perc_vlr_res) * qtde
            
            depr_mensal <- vlr_deprec/meses_vu
            depr_mensal <- round(depr_mensal, digits=2)
            return(depr_mensal)
      }
      
      
      fun_aux_valor_contabil_ativo <- function(vlr_compra, qtde, depr_mensal, n_meses){
            ######################################
            # Retorna o valor contabil do ativo  #
            ######################################
            # Depreciacao acumulada:
            depr_acumulada <- depr_mensal * n_meses
            # Valor Contabil
            vlr_contabil <- ((vlr_compra*qtde) - abs(depr_acumulada))
            vlr_contabil <- round(vlr_contabil, digits=2)
            return(vlr_contabil)
      }
      
      
      fun_aux_calcular_vp_flcx <- function(tx_desc, arr_flcx, arr_meses){
            #######################################################
            # Retorna um array com o valor presente de cada um    #
            #           dos fluxos de caixa em 'arr_flcx'         #
            #######################################################
            nfluxos <- length(arr_flcx)
            arr_vp_fluxos <- sapply(X=1:nfluxos, FUN=function(idx){
                  vp_fluxo <- arr_flcx[idx] / ((1+tx_desc)^(arr_meses[idx]/12))
                  return(round(vp_fluxo, digits=6))
            })
            return(as.numeric(arr_vp_fluxos))
      }
      
      
      fun_aux_lst_outputs_analise <- function(DT_CPA, VLR_CPA, QTDE, PERC_RES, VU, VLR_VDA_UNIT, 
                                              DT_BAL, TX_DESC, ARR_REC_UNIT, ARR_GST_UNIT){
            ###################################################################
            # Objetivo: Retornar uma lista com todos os outputs da analise    #
            # Argumentos: Sao os 10 parametros com os inputs escolhidos nos   #
            #     campos do menu lateral do aplicativo                        #
            # Objeto retornado: Lista com os segunintes objetos:              #
            #      1) Data da compra,                                         #
            #      2) Preco de compra unitario                                #
            #      3) Quantidade de ativos                                    #
            #      4) Percentual do valor residual                            #
            #      5) Vida Util                                               #
            #      6) Valor de venda unitario do ativo                        #
            #      7) Data do Balanco                                         #
            #      8) taxa de desconto                                        #
            #      9) Array com as receitas unitarias                         #
            #     10) Array com os gastos unitarios                           #
            #     11) Data com o fim da vida util do ativo                    #
            #     12) valor residual do ativo                                 #
            #     13) Depreciacao mensal do ativo                             #
            #     14) Meses de utilizacao do ativo                            #
            #     15) Depreciacao acumulada na data do balanco                #
            #     16) Valor contabil do(s) ativo(s) na data do balanco        #
            #     17) Valor de mercado do(s) ativo(s) na data do balanco      #
            #     18) Array com as datas dos fluxos de caixa                  #
            #     19) Numero de fluxos de caixa                               #
            #     20) Array com o numero de meses entre 'data_balanco' e cada #
            #           uma das datas dos fluxos de caixa                     #
            #     21) Array com as receitas totais de cada fluxo              #
            #     22) Array com os gastos totais de cada fluxo                #
            #     23) Array com os fluxos de caixa liquidos                   #
            #     24) Array com os valores presentes dos fluxos de caixa      #
            #           liquidos                                              #
            #     25) Valor em Uso dos ativos                                 #
            #     26) Valor recuperavel                                       #
            #     27) Perda por inpairment                                    #
            #     28) Sequencia de anos de acordo com a vida util             #
            ###################################################################
            # --> Forcar a conversao das datas:
            DT_CPA<- as.Date(DT_CPA, origin="1970-01-01")
            DT_BAL <- tryCatch(as.Date(DT_BAL, origin="1970-01-01"),
                               error=function(e){as.Date("2016-12-31", origin="1970-01-01")})
            
            #--> Valores calculados A partir dos inputs recebidos e funcoes auxiliares
            DT_FIM_VU <- fun_aux_data_fim_vida_util(data_inicio=DT_CPA, vida_util=VU) # Data com o fim da vida util do ativo
            VLR_RESID <- fun_aux_valor_residual(vlr_compra=VLR_CPA, perc_vlr_res=PERC_RES/100, qtde=QTDE) #Valor Residual do ativo
            DEPREC_MES <- fun_aux_depreciacao_mensal(vlr_compra=VLR_CPA, perc_vlr_res=PERC_RES/100, vida_util=VU, qtde=QTDE) # Depreciacao mensal
            MESES_USO <- fun_aux_meses_decorridos(data_ini=DT_CPA, data_fim=DT_BAL) # Numero de meses em que o ativo esta sendo utilizado
            DEPR_ACUMULADA <- round(DEPREC_MES * MESES_USO, digits=2) #Depreciacao acumulada do ativo entre a compa e a data do balanco
            VLR_CONTABIL <- fun_aux_valor_contabil_ativo(vlr_compra=VLR_CPA, qtde=QTDE, depr_mensal=DEPREC_MES, n_meses=MESES_USO) # Valor contabil do ativo na data do balanco
            VLR_MERCADO_TOTAL <- round(VLR_VDA_UNIT * QTDE, digits=2) # Valor de mercado total do(s) ativo(s)
            
            # --> Arrays referentes aos fluxos de caixa liquidos
            # Array com as datas dos fluxos de caixa:
            ARR_DT_FLCX <- fun_aux_gerar_sequencia_datas_fluxo_caixa(data_balanco=DT_BAL, data_fim=DT_FIM_VU)
            # Numero de fluxos de caixa em 'ARR_DT_FLCX'
            NUM_FLUXOS <- length(ARR_DT_FLCX)
            # Array com o numero de meses entre 'DT_BAL' e as datas em 'ARR_DT_FLCX'
            MESES_FLCX <- as.integer(sapply(X=ARR_DT_FLCX, FUN=fun_aux_meses_decorridos, data_ini=DT_BAL))
            # Array com as receitas totais 
            ARR_REC_TOT <- round(ARR_REC_UNIT * QTDE, digits=2)[1:NUM_FLUXOS]
            ARR_REC_TOT[NUM_FLUXOS] <- (ARR_REC_UNIT[NUM_FLUXOS] * QTDE) + VLR_RESID
            # Array com os custos totais
            ARR_GST_TOT <- round(ARR_GST_UNIT * QTDE, digits=2)[1:NUM_FLUXOS]
            # Array com os fluxos de caixa liquidos
            ARR_FLCX_LIQ_TOT <- ARR_REC_TOT - abs(ARR_GST_TOT)
            # Array com os valores presentes dos fluxos de caixa
            ARR_VP_FLCX_LIQ <- fun_aux_calcular_vp_flcx(tx_desc=TX_DESC, arr_flcx=ARR_FLCX_LIQ_TOT, arr_meses=MESES_FLCX)
            # Valor em uso do ativo:
            VLR_USO_ATIVO <- round(sum(ARR_VP_FLCX_LIQ), digits=2)
            # Valor recuperavek do ativo
            VLR_RECUP <- max(VLR_USO_ATIVO, VLR_MERCADO_TOTAL)
            # Valor das perdas por inpairment (caso haja)
            PERDA_IMPAIRMENT <- min((VLR_RECUP - VLR_CONTABIL), 0)
            
            # Sequencia de anos possiveis de analisar:
            SEQ_ANOS_VU <- fun_aux_gerar_seq_anos_vida_util_ativo(data_cpa=DT_CPA, vida_util=VU)
            
            #######################################################
            # Lista com os 28 elementos retornados pela funcao    #
            #######################################################
            lst_output <- list("DT_CPA"=DT_CPA, "VLR_CPA"=VLR_CPA, "QTDE"=QTDE, "PERC_RES"=PERC_RES,
                                    "VU"=VU, "VLR_VDA_UNIT"=VLR_VDA_UNIT, "DT_BAL"=DT_BAL, 
                                    "TX_DESC"=TX_DESC, "ARR_REC_UNIT"=ARR_REC_UNIT, 
                                    "ARR_GST_UNIT"=ARR_GST_UNIT, "DT_FIM_VU"=DT_FIM_VU,
                                    "VLR_RESID"=VLR_RESID, "DEPREC_MES"=DEPREC_MES,
                                    "MESES_USO"=MESES_USO, "DEPR_ACUMULADA"=DEPR_ACUMULADA,
                                    "VLR_CONTABIL"=VLR_CONTABIL, "VLR_MERCADO_TOTAL"=VLR_MERCADO_TOTAL,
                                    "ARR_DT_FLCX"=ARR_DT_FLCX, "NUM_FLUXOS"=NUM_FLUXOS, 
                                    "MESES_FLCX"=MESES_FLCX, "ARR_REC_TOT"=ARR_REC_TOT,
                                    "ARR_GST_TOT"=ARR_GST_TOT, "ARR_FLCX_LIQ_TOT"=ARR_FLCX_LIQ_TOT,
                                    "ARR_VP_FLCX_LIQ"=ARR_VP_FLCX_LIQ, "VLR_USO_ATIVO"=VLR_USO_ATIVO,
                                    "VLR_RECUP"=VLR_RECUP, "PERDA_IMPAIRMENT"=PERDA_IMPAIRMENT,
                                    "SEQ_ANOS_VU"=SEQ_ANOS_VU)
            
            return(lst_output)
      }
      
      
      
      
      ################################################################
      # Inicializar a lista com todos os inputs recebidos e todos    #
      #     os outputs calculados                                    #
      ################################################################
      LST_INPUTS_OUTPUTS <- fun_aux_lst_outputs_analise(DT_CPA, VLR_CPA, QTDE, PERC_RES, VU, VLR_VDA_UNIT, 
                                                        DT_BAL, TX_DESC, ARR_REC_UNIT, ARR_GST_UNIT)
      
      
      
      
      ########################################################
      # Lista com todos os outputs retornados pela classe    #
      ########################################################
      LST_OUTPUTS_EXERCICIO <- list(
            # --> Environment
            thisEnv = thisEnv,
            
            # --> Definir o 'getEnv', que da acesso aos dados do objeto
            getEnv = function(){
                  return(get("thisEnv", thisEnv))
            },
            
            
            
            ##############################################################################
            # Metodo para atualizar os valores da lista 'LST_INPUTS_OUTPUTS' e retornar  #
            #     a lista modificada                                                     #
            ##############################################################################
            atualiz_retornar_lista_input_output = function(DT_CPA, VLR_CPA, QTDE, PERC_RES, VU, 
                                                           VLR_VDA_UNIT, DT_BAL, TX_DESC, ARR_REC_UNIT, 
                                                           ARR_GST_UNIT){
                  # Lista com os valores atualizados
                  lista_nova <- fun_aux_lst_outputs_analise(DT_CPA, VLR_CPA, QTDE, PERC_RES, VU, VLR_VDA_UNIT, 
                                                            DT_BAL, TX_DESC, ARR_REC_UNIT, ARR_GST_UNIT)
                  
                  # Atualizar a lista 'LST_INPUTS_OUTPUTS'
                  assign("LST_INPUTS_OUTPUTS", lista_nova, thisEnv)
                  
                  # Retornar 'LST_INPUTS_OUTPUTS'
                  return(get("LST_INPUTS_OUTPUTS", thisEnv))
            },
            
            
            
            
            ###########################################################################
            # Metodos para retornar o texto com a equacao do valor em uso e a tabela  #
            #     os fluxos de caixa e valores presentes dos fluxos                   #
            ###########################################################################
            get_lst_inputs_outputs = function(){
                  return(get("LST_INPUTS_OUTPUTS", thisEnv))
            }
      )
      
      
      # Definir a lista LST_OUTPUTS_EXERCICIO no environmet atual
      assign("this", LST_OUTPUTS_EXERCICIO, envir=thisEnv)
      
      # Definir o nome da classe
      class(LST_OUTPUTS_EXERCICIO) <- append(class(LST_OUTPUTS_EXERCICIO), "teste_impairment_ativo")
      return(LST_OUTPUTS_EXERCICIO)
}




############################################################################
# Salvar em arquivos RDS a estrutura da classe "testeImpairmentAtivo" e    #
#     o objeto "OBJ_TESTE_IMPAIRM"                                         #
############################################################################
#saveRDS(object=testeImpairmentAtivo, file="testeImpairmentAtivo.RDS")
