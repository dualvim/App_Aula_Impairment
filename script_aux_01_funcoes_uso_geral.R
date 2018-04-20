#####################################################
# script_aux_01_funcoes_uso_geral.R                 #
#####################################################

##########################################
# Funcao para pegar o ano de uma data:   #
##########################################
fun_retorna_ano_data <- function(data_analise){
      # Converte, se necessario, 'data_analise' para Date e extrai o ano
      if(!class(data_analise) == "Date"){
            if(grepl(pattern="/", x=as.character(data_analise))){
                  data_analise <- as.Date(data_analise, format="%d/%m/%Y")
            } else{
                  data_analise <- as.Date(data_analise)
            }
      }
      ano_data <- as.numeric(format(data_analise, "%Y"))
      return(ano_data)
}




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


#################################################################
# Funcao para retornar os anos ao longo da vida util do ativo   #
#################################################################
fun_gerar_seq_anos_vida_util_ativo <- function(data_cpa, vida_util){
      ano_cpa <- fun_retorna_ano_data(data_analise=data_cpa)
      ult_ano <- ano_cpa + vida_util
      seq_anos <- seq(from=ano_cpa, to=ult_ano)
      return(seq_anos)
}




#################################################################
# Funcao que retorna um array com os anos dos fluxos de caixa   #
#################################################################
fun_gerar_seq_anos_fluxo_caixa <- function(arr_anos, ano_data_bal){
      anos_fc <- arr_anos > ano_data_bal
      arr_anos_fc <- arr_anos[anos_fc]
      return(arr_anos_fc)
}



get_tabela_fluxos_caixa = function(lst_dados){
      # Lista com os dados:
      #lst_dados <- objTesteImpairmentAtivo$get_lst_inputs_outputs()
      n <- lst_dados[["NUM_FLUXOS"]]
      datas_format <- format(lst_dados[["ARR_DT_FLCX"]], "%d/%m/%Y")
      
      # Arrays com os valores formatados:
      arr_rec <- sapply(X=lst_dados[["ARR_REC_TOT"]],
                        FUN=fun_aux_retornarValoresFormat,
                        tipo_valor="moeda")
      #Gastos
      arr_gst <- paste0("(", sapply(X=-lst_dados[["ARR_GST_TOT"]],
                                    FUN=fun_aux_retornarValoresFormat, tipo_valor="moeda"), ")")
      # Fluxos liquidos
      arr_fluxos <- sapply(X=lst_dados[["ARR_FLCX_LIQ_TOT"]],
                           FUN=fun_aux_retornarValoresFormat,
                           tipo_valor="moeda")
      
      # VP Fluxos de caixa:
      arr_vp <- sapply(X=lst_dados[["ARR_VP_FLCX_LIQ"]],
                       FUN=fun_aux_retornarValoresFormat,
                       tipo_valor="moeda")
      # Numero de meses:
      n_meses <- as.integer(lst_dados[["MESES_FLCX"]])
      
      
      # Criar uma matriz com os arrays acima:
      mat_results <- data.frame(arr_rec[1:n], arr_gst[1:n], arr_fluxos[1:n], n_meses[1:n], arr_vp[1:n])
      colnames(mat_results) <- c("Receita Bruta", "Gastos", "Fl.Cx. Liq.", "Meses", "VP FlCx Liq.")
      #
      #mat_results <- t(mat_results)
      rownames(mat_results) <- datas_format
      return(mat_results)
}


get_equacao_valor_uso = function(lst_dados){
      # Lista com os dados:
      #lst_dados <- get("LST_INPUTS_OUTPUTS", thisEnv)
      
      # Valores formatados
      fluxos <- sapply(X=lst_dados[["ARR_FLCX_LIQ_TOT"]],
                       FUN=fun_aux_retornarValoresFormat,
                       tipo_valor="geral")
      result <- fun_aux_retornarValoresFormat(vlr_inp=round(lst_dados[["VLR_USO_ATIVO"]], digits=2),
                                              tipo_valor="geral")
      taxa2 <- format(lst_dados[["TX_DESC"]], decimal.mark=",")
      #Numero de fluxos de caixa e arrays com o numero de meses
      n <- lst_dados[["NUM_FLUXOS"]]
      arr_meses <- lst_dados[["MESES_FLCX"]]
      
      # Gerar a expressao com a equacao do valor em uso:
      str_eq <- "$$VlrUso_{At} = "
      for(i in 1:n){
            str_eq <- paste0(str_eq, "\\frac{", fluxos[i], "}{(1+", taxa2, ")^\\frac{", arr_meses[i], "}{12}}")
            
            # Verificar se deve inserir um sinal de '+'
            if(i != n){
                  str_eq <- paste0(str_eq, " + ")
                  
                  # Verificar se deve inserir uma quebra de linha:
                  if((i == 4) | (i == 8)){
                        str_eq <- paste0(str_eq, "\\\\")
                  }
            } else{
                  # Caso em que o resultado sera escrito na emsma linha:
                  if((n %in% c(5, 6, 9, 10))){
                        str_eq <- paste0(str_eq, "\\ = \\ R\\$\\ ", result, "$$")
                  } 
                  # Caso em que o resultado eh escrito apos uma quebra de linha
                  else{
                        str_eq <- paste0(str_eq, "\\\\  = R\\$\\ ", result, "$$")
                  }
                  
                  
            }
      }
      
      # Inserir o resultado da equacao:
      
      
      # Retornar 'str_eq'
      return(str_eq)
}


###############################################
# Metodos que geram os graficos usados no App #
###############################################
graf_barra_vlr_cont_ativo = function(lst_dados){
      ############################################################
      # Gera um grafico com uma barra indicando o valor contabil #
      ############################################################
      # Lista com os dados:
      #lst_dados <- objTesteImpairmentAtivo$get_lst_inputs_outputs()
      valor_cpa_ativo <- lst_dados[["VLR_CPA"]]
      depr_acum <- lst_dados[["DEPR_ACUMULADA"]]
      data_balanco <- lst_dados[["DT_BAL"]]
      
      # Valor contabil do ativo imobiizado:
      vlr_cont <- valor_cpa_ativo - abs(depr_acum)
      # Posicao entre o valor de compra e o valor contabil:
      centro <- (valor_cpa_ativo + vlr_cont)/2
      
      # Proporcao do valor de 'centro' em relacao a 'valor_cpa_ativo'
      prop <- centro/valor_cpa_ativo
      prop_alta <- prop > 0.8
      
      # Valores formatados para a depreciacao acumulada, valor de compra e contabil do ativo
      depr_acumf <- paste0("Depr. Acum. = R$ (", format(depr_acum, decimal.mark=",", big.mark="."), ")")
      valor_cpaf <- paste("Valor do Ativo = R$", format(valor_cpa_ativo, decimal.mark=",", big.mark="."))
      vlr_contf <- paste("Valor contábil = R$", format(vlr_cont, decimal.mark=",", big.mark="."))
      
      # intervalo dos eixos X e Y:
      eixo_x <- c(0.0, 1.85)
      eixo_y <- c(0, valor_cpa_ativo*1.25)
      
      # Valores do eixo Y:
      vals_y <- pretty(eixo_y[1]:eixo_y[2], n=5)
      vals_yf <- format(vals_y, decimal.mark=",", big.mark=".", nsmall=0)
      
      
      ########################
      # Titulo do grafico:   #
      ########################
      # --> Caso a codificacao nativa de caracteres do sistema seja diferente de UTF-8 (geralmente)
      #           no caso do aplicativo estar sendo executado em Windows, converter os caracteres
      #           do titulo de UTF-8 para "MS-ANSI"
      if(getOption("encoding")!="UTF-8"){
            titulo_graf <- iconv(paste("Valor contábil em", data_balanco), from="UTF-8", to="MS-ANSI")
      } else{
            titulo_graf <- paste("Valor contábil em", data_balanco)
      }
      
      
      # Plotar um grafico de barras vazio
      par(family="serif")
      barplot(0, col="white", axes=FALSE, xlim=eixo_x, ylim=eixo_y, main=titulo_graf, xlab="", ylab="")
      # Inserir o eixo Y:
      axis(2, at=vals_y, labels=vals_yf, las=1, cex=0.85, cex.axis=0.85)
      # Inserir linhas de grade:
      abline(h=vals_y, lty=1, col="gray80")
      box(col="black", lty=1)
      
      # Plotar as barras com o valor do ativo e o valor contabil
      barplot(valor_cpa_ativo, col="red", axes=FALSE, add=TRUE)
      barplot(vlr_cont, col="blue", axes=FALSE, add=TRUE)
      
      # Escrever os textos
      text(x=1.2, y=vlr_cont*(1-prop_alta*0.05), pos=4, col="blue", labels=vlr_contf)
      text(x=1.2, y=centro, pos=4, col="red", labels=depr_acumf)
      text(x=1.2, y=valor_cpa_ativo*(1+prop_alta*0.05), pos=4, col="blue", labels=valor_cpaf)
}




graf_diagrama_fluxo_caixa = function(lst_dados, tit_graf=""){
      ############################################################
      # Gera um grafico com uma barra indicando o valor contabil #
      ############################################################
      # Lista com os dados e arrays com os inputs:
      #lst_dados <- objTesteImpairmentAtivo$get_lst_inputs_outputs()
      serie_receitas <- lst_dados[["ARR_REC_TOT"]]
      serie_gastos <- lst_dados[["ARR_GST_TOT"]]
      serie_fluxos <- lst_dados[["ARR_FLCX_LIQ_TOT"]]
      dt_fluxos <- lst_dados[["ARR_DT_FLCX"]]
      dt_fluxos2 <- format(dt_fluxos, "%d/%m/%Y")
      
      # --> Anos de cada uma das datas dos fluxos de caixa
      anos <- as.integer(format(dt_fluxos, "%Y"))
      
      # --> Arrays com os valores formatados
      # Receitas
      receitas_format <- sapply(X=lst_dados[["ARR_REC_TOT"]],
                                FUN=fun_aux_retornarValoresFormat,
                                tipo_valor="geral")
      # Gastos:
      gastos_format <- paste0("(", sapply(X=abs(lst_dados[["ARR_GST_TOT"]]),
                                          FUN=fun_aux_retornarValoresFormat,
                                          tipo_valor="geral"), ")")
      # Fluxos liquidos:
      fluxos_format <- sapply(X=lst_dados[["ARR_FLCX_LIQ_TOT"]],
                              FUN=fun_aux_retornarValoresFormat,
                              tipo_valor="geral")
      
      #############################################
      # Arrays com os parametros para o grafico   #
      #############################################
      # --> Serie com os valores dos anos no eixo X (em intervalos de 10)
      idx_anos <- seq(from=1, to=length(anos), by=1) * 10
      # --> Arrays com os intervalos dos eixos X e Y:
      eixo_x <- c(-7.5, max(idx_anos, na.rm=TRUE)+5)
      eixo_y <- c(min(0, min(serie_fluxos, na.rm=TRUE)*1.1), max(serie_receitas, na.rm=TRUE)*1.1)
      
      
      ################################
      # Gerar o Grafico:             #
      ################################
      par(family="serif")
      # --> Gerar um grafico Vazio:
      plot(x=0, y=0, type="n", axes=FALSE, xlim=eixo_x, ylim=eixo_y, xlab="", ylab="", main=tit_graf)
      
      # --> Desenhar o eixo X horizontal
      #arrows(x0=0, y0=0, x1=eixo_x[2], y1=0, col="black", lty=1, lwd=2)
      Arrows(x0=0, y0=0, x1=eixo_x[2], y1=0, col="black", lty=1, lwd=2, arr.type="triangle", arr.length=0.3, arr.width=0.15)
      
      # --> Escrever os anos:
      #mtext(text=anos, at=idx_anos, side=1, line=0, cex=1.2)
      text(x=idx_anos-1.5, y=-7, labels=dt_fluxos2, pos=1, xpd=TRUE, srt=30, cex=0.8)
      
      # --> Inserir as setas verticais representando os fluxos e os valores de cada um
      for(idx in 1:length(anos)){
            # Para evitar erros, nao plotar os fluxos caso o valor seja 0
            if(serie_receitas[idx] != 0){
                  # Seta com o fluxo da receita:
                  #arrows(x0=idx_anos[idx], y0=0, x1=idx_anos[idx], y1=serie_receitas[idx], col="green4", lty=2, lwd=1, length=0.10)
                  Arrows(x0=idx_anos[idx], y0=0, x1=idx_anos[idx], y1=serie_receitas[idx], col="green4", lty=2, lwd=1, arr.type="triangle", arr.length=0.3, arr.width=0.15)
                  
                  # Texto com o valor da receita
                  text(x=idx_anos[idx], y=serie_receitas[idx], pos=3, labels=receitas_format[idx], cex=0.8, col="green4")
            }
            
            if(serie_gastos[idx] != 0){
                  # Seta do fluxo de caixa liquido:
                  #arrows(x0=idx_anos[idx], y0=0, x1=idx_anos[idx], y1=serie_fluxos[idx], col="blue", lty=1, lwd=2, length=0.10)
                  Arrows(x0=idx_anos[idx], y0=0, x1=idx_anos[idx], y1=serie_fluxos[idx], col="blue", lty=2, lwd=1, arr.type="triangle", arr.length=0.3, arr.width=0.15)
                  
                  # Desenhar uma chave representando o montante dos gastos:
                  brackets(x1=idx_anos[idx]-0.5, y1=serie_fluxos[idx], x2=idx_anos[idx]-0.5, y2=serie_receitas[idx], 
                           lwd=1, h=0.5, ticks=0.5, curvature=0.5, type=2, col="red")
                  # Centro da chave desenhada:
                  centro_chave <- serie_receitas[idx]+(serie_gastos[idx]/2)
                  
                  # Texto com o valor dos gastos:
                  text(x=idx_anos[idx]-0.6, y=centro_chave, pos=2, labels=gastos_format[idx], cex=0.8, col="red")
                  
                  # Escrever o valor do fluxo de caixa
                  text(x=idx_anos[idx], y=serie_fluxos[idx], pos=2, labels=fluxos_format[idx], cex=0.8, col="blue")
            }
            
      }
}




graf_barras_valores_imobilizado = function(lst_dados){
      ############################################################################
      # Grafico de barras com o valor contabil, valor em uso e valor de mercado  #
      ############################################################################
      # Lista com os dados e arrays com os inputs:
      #lst_dados <- objTesteImpairmentAtivo$get_lst_inputs_outputs()
      valor_cont <- lst_dados[["VLR_CONTABIL"]]
      valor_uso <- lst_dados[["VLR_USO_ATIVO"]]
      valor_mercado <- lst_dados[["VLR_MERCADO_TOTAL"]]
      valor_perdas <- lst_dados[["PERDA_IMPAIRMENT"]]
      
      # Valor contabil novo
      valor_cont2 <- valor_cont + valor_perdas
      perda_format <- fun_aux_retornarValoresFormat(vlr_inp=trunc(valor_perdas), tipo_valor="moeda")
      valor_cont2f <- fun_aux_retornarValoresFormat(vlr_inp=trunc(valor_cont2), tipo_valor="moeda")
      
      # Array com os 4 valores do ativo imobilizado:
      arr_vals_imob <- c(valor_uso, valor_mercado, valor_cont)
      # Valores formatados:
      arr_vals_imobf <- sapply(X=trunc(arr_vals_imob),
                               FUN=fun_aux_retornarValoresFormat,
                               tipo_valor="moeda")
      
      # Ajustar o valor de arr_vals_imob[3] de acordo com o fato de haver ganho ou nao
      impairment <- abs(valor_perdas) > 0
      # Titulo do grafico: Ira depender do valor de impairment
      titulo_graf <- c(rep("Impairment", times=impairment), rep("Não há reconhecimento de perdas", times=!impairment))
      
      # --> Valores do eixo Y:
      eixo_y <- c(0, max(arr_vals_imob)*1.25)
      # Valores do eixo Y:
      vals_y <- pretty(eixo_y[1]:eixo_y[2], n=5)
      vals_yf <- format(vals_y, decimal.mark=",", big.mark=".", nsmall=0)
      
      # Valores das barras no eixo X:
      pos_barras_x <- as.numeric(barplot(arr_vals_imob, plot=FALSE))
      # Nomes das barras do grafico:
      nomes_barras <- c("Valor em Uso", "Valor de Mercado", "Valor Contábil")
      #cores das barras
      cores_barras <- c("yellow", "green4", "red")
      
      # Plotar um grafico de barras vazio
      par(family="serif")
      barplot(rep(0, times=length(arr_vals_imob)), names.arg=nomes_barras, col="white", axes=FALSE, ylim=eixo_y, main=titulo_graf, xlab="", ylab="")
      # Inserir o eixo Y:
      axis(2, at=vals_y, labels=vals_yf, las=1, cex=0.85, cex.axis=0.85)
      # Inserir linhas de grade:
      abline(h=vals_y, lty=1, col="gray80")
      box(col="black", lty=1)
      
      # --> Caso haja o reconhecimento de perdas, inserir uma legenda e modificar o valor contabil
      if(impairment){
            legend("topleft", fill=c("red", "blue"), legend=c("Perda Impairment", "ValorContábil"), horiz=TRUE)
            arr_vals_imobf[3] <- paste(arr_vals_imobf[3], perda_format, "=", valor_cont2f)
      }
      
      # Plotar as barras com o valor do ativo e o valor contabil
      barplot(arr_vals_imob, col=cores_barras, axes=FALSE, add=TRUE)
      barplot(c(0,0,valor_cont2), col="blue", axes=FALSE, add=TRUE)
      
      # Escrever os textos
      text(x=pos_barras_x, y=arr_vals_imob, pos=3, col="black", labels=arr_vals_imobf)
      
}
