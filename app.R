##########################
# app.R                  #
##########################
# --> Versao nova do aplicativo em Shiny para a aula de Impairment de ativos
# --> A estrutura interna do aplicativo foi completamente refeita uma versao mais estavel
library(shiny)
library(markdown)
suppressMessages(require(pBrackets)) 
suppressMessages(require(shape)) 
options(digits=10, scipen=99)



# --> array indicando a cor dos textos com na 3a aba
COR_TEXTO <- c("<span style=\"color:blue\">", "<span style=\"color:red\">")

# --> Array com o texto indicando se houve perda ou nao:
RESULT_TESTE <- c("<span style=\"color:blue\"> Não reconhecer perdas!</span>",
                  "<span style=\"color:red\"> Reconhecer <em>Impairment</em> </span>")




#############################################################################
# Carregar a classe 'testeImpairmentAtivo' e o objeto 'OBJ_TESTE_IMPAIRM'   #
#############################################################################
source("script_aux_01_funcoes_uso_geral.R")
source("script_aux_02_classe_testeImpairmentAtivo.R")
source("script_aux_03_gerar_respostas_exercicio.R")


# Gerar 'LST_VALS_INI' e 'OBJ_TESTE_IMPAIRM' a partir do valor de IDX:
IDX <<- 1
LST_VALS_INI <<- LST_LST_VALS_INI[[IDX]]
OBJ_TESTE_IMPAIRM <<- retornar_obj_testeImpairmetAtivo_exerc(nome_ativo=ARR_NOMES_ONIBUS[IDX])




###########################################
# Interface grafica (UI) do Aplicativo    #
###########################################
ui <- fluidPage(
      ###############################
      # Menu Lateral com os inputs  #
      ###############################
      sidebarPanel(
            tabsetPanel(
                  # Aba com os inputs sobre o valor contabilo
                  tabPanel("Dados Imobilizado",
                           #
                           selectInput(input="nome_ativo", label="Nome do ativo", choices=ARR_NOMES_ONIBUS, selected=ARR_NOMES_ONIBUS[IDX]),
                           
                           h4("Data do Balanço"),
                           uiOutput("ui_data_bal"),
                           
                           h4("Dados do Ativo:"),
                           dateInput(input="data_cpa", label="Data da Compra (30/12/20XX)", value=LST_VALS_INI[["DATA_CPA"]], min=as.Date("2010-01-01"), 
                                     max=Sys.Date(), startview="year", format="dd/mm/yyyy", language="pt-BR"),
                           
                           numericInput(inputId="valor_cpa", label="Valor de Compra (unitário):", value=LST_VALS_INI[["VLR_COMPRA"]], min=10000, max=2500000, step=1000),
                           verbatimTextOutput("out_val_cpa_unt_form"),
                           numericInput(inputId="qtde", label="Quantidade", value=LST_VALS_INI[["QTDE"]], min=1, max=5, step=1),
                           numericInput(inputId="vida_util", label="Vida Útil (anos)", value=LST_VALS_INI[["VIDA_UTIL"]], min=5, max=15, step=1),
                           sliderInput(inputId="perc_vlr_res", label="Valor Residual (%)", value=LST_VALS_INI[["PERC_VLR_RES"]]*100, min=0, max=100, step=1, post="%"),
                           verbatimTextOutput("out_val_resid_unt_form"),
                           verbatimTextOutput("out_val_deprec_unt_form"),
                           
                           # Dados para o teste de Impairment:
                           numericInput(inputId="taxa_aa", label="Taxa de desconto (%a.a.):", value=LST_VALS_INI[["TX_DESC"]], min=0.06, max=0.25, step=0.0025),   
                           verbatimTextOutput("out_taxa_desc_aa"),
                           numericInput(inputId="valor_vda", label="Valor de venda (unitário):", value=LST_VALS_INI[["VLR_MERCADO"]], min=10000, max=2500000, step=1000),
                           verbatimTextOutput("out_val_vda_unt_form")
                  ),
                  tabPanel("Receitas",
                           h4("Digite as Receitas Brutas (por unidade)"),
                           lapply(1:10, function(n){
                                 numericInput(paste0("rec", n), label=paste("Receita Bruta"), value=LST_VALS_INI[["SERIE_RECEITAS"]][n], min=50000, max=500000, step=10000)
                           })
                  ),
                  tabPanel("Gastos",
                           h4("Digite os gastos (por unidade)"),
                           #uiOutput("ui_gastos")
                           lapply(1:10, function(n){
                                 numericInput(paste0("gasto", n), label=paste("Gasto"), value=LST_VALS_INI[["SERIE_GASTOS"]][n], min=-500000, max=0, step=10000)
                           })
                  )
            )
      ),
      
      ######################
      # Pagina principal    #
      #######################
      mainPanel(
            h2(textOutput("titulo")),
            
            navbarPage("Analises:",
                       tabPanel("Valor Contábil",
                                h2(textOutput("out_data_balanco")),
                                h4(textOutput("out_val_cpa")),
                                h4(textOutput("out_val_resid")),
                                h4(textOutput("out_valor_deprec")),
                                br(),
                                
                                h4(htmlOutput("out_deprec_acum")),
                                h4(textOutput("out_valor_contabil")),
                                br(),
                                
                                plotOutput("out_graf_vlr_cont")
                       ),
                       tabPanel("Valor em Uso",
                                h2("Diagrama de Fluxos de Caixa"),
                                plotOutput("out_graf_fluxos_cx", width="100%"),
                                br(),
                                tableOutput("out_tabela_fluxos"),
                                br(),
                                
                                h2(htmlOutput("out_valor_uso")),
                                uiOutput("calculo_vlr_uso")
                       ),
                       
                       tabPanel(HTML("Valor Recuperável e <em>Impairment</em>"),
                                h4(htmlOutput("out_valor_uso2")),
                                h4(htmlOutput("out_valor_mercado")),
                                h4(htmlOutput("out_valor_recuperavel_imob")),
                                br(),
                                h4(htmlOutput("out_valor_perdas")),
                                plotOutput("out_graf_vals_imob")
                                
                       )
            )
      ),
      
      ##############################
      # Inserir a documentacao:    #
      ##############################
      fluidRow(column(12,includeMarkdown("Documentacao.md")))
)




###########################################
# Servidor (Server) do Aplicativo         #
###########################################
server <- function(input, output, session) {
      ##############################################################
      # Atualizar os outputs conforme a escolha no menu suspenso   #
      ##############################################################
      observe({
            IDX <<- which(ARR_NOMES_ONIBUS==input$nome_ativo)
            LST_VALS_INI <<- LST_LST_VALS_INI[[IDX]]
            OBJ_TESTE_IMPAIRM <<- retornar_obj_testeImpairmetAtivo_exerc(nome_ativo=ARR_NOMES_ONIBUS[IDX])
            
            # Atualizar os campos com os inputs
            updateDateInput(session, inputId="data_cpa", value=LST_VALS_INI[["DATA_CPA"]])
            updateNumericInput(session, inputId="valor_cpa", value=LST_VALS_INI[["VLR_COMPRA"]])
            updateNumericInput(session, inputId="qtde", value=LST_VALS_INI[["QTDE"]])
            updateNumericInput(session, inputId="vida_util", value=LST_VALS_INI[["VIDA_UTIL"]])
            updateSliderInput(session, inputId="perc_vlr_res", value=LST_VALS_INI[["PERC_VLR_RES"]]*100)
            updateNumericInput(session, inputId="taxa_aa", value=LST_VALS_INI[["TX_DESC"]])
            updateNumericInput(session, inputId="valor_vda", value=LST_VALS_INI[["VLR_MERCADO"]])
            
            # Atualizar os campos com os valores das receitas e gastos:
            lapply(1:10, function(n){
                  if(n <= length(LST_VALS_INI[["SERIE_RECEITAS"]])){
                        updateNumericInput(session, inputId=paste0("rec", n),
                                           label="Receita Bruta",
                                           value=LST_VALS_INI[["SERIE_RECEITAS"]][n],
                                           min=50000, max=500000, step=1000)
                        updateNumericInput(session, inputId=paste0("gasto", n),
                                           label="Gastos",
                                           value=LST_VALS_INI[["SERIE_GASTOS"]][n],
                                           min=-500000, max=0, step=1000)
                  } else{
                        updateNumericInput(session, inputId=paste0("rec", n), label="", value=0, min=50000, max=0, step=0)
                        updateNumericInput(session, inputId=paste0("gasto", n), label="", value=0, min=0, max=0)
                  }
            })
            
      })
      
      
      ########################################################
      # Valores formatados e apresentados na barra lateral:  #
      ########################################################
      output$out_taxa_desc_aa <- renderText({paste0(format(input$taxa_aa*100, decimal.mark=",", nsmall=2), "% a.a.")})
      output$out_val_cpa_unt_form <- renderText({paste("R$", format(input$valor_cpa, decimal.mark=",", big.mark="."))})
      output$out_val_vda_unt_form <- renderText({paste("R$", format(input$valor_vda, decimal.mark=",", big.mark="."))})
      output$out_val_resid_unt_form <- renderText({paste("Valor Residual (Unit.) = R$", format(round(input$valor_cpa*input$perc_vlr_res/100, digits=2), decimal.mark=",", big.mark="."))})
      output$out_val_deprec_unt_form <- renderText({paste("Valor depreciável (Unit.)= R$", format(input$valor_cpa*(1-input$perc_vlr_res/100), decimal.mark=",", big.mark="."))})
      
      
      
      
      ################################################################################
      # Funcoes reativas para gerar os arrays com os valores das receitas e gastos   #
      ################################################################################
      array_receitas <- reactive({c(input$rec1, input$rec2, input$rec3, input$rec4, input$rec5, 
                                    input$rec6, input$rec7, input$rec8, input$rec9, input$rec10)})
      
      array_gastos <- reactive({c(input$gasto1, input$gasto2, input$gasto3, input$gasto4, input$gasto5, 
                                  input$gasto6, input$gasto7, input$gasto8, input$gasto9, input$gasto10)})
      
      
      
      
      ###########################################################################
      # Funcao Reativa para atualizar os valores de 'LST_DADOS_EXERC' conforme  #
      #     forem alterados os valores dos inputs                               #
      ###########################################################################
      
      LST_DADOS_EXERC <- reactive({
            OBJ_TESTE_IMPAIRM$atualiz_retornar_lista_input_output(
                              DT_CPA=input$data_cpa,
                              VLR_CPA=input$valor_cpa,
                              QTDE=input$qtde,
                              PERC_RES=input$perc_vlr_res,
                              VU=input$vida_util,
                              VLR_VDA_UNIT=input$valor_vda,
                              DT_BAL=as.Date(input$data_bal, format="%d/%m/%Y"),
                              TX_DESC=input$taxa_aa,
                              ARR_REC_UNIT=array_receitas(),
                              ARR_GST_UNIT=array_gastos())
      })
      
      
      
      
      
      #################################################
      # Funcoes reativas para gerar as UI dos inputs  #
      #################################################
      # Funcao reativa com os anos dos fluxos de caixa:
      # --> Sequencia com anos referentes a vida util do ativo
      seq_anos_vu_ativo <- reactive({fun_gerar_seq_anos_vida_util_ativo(data_cpa=input$data_cpa, vida_util=input$vida_util)})
      
      output$ui_data_bal <- renderUI({
            seq_datas_bal <- paste0("31/12/", seq_anos_vu_ativo())
            n <- length(seq_datas_bal) - 2
            data_2016 <- "31/12/2016" %in% seq_datas_bal 
            data_padrao <- c(rep("31/12/2016", times=data_2016), rep(seq_datas_bal[2], times=!data_2016))
            selectInput(input="data_bal", label="Data do Balanço", choices=seq_datas_bal[1:n], selected=data_padrao)
      })
      
      
      # --> Atualizar os inputs numericos com as receitas e gastos sempre que alterar o numero de fluxos
      datas_fluxos_cx_format <- reactive({format(LST_DADOS_EXERC()$ARR_DT_FLCX, "%d/%m/%Y")})
      observe({
            lapply(1:10, function(n){
                  if(n<=LST_DADOS_EXERC()$NUM_FLUXOS){
                        updateNumericInput(session, inputId=paste0("rec", n),
                                    label=paste("Receita Bruta", datas_fluxos_cx_format()[n]),
                                    value=array_receitas()[n],
                                    min=50000, max=500000, step=1000)
                        updateNumericInput(session, inputId=paste0("gasto", n),
                                    label=paste("Gastos", datas_fluxos_cx_format()[n]),
                                    value=array_gastos()[n],
                                    min=-500000, max=0, step=1000)
                  } else{
                        updateNumericInput(session, inputId=paste0("rec", n), label="", value=0, min=50000, max=0, step=0)
                        updateNumericInput(session, inputId=paste0("gasto", n), label="", value=0, min=0, max=0)
                  }
            })
      })
      
      
      
      
      ###########################################
      # Outputs primeiro painel ("Analises:")   #
      ###########################################
      # Data do balanco em que esta sendo realizad o teste de impairment:
      output$out_data_balanco <- renderText({
            texto_data <- tryCatch(paste("Valor contábil em", input$data_bal),
                                   error=function(e){
                                         return("Valor contábil em 31/12/20XX")
                                   })
            texto_data
      })
      
      # Outputs referentes a primeira aba: Valor contabil, depreciacao e similares
      output$out_val_cpa <- renderText({paste("Valor pago pelo(s)", rep(input$qtde,times=input$qtde>1), "ativo(s): R$", format(input$valor_cpa*input$qtde, decimal.mark=",", big.mark="."))})
      output$out_val_resid <- renderText({paste("Valor Residual", paste0("(", input$perc_vlr_res, "% do valor de compra):"), format(LST_DADOS_EXERC()$VLR_RESID, decimal.mark=",", big.mark="."))})
      output$out_valor_deprec <- renderText({paste("Valor depreciável do ativo: R$", format((LST_DADOS_EXERC()$VLR_CPA*LST_DADOS_EXERC()$QTDE)-LST_DADOS_EXERC()$VLR_RESID, decimal.mark=",", big.mark="."))})
      output$out_deprec_acum <- renderText({paste("Depreciação acumulada: <span style=\"color:red\"> R$", paste0("(", format(LST_DADOS_EXERC()$DEPR_ACUMULADA, decimal.mark=",", big.mark="."), ") </span>"))})
      output$out_valor_contabil <- renderText({paste("Valor contábil do(s) ativo(s): R$", format(LST_DADOS_EXERC()$VLR_CONTABIL, decimal.mark=",", big.mark="."))})
      
      # Grafico com o valor contabil:
      #output$out_graf_vlr_cont <- renderPlot({OBJ_TESTE_IMPAIRM$graf_barra_vlr_cont_ativo()})
      output$out_graf_vlr_cont <- renderPlot({graf_barra_vlr_cont_ativo(LST_DADOS_EXERC())})
      
      
      #################################################
      # Outputs do segundo painel ("Valor em Uso:")   #
      #################################################
      output$out_graf_fluxos_cx <- renderPlot({
            #OBJ_TESTE_IMPAIRM$graf_diagrama_fluxo_caixa()
            graf_diagrama_fluxo_caixa(LST_DADOS_EXERC())
      })
      
      # Tabela com os fluxos de caixa:
      #output$out_tabela_fluxos <- renderTable({OBJ_TESTE_IMPAIRM$get_tabela_fluxos_caixa()}, rownames=TRUE)
      output$out_tabela_fluxos <- renderTable({get_tabela_fluxos_caixa(LST_DADOS_EXERC())}, rownames=TRUE)
      
      # Valor em uso do ativo
      output$out_valor_uso <- renderText({paste("Valor em Uso dos Ativos = <strong>", format(LST_DADOS_EXERC()$VLR_USO_ATIVO, decimal.mark=",", big.mark=".", nsmall=2), "</strong>")})
      # Equacao demonstrando o calculo do valor:
      output$calculo_vlr_uso <- renderUI({withMathJax(
            helpText(h2(get_equacao_valor_uso(LST_DADOS_EXERC()))))
      }) 
      
      
      ###################################################################
      # Outputs do terceiro painel ("Valor Recuperavel e Impairment")   #
      ###################################################################
      impairment <- reactive({LST_DADOS_EXERC()$PERDA_IMPAIRMENT < 0})
      output$out_valor_uso2 <- renderText({paste0(COR_TEXTO[impairment()+1], "Valor em uso = R$ ", format(LST_DADOS_EXERC()$VLR_USO_ATIVO, decimal.mark=",", big.mark=".", nsmall=2), "</span>")})
      output$out_valor_mercado <- renderText({paste0(COR_TEXTO[impairment()+1], "Valor de mercado = R$ ", format(LST_DADOS_EXERC()$VLR_MERCADO_TOTAL, decimal.mark=",", big.mark=".", nsmall=2), "</span>")})
      output$out_valor_recuperavel_imob <- renderText({paste0(COR_TEXTO[impairment()+1], "Valor Recuperável = min([valor em uso], [valor de mercado]) = R$ ", format(LST_DADOS_EXERC()$VLR_RECUP, decimal.mark=",", big.mark=".", nsmall=2), "</span>")})
      output$out_valor_perdas <- renderText({paste0(COR_TEXTO[impairment()+1], "Perda a reconhecer = R$ ", format(LST_DADOS_EXERC()$PERDA_IMPAIRMENT, decimal.mark=",", big.mark=".", nsmall=2), "</span>")})
      # Grafico
      output$out_graf_vals_imob <- renderPlot({graf_barras_valores_imobilizado(LST_DADOS_EXERC())})
}




###################################
# Executar aplicativo:            #
###################################
shinyApp(ui=ui, server=server, options=list(host="0.0.0.0",port=8080))

