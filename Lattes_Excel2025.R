library(shiny)
library(shinydashboard)
library(pdftools)
library(stringr)
library(dplyr)
library(writexl)
library(fresh)


###########################################################
# Função para extrair nome do currículo
extrair_nome <- function(text_data) {
  nome_extraido <- str_extract(text_data, "\\+\\n{4,}\\s+([^\\n]+)")
  nome_extraido <- str_replace(nome_extraido, "\\+\\n{4,}\\s+", "")
  nome_extraido <- gsub(" ", "_", nome_extraido)
  return(nome_extraido)
}

# Função para extrair seções
extrai_secao <- function(text_data, inicio, fim) {
  lines <- str_split(text_data, "\\n")[[1]]
  lines <- lines[lines != ""]
  
  inicio_idx <- which(str_detect(lines, inicio))
  fim_idx <- which(str_detect(lines, fim))
  
  if (length(inicio_idx) > 0 && length(fim_idx) > 0 && inicio_idx < fim_idx) {
    secao <- str_trim(lines[(inicio_idx + 1):(fim_idx - 1)])
    secao <- paste(secao, collapse = "\n")
    secao <- str_replace(secao, "Ordenar por\\s+Ordem Cronológica\\s+1.\\s+", "")
    artigos <- str_split(secao, "\\n\\d+\\.\\s+")[[1]]
    artigos <- artigos[artigos != ""]
    artigos <- str_trim(artigos)
    return(artigos)
  }
  return(NULL)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$h4("Extrair dados do Lattes Para Excel - 2025"), titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem("Input", tabName = "up", icon = icon("upload"),
                              fileInput("file", label = h3("Arquivo Lattes PDF")),
                              helpText(tags$h5("Escolha um Lattes em formato PDF."))
                     ),  
                     menuItem("Baixar dados", tabName = "down", icon = icon("download"),
                              tags$h5("Baixar dados em formato EXCEL"),
                              downloadButton("downloadData", "Download")
                     ),
                     menuItem("Informações", tabName = "info", icon = icon("info-circle"),
                              htmlOutput("text")
                     )
                   )
  ),
  dashboardBody(
    img(src = "logo_lattes.png", height = 200, width = 400),
    textOutput("aviso"),
    uiOutput("gif_output") # Área onde os GIFs serão mostrados
  )
)

# Server
server <- function(input, output, session) {
  # Definir o horário de entrada para cada sessão de usuário
  session$userData$hora_inicial <- Sys.time()

  dados <- reactive({
    file <- input$file
    if (is.null(file)) return(NULL)
    text_data <- pdf_text(file$datapath)
    return(text_data)
  })
  
  nome_curriculo <- reactive({
    if (is.null(dados())) return("")
    extrair_nome(dados())
  })
  
  secao1 <- reactive({
    if (is.null(dados())) return(NULL)
    extrai_secao(dados(), "Artigos completos publicados em periódicos", "Capítulos de livros publicados")
  })
  
  secao2 <- reactive({
    if (is.null(dados())) return(NULL)
    extrai_secao(dados(), "Capítulos de livros publicados", "Textos em jornais de notícias/revistas") 
  })
  
  secao3 <- reactive({
    if (is.null(dados())) return(NULL)
    extrai_secao(dados(), "Textos em jornais de notícias/revistas", "Trabalhos completos publicados em anais de congressos")
  })
  
  secao4 <- reactive({
    if (is.null(dados())) return(NULL)
    extrai_secao(dados(), "Trabalhos completos publicados em anais de congressos", "Resumos publicados em anais de congressos")
  })
  
  secao5 <- reactive({
    if (is.null(dados())) return(NULL)
    extrai_secao(dados(), "Resumos publicados em anais de congressos", "Apresentações de Trabalho")
  })
  
  secao6 <- reactive({
    if (is.null(dados())) return(NULL)
    extrai_secao(dados(), "Apresentações de Trabalho", "Outras produções bibliográficas")
  })
  
  secao7 <- reactive({
    if (is.null(dados())) return(NULL)
    extrai_secao(dados(), "Outras produções bibliográficas", "Produção técnica")
  })
  
  output$aviso <- renderText({
    if (is.null(dados())) return()
    nome <- nome_curriculo()
    paste("Por favor, clique em 'download' e baixe o arquivo de", nome)
  })
  
  output$text <- renderText({
    HTML("<p align='center'><strong>Sobre</strong></p>
    <h6 align='center'>O arquivo PDF pode ser obtido através do<br/> 
                          Opera Mine Browser, abra seu curriculo lattes<br/> 
                          e clique com o botão direito do mouse.<br/>
                          Você verá a opção 'Save as PDF' <p/>
                          
    <h6 align='center'>Esse soft foi criado para auxiliar<br/> 
                          a organização de dados de produções<br/> bibliográficas
                          do arquivo lattes.<p/>
    <h6 align='center'>Com as as últimas atualizações, esse software <br/>agora extrai também informações das seguintes<br/> categorias:</h6>
    <ol>
      <li>Artigos completos publicados <br/>em periódicos</li>
      <li>Capítulos de livros publicados</li>
      <li>Textos em jornais de <br/>notícias/revistas</li>
      <li>Trabalhos completos publicados <br/>em anais de congressos</li>
      <li>Resumos publicados em anais <br/>de congressos</li>
      <li>Apresentações de Trabalho</li>
      <li>Outras produções <br/>bibliográficas</li>
    </ol>
    <p>Autor: Thiago da Luz Ferreira<br/>
    E-mail: thiagolight27@gmail.com</p>
         Esse App foi atualizado em: 14/02/2025</p>")
  })
  
  # Atualiza o tempo a cada segundo
  output$gif_output <- renderUI({
    invalidateLater(500, session)  # Atualiza a cada 1 segundo
    
    tempo_atual <- Sys.time()
    diferenca_minutos <- as.numeric(difftime(tempo_atual, session$userData$hora_inicial, units = "mins"))
    
    # Definição das condições para exibição dos GIFs
    if (diferenca_minutos >= 0.1 && diferenca_minutos < 0.175) {
      tags$img(src = "drink_water1.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 10 && diferenca_minutos < 10.5) {
      tags$img(src = "cute2.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 10.5 && diferenca_minutos < 30) {
      tags$img(src = "relax7.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 30 && diferenca_minutos < 31) {
      tags$img(src = "drink_water2.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 40 && diferenca_minutos < 60) {
      tags$img(src = "relax8.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 60 && diferenca_minutos < 60.15) {
      tags$img(src = "relax6.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 65 && diferenca_minutos < 75) {
      tags$img(src = "relax8.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 75 && diferenca_minutos < 76) {
      tags$img(src = "drink_water4.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 76 && diferenca_minutos < 85) {
      tags$img(src = "relax8.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 85 && diferenca_minutos < 85.5) {
      tags$img(src = "cute3.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 90 && diferenca_minutos < 95) {
      tags$img(src = "relax4.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 100 && diferenca_minutos < 100.075) {
      tags$img(src = "drink_water1.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 105 && diferenca_minutos < 105.5) {
      tags$img(src = "relax2.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 105.5 && diferenca_minutos < 120) {
      tags$img(src = "relax7.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 120 && diferenca_minutos < 130) {
      tags$img(src = "relax8.gif", height = "300px", width = "300px")
    } else if (diferenca_minutos >= 130 && diferenca_minutos < 130.5) {
      tags$img(src = "drink_water3.gif", height = "300px", width = "300px")

    } else {
      NULL  # Não exibe nenhum GIF se não estiver dentro dos intervalos definidos
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(nome_curriculo(), ".xlsx")
    },
    content = function(file) {
      lista_de_tabelas <- list(
        "Artigos completos publicados em periódicos" = data.frame(Conteudo = secao1(), stringsAsFactors = FALSE),
        "Capítulos de livros publicados" = data.frame(Conteudo = secao2(), stringsAsFactors = FALSE),
        "Textos em jornais de notícias/revistas" = data.frame(Conteudo = secao3(), stringsAsFactors = FALSE),
        "Trabalhos completos publicados em anais de congressos" = data.frame(Conteudo = secao4(), stringsAsFactors = FALSE),
        "Resumos publicados em anais de congressos" = data.frame(Conteudo = secao5(), stringsAsFactors = FALSE),
        "Apresentações de Trabalho" = data.frame(Conteudo = secao6(), stringsAsFactors = FALSE),
        "Outras produções bibliográficas" = data.frame(Conteudo = secao7(), stringsAsFactors = FALSE)
      )
      write_xlsx(lista_de_tabelas, file)
    }
  )
}

shinyApp(ui = ui, server = server)
