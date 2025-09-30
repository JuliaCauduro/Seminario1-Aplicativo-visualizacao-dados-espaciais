library(shiny)
library(bslib)
library(leaflet)
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(DT)
library(writexl)
library(classInt)


# Carrega e executa o script de transformação
source("Transformacao.R")

ui <- fluidPage(
  
  tags$head (
    tags$style(HTML("
      body {
        background-color: #eef4fa;}
      .navbar {
          background-color: #2b6cb0;
          color: #ffffff; 
          font-size: 15px;
            /* Cor padrão das abas */
      .nav li a {
        color: #ffffff !important; /* texto branco na barra */
      }
      /* Hover e aba ativa */
      .nav li a:hover, 
      .nav li.active a {
        background-color: #63b3ed !important; /* azul claro */
        color: #1e3a8a !important; /* azul escuro no texto para contraste */
        font-weight: bold;
      }
      }
      .plot-container {
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.3); 
        padding: 10px; 
        background-color: #f7fbff; 
        border-radius: 5px; 
      }
      .sobre-container {
        display: flex;
        justify-content: center; /* Centraliza horizontalmente */
        height: 100vh; /* Altura da tela */
      }
       .sobre-square {
        width: 1100px; /* Largura do quadrado */
        height: 550px; /* Altura do quadrado */
        background-color: #ffffff; /* Cor do fundo do quadrado */
        box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.5); /* Sombra */
        position: relative;
        border-radius: 5px; /* Bordas arredondadas */
      }
      .sobre-top-color {
        height: 1%; /* Metade do quadrado */
        background-color: #3182ce; /* Cor da parte de cima */
        border-top-left-radius: 5px; /* Bordas arredondadas na parte superior */
        border-top-right-radius: 5px;
      }
      .sobre-text-titulo {
        position: absolute;
        left: 50%;
        top: 10%;
        transform: translate(-50%, -50%); /* Ajusta para centralização */
        color: #2b6cb0; /* Cor do texto */
        font-weight: bold;
        font-size: 25px; /* Tamanho do texto */
      } 
      .sobre-text {
        display: flex;
        justify-content: center;
        position: absolute;
        left: 5%;
        top: 20%;
        color: #1a202c; /* Cor do texto */
        font-size: 15px; /* Tamanho do texto */
        overflow-y: auto; /* permite rolagem vertical */
        max-height: 590px; /* limita altura para caber dentro da caixa */
        padding-right: 10px; /* evita que o scroll fique colado na borda */
      } 
      .sobre-list {
        list-style-type: disc; /* Estilo da lista */
        padding-left: 20px; /* Espaçamento da lista */
        text-align: left; /* Alinhamento do texto */
      }
      .sobre-text-maior {
        display: flex;
        justify-content: center;
        position: absolute;
        left: 5%;
        top: 20%;
        color: #1a202c;
        font-size: 18px; /* Aumentado de 15px para 18px */
        overflow-y: auto;
        max-height: 590px;
        padding-right: 10px;
      }
      "))),
  
  br(),
  
  tags$div(
    style = "text-align: center; margin-bottom: 10px; font-size: 30px; color: #2b6cb0;",
    "Educação pelo mundo"
  ),
  
  navbarPage(
    title = NULL,  
    
    tabPanel(
      "Sobre o trabalho",
      fluidPage(
        div(class = "sobre-container",
            div(class = "sobre-square",
                div(class = "sobre-top-color"),
                div(class = "sobre-text-titulo", "Informações do Trabalho:"),
                div(class = "sobre-text-maior",
                    tags$ul(
                      tags$li(tags$b("Autores: "), "Catarina Pegoraro, Júlia Cauduro e Lucas Feijó"),
                      tags$li(tags$b("Banco de dados usado: "), "World Education Data"),
                      tags$li(tags$b("Última vez atualizado: "), "2023"),
                      tags$li(tags$b("Link do banco de dados: "), 
                              tags$a(href = "https://www.kaggle.com/datasets/nelgiriyewithana/world-educational-data", "Clique aqui para acessar")),
                      tags$li(tags$b("Objetivo: "), 
                              tags$div(
                                style = "text-align: justify; display: inline;",
                                "Esse trabalho tem como objetivo analisar a educação ao redor do mundo por meio de algumas variáveis, como taxa fora da escola, taxa de conclusão dos anos de escolaridade, provas de proficiência e taxa de alfabetização. Assim, torna-se possível a comparação da situação educacional entre os países, identificando as regiões com melhores índices e as disparidades espaciais."
                              ))
                    )
                )
            )
        )
      )
    )
    ,
    
    
    tabPanel("Sobre as variáveis",
             fluidPage(
               div(class = "sobre-container",
                   div(class = "sobre-square",
                       div(class = "sobre-top-color"),
                       div(class = "sobre-text-titulo", "Variáveis presentes na análise:"),
                       div(class = "sobre-text",
                           fluidRow(
                             # Coluna 1
                             column(width = 4,
                                    tags$ul(class = "sobre-list",
                                            tags$li(HTML("<b>Taxa fora da escola - pré-escola masculino</b> (1 a 10 anos)")),
                                            tags$li(HTML("<b>Taxa fora da escola - pré-escola feminino</b> (1 a 10 anos)")),
                                            tags$li(HTML("<b>Taxa fora da escola - ensino primário masculino</b> (6 a 11 anos)")),
                                            tags$li(HTML("<b>Taxa fora da escola - ensino primário feminino</b> (6 a 11 anos)")),
                                            tags$li(HTML("<b>Taxa fora da escola - ensino fundamental 1 masculino</b> (6 a 14 anos)")),
                                            tags$li(HTML("<b>Taxa fora da escola - ensino fundamental 1 feminino</b> (6 a 14 anos)")),
                                            tags$li(HTML("<b>Taxa fora da escola - ensino fundamental 2 masculino</b> (15 a 17 anos)")),
                                            tags$li(HTML("<b>Taxa fora da escola - ensino fundamental 2 feminino</b> (15 a 17 anos)"))
                                    )
                             ),
                             # Coluna 2
                             column(width = 4,
                                    tags$ul(class = "sobre-list",
                                            tags$li(HTML("<b>Taxa de conclusão - primário masculino</b> (6 a 11 anos)")),
                                            tags$li(HTML("<b>Taxa de conclusão - primário feminino</b> (6 a 11 anos)")),
                                            tags$li(HTML("<b>Taxa de conclusão - fundamental 1 masculino</b> (6 a 14 anos)")),
                                            tags$li(HTML("<b>Taxa de conclusão - fundamental 1 feminino</b> (6 a 14 anos)")),
                                            tags$li(HTML("<b>Taxa de conclusão - fundamental 2 masculino</b> (15 a 17 anos)")),
                                            tags$li(HTML("<b>Taxa de conclusão - fundamental 2 feminino</b> (15 a 17 anos)"))
                                    )
                             ),
                             # Coluna 3
                             column(width = 4,
                                    tags$ul(class = "sobre-list",
                                            tags$li(HTML("<b>Proficiência em leitura 2º-3º ano</b>")),
                                            tags$li(HTML("<b>Proficiência em matemática 2º-3º ano</b>")),
                                            tags$li(HTML("<b>Proficiência em leitura final do primário</b>")),
                                            tags$li(HTML("<b>Proficiência em matemática final do primário</b>")),
                                            tags$li(HTML("<b>Proficiência em leitura final do fundamental 1</b>")),
                                            tags$li(HTML("<b>Proficiência em matemática final do fundamental 1</b>")),
                                            tags$li(HTML("<b>Matrícula bruta no primário</b>")),
                                            tags$li(HTML("<b>Matrícula bruta no ensino superior</b>")),
                                            tags$li(HTML("<b>Taxa de alfabetização 15-24 anos masculino</b> (15 a 24 anos)")),
                                            tags$li(HTML("<b>Taxa de alfabetização 15-24 anos feminino</b> (15 a 24 anos)"))
                                    )
                             )
                           )
                       )
                   )
               )
             )
    )
    ,
    
    tabPanel("Mapa",
             fluidPage(     
               sidebarLayout(
                 sidebarPanel(
                   width = 2,
                   style = "background: #90cdf4;", 
                   selectInput("sel_mapa", 
                               label = tags$span(style = "font-weight: bold; font-size: 12px; color: black;", "Selecione uma variável:"),
                               choices = c(
                                 "Taxa fora da escola - pré-escola masculino", "Taxa fora da escola - pré-escola feminino",            
                                 "Taxa fora da escola - ensino primário masculino", "Taxa fora da escola - ensino primário feminino",
                                 "Taxa fora da escola - ensino fundamental 1 masculino", "Taxa fora da escola - ensino fundamental 1 feminino",
                                 "Taxa fora da escola - ensino fundamental 2 masculino", "Taxa fora da escola - ensino fundamental 2 feminino",
                                 "Taxa de conclusão - primário masculino", "Taxa de conclusão - primário feminino",
                                 "Taxa de conclusão - fundamental 1 masculino",  "Taxa de conclusão - fundamental 1 feminino" ,
                                 "Taxa de conclusão - fundamental 2 masculino", "Taxa de conclusão - fundamental 2 feminino",
                                 "Proficiência em leitura 2º-3º ano", "Proficiência em matemática 2º-3º ano",
                                 "Proficiência em leitura final do primário", "Proficiência em matemática final do primário",
                                 "Proficiência em leitura final do fundamental 1", "Proficiência em matemática final do fundamental 1",
                                 "Taxa de alfabetização 15-24 anos masculino", "Taxa de alfabetização 15-24 anos feminino",
                                 "Matrícula bruta no primário",  "Matrícula bruta no ensino superior")),
                   checkboxGroupInput ( "sel_continente", 
                                        label = tags$span(style = "font-weight: bold; font-size: 12px; color: black;", "Selecione um continente:"),
                                        choices = unique(banco_sf$continent),
                                        selected = unique(banco_sf$continent))
                 ),
                 
                 
                 mainPanel(
                   width = 10,
                   fluidRow( 
                     column( width = 3, 
                             div(
                               style = "background-color: #fdfafa; padding: 10px; border-radius: 5px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3); border-top: 7px solid #3182ce; width: 250px; height: 120px;",
                               uiOutput("media_var"))
                     ),
                     column( width = 3, 
                             div(
                               style = "background-color: #fdfafa; padding: 10px; border-radius: 5px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3); border-top: 7px solid #3182ce; width: 250px; height: 120px;",
                               uiOutput("desvio_var"))
                     ),
                     column( width = 3,
                             div(
                               style = "background-color: #fdfafa; padding: 10px; border-radius: 5px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3); border-top: 7px solid #3182ce; width: 250px; height: 120px;",
                               uiOutput("maximo_var"))
                     ),   
                     column( width = 3, 
                             div(
                               style = "background-color: #fdfafa; padding: 10px; border-radius: 5px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3); border-top: 7px solid #3182ce; width: 250px; height: 120px;",
                               uiOutput("minimo_var")))
                   ),
                   br(),
                   fluidRow( 
                     column( width = 12,  
                             leafletOutput(outputId = "map"))
                   )
                 )
               )
             ))
    ,
    
    tabPanel("Comparação feminino x masculino",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   style = "background: #90cdf4;",
                   selectInput("sel_comparacao",
                               label = tags$span(style = "font-weight: bold; font-size: 12px; color: black;", 
                                                 "Selecione a variável:"),
                               choices = c(
                                 "Taxa fora da escola - pré-escola",
                                 "Taxa fora da escola - ensino primário",
                                 "Taxa de conclusão - primário",
                                 "Taxa de conclusão - fundamental 1",
                                 "Taxa de conclusão - fundamental 2",
                                 "Taxa de alfabetização 15-24 anos"
                               )),
                   checkboxGroupInput("sel_cont_comparacao",
                                      label = tags$span(style = "font-weight: bold; font-size: 12px; color: black;", 
                                                        "Selecione continentes:"),
                                      choices = unique(banco_sf$continent),
                                      selected = unique(banco_sf$continent))
                 ),
                 mainPanel(
                   width = 9,
                   plotlyOutput("grafico_comparacao", height = "500px")
                 )
               )
             ))
  )
)


server <- function(input, output) {
  
  # Mapa:
  
  output$map <- renderLeaflet({
    req(input$sel_mapa)
    
    intervalos_jenks = classInt::classIntervals(var = banco_sf[[input$sel_mapa]], n = 7, style = "jenks")
    
    intervalos_jenks[["brks"]][[2]] = 1
    
    paleta_variavel = colorBin(palette="PuBu", 
                                   domain = banco_sf[[input$sel_mapa]],
                                   bins = intervalos_jenks[["brks"]], 
                                   reverse = FALSE)
    
    leaflet(data = banco_sf) %>% 
      addPolygons(color = 'gray', 
                  weight = 1, 
                  fillColor = ~ paleta_variavel(banco_sf[[input$sel_mapa]]),
                  fillOpacity = 0.6,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "white",
                    fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  label = ~paste(
                    "País: ", "<b>", `Países e áreas`, "</b> ", "<br>",
                    input$sel_mapa, ":<b>", round(banco_sf[[input$sel_mapa]], 4), "</b>"
                  ) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("background-color" = "rgba(144, 205, 244, 0.5)",
                                 "font-weight" = "normal", 
                                 "font-size" = "12px", 
                                 "padding" = "3px"),
                    textsize = "15px",
                    direction = "auto"
                  )) %>%
      addLegend(pal = paleta_variavel, 
                values = ~banco_sf[[input$sel_mapa]], 
                title = paste(input$sel_mapa),
                opacity = 0.8) %>%
      addTiles("https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png") %>%  
      setView(lng = 0, lat = 0.7, zoom = 2)
  })
  
  observe({
    req(input$sel_mapa, input$sel_continente)
    paleta_variavel = colorNumeric(palette = "PuBu", domain = banco_sf[[input$sel_mapa]]) 
    leafletProxy("map", data = banco_sf) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ifelse(continent %in% input$sel_continente,
                            paleta_variavel(banco_sf[[input$sel_mapa]]),
                            "gray"),
        color = 'gray',
        weight = 1, 
        fillOpacity = 0.6,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 0.5,
          bringToFront = TRUE),
        label = ~paste(
          "País: ", "<b>", `Países e áreas`, "</b> ", "<br>",
          input$sel_mapa,  ":<b>", round(banco_sf[[input$sel_mapa]], 4), "</b>"
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("background-color" = "rgba(144, 205, 244, 0.5)",
                       "font-weight" = "normal", 
                       "font-size" = "12px", 
                       "padding" = "3px"),
          textsize = "15px",
          direction = "auto"
        )
      ) })
  
  dados_filtrados <- reactive ({
    req(input$sel_continente)
    subset(banco_sf, continent %in% input$sel_continente)
  })
  
  # Caixas com as medidas descritivas:
  output$media_var <- renderUI({
    media_variavel = mean(dados_filtrados()[[input$sel_mapa]], na.rm = TRUE)
    tags$div(
      tags$h4(style = "text-align: center; font-size: 18px;",
              paste("Média")),
      tags$h4(style = "font-weight: bold; text-align: center; color: black;font-size: 14px;",
              paste(input$sel_mapa)),
      tags$h4(style = "text-align: center;font-size: 18px;",
              round(media_variavel, 3))
    )
  })
  
  output$desvio_var <- renderUI({
    desvio_variavel = sd(dados_filtrados()[[input$sel_mapa]], na.rm = TRUE)
    tags$div(
      tags$h4(style = "text-align: center; font-size: 18px;",
              paste("Desvio Padrão")),
      tags$h4(style = "font-weight: bold; text-align: center;font-size: 14px;",
              paste(input$sel_mapa)),
      tags$h4(style = "text-align: center;font-size: 18px;",
              round(desvio_variavel, 3))
    )
  })
  
  output$maximo_var <- renderUI({
    maximo_variavel = max(dados_filtrados()[[input$sel_mapa]], na.rm = TRUE)
    tags$div(
      tags$h4(style = "text-align: center;font-size: 18px;",
              paste("Máximo")),
      tags$h4(style = "font-weight: bold; text-align: center;font-size: 14px;",
              paste(input$sel_mapa)),
      tags$h4(style = "text-align: center;font-size: 18px;",
              round(maximo_variavel, 3))
    )
  })
  
  output$minimo_var <- renderUI({
    minimo_variavel = min(dados_filtrados()[[input$sel_mapa]], na.rm = TRUE)
    tags$div(
      tags$h4(style = "text-align: center;font-size: 18px;",
              paste("Mínimo")),
      tags$h4(style = "font-weight: bold; text-align: center;font-size: 14px;",
              paste(input$sel_mapa)),
      tags$h4(style = "text-align: center;font-size: 18px;",
              round(minimo_variavel, 3))
    )
  })
  
  # Gráfico Comparação Meninos x Meninas
  output$grafico_comparacao <- renderPlotly({
    req(input$sel_comparacao, input$sel_cont_comparacao)
    
    dados_filtrados_comparacao <- subset(banco_sf, continent %in% input$sel_cont_comparacao)
    
    var_masc <- paste(input$sel_comparacao, "masculino")
    var_fem  <- paste(input$sel_comparacao, "feminino")
    
    df_long <- data.frame(
      Continente = dados_filtrados_comparacao$continent,
      Masculino  = dados_filtrados_comparacao[[var_masc]],
      Feminino   = dados_filtrados_comparacao[[var_fem]]
    ) %>%
      tidyr::pivot_longer(cols = c("Masculino", "Feminino"),
                          names_to = "Sexo", values_to = "Valor") %>%
      dplyr::group_by(Continente, Sexo) %>%
      dplyr::summarise(Media = mean(Valor, na.rm = TRUE), .groups = "drop")

    
    grafico <- ggplot(df_long, aes(
      x = Continente,
      y = Media,
      fill = Sexo,
      text = paste("Média:", round(Media, 2))
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(
        title = paste("Comparação feminino x masculino:", input$sel_comparacao),
        y = "Média", x = "Continente"
      ) +
      scale_fill_manual(values = c("Masculino" = "darkblue", "Feminino" = "#f883b3")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
      )
    
    ggplotly(grafico, tooltip = "text")
    
    
  })
  }


shinyApp(ui = ui, server = server)

