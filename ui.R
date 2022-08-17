library(shiny)
library(shinydashboard)
library(magrittr)

# Teste Interface seca.
# To do list:
# Criar os arquivos de idiomas
# Colocar as notações estatísticas ex:  E[Zt]= Sum(Zt*P(T=t))
# Criar um botão de help, para tornar a ferramenta mais independente
# Separar os arquivos


dashboardPage(
    dashboardHeader(title = "Lar"), #Cabeçalho
    dashboardSidebar( #Menu Lateral
        fileInput("exp", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        #Inputs condicionados às abas que se encontram no corpo do código
      selectInput("tab", "Selecione a tábua de vida", choices = c("ALLG 72" =  "ALLG.72",
                                                                  "AMERICAN EXPERIENCE" = "AMERICAN.EXPERIENCE",
                                                                  "AT2000 Suavizada 10 _FEM" = "AT2000..Suavizada.10.._FEM",
                                                                  "AT2000 Suavizada 10 _MAS" = "AT2000..Suavizada.10.._MAS",
                                                                  "AT 2000 FEMALE" = "AT.2000.FEMALE",
                                                                  "AT 2000 MALE" = "AT.2000.MALE",
                                                                  "AT 49 FEMALE" = "AT.49.FEMALE",
                                                                  "AT 49 MALE" =  "AT.49.MALE",
                                                                  "AT 50" =  "AT.50",
                                                                  "AT 55" =  "AT.55",
                                                                  "AT 71" =  "AT.71",
                                                                  "AT 83 FEMALE Basic " = "AT.83.FEMALE..Basic.",
                                                                  "AT 83 FEMALE IAM " = "AT.83.FEMALE..IAM.",
                                                                  "AT 83 MALE Basic " = "AT.83.MALE..Basic.",
                                                                  "AT 83 MALE IAM " = "AT.83.MALE..IAM.",
                                                                  "BR EMSmt v 2010 f" = "BR.EMSmt.v.2010.f",
                                                                  "BR EMSmt v 2010 m" = "BR.EMSmt.v.2010.m",
                                                                  "BR EMSsb v 2010 f" = "BR.EMSsb.v.2010.f",
                                                                  "BR EMSsb v 2010 m" = "BR.EMSsb.v.2010.m",
                                                                  "BR EMSmt v 2015 f" = "BR.EMSmt.v.2015.f",
                                                                  "BR EMSmt v 2015 m" = "BR.EMSmt.v.2015.m",
                                                                  "BR EMSsb v 2015 f" = "BR.EMSsb.v.2015.f",
                                                                  "BR EMSsb v 2015 m" = "BR.EMSsb.v.2015.m",
                                                                  "BR EMSmt v 2021 f" = "BR.EMSmt.v.2021.f",
                                                                  "BR EMSmt v 2021 m" = "BR.EMSmt.v.2021.m",
                                                                  "BR EMSsb v 2021 f" = "BR.EMSsb.v.2021.f",
                                                                  "BR EMSsb v 2021 m" = "BR.EMSsb.v.2021.m",
                                                                  "CSG 60" =  "CSG.60",
                                                                  "CSO 41" =  "CSO.41",
                                                                  "CSO 58" =  "CSO.58",
                                                                  "CSO58 FEM AGE LAST" = "CSO58...FEM...AGE.LAST",
                                                                  "CSO58 FEM AGE NEAREST" = "CSO58...FEM...AGE.NEAREST",
                                                                  "CSO58 MAS AGE LAST" = "CSO58...MAS...AGE.LAST",
                                                                  "CSO58 MAS AGE NEAREST" = "CSO58...MAS...AGE.NEAREST",
                                                                  "CSO58 FEMALE" = "CSO58.FEMALE",
                                                                  "CSO58 MALE" =  "CSO58.MALE",
                                                                  "CSO80" =  "CSO80",
                                                                  "EB7 75" =  "EB7.75",
                                                                  "GAM 71 FEMALE" = "GAM.71.FEMALE",
                                                                  "GAM 71 MALE" =  "GAM.71.MALE",
                                                                  "GAM 83 FEMALE suav 10 " = "GAM.83...FEMALE..suav.10..",
                                                                  "GAM83_BASICA FEMALE" = "GAM83_BASICA...FEMALE",
                                                                  "GAM83_BASICA MASC" = "GAM83_BASICA...MASC",
                                                                  "GAM 83 MASC suav 10 " = "GAM.83...MASC..suav.10..",
                                                                  "GAM 94 FEMALE" = "GAM.94.FEMALE",
                                                                  "GAM 94MALE" =  "GAM.94MALE",
                                                                  "IBGE 2006 Ambos os Sexos" = "IBGE.2006..............Ambos.os.Sexos",
                                                                  "IBGE 2007 Ambos os Sexos" = "IBGE.2007.................Ambos.os.Sexos",
                                                                  "IBGE 2008 Ambos os Sexos" = "IBGE.2008.................Ambos.os.Sexos",
                                                                  "IBGE 2009 Ambos os Sexos" = "IBGE.2009.................Ambos.os.Sexos")),

        numericInput("alfa", "Nível de Significância", min = 0, max = 1, value = 0.01, step = 0.01 ),
        # sliderInput('rangeIdades', 'Idades', ticks=FALSE, width="100%", min = 0,
        #             max = 120, value = c(0, 120), sep=''),
        #tags$img(src="github.png",width=150, align="middle"),
        tags$a( href = "http://unifal-mg.edu.br/lar/",
                tags$img(src="LAR.png",width=150, align="middle")),
        tags$a( href = "http://unifal-mg.edu.br/portal/",
                tags$img(src="UNIFAL-MG.png",width=150, align="middle"))


    ),
    
    dashboardBody( #Corpo da página
        #Abas usadas para organizar a página por produtos e chamar a saída respectiva para o mesmo

        tags$head(tags$link(rel = "stylesheet",
                       type = "text/css",
                       href = "styles.css")),

        fluidRow(
          tabsetPanel(type = "tab",
                      tabPanel("Tábua Selecionada", icon=icon("table"),
                               box(
                                 title = "Visão da Tabela", status = "primary", #solidHeader = TRUE,
                                 collapsible = TRUE,
                                 tableOutput("infoExp") %>% 
                                   tagAppendAttributes(style= 'overflow-y: auto; height: 40vh;')
                               ),
                               box(
                                 title = "Comparação da experiência Tábua selecionada", status = "primary", #solidHeader = TRUE,
                                 collapsible = TRUE,
                                 tableOutput("expTabuas") %>% 
                                   tagAppendAttributes(style= 'overflow-y: auto; height: 40vh;')
                               ) , value = 1
                      ),
                      tabPanel("Testes de Adêrencia", icon=icon("chart-line"),
                               box(
                                 title = "Relatório", status = "primary", #solidHeader = TRUE,
                                 collapsible = TRUE,
                                 verbatimTextOutput("reportAderencia") %>% 
                                   tagAppendAttributes(style= 'overflow-y: auto; height: 40vh;')),          
                               box(
                                 title = "Rank das Tábuas por Desvios do KS", status = "primary", 
                                 tableOutput("reportKS2") %>% 
                                   tagAppendAttributes(style= 'overflow-y: auto; height: 40vh;')
                               ), value = 2
                      ),
                      # tabPanel("Teste Qui-Quadrado", icon=icon("chart-line"),
                      #          box(
                      #            title = "Relatório", status = "primary", #solidHeader = TRUE,
                      #            collapsible = TRUE,
                      #            verbatimTextOutput("chiSqrTest") %>% 
                      #              tagAppendAttributes(style= 'overflow-y: auto; height: 40vh;')), value = 3
                      # ),
                      id = "abaselecionada")#,
          # box(
          # 
          #   print('a')
          # )
        ),
        
        # conditionalPanel(condition = "input.abaselecionada==1",
        #                  fluidRow(
        #                    box(width=12,
        #                        title = "Teste 1", status = "primary", #solidHeader = TRUE,
        #                        collapsible = TRUE,
        #                        plotOutput("plot3")
        #                    )
        #                  )),
        #fluidRow(
          #conditionalPanel(condition = "input.abaselecionada==1",
         fluidRow(
           box(#width=12,
               title = "Tábua selecionada x Experiência (log)", status = "primary", #solidHeader = TRUE,
               collapsible = TRUE,
               plotOutput("plotTabuas")
           ),
           box(#width=12,
             title = "Experiência - Tábua", status = "primary", #solidHeader = TRUE,
             collapsible = TRUE,
             plotOutput("plotTabuasDif")
           )
         
        )#,
        # fluidRow(
        #   tags$a( href = "http://unifal-mg.edu.br/lar/",
        #       tags$img(src="LAR.png",width=150, align="middle")),
        #   tags$a( href = "http://unifal-mg.edu.br/portal/",
        #       tags$img(src="UNIFAL-MG.png",width=150, align="middle")),
        #   tags$a( href = "https://github.com/walefmachado/interface-atuarial/",
        #       tags$img(src="github.png",width=100, align="right"))
        #   
        # )
        
        #,
        # box(
        #   radioButtons(inputId = "premio", label = "Prêmio", choices= c("Puro Único"=1, "Nivelado pela duração do produto"=2, "Nivelado Personalizado"=3)),
        #     conditionalPanel(condition = "input.premio==3",
        #                      numericInput("npremio", "Periodo de pagamento", min = 0, max = (nrow(dados)-1), value = 1, step = 1))
        # )
        
        #plotlyOutput("plot"), #Saída do gráfico definida pelo UI
    )
)
